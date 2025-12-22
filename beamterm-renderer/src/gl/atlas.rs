use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use beamterm_data::{FontAtlasData, FontStyle, Glyph};
use compact_str::{CompactString, ToCompactString};
use web_sys::console;

use crate::error::Error;
use crate::font::{
    glyph_bounds::GlyphBounds,
    glyph_cache::{GlyphCache, GlyphKey, GlyphRender},
    glyph_rasterizer::GlyphRasterizer,
    runtime_font_system::RuntimeFontSystem,
};

use super::GL;

/// A texture atlas containing font glyphs for efficient WebGL text rendering.
///
/// `FontAtlas` manages a WebGL 2D texture array where each layer contains a single
/// character glyph. This design enables efficient instanced rendering of text by
/// allowing the GPU to select the appropriate character layer for each rendered cell.
///
/// # Architecture
/// The atlas uses a **WebGL 2D texture array** where:
/// - Each layer contains one character glyph
/// - ASCII characters use their ASCII value as the layer index
/// - Non-ASCII characters are stored in a hash map for layer lookup
/// - All glyphs have uniform cell dimensions for consistent spacing
///
/// # Dynamic vs Static Atlas
/// - Static atlas: Loaded from pre-generated FontAtlasData (existing behavior)
/// - Dynamic atlas: Rasterizes glyphs on-demand using RuntimeFontSystem
#[derive(Debug)]
pub struct FontAtlas {
    /// The underlying texture
    texture: crate::gl::texture::Texture,
    /// Symbol to 3d texture index
    glyph_coords: HashMap<CompactString, u32>,
    /// Base glyph identifier to symbol mapping
    symbol_lookup: HashMap<u32, CompactString>,
    /// The size of each character cell in pixels
    cell_size: (i32, i32),
    /// The number of slices in the atlas texture
    num_slices: u32,
    /// Underline configuration
    underline: beamterm_data::LineDecoration,
    /// Strikethrough configuration  
    strikethrough: beamterm_data::LineDecoration,
    /// Tracks glyphs that were requested but not found in the atlas
    glyph_tracker: GlyphTracker,
    /// The last assigned halfwidth base glyph ID, before fullwidth
    last_halfwidth_base_glyph_id: u32,
    /// Dynamic rendering state (None for static atlas)
    dynamic_state: Option<DynamicAtlasState>,
}

/// State for dynamic atlas rendering
#[derive(Debug)]
struct DynamicAtlasState {
    /// Font system for runtime glyph rasterization
    font_system: Rc<RefCell<RuntimeFontSystem>>,
    /// Cache for rendered glyphs
    glyph_cache: GlyphCache,
    /// Next available glyph ID for allocation (base ID, without style flags)
    next_glyph_id: u32,
    /// Glyph bounds (cell dimensions)
    glyph_bounds: GlyphBounds,
}

impl FontAtlas {
    /// Loads the default embedded font atlas.
    pub fn load_default(gl: &web_sys::WebGl2RenderingContext) -> Result<Self, Error> {
        let config = FontAtlasData::default();
        Self::load(gl, config)
    }

    /// Creates a TextureAtlas from a grid of equal-sized cells
    pub fn load(
        gl: &web_sys::WebGl2RenderingContext,
        config: FontAtlasData,
    ) -> Result<Self, Error> {
        let texture = crate::gl::texture::Texture::from_font_atlas_data(gl, GL::RGBA, &config)?;
        let num_slices = config.texture_dimensions.2;

        let texture_layers = config
            .glyphs
            .iter()
            .map(|g| g.id as i32)
            .max()
            .unwrap_or(0)
            + 1;

        let (cell_width, cell_height) = config.cell_size;
        let mut layers = HashMap::new();
        let mut symbol_lookup = HashMap::new();

        // we only store the normal-styled glyphs (incl emoji) in the atlas lookup,
        // as the correct layer id can be derived from the base glyph id plus font style.
        //
        // emoji are (currently all) double-width and occupy two consecutive glyph ids,
        // but we only store the first id in the lookup.
        config.glyphs.iter()
            .filter(|g| g.style == FontStyle::Normal) // only normal style glyphs
            .filter(|g| !g.is_ascii())                // only non-ascii glyphs
            .for_each(|g| {
                symbol_lookup.insert(g.id, g.symbol.clone());
                layers.insert(g.symbol.clone(), g.id);
            });

        Ok(Self {
            texture,
            glyph_coords: layers,
            last_halfwidth_base_glyph_id: config.max_halfwidth_base_glyph_id,
            symbol_lookup,
            cell_size: (cell_width, cell_height),
            num_slices: num_slices as u32,
            underline: config.underline,
            strikethrough: config.strikethrough,
            glyph_tracker: GlyphTracker::new(),
            dynamic_state: None, // Static atlas
        })
    }

    /// Creates a new dynamic font atlas that rasterizes glyphs on-demand.
    ///
    /// # Arguments
    /// * `gl` - WebGL2 context
    /// * `font_system` - Runtime font system for glyph rasterization
    /// * `font_size` - Font size in pixels
    /// * `line_height` - Line height multiplier (e.g., 1.2)
    ///
    /// # Returns
    /// A new `FontAtlas` configured for dynamic rendering
    pub fn new_dynamic(
        gl: &web_sys::WebGl2RenderingContext,
        font_system: Rc<RefCell<RuntimeFontSystem>>,
        font_size: f32,
        line_height: f32,
    ) -> Result<Self, Error> {
        // Calculate glyph bounds by measuring a block character
        let glyph_bounds = Self::calculate_glyph_bounds(font_system.clone(), font_size, line_height)?;

        // Calculate texture dimensions
        // Each layer contains 32 glyphs (1 column x 32 rows)
        const GLYPHS_PER_SLICE: i32 = 32;
        const GRID_WIDTH: i32 = 1;
        const GRID_HEIGHT: i32 = 32;

        let cell_width = glyph_bounds.width_with_padding();
        let cell_height = glyph_bounds.height_with_padding();

        let slice_width = GRID_WIDTH * cell_width;
        let slice_height = GRID_HEIGHT * cell_height;

        // Initial texture size: 32 layers (1024 glyphs capacity)
        let initial_layers = 32;
        let texture_width = slice_width;
        let texture_height = slice_height;

        // Create empty texture
        let texture = crate::gl::texture::Texture::new_empty(
            gl,
            GL::RGBA,
            texture_width,
            texture_height,
            initial_layers,
        )?;

        // Start allocating glyph IDs after ASCII range (0x80)
        // This leaves 0x00-0x7F for ASCII direct mapping
        let next_glyph_id = 0x80;

        // Calculate underline and strikethrough positions (simplified, can be improved)
        let underline = beamterm_data::LineDecoration {
            position: 0.8, // 80% of cell height
            thickness: 1.0,
        };
        let strikethrough = beamterm_data::LineDecoration {
            position: 0.5, // 50% of cell height
            thickness: 1.0,
        };

        Ok(Self {
            texture,
            glyph_coords: HashMap::new(),
            symbol_lookup: HashMap::new(),
            cell_size: (cell_width, cell_height),
            num_slices: initial_layers as u32,
            underline,
            strikethrough,
            glyph_tracker: GlyphTracker::new(),
            last_halfwidth_base_glyph_id: next_glyph_id - 1, // Will be updated as glyphs are added
            dynamic_state: Some(DynamicAtlasState {
                font_system,
                glyph_cache: GlyphCache::new(),
                next_glyph_id,
                glyph_bounds,
            }),
        })
    }

    /// Calculates glyph bounds by rasterizing a block character (U+2588).
    ///
    /// This provides a simple estimate of cell dimensions. For optimal results,
    /// use the full optimization from beamterm-atlas, but for dynamic atlas
    /// this is sufficient for initial implementation.
    fn calculate_glyph_bounds(
        font_system: Rc<RefCell<RuntimeFontSystem>>,
        font_size: f32,
        line_height: f32,
    ) -> Result<GlyphBounds, Error> {
        use cosmic_text::Metrics;

        let mut font_system_borrow = font_system.borrow_mut();
        let metrics = Metrics::new(font_size, font_size * line_height);

        // Rasterize block character to measure bounds
        let font_family_name = font_system_borrow.font_family_name().to_string();
        let mut rasterizer = GlyphRasterizer::new("\u{2588}") // Full block
            .font_family_name(&font_family_name)
            .font_style(beamterm_data::FontStyle::Normal);

        // Rasterize - this borrows font_system mutably
        let mut buffer = rasterizer.rasterize(font_system_borrow.font_system_mut(), metrics)?;
        
        // Drop the first borrow so we can borrow again
        drop(font_system_borrow);
        
        // Now borrow the entire RuntimeFontSystem again
        // SAFETY: We need to borrow font_system for buffer.borrow_with() AND cache for measure_glyph_bounds.
        // These are separate fields, so it's safe to use unsafe to get both references.
        let mut font_system_borrow2 = font_system.borrow_mut();
        
        // Get raw pointers to both fields
        let font_system_ptr: *mut cosmic_text::FontSystem = &mut font_system_borrow2.font_system;
        let cache_ptr: *mut cosmic_text::SwashCache = &mut font_system_borrow2.cache;
        
        // Now we can use both through raw pointers
        let mut buffer_borrowed = buffer.borrow_with(unsafe { &mut *font_system_ptr });
        let bounds = crate::font::glyph_bounds::measure_glyph_bounds(
            &mut buffer_borrowed,
            unsafe { &mut *cache_ptr },
        );
        
        // Explicitly drop buffer_borrowed before font_system_borrow2 goes out of scope
        drop(buffer_borrowed);

        Ok(bounds)
    }

    /// Binds the atlas texture to the specified texture unit
    pub fn bind(&self, gl: &web_sys::WebGl2RenderingContext, texture_unit: u32) {
        self.texture.bind(gl, texture_unit);
    }

    pub fn cell_size(&self) -> (i32, i32) {
        let (w, h) = self.cell_size;
        (
            w - 2 * FontAtlasData::PADDING,
            h - 2 * FontAtlasData::PADDING,
        )
    }

    /// Returns the underline configuration
    pub fn underline(&self) -> beamterm_data::LineDecoration {
        self.underline
    }

    /// Returns the strikethrough configuration
    pub fn strikethrough(&self) -> beamterm_data::LineDecoration {
        self.strikethrough
    }

    /// Returns the symbol for the given glyph ID, if it exists
    pub fn get_symbol(&self, glyph_id: u32) -> Option<Cow<'_, str>> {
        let base_glyph_id = if glyph_id & Glyph::EMOJI_FLAG != 0 {
            glyph_id & Glyph::GLYPH_ID_EMOJI_MASK
        } else {
            glyph_id & Glyph::GLYPH_ID_MASK
        };

        if (0x20..0x80).contains(&(base_glyph_id as u16)) {
            // ASCII characters are directly mapped to their code point
            let ch = base_glyph_id as u8 as char;
            Some(Cow::from(ch.to_compact_string()))
        } else {
            self.symbol_lookup
                .get(&base_glyph_id)
                .map(|s| Cow::from(s.as_str()))
        }
    }

    /// Returns the base glyph identifier for the given key
    pub fn get_base_glyph_id(&self, key: &str) -> Option<u32> {
        if key.len() == 1 {
            let ch = key.chars().next().unwrap();
            if ch.is_ascii() {
                // 0x00..0x7f double as layer
                let id = ch as u32;
                return Some(id);
            }
        }

        match self.glyph_coords.get(key) {
            Some(id) => Some(*id),
            None => {
                self.glyph_tracker.record_missing(key);
                None
            },
        }
    }

    /// Returns the maximum assigned halfwidth base glyph ID.
    pub fn get_max_halfwidth_base_glyph_id(&self) -> u32 {
        self.last_halfwidth_base_glyph_id
    }

    /// Returns a reference to the glyph tracker for accessing missing glyphs.
    pub fn glyph_tracker(&self) -> &GlyphTracker {
        &self.glyph_tracker
    }

    /// Returns the total number of glyphs available in the atlas.
    /// This includes ASCII characters (0x20..0x80) plus non-ASCII glyphs.
    pub(crate) fn glyph_count(&self) -> u32 {
        // ASCII printable characters: 0x20..0x80 (96 characters)
        let ascii_count = 0x80 - 0x20;
        // Non-ASCII glyphs stored in symbol_lookup
        let non_ascii_count = self.symbol_lookup.len() as u32;
        ascii_count + non_ascii_count
    }

    pub(crate) fn get_symbol_lookup(&self) -> &HashMap<u32, CompactString> {
        &self.symbol_lookup
    }
}

/// Tracks glyphs that were requested but not found in the font atlas.
#[derive(Debug, Default)]
pub struct GlyphTracker {
    missing: RefCell<HashSet<CompactString>>,
}

impl GlyphTracker {
    /// Creates a new empty glyph tracker.
    pub fn new() -> Self {
        Self { missing: RefCell::new(HashSet::new()) }
    }

    /// Records a glyph as missing.
    pub fn record_missing(&self, glyph: &str) {
        self.missing.borrow_mut().insert(glyph.into());
    }

    /// Returns a copy of all missing glyphs.
    pub fn missing_glyphs(&self) -> HashSet<CompactString> {
        self.missing.borrow().clone()
    }

    /// Clears all tracked missing glyphs.
    pub fn clear(&self) {
        self.missing.borrow_mut().clear();
    }

    /// Returns the number of unique missing glyphs.
    pub fn len(&self) -> usize {
        self.missing.borrow().len()
    }

    /// Returns true if no glyphs are missing.
    pub fn is_empty(&self) -> bool {
        self.missing.borrow().is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glyph_tracker() {
        let tracker = GlyphTracker::new();

        // Initially empty
        assert!(tracker.is_empty());
        assert_eq!(tracker.len(), 0);

        // Record some missing glyphs
        tracker.record_missing("🎮");
        tracker.record_missing("🎯");
        tracker.record_missing("🎮"); // Duplicate

        assert!(!tracker.is_empty());
        assert_eq!(tracker.len(), 2); // Only unique glyphs

        // Check the missing glyphs
        let missing = tracker.missing_glyphs();
        assert!(missing.contains(&CompactString::new("🎮")));
        assert!(missing.contains(&CompactString::new("🎯")));

        // Clear and verify
        tracker.clear();
        assert!(tracker.is_empty());
        assert_eq!(tracker.len(), 0);
    }
}
