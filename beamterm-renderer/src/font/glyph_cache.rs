use std::collections::HashMap;

use beamterm_data::FontStyle;
use compact_str::{CompactString, ToCompactString};

/// Key for identifying a cached glyph in the glyph cache.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlyphKey {
    /// The symbol/character string
    pub symbol: CompactString,
    /// The font style (Normal, Bold, Italic, BoldItalic)
    pub style: FontStyle,
}

impl std::hash::Hash for GlyphKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
        // FontStyle is Copy, so we can hash it as u8
        (self.style as u8).hash(state);
    }
}

impl GlyphKey {
    pub fn new(symbol: &str, style: FontStyle) -> Self {
        Self {
            symbol: symbol.to_compact_string(),
            style,
        }
    }
}

/// Cached glyph render information.
#[derive(Debug, Clone)]
pub struct GlyphRender {
    /// The glyph ID assigned to this glyph
    pub glyph_id: u32,
}

/// Cache for rendered glyphs.
///
/// This cache stores glyph render information to avoid re-rasterizing
/// the same glyph multiple times. The cache maps (symbol, style) pairs
/// to their assigned glyph IDs.
#[derive(Debug)]
pub struct GlyphCache {
    cache: HashMap<GlyphKey, GlyphRender>,
}

impl GlyphCache {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    /// Gets the cached glyph render for the given key, if it exists.
    pub fn get(&self, key: &GlyphKey) -> Option<&GlyphRender> {
        self.cache.get(key)
    }

    /// Inserts a new glyph render into the cache.
    pub fn insert(&mut self, key: GlyphKey, render: GlyphRender) {
        self.cache.insert(key, render);
    }

    /// Returns the number of cached glyphs.
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Returns true if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }
}

impl Default for GlyphCache {
    fn default() -> Self {
        Self::new()
    }
}

