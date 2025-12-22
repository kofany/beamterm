use cosmic_text::{FontSystem, Metrics, SwashCache};

use crate::error::Error;

/// Runtime font system for dynamic glyph rasterization.
///
/// This manages the cosmic-text FontSystem and SwashCache needed for
/// runtime glyph rendering. It handles font loading and provides
/// the necessary components for glyph rasterization.
pub struct RuntimeFontSystem {
    /// The cosmic-text font system for font loading and text shaping
    pub(crate) font_system: FontSystem,
    /// Cache for glyph rendering performance
    pub(crate) cache: SwashCache,
    /// Font metrics (size, line height)
    pub(crate) metrics: Metrics,
    /// The font family name currently in use
    font_family_name: String,
    /// Emoji font family name (e.g., "Noto Color Emoji")
    emoji_font_family_name: String,
    /// Font size in points
    font_size: f32,
    /// Line height multiplier (e.g., 1.2 for 120% line height)
    line_height: f32,
}

impl RuntimeFontSystem {
    /// Creates a new RuntimeFontSystem with the specified font configuration.
    ///
    /// # Arguments
    ///
    /// * `font_family_name` - The font family name to use (e.g., "JetBrains Mono Nerd Font")
    /// * `emoji_font_family_name` - Emoji font family name (defaults to "Noto Color Emoji")
    /// * `font_size` - Font size in points
    /// * `line_height` - Line height multiplier (defaults to 1.2)
    ///
    /// # Returns
    ///
    /// A new RuntimeFontSystem instance, or an error if the font cannot be loaded
    pub fn new(
        font_family_name: String,
        emoji_font_family_name: String,
        font_size: f32,
        line_height: f32,
    ) -> Result<Self, Error> {
        let mut font_system = FontSystem::new();
        let cache = SwashCache::new();
        let metrics = Metrics::new(font_size, font_size * line_height);

        // TODO: Load the specified font family
        // For now, we'll rely on system fonts or pre-loaded fonts
        // In WASM, system fonts are limited, so fonts should be loaded via load_font_data()

        Ok(Self {
            font_system,
            cache,
            metrics,
            font_family_name,
            emoji_font_family_name,
            font_size,
            line_height,
        })
    }

    /// Returns the font family name
    pub fn font_family_name(&self) -> &str {
        &self.font_family_name
    }

    /// Returns the emoji font family name
    pub fn emoji_font_family_name(&self) -> &str {
        &self.emoji_font_family_name
    }

    /// Returns the font size in points
    pub fn font_size(&self) -> f32 {
        self.font_size
    }

    /// Returns the line height multiplier
    pub fn line_height(&self) -> f32 {
        self.line_height
    }

    /// Returns a mutable reference to the font system (for loading fonts)
    pub fn font_system_mut(&mut self) -> &mut FontSystem {
        &mut self.font_system
    }

    /// Returns a mutable reference to the cache (for glyph rendering)
    pub fn cache_mut(&mut self) -> &mut SwashCache {
        &mut self.cache
    }

    /// Returns the metrics
    pub fn metrics(&self) -> Metrics {
        self.metrics
    }

    /// Updates the font size and recalculates metrics
    pub fn set_font_size(&mut self, font_size: f32) {
        self.font_size = font_size;
        self.metrics = Metrics::new(font_size, font_size * self.line_height);
    }

    /// Loads font data from bytes into the font system.
    ///
    /// This is useful for embedding fonts or loading fonts from Tauri backend.
    ///
    /// # Arguments
    ///
    /// * `font_data` - The font file data (TTF/OTF bytes)
    ///
    /// Note: load_font_data doesn't return the font ID. After loading,
    /// you need to search for the font by family name to use it.
    pub fn load_font_data(&mut self, font_data: Vec<u8>) {
        let db = self.font_system.db_mut();
        db.load_font_data(font_data);
    }
}

