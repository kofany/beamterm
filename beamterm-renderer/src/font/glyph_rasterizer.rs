use beamterm_data::FontStyle;
use cosmic_text::{Attrs, Family, Metrics, Style, Weight, Buffer, FontSystem};

use crate::error::Error;

/// Builder for rasterizing individual glyphs using cosmic-text
#[derive(Debug)]
pub struct GlyphRasterizer<'a> {
    symbol: &'a str,
    font_family_name: Option<&'a str>,
    font_style: FontStyle,
    monospace_width: Option<u32>,
    buffer_size: Option<(f32, f32)>,
}

impl<'a> GlyphRasterizer<'a> {
    pub fn new(symbol: &'a str) -> Self {
        Self {
            symbol,
            font_family_name: None,
            font_style: FontStyle::Normal,
            monospace_width: None,
            buffer_size: None,
        }
    }

    pub fn font_family_name(mut self, font_family_name: &'a str) -> Self {
        self.font_family_name = Some(font_family_name);
        self
    }

    pub fn font_style(mut self, font_style: FontStyle) -> Self {
        self.font_style = font_style;
        self
    }

    pub fn monospace_width(mut self, width: u32) -> Self {
        self.monospace_width = Some(width);
        self
    }

    pub fn buffer_size(mut self, width: f32, height: f32) -> Self {
        self.buffer_size = Some((width, height));
        self
    }

    pub fn rasterize(
        self,
        font_system: &mut FontSystem,
        metrics: Metrics,
    ) -> Result<Buffer, Error> {
        use web_sys::console;
        
        let font_family_name = self
            .font_family_name
            .ok_or_else(|| Error::Data("font family name must be set before rasterizing".to_string()))?;

        console::log_1(&format!("[beamterm] GlyphRasterizer::rasterize: symbol='{}', font_family='{}'", self.symbol, font_family_name).into());

        let mut buffer = Buffer::new(font_system, metrics);
        let (width, height) = self.buffer_size.unwrap_or((200.0, 200.0));
        buffer.set_size(font_system, Some(width), Some(height));
        buffer.set_monospace_width(font_system, self.monospace_width.map(|w| w as f32));

        let attrs = create_text_attrs(font_family_name, self.font_style);
        buffer.set_text(
            font_system,
            self.symbol,
            &attrs,
            cosmic_text::Shaping::Advanced,
        );
        buffer.shape_until_scroll(font_system, true);
        
        let layout_runs = buffer.layout_runs();
        let run_count: usize = layout_runs.count();
        console::log_1(&format!("[beamterm] Rasterized glyph '{}', layout_runs count: {}", self.symbol, run_count).into());
        if run_count == 0 {
            console::error_1(&format!("[beamterm] Warning: No layout runs after rasterization for '{}' - font may not be found!", self.symbol).into());
        }

        Ok(buffer)
    }
}

/// Creates text attributes for cosmic-text based on font family and style
fn create_text_attrs(font_family: &str, style: FontStyle) -> Attrs<'_> {
    let attrs = Attrs::new()
        .family(Family::Name(font_family))
        .style(Style::Normal)
        .weight(Weight::NORMAL);

    use FontStyle::*;
    match style {
        Normal => attrs,
        Bold => attrs.weight(Weight::BOLD),
        Italic => attrs.style(Style::Italic),
        BoldItalic => attrs.style(Style::Italic).weight(Weight::BOLD),
    }
}

