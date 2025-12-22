use std::{cell::RefCell, rc::Rc};

use beamterm_data::{FontAtlasData, Glyph};
use compact_str::CompactString;
use serde_wasm_bindgen::from_value;
use unicode_segmentation::UnicodeSegmentation;
use wasm_bindgen::prelude::*;
use web_sys::console;

use crate::{
    gl::{
        CellData, CellQuery as RustCellQuery, FontAtlas, Renderer,
        SelectionMode as RustSelectionMode, TerminalGrid, select,
    },
    mouse::{DefaultSelectionHandler, TerminalMouseEvent, TerminalMouseHandler},
};

/// JavaScript wrapper for the terminal renderer
#[wasm_bindgen]
#[derive(Debug)]
pub struct BeamtermRenderer {
    renderer: Renderer,
    terminal_grid: Rc<RefCell<TerminalGrid>>,
    mouse_handler: Option<TerminalMouseHandler>,
}

/// JavaScript wrapper for cell data
#[wasm_bindgen]
#[derive(Debug, Default, serde::Deserialize)]
pub struct Cell {
    symbol: CompactString,
    style: u16,
    fg: u32,
    bg: u32,
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy)]
pub struct CellStyle {
    fg: u32,
    bg: u32,
    style_bits: u16,
}

#[wasm_bindgen]
#[derive(Debug, Clone, Copy)]
pub struct Size {
    pub width: u16,
    pub height: u16,
}

#[wasm_bindgen]
#[derive(Debug)]
pub struct Batch {
    terminal_grid: Rc<RefCell<TerminalGrid>>,
    gl: web_sys::WebGl2RenderingContext,
}

/// Selection mode for text selection in the terminal
#[wasm_bindgen]
#[derive(Debug, Clone, Copy)]
pub enum SelectionMode {
    /// Rectangular block selection
    Block,
    /// Linear text flow selection
    Linear,
}

/// Type of mouse event
#[wasm_bindgen]
#[derive(Debug, Clone, Copy)]
pub enum MouseEventType {
    /// Mouse button pressed
    MouseDown,
    /// Mouse button released
    MouseUp,
    /// Mouse moved
    MouseMove,
}

/// Mouse event data with terminal coordinates
#[wasm_bindgen]
#[derive(Debug, Clone, Copy)]
pub struct MouseEvent {
    /// Type of mouse event
    pub event_type: MouseEventType,
    /// Column in terminal grid (0-based)
    pub col: u16,
    /// Row in terminal grid (0-based)
    pub row: u16,
    /// Mouse button (0=left, 1=middle, 2=right)
    pub button: i16,
    /// Whether Ctrl key was pressed
    pub ctrl_key: bool,
    /// Whether Shift key was pressed
    pub shift_key: bool,
    /// Whether Alt key was pressed
    pub alt_key: bool,
}

/// Query for selecting cells in the terminal
#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct CellQuery {
    inner: RustCellQuery,
}

#[wasm_bindgen]
impl CellQuery {
    /// Create a new cell query with the specified selection mode
    #[wasm_bindgen(constructor)]
    pub fn new(mode: SelectionMode) -> CellQuery {
        CellQuery { inner: select(mode.into()) }
    }

    /// Set the starting position for the selection
    pub fn start(mut self, col: u16, row: u16) -> CellQuery {
        self.inner = self.inner.start((col, row));
        self
    }

    /// Set the ending position for the selection
    pub fn end(mut self, col: u16, row: u16) -> CellQuery {
        self.inner = self.inner.end((col, row));
        self
    }

    /// Configure whether to trim trailing whitespace from lines
    #[wasm_bindgen(js_name = "trimTrailingWhitespace")]
    pub fn trim_trailing_whitespace(mut self, enabled: bool) -> CellQuery {
        self.inner = self.inner.trim_trailing_whitespace(enabled);
        self
    }

    /// Check if the query is empty (no selection range)
    #[wasm_bindgen(js_name = "isEmpty")]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

#[wasm_bindgen]
pub fn style() -> CellStyle {
    CellStyle::new()
}

#[wasm_bindgen]
pub fn cell(symbol: &str, style: CellStyle) -> Cell {
    Cell {
        symbol: symbol.into(),
        style: style.style_bits,
        fg: style.fg,
        bg: style.bg,
    }
}

#[wasm_bindgen]
impl CellStyle {
    /// Create a new TextStyle with default (normal) style
    #[wasm_bindgen(constructor)]
    pub fn new() -> CellStyle {
        Default::default()
    }

    /// Sets the foreground color
    #[wasm_bindgen]
    pub fn fg(mut self, color: u32) -> CellStyle {
        self.fg = color;
        self
    }

    /// Sets the background color
    #[wasm_bindgen]
    pub fn bg(mut self, color: u32) -> CellStyle {
        self.bg = color;
        self
    }

    /// Add bold style
    #[wasm_bindgen]
    pub fn bold(mut self) -> CellStyle {
        self.style_bits |= Glyph::BOLD_FLAG as u16;
        self
    }

    /// Add italic style
    #[wasm_bindgen]
    pub fn italic(mut self) -> CellStyle {
        self.style_bits |= Glyph::ITALIC_FLAG as u16;
        self
    }

    /// Add underline effect
    #[wasm_bindgen]
    pub fn underline(mut self) -> CellStyle {
        self.style_bits |= Glyph::UNDERLINE_FLAG as u16;
        self
    }

    /// Add strikethrough effect
    #[wasm_bindgen]
    pub fn strikethrough(mut self) -> CellStyle {
        self.style_bits |= Glyph::STRIKETHROUGH_FLAG as u16;
        self
    }

    /// Get the combined style bits
    #[wasm_bindgen(getter)]
    pub fn bits(&self) -> u16 {
        self.style_bits
    }
}

impl Default for CellStyle {
    fn default() -> Self {
        CellStyle {
            fg: 0xFFFFFF,  // Default foreground color (white)
            bg: 0x000000,  // Default background color (black)
            style_bits: 0, // No styles applied
        }
    }
}

#[wasm_bindgen]
impl Batch {
    /// Updates a single cell at the given position.
    #[wasm_bindgen(js_name = "cell")]
    pub fn cell(&mut self, x: u16, y: u16, cell_data: &Cell) {
        self.terminal_grid
            .borrow_mut()
            .update_cell(Some(&self.gl), x, y, cell_data.as_cell_data());
    }

    /// Updates a cell by its buffer index.
    #[wasm_bindgen(js_name = "cellByIndex")]
    pub fn cell_by_index(&mut self, idx: usize, cell_data: &Cell) {
        self.terminal_grid
            .borrow_mut()
            .update_cell_by_index(Some(&self.gl), idx, cell_data.as_cell_data());
    }

    /// Updates multiple cells from an array.
    /// Each element should be [x, y, cellData].
    #[wasm_bindgen(js_name = "cells")]
    pub fn cells(&mut self, cells_json: JsValue) -> Result<(), JsValue> {
        let updates = from_value::<Vec<(u16, u16, Cell)>>(cells_json)
            .map_err(|e| JsValue::from_str(&e.to_string()));

        match updates {
            Ok(cells) => {
                let cell_data = cells
                    .iter()
                    .map(|(x, y, data)| (*x, *y, data.as_cell_data()));

                let mut terminal_grid = self.terminal_grid.borrow_mut();
                terminal_grid
                    .update_cells_by_position(&self.gl, cell_data)
                    .map_err(|e| JsValue::from_str(&e.to_string()))
            },
            e => e.map(|_| ()),
        }
    }

    /// Write text to the terminal
    #[wasm_bindgen(js_name = "text")]
    pub fn text(&mut self, x: u16, y: u16, text: &str, style: &CellStyle) -> Result<(), JsValue> {
        let mut terminal_grid = self.terminal_grid.borrow_mut();
        let (cols, rows) = terminal_grid.terminal_size();

        if y >= rows {
            return Ok(()); // oob, ignore
        }

        let mut width_offset: u16 = 0;
        for (i, ch) in text.graphemes(true).enumerate() {
            let current_col = x + width_offset + i as u16;
            if current_col >= cols {
                break;
            }

            let cell = CellData::new_with_style_bits(ch, style.style_bits as u32, style.fg, style.bg);
            terminal_grid.update_cell(None, current_col, y, cell);

            if emojis::get(ch).is_some() {
                width_offset += 1;
            }
        }

        Ok(())
    }

    /// Fill a rectangular region
    #[wasm_bindgen(js_name = "fill")]
    pub fn fill(
        &mut self,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        cell_data: &Cell,
    ) -> Result<(), JsValue> {
        let mut terminal_grid = self.terminal_grid.borrow_mut();
        let (cols, rows) = terminal_grid.terminal_size();

        let width = (x + width).min(cols).saturating_sub(x);
        let height = (y + height).min(rows).saturating_sub(y);

        let fill_cell = cell_data.as_cell_data();
        for y in y..y + height {
            for x in x..x + width {
                terminal_grid.update_cell(None, x, y, fill_cell);
            }
        }

        Ok(())
    }

    /// Clear the terminal with specified background color
    #[wasm_bindgen]
    pub fn clear(&mut self, bg: u32) -> Result<(), JsValue> {
        let mut terminal_grid = self.terminal_grid.borrow_mut();
        let (cols, rows) = terminal_grid.terminal_size();

        let clear_cell = CellData::new_with_style_bits(" ", 0, 0xFFFFFF, bg);
        // Get gl from renderer - we need it for dynamic rendering
        // For now, clear doesn't need dynamic rendering (it's just a space)
        for y in 0..rows {
            for x in 0..cols {
                terminal_grid.update_cell(None, x, y, clear_cell);
            }
        }

        Ok(())
    }

    /// Synchronize all pending updates to the GPU
    #[wasm_bindgen]
    #[deprecated(since = "0.4.0", note = "no-op, flush is now automatic")]
    #[allow(deprecated)]
    pub fn flush(&mut self) -> Result<(), JsValue> {
        Ok(())
    }
}

#[wasm_bindgen]
impl Cell {
    #[wasm_bindgen(constructor)]
    pub fn new(symbol: String, style: &CellStyle) -> Cell {
        Cell {
            symbol: symbol.into(),
            style: style.style_bits,
            fg: style.fg,
            bg: style.bg,
        }
    }

    #[wasm_bindgen(getter)]
    pub fn symbol(&self) -> String {
        self.symbol.to_string()
    }

    #[wasm_bindgen(setter)]
    pub fn set_symbol(&mut self, symbol: String) {
        self.symbol = symbol.into();
    }

    #[wasm_bindgen(getter)]
    pub fn fg(&self) -> u32 {
        self.fg
    }

    #[wasm_bindgen(setter)]
    pub fn set_fg(&mut self, color: u32) {
        self.fg = color;
    }

    #[wasm_bindgen(getter)]
    pub fn bg(&self) -> u32 {
        self.bg
    }

    #[wasm_bindgen(setter)]
    pub fn set_bg(&mut self, color: u32) {
        self.bg = color;
    }

    #[wasm_bindgen(getter)]
    pub fn style(&self) -> u16 {
        self.style
    }

    #[wasm_bindgen(setter)]
    pub fn set_style(&mut self, style: u16) {
        self.style = style;
    }
}

impl Cell {
    pub fn as_cell_data(&self) -> CellData<'_> {
        CellData::new_with_style_bits(&self.symbol, self.style as u32, self.fg, self.bg)
    }
}

#[wasm_bindgen]
impl BeamtermRenderer {
    /// Create a new terminal renderer with default static font atlas
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<BeamtermRenderer, JsValue> {
        Self::new_with_font(canvas_id, None, None, None)
    }

    /// Create a new terminal renderer with dynamic font atlas
    ///
    /// # Arguments
    /// * `canvas_id` - CSS selector for the canvas element (e.g., "#terminal-canvas")
    /// * `font_family` - Optional font family name (e.g., "JetBrains Mono Nerd Font"). If None, uses default embedded font.
    /// * `font_size` - Optional font size in points (default: 14.0)
    /// * `line_height` - Optional line height multiplier (default: 1.2)
    ///
    /// # Examples
    ///
    /// ```javascript
    /// // Use default embedded font with default size
    /// const renderer = new BeamtermRenderer('#canvas', null, null, null);
    ///
    /// // Use default font with custom size
    /// const renderer = new BeamtermRenderer('#canvas', null, 16.0, 1.2);
    ///
    /// // Use custom font
    /// const renderer = new BeamtermRenderer('#canvas', 'Fira Code', 14.0, 1.2);
    /// ```
    #[wasm_bindgen(js_name = "newWithFont")]
    pub fn new_with_font(
        canvas_id: &str,
        font_family: Option<String>,
        font_size: Option<f32>,
        line_height: Option<f32>,
    ) -> Result<BeamtermRenderer, JsValue> {
        console_error_panic_hook::set_once();

        let renderer = Renderer::create(canvas_id)
            .map_err(|e| JsValue::from_str(&format!("Failed to create renderer: {e}")))?;

        let gl = renderer.gl();
        
        // Create atlas (static or dynamic)
        let atlas = if font_family.is_some() || font_size.is_some() || line_height.is_some() {
            // Dynamic font atlas
            let font_family = font_family.unwrap_or_else(|| "JetBrains Mono Nerd Font".to_string());
            let font_size = font_size.unwrap_or(14.0);
            let line_height = line_height.unwrap_or(1.2);

            use crate::font::{default_font, runtime_font_system::RuntimeFontSystem};
            use std::rc::Rc;
            use std::cell::RefCell;

            let mut font_system = RuntimeFontSystem::new(
                font_family.clone(),
                "Noto Color Emoji".to_string(),
                font_size,
                line_height,
            ).map_err(|e| JsValue::from_str(&format!("Failed to create font system: {e}")))?;

            // Load default embedded font if using default font family
            if font_family == "JetBrains Mono Nerd Font" {
                console::log_1(&"[beamterm] Attempting to load default embedded font".into());
                match default_font::load_default_font(font_system.font_system_mut()) {
                    Ok(loaded_family) => {
                        console::log_1(&format!("[beamterm] Default font loaded successfully: {}", loaded_family).into());
                    }
                    Err(e) => {
                        console::error_1(&format!("[beamterm] Failed to load default font: {}", e).into());
                        return Err(JsValue::from_str(&format!("Failed to load default font: {}", e)));
                    }
                }
            } else {
                console::log_1(&format!("[beamterm] Using custom font family: {}", font_family).into());
            }

            FontAtlas::new_dynamic(
                gl,
                Rc::new(RefCell::new(font_system)),
                font_size,
                line_height,
            ).map_err(|e| JsValue::from_str(&format!("Failed to create dynamic atlas: {e}")))?
        } else {
            // Static atlas (default)
            let atlas_data = FontAtlasData::default();
            FontAtlas::load(gl, atlas_data)
                .map_err(|e| JsValue::from_str(&format!("Failed to load font atlas: {e}")))?
        };

        let canvas_size = renderer.canvas_size();
        let terminal_grid = TerminalGrid::new(gl, atlas, canvas_size)
            .map_err(|e| JsValue::from_str(&format!("Failed to create terminal grid: {e}")))?;

        console::log_1(&"BeamtermRenderer initialized successfully".into());
        let terminal_grid = Rc::new(RefCell::new(terminal_grid));
        Ok(BeamtermRenderer { renderer, terminal_grid, mouse_handler: None })
    }

    /// Enable default mouse selection behavior with built-in copy to clipboard
    #[wasm_bindgen(js_name = "enableSelection")]
    pub fn enable_selection(
        &mut self,
        mode: SelectionMode,
        trim_whitespace: bool,
    ) -> Result<(), JsValue> {
        // clean up existing mouse handler if present
        if let Some(old_handler) = self.mouse_handler.take() {
            old_handler.cleanup();
        }

        let selection_tracker = self.terminal_grid.borrow().selection_tracker();
        let handler =
            DefaultSelectionHandler::new(self.terminal_grid.clone(), mode.into(), trim_whitespace);

        let mouse_handler = TerminalMouseHandler::new(
            self.renderer.canvas(),
            self.terminal_grid.clone(),
            handler.create_event_handler(selection_tracker),
        )
        .map_err(|e| JsValue::from_str(&format!("Failed to create mouse handler: {e}")))?;

        self.mouse_handler = Some(mouse_handler);
        Ok(())
    }

    /// Set a custom mouse event handler
    #[wasm_bindgen(js_name = "setMouseHandler")]
    pub fn set_mouse_handler(&mut self, handler: js_sys::Function) -> Result<(), JsValue> {
        // Clean up existing mouse handler if present
        if let Some(old_handler) = self.mouse_handler.take() {
            old_handler.cleanup();
        }

        let handler_closure = {
            let handler = handler.clone();
            move |event: TerminalMouseEvent, _grid: &TerminalGrid| {
                let js_event = MouseEvent::from(event);
                let this = JsValue::null();
                let args = js_sys::Array::new();
                args.push(&JsValue::from(js_event));

                if let Err(e) = handler.apply(&this, &args) {
                    console::error_1(&format!("Mouse handler error: {e:?}").into());
                }
            }
        };

        let mouse_handler = TerminalMouseHandler::new(
            self.renderer.canvas(),
            self.terminal_grid.clone(),
            handler_closure,
        )
        .map_err(|e| JsValue::from_str(&format!("Failed to create mouse handler: {e}")))?;

        self.mouse_handler = Some(mouse_handler);
        Ok(())
    }

    /// Get selected text based on a cell query
    #[wasm_bindgen(js_name = "getText")]
    pub fn get_text(&self, query: &CellQuery) -> String {
        self.terminal_grid
            .borrow()
            .get_text(query.inner)
            .to_string()
    }

    /// Copy text to the system clipboard
    #[wasm_bindgen(js_name = "copyToClipboard")]
    pub fn copy_to_clipboard(&self, text: &str) {
        use wasm_bindgen_futures::spawn_local;
        let text = text.to_string();

        spawn_local(async move {
            if let Some(window) = web_sys::window() {
                let clipboard = window.navigator().clipboard();
                match wasm_bindgen_futures::JsFuture::from(clipboard.write_text(&text)).await {
                    Ok(_) => {
                        console::log_1(
                            &format!("Copied {} characters to clipboard", text.len()).into(),
                        );
                    },
                    Err(err) => {
                        console::error_1(&format!("Failed to copy to clipboard: {err:?}").into());
                    },
                }
            }
        });
    }

    /// Clear any active selection
    #[wasm_bindgen(js_name = "clearSelection")]
    pub fn clear_selection(&self) {
        self.terminal_grid
            .borrow()
            .selection_tracker()
            .clear();
    }

    /// Check if there is an active selection
    #[wasm_bindgen(js_name = "hasSelection")]
    pub fn has_selection(&self) -> bool {
        self.terminal_grid
            .borrow()
            .selection_tracker()
            .get_query()
            .is_some()
    }

    /// Create a new render batch
    #[wasm_bindgen(js_name = "batch")]
    pub fn new_render_batch(&mut self) -> Batch {
        let gl = self.renderer.gl().clone();
        let terminal_grid = self.terminal_grid.clone();
        Batch { terminal_grid, gl }
    }

    /// Get the terminal dimensions in cells
    #[wasm_bindgen(js_name = "terminalSize")]
    pub fn terminal_size(&self) -> Size {
        let (cols, rows) = self.terminal_grid.borrow().terminal_size();
        Size { width: cols, height: rows }
    }

    /// Get the cell size in pixels
    #[wasm_bindgen(js_name = "cellSize")]
    pub fn cell_size(&self) -> Size {
        let (width, height) = self.terminal_grid.borrow().cell_size();
        Size { width: width as u16, height: height as u16 }
    }

    /// Returns the number of glyphs currently available in the atlas (ASCII + dynamic).
    #[wasm_bindgen(js_name = "getGlyphCount")]
    pub fn get_glyph_count(&self) -> u32 {
        self.terminal_grid.borrow().atlas().glyph_count()
    }

    /// Render the terminal to the canvas
    #[wasm_bindgen]
    pub fn render(&mut self) {
        let mut grid = self.terminal_grid.borrow_mut();
        let _ = grid.flush_cells(self.renderer.gl());

        self.renderer.begin_frame();
        self.renderer.render(&*grid);
        self.renderer.end_frame();
    }

    /// Resize the terminal to fit new canvas dimensions
    #[wasm_bindgen]
    pub fn resize(&mut self, width: i32, height: i32) -> Result<(), JsValue> {
        self.renderer.resize(width, height);

        let gl = self.renderer.gl();
        self.terminal_grid
            .borrow_mut()
            .resize(gl, (width, height))
            .map_err(|e| JsValue::from_str(&format!("Failed to resize: {e}")))?;

        // Update mouse handler dimensions if present
        if let Some(mouse_handler) = &mut self.mouse_handler {
            let (cols, rows) = self.terminal_grid.borrow().terminal_size();
            mouse_handler.update_dimensions(cols, rows);
        }

        Ok(())
    }
}

// Convert between Rust and WASM types
impl From<SelectionMode> for RustSelectionMode {
    fn from(mode: SelectionMode) -> Self {
        match mode {
            SelectionMode::Block => RustSelectionMode::Block,
            SelectionMode::Linear => RustSelectionMode::Linear,
        }
    }
}

impl From<RustSelectionMode> for SelectionMode {
    fn from(mode: RustSelectionMode) -> Self {
        match mode {
            RustSelectionMode::Block => SelectionMode::Block,
            RustSelectionMode::Linear => SelectionMode::Linear,
        }
    }
}

impl From<TerminalMouseEvent> for MouseEvent {
    fn from(event: TerminalMouseEvent) -> Self {
        use crate::mouse::MouseEventType as RustMouseEventType;

        let event_type = match event.event_type {
            RustMouseEventType::MouseDown => MouseEventType::MouseDown,
            RustMouseEventType::MouseUp => MouseEventType::MouseUp,
            RustMouseEventType::MouseMove => MouseEventType::MouseMove,
        };

        MouseEvent {
            event_type,
            col: event.col,
            row: event.row,
            button: event.button(),
            ctrl_key: event.ctrl_key(),
            shift_key: event.shift_key(),
            alt_key: event.alt_key(),
        }
    }
}

/// Initialize the WASM module
#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
    console::log_1(&"beamterm WASM module loaded".into());
}
