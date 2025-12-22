# Testing Dynamic Font Atlas in terX

## Overview

Dynamic font atlas allows runtime glyph rasterization, eliminating the need for pre-generated static atlases. This enables:
- Support for arbitrary font sizes without bundling multiple atlases
- Smaller binary size (no need for ~50MB of static atlases)
- Runtime font changes

## What Was Implemented

1. **Dynamic Atlas Foundation** - Runtime font system with `cosmic-text` integration
2. **On-Demand Glyph Rendering** - Automatic rasterization when glyphs are missing
3. **Texture Growth** - Atlas automatically expands when full
4. **Emoji & Fullwidth Support** - Proper handling of double-width characters
5. **Terminal Builder Integration** - `dynamic_font()` method in Rust API
6. **WASM API Support** - `newWithFont()` method in JavaScript API

## How to Test in terX

### Option 1: Use WASM API (Recommended)

Update `src/main.ts` to use dynamic font:

```typescript
// Instead of:
const renderer = new BeamtermRenderer('#terminal-canvas');

// Use:
const renderer = BeamtermRenderer.newWithFont(
    '#terminal-canvas',
    null,           // Use default embedded font (JetBrains Nerd Mono)
    14.0,           // Font size in points
    1.2             // Line height multiplier
);

// Or with custom font family (if you have font data available):
const renderer = BeamtermRenderer.newWithFont(
    '#terminal-canvas',
    'JetBrains Mono Nerd Font',  // Font family name
    14.0,
    1.2
);
```

### Option 2: Load Custom Font via Tauri (Future Enhancement)

For custom fonts, you'll need to:
1. Add a Tauri command to load font data from filesystem
2. Load font data in WASM and pass to `RuntimeFontSystem::load_font_data()`

Example Tauri command:
```rust
#[tauri::command]
fn load_font_family(font_path: String) -> Result<Vec<u8>, String> {
    std::fs::read(&font_path)
        .map_err(|e| format!("Failed to read font file: {}", e))
}
```

Then in TypeScript:
```typescript
// Load font from Tauri backend
const fontData = await invoke<number[]>('load_font_family', { 
    fontPath: '/path/to/font.ttf' 
});

// Pass to renderer (this requires extending WASM API)
// renderer.loadFontData(new Uint8Array(fontData));
```

## Current Limitations

1. **Default Font Only**: Currently only the embedded JetBrains Nerd Mono font is available without additional font loading
2. **No Runtime Font Loading**: Loading custom fonts requires extending the WASM API
3. **Texture Growth Performance**: Growing texture loses existing glyphs (but they're cached, so re-rendering is fast)

## Testing Steps

1. **Update terX to use dynamic font**:
   ```typescript
   // In src/main.ts, line ~318
   const renderer = BeamtermRenderer.newWithFont(
       '#terminal-canvas',
       null,  // Use default font
       14.0,  // 14pt font
       1.2    // 1.2x line height
   );
   ```

2. **Rebuild beamterm-renderer**:
   ```bash
   cd /Users/k/dev/beamterm
   npm run build  # or whatever build command you use
   ```

3. **Update terX dependencies**:
   ```bash
   cd /Users/k/dev/terX
   # Update package.json to point to local beamterm or publish new version
   npm install
   ```

4. **Test Nerd Font symbols**:
   - Run a command that uses Nerd Font icons (e.g., `ls` with exa/bat)
   - Check browser console for any errors
   - Symbols should render automatically on first use

5. **Monitor glyph rendering**:
   - Open browser DevTools console
   - Watch for "Atlas texture grown to X layers" messages
   - Check `renderer.getGlyphCount()` to see how many glyphs are rendered

## Debugging

```javascript
// In browser console after initialization:
const renderer = window.terxDebug.renderer;

// Check glyph count
console.log('Glyph count:', renderer.getGlyphCount());

// Test specific symbol rendering
renderer.checkSymbol('→');  // Should return glyph ID

// Check missing glyphs (for static atlas)
const missing = renderer.getMissingGlyphs();
console.log('Missing glyphs:', missing);

// For dynamic atlas, glyphs should render automatically
// Monitor console for rasterization messages
```

## Next Steps

1. **Add font loading to WASM API**: Extend `BeamtermRenderer` with `loadFontData(fontData: Uint8Array)` method
2. **Add Tauri font loader**: Create command to load fonts from filesystem
3. **Performance optimization**: Consider texture copying when growing (currently data is lost)
4. **Font size changes**: Add ability to change font size at runtime (requires recreating atlas)

## Notes

- Dynamic atlas starts with 32 layers (1024 glyph capacity)
- Texture grows by doubling layers when full
- Glyphs are cached in memory for fast re-rendering
- Emoji and fullwidth characters occupy 2 consecutive glyph IDs

