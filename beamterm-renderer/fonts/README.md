# Fonts Directory

This directory contains embedded font files for the beamterm renderer.

## Default Font

The default font is **JetBrains Mono Nerd Font Regular**.

To add the font:
1. Download JetBrains Mono Nerd Font from https://www.nerdfonts.com/font-downloads
2. Place `JetBrainsMonoNerdFont-Regular.ttf` in this directory
3. The font will be embedded at compile time using `include_bytes!` macro

The font file will be included in the WASM binary, so it's always available without external dependencies.

