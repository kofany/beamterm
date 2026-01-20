# beamterm - kofany fork

Fork repozytorium [junkdog/beamterm](https://github.com/junkdog/beamterm) z poprawkami dla terX.

## Aktywny branch

**`beamterm-terx`** - główny branch z poprawkami:
- HiDPI selection fix (mnożenie koordynat myszy przez DPR w `mouse.rs`)
- Selection drag threshold
- Dynamic font atlas fixes

## Publikacja do npm (@kofany/beamterm-terx)

### Wymagania
- Rust + wasm-pack
- npm **Automation token** (Classic Token → Automation) - omija 2FA
- Granular token NIE działa jeśli masz 2FA włączone

### Kroki publikacji

```bash
# 1. Upewnij się że jesteś na właściwym branchu
git checkout beamterm-terx

# 2. Zrób zmiany w kodzie (np. w beamterm-renderer/src/)

# 3. Clean build z js-api feature
cd beamterm-renderer
rm -rf dist pkg target
wasm-pack build --target bundler --out-dir dist/bundler -- --features js-api

# 4. Zaktualizuj package.json (nazwa i wersja)
# dist/bundler/package.json powinien mieć:
#   "name": "@kofany/beamterm-terx"
#   "version": "0.12.X"  <- zwiększ wersję!

# 5. Publikuj
cd dist/bundler
npm publish --access public
```

### Aktualizacja package.json po buildzie

wasm-pack generuje package.json z Cargo.toml. Po każdym buildzie musisz ręcznie zmienić:

```json
{
  "name": "@kofany/beamterm-terx",
  "version": "0.12.X",
  "repository": {
    "type": "git",
    "url": "https://github.com/kofany/beamterm"
  },
  "publishConfig": {
    "access": "public"
  }
}
```

### Po publikacji

W terX wystarczy:
```bash
bun update @kofany/beamterm-terx
```

## Kluczowe pliki

| Plik | Opis |
|------|------|
| `beamterm-renderer/src/mouse.rs` | Obsługa myszy, selekcja, HiDPI fix |
| `beamterm-renderer/src/wasm.rs` | Eksporty JS API |
| `beamterm-renderer/src/terminal.rs` | Główna struktura renderera |
| `beamterm-renderer/Cargo.toml` | Feature `js-api` wymagany do budowania |

## HiDPI Fix (mouse.rs)

Kluczowa poprawka w `pixel_to_cell`:
```rust
let dpr = web_sys::window().map(|w| w.device_pixel_ratio()).unwrap_or(1.0) as f32;
let x = event.offset_x() as f32 * dpr;  // CSS → Physical
let y = event.offset_y() as f32 * dpr;
```

Mouse events zwracają CSS pixels, cell dimensions są w physical pixels.
