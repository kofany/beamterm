use cosmic_text::FontSystem;

/// Embedded JetBrains Nerd Mono Regular font data.
/// 
/// This font is embedded at compile time using `include_bytes!` macro.
/// The font file is located at `beamterm-renderer/fonts/JetBrainsMonoNerdFont-Regular.ttf`.
const JETBRAINS_NERD_MONO_REGULAR: &[u8] = include_bytes!("../../fonts/JetBrainsMonoNerdFont-Regular.ttf");

/// Loads the default embedded JetBrains Nerd Mono font into the font system.
/// 
/// This function should be called during initialization if no custom font is provided.
/// The font is loaded from embedded binary data, so no external file access is required.
/// 
/// # Arguments
/// 
/// * `font_system` - The cosmic-text FontSystem to load the font into
/// 
/// # Returns
/// 
/// The font family name as detected by cosmic-text after loading (e.g., "JetBrains Mono Nerd Font")
pub fn load_default_font(font_system: &mut FontSystem) -> Result<String, String> {
    let db = font_system.db_mut();
    
    // Load font data into the database
    let font_data = JETBRAINS_NERD_MONO_REGULAR.to_vec();
    db.load_font_data(font_data);
    
    // Find the font by searching for "JetBrains" in family names
    // After loading, we need to search for it in the database
    let face = db.faces()
        .find(|face| {
            face.families.iter().any(|(name, _)| {
                name.to_lowercase().contains("jetbrains")
            })
        })
        .ok_or_else(|| "Failed to find loaded JetBrains font".to_string())?;
    
    let family_name = face.families.first()
        .map(|(name, _)| name.clone())
        .ok_or_else(|| "Font has no family name".to_string())?;
    
    Ok(family_name)
}

