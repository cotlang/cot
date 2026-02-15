use zed_extension_api::{self as zed, LanguageServerId, Result};

struct CotExtension;

impl zed::Extension for CotExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        // Find `cot` on PATH
        let path = worktree
            .which("cot")
            .ok_or_else(|| "'cot' not found on PATH. Install Cot or add it to your PATH.".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec!["lsp".to_string()],
            env: Default::default(),
        })
    }
}

zed::register_extension!(CotExtension);
