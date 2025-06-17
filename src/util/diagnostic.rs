use crate::util::{Span, error::*};
use colored::*;

/// Diagnostic renderer for pretty-printing compiler errors and warnings
pub struct DiagnosticRenderer {
    use_colors: bool,
}

impl DiagnosticRenderer {
    pub fn new(use_colors: bool) -> Self {
        Self { use_colors }
    }
    
    /// Render a single diagnostic to a string
    pub fn render_diagnostic(&self, diagnostic: &Diagnostic, source: Option<&str>) -> String {
        let mut output = String::new();
        
        // Render the main diagnostic line
        let level_str = self.format_level(diagnostic.level);
        output.push_str(&format!("{}: {}\n", level_str, diagnostic.message));
        
        // Render span information if available
        if let (Some(span), Some(source)) = (&diagnostic.span, source) {
            output.push_str(&self.render_span_context(span, source));
            output.push('\n');
        }
        
        // Render notes
        for note in &diagnostic.notes {
            let note_str = self.format_level(DiagnosticLevel::Note);
            output.push_str(&format!("{}: {}\n", note_str, note));
        }
        
        // Render help if available
        if let Some(help) = &diagnostic.help {
            let help_str = self.format_level(DiagnosticLevel::Help);
            output.push_str(&format!("{}: {}\n", help_str, help));
        }
        
        output
    }
    
    /// Render multiple diagnostics
    pub fn render_diagnostics(&self, diagnostics: &[Diagnostic], source: Option<&str>) -> String {
        diagnostics
            .iter()
            .map(|d| self.render_diagnostic(d, source))
            .collect::<Vec<_>>()
            .join("\n")
    }
    
    /// Format diagnostic level with colors if enabled
    fn format_level(&self, level: DiagnosticLevel) -> String {
        let text = level.to_string();
        
        if !self.use_colors {
            return text;
        }
        
        match level {
            DiagnosticLevel::Error => text.red().bold().to_string(),
            DiagnosticLevel::Warning => text.yellow().bold().to_string(),
            DiagnosticLevel::Note => text.blue().bold().to_string(),
            DiagnosticLevel::Help => text.green().bold().to_string(),
        }
    }
    
    /// Render span context with source code
    fn render_span_context(&self, span: &Span, source: &str) -> String {
        let lines: Vec<&str> = source.lines().collect();
        let line_idx = (span.line.saturating_sub(1)) as usize;
        
        if line_idx >= lines.len() {
            return format!("  --> {}:{}", span.line, span.column);
        }
        
        let mut output = String::new();
        
        // Show location
        output.push_str(&format!("  --> {}:{}\n", span.line, span.column));
        
        // Calculate line number width for padding
        let line_num_width = span.line.to_string().len().max(3);
        
        // Show context lines (before)
        let context_start = line_idx.saturating_sub(2);
        for i in context_start..line_idx {
            if i < lines.len() {
                let line_num = i + 1;
                output.push_str(&format!(
                    "{:width$} | {}\n",
                    line_num,
                    lines[i],
                    width = line_num_width
                ));
            }
        }
        
        // Show the error line
        if line_idx < lines.len() {
            let line = lines[line_idx];
            output.push_str(&format!(
                "{:width$} | {}\n",
                span.line,
                line,
                width = line_num_width
            ));
            
            // Show the error indicator
            let indicator_offset = span.column as usize;
            let indicator_len = span.len().max(1);
            
            output.push_str(&format!(
                "{:width$} | {:offset$}{:^<len$}\n",
                "",
                "",
                "",
                width = line_num_width,
                offset = indicator_offset,
                len = indicator_len
            ));
        }
        
        // Show context lines (after)
        let context_end = (line_idx + 3).min(lines.len());
        for i in (line_idx + 1)..context_end {
            let line_num = i + 1;
            output.push_str(&format!(
                "{:width$} | {}\n",
                line_num,
                lines[i],
                width = line_num_width
            ));
        }
        
        output
    }
}

impl Default for DiagnosticRenderer {
    fn default() -> Self {
        Self::new(true) // Use colors by default
    }
}

/// Builder for creating diagnostic messages
#[derive(Debug)]
pub struct DiagnosticBuilder {
    diagnostic: Diagnostic,
}

impl DiagnosticBuilder {
    pub fn error(message: String) -> Self {
        Self {
            diagnostic: Diagnostic::error(message, None),
        }
    }
    
    pub fn warning(message: String) -> Self {
        Self {
            diagnostic: Diagnostic::warning(message, None),
        }
    }
    
    pub fn note(message: String) -> Self {
        Self {
            diagnostic: Diagnostic::note(message, None),
        }
    }
    
    pub fn with_span(mut self, span: Span) -> Self {
        self.diagnostic.span = Some(span);
        self
    }
    
    pub fn with_note(mut self, note: String) -> Self {
        self.diagnostic.notes.push(note);
        self
    }
    
    pub fn with_help(mut self, help: String) -> Self {
        self.diagnostic.help = Some(help);
        self
    }
    
    pub fn build(self) -> Diagnostic {
        self.diagnostic
    }
}

/// Macro for creating diagnostics easily
#[macro_export]
macro_rules! diagnostic {
    (error: $msg:expr) => {
        $crate::util::diagnostic::DiagnosticBuilder::error($msg.to_string()).build()
    };
    (error: $msg:expr, span: $span:expr) => {
        $crate::util::diagnostic::DiagnosticBuilder::error($msg.to_string())
            .with_span($span)
            .build()
    };
    (warning: $msg:expr) => {
        $crate::util::diagnostic::DiagnosticBuilder::warning($msg.to_string()).build()
    };
    (warning: $msg:expr, span: $span:expr) => {
        $crate::util::diagnostic::DiagnosticBuilder::warning($msg.to_string())
            .with_span($span)
            .build()
    };
    (note: $msg:expr) => {
        $crate::util::diagnostic::DiagnosticBuilder::note($msg.to_string()).build()
    };
    (note: $msg:expr, span: $span:expr) => {
        $crate::util::diagnostic::DiagnosticBuilder::note($msg.to_string())
            .with_span($span)
            .build()
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_diagnostic_rendering() {
        let renderer = DiagnosticRenderer::new(false);
        let span = Span::new(0, 5, 1, 1);
        let diagnostic = Diagnostic::error("Test error".to_string(), Some(span))
            .with_note("This is a note".to_string())
            .with_help("Try this instead".to_string());
        
        let source = "hello world\nthis is line 2";
        let output = renderer.render_diagnostic(&diagnostic, Some(source));
        
        assert!(output.contains("error: Test error"));
        assert!(output.contains("note: This is a note"));
        assert!(output.contains("help: Try this instead"));
    }
    
    #[test]
    fn test_span_context_rendering() {
        let renderer = DiagnosticRenderer::new(false);
        let span = Span::new(6, 11, 1, 7);
        
        let source = "hello world\nthis is line 2";
        let context = renderer.render_span_context(&span, source);
        
        assert!(context.contains("1:7"));
        assert!(context.contains("hello world"));
    }
}