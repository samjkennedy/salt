use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
}

impl Diagnostic {

    pub fn report_with_source(&self, filename: &String, src: &String) {
        let mut line_start = 0;

        for (line_index, line) in src.lines().enumerate() {
            let line_len = line.len();
            let line_end = line_start + line_len;

            if self.span.start >= line_start && self.span.start < line_end {
                let offset_in_line = self.span.start - line_start;
                let marker_len = if offset_in_line + self.span.length > line_len {
                    line_len.saturating_sub(offset_in_line)
                } else {
                    self.span.length.max(1)
                };

                let location = format!("{}:{}:{}", filename, line_index + 1, offset_in_line + 1);

                let marker_line: String = " ".repeat(offset_in_line) + &"^".repeat(marker_len);

                eprintln!(
                    "error: {} at {}\n {}|\t{}\n  |\t{}", //TODO correctly account for the width of the line number
                    self.message, location,
                    line_index + 1,
                    line,
                    marker_line
                );
                return;
            }

            // Advance line_start to the beginning of the next line
            // +2 assumes Windows newlines (\n)
            line_start = line_end + 2; //TODO: handle newlines more gracefully
        }

        // fallback in case span doesn't match any line
        eprintln!(
            "error: {} (at byte offset {})",
            self.message,
            self.span.start
        );
    }
}