/// A position in the XML document.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub const ZERO: Position = Position { line: 0, column: 0 };
}

impl std::ops::Add for Position {
    type Output = Position;
    fn add(self, rhs: Self) -> Self::Output {
        let line = self.line + rhs.line;
        let column = if rhs.line == 0 { self.column + rhs.column } else { rhs.column };
        Position { line, column }
    }
}

impl std::ops::AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        self.line = self.line + rhs.line;
        self.column = if rhs.line == 0 { self.column + rhs.column } else { rhs.column };
    }
}

/// A span in the XMl document, composed of the position as well as the str reference.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Span<'src> {
    pub(crate) position: Position,
    pub(crate) span: &'src str,
}

impl<'src> Span<'src> {
    /// Get the inner string from the span.
    pub fn str(&self) -> &'src str {
        self.span
    }

    /// Get the position in the document at which the span starts.
    pub fn pos(&self) -> Position {
        self.position
    }
}

impl<'src> Span<'src> {
    pub(crate) fn new(position: Position, span: &'src str) -> Self {
        Span { position, span }
    }

    pub(crate) fn split_span<T>(&self, pat: &'static str) -> Result<(Self, Self), crate::error::XmlParsingError<'src>> {
        let pattern_index = self
            .span
            .find(pat)
            .ok_or_else(|| crate::error::XmlParsingError::unclosed::<T>(*self, pat))?;
        let (first, rest) = self.span.split_at(pattern_index);
        /* Advance second position */
        let mut rest_position = self.position;
        for ch in rest.chars().chain(pat.chars()) {
            match ch {
                '\n' => {
                    rest_position.line += 1;
                    rest_position.column = 0;
                }
                '\r' => {
                    rest_position.column = 0;
                }
                _ => {
                    rest_position.column += 1;
                }
            }
        }
        Ok((
            Self {
                position: self.position,
                span: first,
            },
            Self {
                position: rest_position,
                span: &rest[pat.len()..],
            },
        ))
    }

    pub(crate) fn first_char(&self) -> Option<char> {
        self.span.chars().next()
    }

    pub(crate) fn strip_prefix(&self, prefix: &'static str) -> Option<Self> {
        let span = self.span.strip_prefix(prefix)?;
        let mut position = self.position;
        for ch in prefix.chars() {
            match ch {
                '\n' => {
                    position.line += 1;
                    position.column = 0;
                }
                '\r' => {
                    position.column = 0;
                }
                _ => {
                    position.column += 1;
                }
            }
        }
        Some(Self { position, span })
    }

    pub(crate) fn bump(&mut self) {
        match self.first_char() {
            Some(ch) => {
                self.span = &self.span[ch.len_utf8()..];
                match ch {
                    '\n' => {
                        self.position.line += 1;
                        self.position.column = 0;
                    }
                    '\r' => {
                        self.position.column = 0;
                    }
                    _ => {
                        self.position.column += 1;
                    }
                }
            }
            _ => {}
        }
    }
}

impl<'src> std::fmt::Display for Span<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.span.fmt(f)
    }
}
