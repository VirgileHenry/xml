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
    pub(crate) fn new(position: Position, span: &'src str) -> Self {
        Span { position, span }
    }

    /// Get the inner string from the span.
    pub fn str(&self) -> &'src str {
        self.span
    }

    /// Get the position in the document at which the span starts.
    pub fn pos(&self) -> Position {
        self.position
    }
}

impl<'src> std::ops::Deref for Span<'src> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.span
    }
}

impl<'src> std::fmt::Display for Span<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.span.fmt(f)
    }
}
