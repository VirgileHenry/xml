use std::fmt::Debug;

#[derive(Debug)]
pub enum XmlParsingError<'src> {
    Unexpected {
        expected: Vec<&'static str>,
        obtained: crate::span::Span<'src>,
    },
    Unclosed {
        elem: &'static str,
        opened_at: crate::span::Span<'src>,
        closing_tag: &'static str,
    },
    InvalidTarget {
        target: crate::span::Span<'src>,
    },
}

impl<'src> XmlParsingError<'src> {
    pub fn unexpected(expected: &[&'static str], obtained: crate::span::Span<'src>) -> Self {
        let max_expected_char_count = expected.iter().map(|s| s.chars().count()).max().unwrap_or(0);
        let obtained_shown_length = max_expected_char_count.min(obtained.span.chars().count());
        let obtained = match obtained.span.char_indices().skip(obtained_shown_length).next() {
            Some((i, _)) => crate::span::Span::new(obtained.pos(), &obtained.span[..i]),
            None => obtained,
        };
        let expected = expected.iter().cloned().collect();
        XmlParsingError::Unexpected { expected, obtained }
    }

    pub fn unclosed<T>(opened_at: crate::span::Span<'src>, closing_tag: &'static str) -> Self {
        let elem = std::any::type_name::<T>().split("::").last().unwrap_or("Unknown");
        XmlParsingError::Unclosed {
            elem,
            opened_at,
            closing_tag,
        }
    }

    pub fn invalid_target(target: crate::span::Span<'src>) -> Self {
        XmlParsingError::InvalidTarget { target }
    }
}

impl<'src> std::fmt::Display for XmlParsingError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            XmlParsingError::Unexpected { expected, obtained } => {
                write!(
                    f,
                    "Unexpected character at line {}, col {}: expected one of ",
                    obtained.position.line, obtained.position.column
                )?;
                for expected in expected.iter() {
                    write!(f, "{}, ", expected)?;
                }
                write!(f, "found {}", obtained.span)?;
            }
            XmlParsingError::Unclosed {
                elem,
                opened_at,
                closing_tag,
            } => {
                write!(
                    f,
                    "Unclosed {} opened at line {}, col {}: {}... missing closing tag: {}",
                    elem, opened_at.position.line, opened_at.position.column, opened_at.span, closing_tag
                )?;
            }
            XmlParsingError::InvalidTarget { target } => {
                write!(
                    f,
                    "Invalid target at line {}, col {}: target can't be \"XML\", found {}",
                    target.position.line, target.position.column, target.span
                )?;
            }
        }
        Ok(())
    }
}

impl<'src> std::error::Error for XmlParsingError<'src> {}
