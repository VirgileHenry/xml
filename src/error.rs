use std::fmt::Debug;

#[derive(Debug)]
pub struct XmlParsingError<'src> {
    kind: XmlParsingErrorKind<'src>,
    parsing_stack: Vec<&'static str>,
}

impl<'src> XmlParsingError<'src> {
    pub fn unexpected(expected: &[&'static str], obtained: &'src str) -> Self {
        let max_expected_char_count = expected.iter().map(|s| s.chars().count()).max().unwrap_or(0);
        let obtained_shown_length = max_expected_char_count.min(obtained.len());
        let obtained = match obtained.char_indices().skip(obtained_shown_length).next() {
            Some((i, _)) => &obtained[..i],
            None => obtained,
        };
        let expected = expected.iter().cloned().collect();
        XmlParsingError {
            kind: XmlParsingErrorKind::Unexpected { expected, obtained },
            parsing_stack: Vec::new(),
        }
    }

    pub fn unclosed<T>(closing_tag: &'static str) -> Self {
        XmlParsingError {
            kind: XmlParsingErrorKind::Unclosed {
                elem: std::any::type_name::<T>(),
                closing_tag,
            },
            parsing_stack: Vec::new(),
        }
    }

    pub fn invalid_target(target: &'src str) -> Self {
        XmlParsingError {
            kind: XmlParsingErrorKind::InvalidTarget { target },
            parsing_stack: Vec::new(),
        }
    }

    pub fn add_ctx<T>(self) -> Self {
        let mut mut_self = self;
        mut_self.parsing_stack.push(std::any::type_name::<T>());
        mut_self
    }
}

impl<'src> std::fmt::Display for XmlParsingError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "While parsing: ")?;
        let stack_length = self.parsing_stack.len();
        for elem in self.parsing_stack.iter().rev().take(stack_length.saturating_sub(1)) {
            write!(f, "{elem} -> ")?;
        }
        if let Some(elem) = self.parsing_stack.first() {
            write!(f, "{elem}")?;
        }
        writeln!(f, "")?;
        std::fmt::Display::fmt(&self.kind, f)?;
        Ok(())
    }
}

impl<'src> std::error::Error for XmlParsingError<'src> {}

#[derive(Debug)]
pub enum XmlParsingErrorKind<'src> {
    Unexpected {
        expected: Vec<&'static str>,
        obtained: &'src str,
    },
    Unclosed {
        elem: &'static str,
        closing_tag: &'static str,
    },
    InvalidTarget {
        target: &'src str,
    },
}

impl<'src> std::fmt::Display for XmlParsingErrorKind<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected { expected, obtained } => match expected.len() {
                0 => write!(f, "Unexpected \"{obtained}\"")?,
                1 => write!(f, "Expected \"{}\", obtained \"{obtained}\"", expected[0])?,
                length => {
                    write!(f, "Expected one of ")?;
                    for elem in expected.iter().take(length - 1) {
                        write!(f, "{elem}, ")?;
                    }
                    write!(f, "{}, obtained {obtained}", expected[length - 1])?;
                }
            },
            Self::Unclosed { elem, closing_tag } => write!(f, "Unclosed element {elem}, expected closing tag {closing_tag}")?,
            Self::InvalidTarget { target } => write!(f, "Invalid target: \"{target}\"")?,
        }
        Ok(())
    }
}
