pub struct XmlParsingError<'src> {
    kind: XmlParsingErrorKind<'src>,
    parsing_stack: Vec<&'static str>,
}

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

    pub fn add_ctx<T>(&mut self) {
        self.parsing_stack.push(std::any::type_name::<T>());
    }
}
