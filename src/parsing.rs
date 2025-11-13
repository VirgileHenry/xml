use crate::error::XmlParsingError;

/// The character set trait allows to define characters set,
/// and create functions to expect chars and strings that are only chars from within the set.
/// This is useful to parse litterals that can only be from given sets.
pub trait CharacterSet {
    /// Name of the character set, that will appear in the errors when it was expected.
    const NAME: &'static str;
    const CONTAINS_SPACES: bool;
    /// TODO
    fn munch(input: &str) -> Option<usize>;
}

/// Decimals digits character set, so 0-9.
pub struct DecimalDigits;

impl CharacterSet for DecimalDigits {
    const NAME: &'static str = "[0-9]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        match input.chars().next()? {
            '0'..='9' => Some(1),
            _ => None,
        }
    }
}

/// Hexadecimal digits, 0-9 + a-f + A-F.
pub struct HexadecimalDigits;

impl CharacterSet for HexadecimalDigits {
    const NAME: &'static str = "[0-9a-fA-F]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        match input.chars().next()? {
            '0'..='9' | 'A'..='F' | 'a'..='f' => Some(1),
            _ => None,
        }
    }
}

/// [3] - Spaces
///
/// https://www.w3.org/TR/xml/#NT-S
pub struct Spaces;

impl CharacterSet for Spaces {
    const NAME: &'static str = "\\s";
    const CONTAINS_SPACES: bool = true;
    fn munch(input: &str) -> Option<usize> {
        match input.chars().next()? {
            '\n' | 'r' | ' ' | '\t' => Some(1),
            _ => None,
        }
    }
}

/// [4] - Name Characters
///
/// https://www.w3.org/TR/xml/#NT-NameChar
pub struct NameChar;

impl CharacterSet for NameChar {
    const NAME: &'static str = "[Name Char]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            ':'
            | 'A'..='Z'
            | '_'
            | 'a'..='z'
            | '\u{C0}'..='\u{D6}'
            | '\u{D8}'..='\u{F6}'
            | '\u{F8}'..='\u{2FF}'
            | '\u{370}'..='\u{37D}'
            | '\u{37F}'..='\u{1FFF}'
            | '\u{200C}'..='\u{200D}'
            | '\u{2070}'..='\u{218F}'
            | '\u{2C00}'..='\u{2FEF}'
            | '\u{3001}'..='\u{D7FF}'
            | '\u{F900}'..='\u{FDCF}'
            | '\u{FDF0}'..='\u{FFFD}'
            | '\u{10000}'..='\u{EFFFF}' => Some(next_char.len_utf8()),
            _ => None,
        }
    }
}

/// [4a] - Name Start Characters
///
/// https://www.w3.org/TR/xml/#NT-NameStartChar
pub struct NameStartChar;

impl CharacterSet for NameStartChar {
    const NAME: &'static str = "[Name Start Char]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            ':'
            | 'A'..='Z'
            | '_'
            | 'a'..='z'
            | '\u{C0}'..='\u{D6}'
            | '\u{D8}'..='\u{F6}'
            | '\u{F8}'..='\u{2FF}'
            | '\u{370}'..='\u{37D}'
            | '\u{37F}'..='\u{1FFF}'
            | '\u{200C}'..='\u{200D}'
            | '\u{2070}'..='\u{218F}'
            | '\u{2C00}'..='\u{2FEF}'
            | '\u{3001}'..='\u{D7FF}'
            | '\u{F900}'..='\u{FDCF}'
            | '\u{FDF0}'..='\u{FFFD}'
            | '\u{10000}'..='\u{EFFFF}'
            | '-'
            | '.'
            | '0'..='9'
            | '\u{B7}'
            | '\u{0300}'..='\u{036F}'
            | '\u{203F}'..='\u{2040}' => Some(next_char.len_utf8()),
            _ => None,
        }
    }
}

/// [13] Public Id Character
///
/// https://www.w3.org/TR/xml/#NT-PubidChar
pub struct PubidChar;

impl CharacterSet for PubidChar {
    const NAME: &'static str = "[Pub]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        match input.chars().next()? {
            '\n'
            | 'r'
            | ' '
            | '\t'
            | '0'..='9'
            | 'a'..='z'
            | 'A'..='Z'
            | '-'
            | '\''
            | '('
            | ')'
            | '+'
            | ','
            | '.'
            | '/'
            | ':'
            | '='
            | '?'
            | ';'
            | '!'
            | '*'
            | '#'
            | '@'
            | '$'
            | '_'
            | '%' => Some(1),
            _ => None,
        }
    }
}

/// Alphabetical Character set.
///
/// Only contains a-z and A-Z.
pub struct LatinAlphabet;

impl CharacterSet for LatinAlphabet {
    const NAME: &'static str = "[Latin Character]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        match input.chars().next()? {
            'a'..='z' | 'A'..='Z' => Some(1),
            _ => None,
        }
    }
}

pub struct ExtendedLatinAlphabet;

impl CharacterSet for ExtendedLatinAlphabet {
    const NAME: &'static str = "[Extended Latin Character]";
    const CONTAINS_SPACES: bool = false;
    fn munch(input: &str) -> Option<usize> {
        match input.chars().next()? {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '.' | '_' | '-' => Some(1),
            _ => None,
        }
    }
}

pub struct CharDataCharSet;

impl CharacterSet for CharDataCharSet {
    const NAME: &'static str = "[^<&]";
    const CONTAINS_SPACES: bool = true;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            '<' | '&' => None,
            ch => Some(ch.len_utf8()),
        }
    }
}

pub struct SingleQuotedAttValueCharacters;

impl CharacterSet for SingleQuotedAttValueCharacters {
    const NAME: &'static str = "[^<&']";
    const CONTAINS_SPACES: bool = true;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            '<' | '&' | '\'' => None,
            ch => Some(ch.len_utf8()),
        }
    }
}

pub struct DoubleQuotedAttValueCharacters;

impl CharacterSet for DoubleQuotedAttValueCharacters {
    const NAME: &'static str = "[^<&\"]";
    const CONTAINS_SPACES: bool = true;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            '<' | '&' | '"' => None,
            ch => Some(ch.len_utf8()),
        }
    }
}

pub struct SingleQuotedEntityValueCharacters;

impl CharacterSet for SingleQuotedEntityValueCharacters {
    const NAME: &'static str = "[^<&%']";
    const CONTAINS_SPACES: bool = true;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            '<' | '&' | '%' | '\'' => None,
            ch => Some(ch.len_utf8()),
        }
    }
}

pub struct DoubleQuotedEntityValueCharacters;

impl CharacterSet for DoubleQuotedEntityValueCharacters {
    const NAME: &'static str = "[^<&%\"]";
    const CONTAINS_SPACES: bool = true;
    fn munch(input: &str) -> Option<usize> {
        let next_char = input.chars().next()?;
        match next_char {
            '<' | '&' | '%' | '"' => None,
            ch => Some(ch.len_utf8()),
        }
    }
}

/// Parse the next literal from a given charset.
/// Expect a string literal.
///
/// The first character is from the `First` char set.
/// The rest of the string is from the `Rest` charset.
///
/// This will only fail if the first encounterd character is not in the First char set.
pub fn expect_string<'src, Start: CharacterSet, Rest: CharacterSet>(
    input: &mut crate::span::Span<'src>,
) -> Result<crate::span::Span<'src>, XmlParsingError<'src>> {
    let mut result = *input;

    /* We require at least one character, otherwise it's an error! */
    let mut length = Start::munch(input.span).ok_or_else(|| {
        let expected = &[Start::NAME];
        XmlParsingError::unexpected(expected, *input)
    })?;

    /* If the character set contains spaces, update the position accordingly */
    /* the if will get optimized away, since it's a constant based on the generic */
    if Start::CONTAINS_SPACES {
        match input.span.chars().next() {
            Some('\n') => {
                input.position.line += 1;
                input.position.column = 0;
            }
            Some('\r') => input.position.column = 0,
            _ => input.position.column += 1,
        }
    } else {
        input.position.column += 1;
    }
    input.span = &input.span[length..];

    /* Once at least one character is found, munch as much as possible */
    while let Some(additional) = Rest::munch(input.span) {
        /* Similarly, optimized of the char set does not contains spaces. */
        if Rest::CONTAINS_SPACES {
            match input.span.chars().next() {
                Some('\n') => {
                    input.position.line += 1;
                    input.position.column = 0;
                }
                Some('\r') => input.position.column = 0,
                _ => input.position.column += 1,
            }
        } else {
            input.position.column += 1;
        }
        length += additional;
        input.span = &input.span[additional..];
    }

    /* Restrict the span of the result to the parsed length */
    result.span = &result.span[..length];

    Ok(result)
}

/// Advance the given input string to skip any number of spaces as defined
/// in the XML documentation: https://www.w3.org/TR/xml/#NT-S
///
/// The function returns the number of character / bytes skipped in the form of a position.
pub fn skip_whitespaces<'src>(input: &mut crate::span::Span<'src>) -> crate::span::Span<'src> {
    let mut result = *input;
    let mut length = 0;
    loop {
        /* We could use the Spaces char set, but we prefer to unpack once to update the position */
        match input.span.chars().next() {
            Some('\n') => {
                length += 1;
                input.position.line += 1;
                input.position.column = 0;
                input.span = &input.span[1..];
            }
            Some('\r') => {
                length += 1;
                input.position.column = 0;
                input.span = &input.span[1..];
            }
            Some(' ') | Some('\t') => {
                length += 1;
                input.position.column += 1;
                input.span = &input.span[1..];
            }
            _ => break,
        }
    }

    result.span = &result.span[..length];

    result
}

/// Skip the whitespaces, expecting to find at least one.
///
/// If no whitespaces are encountered, will return an error.
pub fn expect_whitespaces<'src>(input: &mut crate::span::Span<'src>) -> Result<crate::span::Span<'src>, XmlParsingError<'src>> {
    let span = skip_whitespaces(input);
    if span.pos() == crate::span::Position::ZERO {
        Err(XmlParsingError::unexpected(&[Spaces::NAME], *input))
    } else {
        Ok(span)
    }
}

/// Expects a fixed byte sequence, or throws an error.
pub fn expect_bytes<'src>(input: &mut crate::span::Span<'src>, expected: &'static str) -> Result<(), XmlParsingError<'src>> {
    match input.span.strip_prefix(expected) {
        Some(stripped) => {
            input.span = stripped;
            input.position.column += expected.chars().count();
            Ok(())
        }
        None => Err(XmlParsingError::unexpected(&[expected], *input)),
    }
}

/// Expect the "XML" literal, where any of the three letters can be either uppercased or lowercased.
pub fn expect_not_xml(input: crate::span::Span) -> Result<(), XmlParsingError> {
    let bytes = input.span.as_bytes();
    if bytes.len() != 3 {
        return Ok(());
    }
    match bytes[..3] {
        [0x58 | 0x78, 0x4D | 0x6D, 0x4C | 0x6C] => Err(XmlParsingError::invalid_target(input)),
        _ => Ok(()),
    }
}
