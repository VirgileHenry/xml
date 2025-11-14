mod error;
mod expect;
mod parsing;
mod span;

use parsing::*;

pub use error::XmlParsingError;
pub use expect::ExpectXml;
pub use expect::ExpectXmlError;

pub fn parse_xml<'src>(input: &'src str) -> Result<Document<'src>, XmlParsingError<'src>> {
    let mut input = span::Span::new(span::Position::ZERO, input);
    Document::parse(&mut input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QuoteKind {
    Single,
    Double,
}

impl QuoteKind {
    fn parse<'src>(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        match input.first_char() {
            Some('"') => {
                input.span = &input.span[1..];
                Ok(QuoteKind::Double)
            }
            Some('\'') => {
                input.span = &input.span[1..];
                Ok(QuoteKind::Single)
            }
            _ => {
                let expected = &["\"", "'"];
                Err(XmlParsingError::unexpected(expected, *input))
            }
        }
    }
    fn to_char(self) -> char {
        match self {
            QuoteKind::Single => '\'',
            QuoteKind::Double => '"',
        }
    }
    fn to_str(self) -> &'static str {
        match self {
            QuoteKind::Single => "'",
            QuoteKind::Double => "\"",
        }
    }
}

impl std::fmt::Display for QuoteKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        f.write_char(self.to_char())
    }
}

/// Repetition modifiers, a special set of characters that are used in element definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RepetitionOperator {
    /// Zero or one element, the '?' repetition operator.
    ZeroOrOne,
    /// Zero or more element, the '*' repetition operator.
    ZeroOrMore,
    /// One or more element, the '+' repetition operator.
    OneOrMore,
}

impl RepetitionOperator {
    fn try_parse(input: &mut span::Span) -> Option<Self> {
        match input.first_char() {
            Some('?') => {
                input.bump();
                Some(RepetitionOperator::ZeroOrOne)
            }
            Some('*') => {
                input.bump();
                Some(RepetitionOperator::ZeroOrMore)
            }
            Some('+') => {
                input.bump();
                Some(RepetitionOperator::OneOrMore)
            }
            _ => None,
        }
    }
    fn char(self) -> char {
        match self {
            RepetitionOperator::ZeroOrOne => '?',
            RepetitionOperator::ZeroOrMore => '*',
            RepetitionOperator::OneOrMore => '+',
        }
    }
}

impl std::fmt::Display for RepetitionOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        f.write_char(self.char())
    }
}

trait XmlElement<'src>: Sized {
    fn position(&self) -> span::Position;
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>>;
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()>;
}

/// [1] - Document
///
/// https://www.w3.org/TR/xml/#NT-document
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Document<'src> {
    pub prolog: Prolog<'src>,
    pub element: Element<'src>,
    pub misc: Vec<Miscellaneous<'src>>,
}

impl<'src> Document<'src> {
    pub fn write_xml<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        self.write(output)
    }
}

impl<'src> XmlElement<'src> for Document<'src> {
    fn position(&self) -> span::Position {
        span::Position::ZERO
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let prolog = Prolog::parse(input)?;
        let element = Element::parse(input)?;

        let mut misc = Vec::new();
        loop {
            skip_whitespaces(input);
            if input.str().starts_with(Comment::OPENING_TAG) || input.str().starts_with(Pi::OPENING_TAG) {
                misc.push(Miscellaneous::parse(input)?);
            } else {
                break;
            }
        }

        Ok(Self { prolog, element, misc })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        self.prolog.write(output)?;
        output.write_all("\n".as_bytes())?;
        self.element.write(output)?;
        output.write_all("\n".as_bytes())?;
        for misc in self.misc.iter() {
            misc.write(output)?;
        }
        output.write_all("\n".as_bytes())?;
        Ok(())
    }
}

/// [5] - Name
///
/// https://www.w3.org/TR/xml/#NT-Name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name<'src>(span::Span<'src>);

impl<'src> XmlElement<'src> for Name<'src> {
    fn position(&self) -> span::Position {
        self.0.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        Ok(Self(expect_string::<NameStartChar, NameChar>(input)?))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.str().as_bytes())
    }
}

impl<'src> std::ops::Deref for Name<'src> {
    type Target = span::Span<'src>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'src> std::fmt::Display for Name<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.str())
    }
}

/// [8] - Nm Token
///
/// https://www.w3.org/TR/xml/#NT-Nmtoken
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NmToken<'src>(span::Span<'src>);

impl<'src> XmlElement<'src> for NmToken<'src> {
    fn position(&self) -> span::Position {
        self.0.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        Ok(Self(expect_string::<NameChar, NameChar>(input)?))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.str().as_bytes())
    }
}

impl<'src> std::ops::Deref for NmToken<'src> {
    type Target = span::Span<'src>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'src> std::fmt::Display for NmToken<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.str())
    }
}

/// [9] - Entity Value
///
/// https://www.w3.org/TR/xml/#NT-EntityValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityValue<'src> {
    pub position: span::Position,
    pub literal: Vec<EntityValueElem<'src>>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for EntityValue<'src> {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let prev_input = *input;

        let position = input.pos();
        let quote = QuoteKind::parse(input)?;

        let mut literal = Vec::new();

        match quote {
            QuoteKind::Single => loop {
                match input.first_char() {
                    Some('&') => literal.push(EntityValueElem::Reference(Reference::parse(input)?)),
                    Some('%') => literal.push(EntityValueElem::PEReference(PEReference::parse(input)?)),
                    Some('\'') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<%&]"], *input)),
                    Some(_) => literal.push(EntityValueElem::CharSlice(CharSlice(expect_string::<
                        SingleQuotedEntityValueCharacters,
                        SingleQuotedEntityValueCharacters,
                    >(input)?))),
                    None => return Err(XmlParsingError::unclosed::<Self>(prev_input, "'")),
                }
            },
            QuoteKind::Double => loop {
                match input.first_char() {
                    Some('&') => literal.push(EntityValueElem::Reference(Reference::parse(input)?)),
                    Some('%') => literal.push(EntityValueElem::PEReference(PEReference::parse(input)?)),
                    Some('"') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<%&]"], *input)),
                    Some(_) => literal.push(EntityValueElem::CharSlice(CharSlice(expect_string::<
                        DoubleQuotedEntityValueCharacters,
                        DoubleQuotedEntityValueCharacters,
                    >(input)?))),
                    None => return Err(XmlParsingError::unclosed::<Self>(prev_input, "\"")),
                }
            },
        }

        Ok(Self {
            position,
            literal,
            quote,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.quote.to_str().as_bytes())?;
        for elem in self.literal.iter() {
            match elem {
                EntityValueElem::CharSlice(CharSlice(slice)) => output.write_all(slice.str().as_bytes())?,
                EntityValueElem::Reference(reference) => reference.write(output)?,
                EntityValueElem::PEReference(reference) => reference.write(output)?,
            }
        }
        output.write_all(self.quote.to_str().as_bytes())?;
        Ok(())
    }
}

/// [9a] - Entity Value Element
///
/// Element in the Entity Value list
///
/// https://www.w3.org/TR/xml/#NT-EntityValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityValueElem<'src> {
    CharSlice(CharSlice<'src>),
    Reference(Reference<'src>),
    PEReference(PEReference<'src>),
}

/// [9b] - Character Slice
///
/// Raw characters in the Entity Value Element
///
/// https://www.w3.org/TR/xml/#NT-EntityValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharSlice<'src>(span::Span<'src>);

impl<'src> std::ops::Deref for CharSlice<'src> {
    type Target = span::Span<'src>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// [10] - Attribute Value
///
/// https://www.w3.org/TR/xml/#NT-AttValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttValue<'src> {
    pub position: span::Position,
    pub literal: Vec<AttValueElem<'src>>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for AttValue<'src> {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let prev_input = *input;

        let position = input.pos();
        let quote = QuoteKind::parse(input)?;

        let mut literal = Vec::new();

        match quote {
            QuoteKind::Single => loop {
                match input.first_char() {
                    Some('&') => literal.push(AttValueElem::Reference(Reference::parse(input)?)),
                    Some('\'') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<&]"], *input)),
                    Some(_) => literal.push(AttValueElem::CharSlice(CharSlice(expect_string::<
                        SingleQuotedAttValueCharacters,
                        SingleQuotedAttValueCharacters,
                    >(input)?))),
                    None => return Err(XmlParsingError::unclosed::<Self>(prev_input, "'")),
                }
            },
            QuoteKind::Double => loop {
                match input.first_char() {
                    Some('&') => literal.push(AttValueElem::Reference(Reference::parse(input)?)),
                    Some('"') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<&]"], *input)),
                    Some(_) => literal.push(AttValueElem::CharSlice(CharSlice(expect_string::<
                        DoubleQuotedAttValueCharacters,
                        DoubleQuotedAttValueCharacters,
                    >(input)?))),
                    None => return Err(XmlParsingError::unclosed::<Self>(prev_input, "\"")),
                }
            },
        }
        expect_bytes(input, quote.to_str())?;

        Ok(Self {
            position,
            literal,
            quote,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.quote.to_str().as_bytes())?;
        for elem in self.literal.iter() {
            match elem {
                AttValueElem::CharSlice(slice) => output.write_all(slice.str().as_bytes())?,
                AttValueElem::Reference(reference) => reference.write(output)?,
            }
        }
        output.write_all(self.quote.to_str().as_bytes())?;
        Ok(())
    }
}

impl<'src> std::fmt::Display for AttValue<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.quote.to_str())?;
        for elem in self.literal.iter() {
            match elem {
                AttValueElem::CharSlice(slice) => f.write_str(slice.str())?,
                AttValueElem::Reference(reference) => reference.fmt(f)?,
            }
        }
        f.write_str(self.quote.to_str())?;
        Ok(())
    }
}

/// [10a] - Attribute Value Element
///
/// Element in the Attribute Valute list
///
/// https://www.w3.org/TR/xml/#NT-AttValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttValueElem<'src> {
    CharSlice(CharSlice<'src>),
    Reference(Reference<'src>),
}

/// [11] - System Literal
///
/// https://www.w3.org/TR/xml/#NT-SystemLiteral
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SystemLiteral<'src> {
    pub literal: span::Span<'src>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for SystemLiteral<'src> {
    fn position(&self) -> span::Position {
        self.literal.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let quote = QuoteKind::parse(input)?;
        let (literal, rest) = input.split_span::<Self>(quote.to_str())?;
        *input = rest;
        Ok(Self { literal, quote })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", self.quote, self.literal, self.quote)
    }
}

/// [12] - Public Id Literal
///
/// https://www.w3.org/TR/xml/#NT-PubidLiteral
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PubidLiteral<'src> {
    pub literal: span::Span<'src>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for PubidLiteral<'src> {
    fn position(&self) -> span::Position {
        self.literal.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let quote = QuoteKind::parse(input)?;
        let literal = expect_string::<PubidChar, PubidChar>(input)?;
        expect_bytes(input, quote.to_str())?;
        Ok(Self { literal, quote })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", self.quote, self.literal, self.quote)
    }
}

/// [14] - Character Data
///
/// https://www.w3.org/TR/xml/#NT-Comment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharData<'src>(span::Span<'src>);

impl<'src> XmlElement<'src> for CharData<'src> {
    fn position(&self) -> span::Position {
        self.0.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        Ok(Self(expect_string::<CharDataCharSet, CharDataCharSet>(input)?))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.str().as_bytes())
    }
}

impl<'src> std::ops::Deref for CharData<'src> {
    type Target = span::Span<'src>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// [15] - Comment
///
/// https://www.w3.org/TR/xml/#NT-Comment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment<'src> {
    pub comment: span::Span<'src>,
}

impl<'src> Comment<'src> {
    const OPENING_TAG: &'static str = "<!--";
    const CLOSING_TAG: &'static str = "-->";
}

impl<'src> XmlElement<'src> for Comment<'src> {
    fn position(&self) -> span::Position {
        self.comment.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        skip_whitespaces(input);
        expect_bytes(input, Self::OPENING_TAG)?;
        // Fixme: "--" shall not appear in comments
        let (comment, rest) = input.split_span::<Self>(Self::CLOSING_TAG)?;
        *input = rest;
        Ok(Self { comment })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.comment, Self::CLOSING_TAG)
    }
}

/// [16] - Processing Instruction (PI)
///
/// https://www.w3.org/TR/xml/#NT-PI
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pi<'src> {
    pub target: Name<'src>,
    pub instruction: Option<span::Span<'src>>,
}

impl<'src> Pi<'src> {
    const OPENING_TAG: &'static str = "<?";
    const CLOSING_TAG: &'static str = "?>";
}

impl<'src> XmlElement<'src> for Pi<'src> {
    fn position(&self) -> span::Position {
        self.target.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let target = Name::parse(input)?;
        expect_not_xml(target.0)?;

        let instruction = if input.str().starts_with(Self::CLOSING_TAG) {
            None /* Got closing delimiter, no instruction */
        } else {
            skip_whitespaces(input);
            let (instruction, rest) = input.split_span::<Self>(Self::CLOSING_TAG)?;
            *input = rest;
            Some(instruction)
        };

        Ok(Self { target, instruction })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self.instruction {
            None => write!(output, "{}{}{}", Self::OPENING_TAG, self.target, Self::CLOSING_TAG),
            Some(instruction) => write!(
                output,
                "{}{} {}{}",
                Self::OPENING_TAG,
                self.target,
                instruction,
                Self::CLOSING_TAG
            ),
        }
    }
}

/// [18] - C DATA Section
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CDSect<'src> {
    data: span::Span<'src>,
}

impl<'src> CDSect<'src> {
    const OPENING_TAG: &'static str = "<![CDATA[";
    const CLOSING_TAG: &'static str = "]]>";
}

impl<'src> XmlElement<'src> for CDSect<'src> {
    fn position(&self) -> span::Position {
        self.data.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        /* The usage of find is required, as the data can be anything as per spec */
        let (data, rest) = input.split_span::<Self>(Self::CLOSING_TAG)?;
        *input = rest;
        Ok(Self { data })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.data, Self::CLOSING_TAG)
    }
}

/// [22] - Prolog
///
/// https://www.w3.org/TR/xml/#sec-prolog-dtd
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prolog<'src> {
    pub declaration: Option<XmlDeclaration<'src>>,
    pub misc: Vec<Miscellaneous<'src>>,
    pub doc_type_decl: Option<DoctypeDecl<'src>>,
}

impl<'src> XmlElement<'src> for Prolog<'src> {
    fn position(&self) -> span::Position {
        span::Position::ZERO
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let declaration = if input.str().starts_with(XmlDeclaration::OPENING_TAG) {
            Some(XmlDeclaration::parse(input)?)
        } else {
            None
        };

        let mut misc = Vec::new();
        loop {
            skip_whitespaces(input);
            if input.str().starts_with(Comment::OPENING_TAG) || input.str().starts_with(Pi::OPENING_TAG) {
                misc.push(Miscellaneous::parse(input)?);
            } else {
                break;
            }
        }

        let doc_type_decl = if input.str().starts_with(DoctypeDecl::OPENING_TAG) {
            Some(DoctypeDecl::parse(input)?)
        } else {
            None
        };

        loop {
            skip_whitespaces(input);
            if input.str().starts_with(Comment::OPENING_TAG) || input.str().starts_with(Pi::OPENING_TAG) {
                misc.push(Miscellaneous::parse(input)?);
            } else {
                break;
            }
        }

        Ok(Self {
            declaration,
            misc,
            doc_type_decl,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        if let Some(decl) = &self.declaration {
            decl.write(output)?;
        }
        for misc in self.misc.iter() {
            misc.write(output)?;
        }
        if let Some(doc_type_decl) = &self.doc_type_decl {
            doc_type_decl.write(output)?;
        }
        Ok(())
    }
}

/// [23] - Xml Declaration
///
/// https://www.w3.org/TR/xml/#NT-XMLDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct XmlDeclaration<'src> {
    pub version: VersionInfo,
    pub encoding: Option<EncodingDecl<'src>>,
    pub standalone: Option<SDDecl>,
}

impl<'src> XmlDeclaration<'src> {
    const OPENING_TAG: &'static str = "<?xml";
    const CLOSING_TAG: &'static str = "?>";
}

impl<'src> XmlElement<'src> for XmlDeclaration<'src> {
    fn position(&self) -> span::Position {
        self.version.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        skip_whitespaces(input);
        expect_bytes(input, Self::OPENING_TAG)?;
        /* Parse the version */
        let version = VersionInfo::parse(input)?;

        /* Parse encoding if present */
        let mut temp = *input;
        skip_whitespaces(&mut temp);
        let encoding = if temp.str().starts_with("encoding") {
            Some(EncodingDecl::parse(input)?)
        } else {
            None
        };

        /* Parse standalone if present */
        let mut temp = *input;
        skip_whitespaces(&mut temp);
        let standalone = if temp.str().starts_with("standalone") {
            Some(SDDecl::parse(input)?)
        } else {
            None
        };

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;

        Ok(Self {
            version,
            encoding,
            standalone,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write(Self::OPENING_TAG.as_bytes())?;
        self.version.write(output)?;
        if let Some(encoding) = &self.encoding {
            encoding.write(output)?;
        }
        if let Some(standalone) = &self.standalone {
            standalone.write(output)?;
        }
        output.write(Self::CLOSING_TAG.as_bytes())?;
        Ok(())
    }
}

/// [24] - Version Information
///
/// https://www.w3.org/TR/xml/#NT-VersionInfo
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VersionInfo {
    pub position: span::Position,
    pub major: usize,
    pub minor: usize,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for VersionInfo {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input)?;

        let position = input.pos();
        expect_bytes(input, "version")?;

        skip_whitespaces(input);
        expect_bytes(input, "=")?;
        skip_whitespaces(input);

        let quote = QuoteKind::parse(input)?;

        let major = expect_string::<DecimalDigits, DecimalDigits>(input)?;
        let major = major
            .str()
            .parse()
            .map_err(|_| XmlParsingError::unexpected(&["uint"], major))?;
        expect_bytes(input, ".")?;
        let minor = expect_string::<DecimalDigits, DecimalDigits>(input)?;
        let minor = minor
            .str()
            .parse()
            .map_err(|_| XmlParsingError::unexpected(&["uint"], minor))?;

        expect_bytes(input, quote.to_str())?;

        Ok(Self {
            position,
            major,
            minor,
            quote,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, " version={}{}.{}{}", self.quote, self.major, self.minor, self.quote)
    }
}

/// [27] - Miscellaneous (Misc)
///
/// https://www.w3.org/TR/xml/#NT-Misc
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Miscellaneous<'src> {
    Comment(Comment<'src>),
    Pi(Pi<'src>),
}

impl<'src> XmlElement<'src> for Miscellaneous<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::Comment(comment) => comment.position(),
            Self::Pi(pi) => pi.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(Comment::OPENING_TAG) {
            Ok(Miscellaneous::Comment(Comment::parse(input)?))
        } else if input.str().starts_with(Pi::OPENING_TAG) {
            Ok(Miscellaneous::Pi(Pi::parse(input)?))
        } else {
            Err(XmlParsingError::unexpected(&["<!--", "<?"], *input))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::Comment(comment) => comment.write(output),
            Self::Pi(pi) => pi.write(output),
        }
    }
}

/// [28] - Doctype Declaration
///
/// https://www.w3.org/TR/xml/#NT-doctypedecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DoctypeDecl<'src> {
    pub name: Name<'src>,
    pub external_id: Option<ExternalID<'src>>,
    pub int_subset: Option<IntSubset<'src>>,
}

impl<'src> DoctypeDecl<'src> {
    const OPENING_TAG: &'static str = "<!DOCTYPE";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for DoctypeDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        skip_whitespaces(input);
        let name = Name::parse(input)?;
        skip_whitespaces(input);
        let external_id = if input.str().starts_with(ExternalID::SYSTEM_TAG) || input.str().starts_with(ExternalID::PUBLIC_TAG) {
            Some(ExternalID::parse(input)?)
        } else {
            None
        };
        skip_whitespaces(input);
        let int_subset = if input.str().starts_with("[") {
            expect_bytes(input, "[")?;
            let subset = IntSubset::parse(input)?;
            expect_bytes(input, "]")?;
            skip_whitespaces(input);
            Some(subset)
        } else {
            None
        };
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(Self {
            name,
            external_id,
            int_subset,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} {}", Self::OPENING_TAG, self.name)?;
        if let Some(external_id) = &self.external_id {
            write!(output, " ")?;
            external_id.write(output)?;
        }
        write!(output, " ")?;
        if let Some(int_subset) = &self.int_subset {
            write!(output, "[")?;
            int_subset.write(output)?;
            write!(output, "] ")?;
        }
        write!(output, "{}", Self::CLOSING_TAG)
    }
}

/// [28a] - Declaration Separator
///
/// https://www.w3.org/TR/xml/#NT-DeclSep
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclSep<'src> {
    PEReference(PEReference<'src>),
    Spaces(Spaces<'src>),
}

impl<'src> XmlElement<'src> for DeclSep<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::PEReference(pe_ref) => pe_ref.position(),
            Self::Spaces(spaces) => spaces.0.pos(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(PEReference::OPENING_TAG) {
            Ok(Self::PEReference(PEReference::parse(input)?))
        } else {
            Ok(Self::Spaces(Spaces(expect_whitespaces(input)?)))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::PEReference(pe_ref) => pe_ref.write(output),
            Self::Spaces(_) => write!(output, " "),
        }
    }
}

/// [28aa] - Declaration Separator Space
///
/// Space within a Declaration Separator
///
/// https://www.w3.org/TR/xml/#NT-DeclSep
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spaces<'src>(span::Span<'src>);

impl<'src> std::ops::Deref for Spaces<'src> {
    type Target = span::Span<'src>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// [28b] - Int Subset
///
/// https://www.w3.org/TR/xml/#NT-intSubset
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntSubset<'src> {
    pub position: span::Position,
    pub elements: Vec<IntSubsetElement<'src>>,
}

impl<'src> XmlElement<'src> for IntSubset<'src> {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        let mut elements = Vec::new();

        loop {
            match input.first_char() {
                /* Spaces or "%" make a decl separator */
                Some(' ') | Some('\t') | Some('\r') | Some('\n') | Some('%') => {
                    elements.push(IntSubsetElement::DeclSep(DeclSep::parse(input)?))
                }
                /* "<" are the start of a markup declaration */
                Some('<') => elements.push(IntSubsetElement::MarkupDecl(MarkupDecl::parse(input)?)),
                /* "]" is the expected character after the int subset */
                Some(']') => break,
                Some(_) => return Err(XmlParsingError::unexpected(&["space", "]"], *input)),
                None => return Err(XmlParsingError::unexpected(&["space", "]"], *input)),
            }
        }

        Ok(Self { position, elements })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        for element in self.elements.iter() {
            match element {
                IntSubsetElement::DeclSep(decl_sep) => decl_sep.write(output)?,
                IntSubsetElement::MarkupDecl(markup_decl) => markup_decl.write(output)?,
            }
        }
        Ok(())
    }
}

/// [28ba] - Int Subset Element
///
/// Element in the Int Subset
///
/// https://www.w3.org/TR/xml/#NT-intSubset
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntSubsetElement<'src> {
    MarkupDecl(MarkupDecl<'src>),
    DeclSep(DeclSep<'src>),
}

/// [29] - Markup Declaration
///
/// https://www.w3.org/TR/xml/#NT-markupdecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MarkupDecl<'src> {
    ElementDecl(ElementDecl<'src>),
    AttListDecl(AttListDecl<'src>),
    EntityDecl(EntityDecl<'src>),
    NotationDecl(NotationDecl<'src>),
    Pi(Pi<'src>),
    Comment(Comment<'src>),
}

impl<'src> XmlElement<'src> for MarkupDecl<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::ElementDecl(element) => element.position(),
            Self::AttListDecl(att_list_decl) => att_list_decl.position(),
            Self::EntityDecl(decl) => decl.position(),
            Self::NotationDecl(decl) => decl.position(),
            Self::Pi(pi) => pi.position(),
            Self::Comment(comment) => comment.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(ElementDecl::OPENING_TAG) {
            Ok(Self::ElementDecl(ElementDecl::parse(input)?))
        } else if input.str().starts_with(AttListDecl::OPENING_TAG) {
            Ok(Self::AttListDecl(AttListDecl::parse(input)?))
        } else if input.str().starts_with(EntityDecl::OPENING_TAG) {
            Ok(Self::EntityDecl(EntityDecl::parse(input)?))
        } else if input.str().starts_with(NotationDecl::OPENING_TAG) {
            Ok(Self::NotationDecl(NotationDecl::parse(input)?))
        } else if input.str().starts_with(Pi::OPENING_TAG) {
            Ok(Self::Pi(Pi::parse(input)?))
        } else if input.str().starts_with(Comment::OPENING_TAG) {
            Ok(Self::Comment(Comment::parse(input)?))
        } else {
            let expected = &[
                ElementDecl::OPENING_TAG,
                AttListDecl::OPENING_TAG,
                EntityDecl::OPENING_TAG,
                NotationDecl::OPENING_TAG,
                Pi::OPENING_TAG,
                Comment::OPENING_TAG,
            ];
            Err(XmlParsingError::unexpected(expected, *input))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::ElementDecl(element) => element.write(output),
            Self::AttListDecl(att_list_decl) => att_list_decl.write(output),
            Self::EntityDecl(decl) => decl.write(output),
            Self::NotationDecl(decl) => decl.write(output),
            Self::Pi(pi) => pi.write(output),
            Self::Comment(comment) => comment.write(output),
        }
    }
}

/// [32] - Standalone Declaration
///
/// https://www.w3.org/TR/xml/#NT-SDDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SDDecl {
    pub position: span::Position,
    pub standalone: bool,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for SDDecl {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input)?;

        let position = input.pos();
        expect_bytes(input, "standalone")?;

        skip_whitespaces(input);
        expect_bytes(input, "=")?;
        skip_whitespaces(input);

        let quote = QuoteKind::parse(input)?;

        let standalone = if let Some(stripped) = input.strip_prefix("yes") {
            *input = stripped;
            true
        } else if let Some(stripped) = input.strip_prefix("no") {
            *input = stripped;
            false
        } else {
            return Err(XmlParsingError::unexpected(&["yes", "no"], *input));
        };
        expect_bytes(input, quote.to_str())?;
        Ok(Self {
            position,
            standalone,
            quote,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        let standalone = match self.standalone {
            true => "yes",
            false => "no",
        };
        write!(output, " standalone={}{}{}", self.quote, standalone, self.quote)
    }
}

/// [39] - Element
///
/// https://www.w3.org/TR/xml/#NT-element
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Element<'src> {
    EmptyElemTag(EmptyElemTag<'src>),
    Element(NonEmptyElement<'src>),
}

impl<'src> Element<'src> {
    const OPENING_TAG: &'static str = "<";

    fn parse_start_or_empty(
        input: &mut span::Span<'src>,
    ) -> Result<Result<STag<'src>, EmptyElemTag<'src>>, XmlParsingError<'src>> {
        /* Since start and empty tags are A LOT alike, parse anyway and make the decision later */
        expect_bytes(input, STag::OPENING_TAG)?;
        let name = Name::parse(input)?;

        let mut attributes = Vec::new();
        loop {
            /* Check for terminator */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.str().starts_with(STag::CLOSING_TAG) | temp.str().starts_with(EmptyElemTag::CLOSING_TAG) {
                break;
            }
            /* No terminator, parse next attribute */
            expect_whitespaces(input)?;
            attributes.push(Attribute::parse(input)?);
        }

        skip_whitespaces(input);
        if let Some(stripped) = input.strip_prefix(STag::CLOSING_TAG) {
            *input = stripped;
            Ok(Ok(STag { name, attributes }))
        } else {
            expect_bytes(input, EmptyElemTag::CLOSING_TAG)?;
            Ok(Err(EmptyElemTag { name, attributes }))
        }
    }
}

impl<'src> XmlElement<'src> for Element<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::EmptyElemTag(empty) => empty.position(),
            Self::Element(elem) => elem.s_tag.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        match Self::parse_start_or_empty(input)? {
            Ok(s_tag) => {
                let content = Content::parse(input)?;
                let e_tag = ETag::parse(input)?;
                Ok(Self::Element(NonEmptyElement { s_tag, content, e_tag }))
            }
            Err(empty_elem_tag) => Ok(Self::EmptyElemTag(empty_elem_tag)),
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::EmptyElemTag(empty) => empty.write(output),
            Self::Element(NonEmptyElement { s_tag, content, e_tag }) => {
                s_tag.write(output)?;
                content.write(output)?;
                e_tag.write(output)?;
                Ok(())
            }
        }
    }
}

/// [39a] - Non Empty Element
///
/// Element that is not an EmptyElement
///
/// https://www.w3.org/TR/xml/#NT-element
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NonEmptyElement<'src> {
    s_tag: STag<'src>,
    content: Content<'src>,
    e_tag: ETag<'src>,
}

/// [40] - Start Tag
///
/// https://www.w3.org/TR/xml/#NT-STag
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct STag<'src> {
    pub name: Name<'src>,
    pub attributes: Vec<Attribute<'src>>,
}

impl<'src> STag<'src> {
    const OPENING_TAG: &'static str = "<";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for STag<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let name = Name::parse(input)?;

        let mut attributes = Vec::new();
        loop {
            /* Check for terminator */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.str().starts_with(Self::CLOSING_TAG) {
                break;
            }
            /* No terminator, parse next attribute */
            expect_whitespaces(input)?;
            attributes.push(Attribute::parse(input)?);
        }

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;

        Ok(Self { name, attributes })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}", Self::OPENING_TAG, self.name)?;
        for attribute in self.attributes.iter() {
            output.write_all(" ".as_bytes())?;
            attribute.write(output)?;
        }
        output.write_all(Self::CLOSING_TAG.as_bytes())?;
        Ok(())
    }
}

/// [41] - Attribute
///
/// https://www.w3.org/TR/xml/#NT-Attribute
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute<'src> {
    pub name: Name<'src>,
    pub value: AttValue<'src>,
}

impl<'src> XmlElement<'src> for Attribute<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let name = Name::parse(input)?;
        skip_whitespaces(input);
        expect_bytes(input, "=")?;
        skip_whitespaces(input);
        let value = AttValue::parse(input)?;
        Ok(Self { name, value })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}=", self.name)?;
        self.value.write(output)?;
        Ok(())
    }
}

/// [42] - End Tag
///
/// https://www.w3.org/TR/xml/#NT-ETag
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ETag<'src> {
    pub name: Name<'src>,
}

impl<'src> ETag<'src> {
    const OPENING_TAG: &'static str = "</";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for ETag<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let name = Name::parse(input)?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;

        Ok(Self { name })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.name, Self::CLOSING_TAG)
    }
}

/// [43] - Content
///
/// https://www.w3.org/TR/xml/#NT-content
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Content<'src> {
    pub position: span::Position,
    pub first_chars: Option<CharData<'src>>,
    pub content: Vec<(ContentElement<'src>, Option<CharData<'src>>)>,
}

impl<'src> XmlElement<'src> for Content<'src> {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        let first_chars = if input.str().starts_with(ContentElement::OPENING_CHARS) {
            None
        } else {
            Some(CharData::parse(input)?)
        };

        let mut content = Vec::new();
        loop {
            /* The ETag start indicates we are done here */
            if input.str().starts_with(ETag::OPENING_TAG) {
                break;
            }
            /* Otherwise, we have remaining content to parse! */
            let element = if input.str().starts_with(CDSect::OPENING_TAG) {
                ContentElement::CDSect(CDSect::parse(input)?)
            } else if input.str().starts_with(Pi::OPENING_TAG) {
                ContentElement::Pi(Pi::parse(input)?)
            } else if input.str().starts_with(Comment::OPENING_TAG) {
                ContentElement::Comment(Comment::parse(input)?)
            } else if input.str().starts_with(Element::OPENING_TAG) {
                ContentElement::Element(Element::parse(input)?)
            } else if input.str().starts_with(Reference::OPENING_TAG) {
                ContentElement::Reference(Reference::parse(input)?)
            } else {
                return Err(XmlParsingError::unexpected(
                    &[
                        CDSect::OPENING_TAG,
                        Pi::OPENING_TAG,
                        Comment::OPENING_TAG,
                        Element::OPENING_TAG,
                        Reference::OPENING_TAG,
                    ],
                    *input,
                ));
            };

            let chars = if input.str().starts_with(ContentElement::OPENING_CHARS) {
                None
            } else {
                Some(CharData::parse(input)?)
            };

            content.push((element, chars));
        }

        Ok(Self {
            position,
            first_chars,
            content,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        if let Some(chars) = &self.first_chars {
            output.write_all(chars.str().as_bytes())?;
        }
        for (element, chars) in self.content.iter() {
            match element {
                ContentElement::Element(elem) => elem.write(output)?,
                ContentElement::Reference(reference) => reference.write(output)?,
                ContentElement::CDSect(cd_sect) => cd_sect.write(output)?,
                ContentElement::Pi(pi) => pi.write(output)?,
                ContentElement::Comment(comment) => comment.write(output)?,
            }
            if let Some(chars) = chars {
                output.write_all(chars.str().as_bytes())?;
            }
        }
        Ok(())
    }
}

/// [43a] - Content Element
///
/// Element in the Content list
///
/// https://www.w3.org/TR/xml/#NT-content
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContentElement<'src> {
    Element(Element<'src>),
    Reference(Reference<'src>),
    CDSect(CDSect<'src>),
    Pi(Pi<'src>),
    Comment(Comment<'src>),
}

impl<'src> ContentElement<'src> {
    /// List of all characters that are at the start of the content special elems.
    /// While parsing a Content, if we do not encounter one of these, we are parsing char data.
    const OPENING_CHARS: &'static [char] = &['<', '&'];
}

/// [44] - Empty Element Tag
///
/// https://www.w3.org/TR/xml/#NT-EmptyElemTag
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EmptyElemTag<'src> {
    name: Name<'src>,
    attributes: Vec<Attribute<'src>>,
}

impl<'src> EmptyElemTag<'src> {
    const OPENING_TAG: &'static str = "<";
    const CLOSING_TAG: &'static str = "/>";
}

impl<'src> XmlElement<'src> for EmptyElemTag<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let name = Name::parse(input)?;

        let mut attributes = Vec::new();
        loop {
            /* Check for terminator */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.str().starts_with(Self::CLOSING_TAG) {
                break;
            }
            /* No terminator, parse next attribute */
            expect_whitespaces(input)?;
            attributes.push(Attribute::parse(input)?);
        }

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;

        Ok(Self { name, attributes })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}", Self::OPENING_TAG, self.name)?;
        for attribute in self.attributes.iter() {
            output.write_all(" ".as_bytes())?;
            attribute.write(output)?;
        }
        output.write_all(Self::CLOSING_TAG.as_bytes())?;
        Ok(())
    }
}

/// [45] - Element declaration.
///
/// https://www.w3.org/TR/xml/#NT-elementdecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ElementDecl<'src> {
    pub name: Name<'src>,
    pub content_spec: ContentSpec<'src>,
}

impl<'src> ElementDecl<'src> {
    const OPENING_TAG: &'static str = "<!ELEMENT";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for ElementDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;
        expect_whitespaces(input)?;
        let content_spec = ContentSpec::parse(input)?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(Self { name, content_spec })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} {} ", Self::OPENING_TAG, self.name)?;
        self.content_spec.write(output)?;
        write!(output, "{}", Self::CLOSING_TAG)
    }
}

/// [46] - Content Specification
///
/// https://www.w3.org/TR/xml/#NT-contentspec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContentSpec<'src> {
    Empty(ContentSpecEmpty),
    Any(ContentSpecAny),
    Mixed(Mixed<'src>),
    Children(Children<'src>),
}

impl<'src> XmlElement<'src> for ContentSpec<'src> {
    fn position(&self) -> span::Position {
        match self {
            ContentSpec::Empty(empty) => empty.position,
            ContentSpec::Any(any) => any.position,
            ContentSpec::Mixed(mixed) => mixed.position(),
            ContentSpec::Children(children) => children.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        if let Some(prefixed) = input.strip_prefix("EMPTY") {
            *input = prefixed;
            Ok(Self::Empty(ContentSpecEmpty { position }))
        } else if let Some(prefixed) = input.strip_prefix("ANY") {
            *input = prefixed;
            Ok(Self::Any(ContentSpecAny { position }))
        } else {
            let mut temp = *input;
            expect_bytes(&mut temp, "(")?;
            skip_whitespaces(&mut temp);
            if temp.str().starts_with(Mixed::PCDATA_TAG) {
                Ok(Self::Mixed(Mixed::parse(input)?))
            } else {
                Ok(Self::Children(Children::parse(input)?))
            }
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            ContentSpec::Empty(_) => output.write_all("EMPTY".as_bytes()),
            ContentSpec::Any(_) => output.write_all("ANY".as_bytes()),
            ContentSpec::Mixed(_) => unimplemented!(),
            ContentSpec::Children(children) => children.write(output),
        }
    }
}

/// [46a] - Empty Content Specification
///
/// The "EMPTY" Content Specification
///
/// https://www.w3.org/TR/xml/#NT-contentspec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContentSpecEmpty {
    position: span::Position,
}

/// [46b] - Any Content Specification
///
/// The "ANY" Content Specification
///
/// https://www.w3.org/TR/xml/#NT-contentspec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContentSpecAny {
    position: span::Position,
}

/// [47] - Children
///
/// https://www.w3.org/TR/xml/#NT-children
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Children<'src> {
    Choice(ChildrenChoice<'src>),
    Seq(ChildrenSeq<'src>),
}

impl<'src> XmlElement<'src> for Children<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::Choice(choice) => choice.position(),
            Self::Seq(seq) => seq.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        expect_bytes(input, "(")?;

        skip_whitespaces(input);

        let mut cps = Vec::new();
        cps.push(Cp::parse(input)?);

        skip_whitespaces(input);
        match input.first_char() {
            /* Closing parens right after the first elem, it's a one element sequence */
            Some(')') => {
                input.bump();
                let repetition = RepetitionOperator::try_parse(input);
                Ok(Self::Seq(ChildrenSeq {
                    seq: Seq { position, sequence: cps },
                    repetition,
                }))
            }
            /* A comma indicates a sequence of more than one element */
            Some(',') => loop {
                input.bump();
                skip_whitespaces(input);
                cps.push(Cp::parse(input)?);
                skip_whitespaces(input);
                /* If we have a closing parens, terminate the sequence */
                if input.first_char() == Some(')') {
                    input.bump();
                    let repetition = RepetitionOperator::try_parse(input);
                    break Ok(Self::Seq(ChildrenSeq {
                        seq: Seq { position, sequence: cps },
                        repetition,
                    }));
                }
                /* Otherwise, keep munching at the sequence */
            },
            /* A vertical bar indicate a choice of multiple elements */
            Some('|') => loop {
                input.bump();
                skip_whitespaces(input);
                cps.push(Cp::parse(input)?);
                skip_whitespaces(input);
                /* If we have a closing parens, terminate the sequence */
                if input.first_char() == Some(')') {
                    input.bump();
                    let repetition = RepetitionOperator::try_parse(input);
                    break Ok(Self::Choice(ChildrenChoice {
                        choice: Choice { position, choices: cps },
                        repetition,
                    }));
                }
                /* Otherwise, keep munching at the sequence */
            },
            _ => Err(XmlParsingError::unexpected(&[")", ",", "|"], *input)),
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Children::Choice(ChildrenChoice { choice, repetition }) => {
                choice.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
            Children::Seq(ChildrenSeq { seq, repetition }) => {
                seq.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
        }
    }
}

/// [47a] - Children Choice
///
/// Children Choice variation.
///
/// https://www.w3.org/TR/xml/#NT-children
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChildrenChoice<'src> {
    choice: Choice<'src>,
    repetition: Option<RepetitionOperator>,
}

impl<'src> ChildrenChoice<'src> {
    fn position(&self) -> span::Position {
        self.choice.position()
    }
}

/// [47b] - Children Seq
///
/// Children Seq variation.
///
/// https://www.w3.org/TR/xml/#NT-children
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChildrenSeq<'src> {
    seq: Seq<'src>,
    repetition: Option<RepetitionOperator>,
}

impl<'src> ChildrenSeq<'src> {
    fn position(&self) -> span::Position {
        self.seq.position()
    }
}

/// [48] - Content Particle
///
/// https://www.w3.org/TR/xml/#NT-cp
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cp<'src> {
    Name(CpName<'src>),
    Choice(CpChoice<'src>),
    Seq(CpSeq<'src>),
}

impl<'src> XmlElement<'src> for Cp<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::Name(name) => name.name.pos(),
            Self::Choice(choice) => choice.position(),
            Self::Seq(seq) => seq.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with("(") {
            /* Since a content particle that is not a name is basically a children, use it and unpack */
            Ok(match Children::parse(input)? {
                Children::Seq(ChildrenSeq { seq, repetition }) => Self::Seq(CpSeq { seq, repetition }),
                Children::Choice(ChildrenChoice { choice, repetition }) => Self::Choice(CpChoice { choice, repetition }),
            })
        } else {
            let name = Name::parse(input)?;
            let repetition = RepetitionOperator::try_parse(input);
            Ok(Self::Name(CpName { name, repetition }))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Cp::Name(CpName { name, repetition }) => match repetition {
                Some(repetition) => write!(output, "{name}{repetition}"),
                None => output.write_all(name.str().as_bytes()),
            },
            Cp::Choice(CpChoice { choice, repetition }) => {
                choice.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
            Cp::Seq(CpSeq { seq, repetition }) => {
                seq.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
        }
    }
}

/// [48a] - Content Particle Name
///
/// The Name variation of a Content Particle
///
/// https://www.w3.org/TR/xml/#NT-cp
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CpName<'src> {
    name: Name<'src>,
    repetition: Option<RepetitionOperator>,
}

/// [48b] - Content Particle Choice
///
/// The Choice variation of a Content Particle
///
/// https://www.w3.org/TR/xml/#NT-cp
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CpChoice<'src> {
    choice: Choice<'src>,
    repetition: Option<RepetitionOperator>,
}

impl<'src> CpChoice<'src> {
    fn position(&self) -> span::Position {
        self.choice.position()
    }
}

/// [48c] - Content Particle Seq
///
/// The Seq variation of a Content Particle
///
/// https://www.w3.org/TR/xml/#NT-cp
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CpSeq<'src> {
    seq: Seq<'src>,
    repetition: Option<RepetitionOperator>,
}

impl<'src> CpSeq<'src> {
    fn position(&self) -> span::Position {
        self.seq.position()
    }
}

/// [49] - Choice
///
/// https://www.w3.org/TR/xml/#NT-choice
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Choice<'src> {
    pub position: span::Position,
    pub choices: Vec<Cp<'src>>,
}

impl<'src> Choice<'src> {
    fn position(&self) -> span::Position {
        self.position
    }

    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        for (i, choice) in self.choices.iter().enumerate() {
            choice.write(output)?;
            if i < self.choices.len() - 1 {
                write!(output, " | ")?;
            }
        }
        Ok(())
    }
}

/// [50] - Seq
///
/// https://www.w3.org/TR/xml/#NT-seq
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Seq<'src> {
    pub position: span::Position,
    pub sequence: Vec<Cp<'src>>,
}

impl<'src> Seq<'src> {
    fn position(&self) -> span::Position {
        self.position
    }

    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        for (i, item) in self.sequence.iter().enumerate() {
            item.write(output)?;
            if i < self.sequence.len() - 1 {
                write!(output, ", ")?;
            }
        }
        Ok(())
    }
}

/// [51] - Mixed
///
/// https://www.w3.org/TR/xml/#NT-Mixed
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mixed<'src> {
    MixedEmpty(MixedEmpty),
    MixedContent(MixedContent<'src>),
}

impl<'src> Mixed<'src> {
    const PCDATA_TAG: &'static str = "#PCDATA";
}

impl<'src> XmlElement<'src> for Mixed<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::MixedEmpty(mixed) => mixed.position,
            Self::MixedContent(content) => content.position,
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, "(")?;
        skip_whitespaces(input);

        let position = input.pos();
        expect_bytes(input, Self::PCDATA_TAG)?;
        skip_whitespaces(input);

        match input.first_char() {
            /* Parse a name sequence for mixed content */
            Some('|') => {
                input.bump();
                let mut names = Vec::new();
                loop {
                    skip_whitespaces(input);
                    names.push(Name::parse(input)?);
                    skip_whitespaces(input);
                    match input.first_char() {
                        Some(')') => break,
                        Some('|') => input.bump(),
                        _ => return Err(XmlParsingError::unexpected(&[")", "|"], *input)),
                    }
                }
                expect_bytes(input, ")*")?;
                Ok(Self::MixedContent(MixedContent { position, names }))
            }
            Some(')') => {
                input.bump();
                Ok(Self::MixedEmpty(MixedEmpty { position }))
            }
            _ => return Err(XmlParsingError::unexpected(&[")", "|"], *input)),
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "( {} ", Self::PCDATA_TAG)?;
        match self {
            Self::MixedEmpty(_) => write!(output, ")")?,
            Self::MixedContent(content) => {
                for name in content.names.iter() {
                    write!(output, "| {name} ")?;
                }
                write!(output, ")")?;
            }
        }
        Ok(())
    }
}

/// [51a] - Mixed Empty
///
/// The Empty variation of the Mixed Content
///
/// https://www.w3.org/TR/xml/#NT-Mixed
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MixedEmpty {
    position: span::Position,
}

/// [51b] - Mixed Content
///
/// The variation with content of the Mixed content
///
/// https://www.w3.org/TR/xml/#NT-Mixed
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MixedContent<'src> {
    position: span::Position,
    names: Vec<Name<'src>>,
}

/// [52] - Attribute List Declaration
///
/// https://www.w3.org/TR/xml/#NT-AttlistDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttListDecl<'src> {
    pub name: Name<'src>,
    pub definitions: Vec<AttDef<'src>>,
}

impl<'src> AttListDecl<'src> {
    const OPENING_TAG: &'static str = "<!ATTLIST";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for AttListDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;

        let mut definitions = Vec::new();

        loop {
            /* Check if we may have a terminator after spaces */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.str().starts_with(Self::CLOSING_TAG) {
                break;
            }
            /* Otherwise, keep parsing the attribute definition list */
            definitions.push(AttDef::parse(input)?);
        }

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;

        Ok(Self { name, definitions })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} {}", Self::OPENING_TAG, self.name)?;
        for definition in self.definitions.iter() {
            definition.write(output)?;
        }
        write!(output, " {}", Self::CLOSING_TAG)?;
        Ok(())
    }
}

/// [53] - Attribut Definition
///
/// https://www.w3.org/TR/xml/#NT-AttDef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttDef<'src> {
    pub name: Name<'src>,
    pub attribute_type: AttributeType<'src>,
    pub default_decl: DefaultDecl<'src>,
}

impl<'src> XmlElement<'src> for AttDef<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;
        expect_whitespaces(input)?;
        let attribute_type = AttributeType::parse(input)?;
        expect_whitespaces(input)?;
        let default_decl = DefaultDecl::parse(input)?;
        Ok(Self {
            name,
            attribute_type,
            default_decl,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(" ".as_bytes())?;
        self.name.write(output)?;
        output.write_all(" ".as_bytes())?;
        self.attribute_type.write(output)?;
        output.write_all(" ".as_bytes())?;
        self.default_decl.write(output)?;
        Ok(())
    }
}

/// [54] - Attribute Type
///
/// https://www.w3.org/TR/xml/#NT-AttType
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeType<'src> {
    StringType(StringType),
    TokenizedType(TokenizedType),
    EnumeratedType(EnumeratedType<'src>),
}

impl<'src> XmlElement<'src> for AttributeType<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::StringType(string) => string.position(),
            Self::TokenizedType(tokenized) => tokenized.position(),
            Self::EnumeratedType(enumerated) => enumerated.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(StringType::TAG) {
            Ok(Self::StringType(StringType::parse(input)?))
        } else if input.str().starts_with(TokenizedType::ID_TAG)
            || input.str().starts_with(TokenizedType::IDREF_TAG)
            || input.str().starts_with(TokenizedType::IDREFS_TAG)
            || input.str().starts_with(TokenizedType::ENTITY_TAG)
            || input.str().starts_with(TokenizedType::ENTITIES_TAG)
            || input.str().starts_with(TokenizedType::NMTOKEN_TAG)
            || input.str().starts_with(TokenizedType::NMTOKENS_TAG)
        {
            Ok(Self::TokenizedType(TokenizedType::parse(input)?))
        } else if input.str().starts_with(NotationType::OPENING_TAG) || input.str().starts_with("(") {
            Ok(Self::EnumeratedType(EnumeratedType::parse(input)?))
        } else {
            Err(XmlParsingError::unexpected(
                &[
                    StringType::TAG,
                    TokenizedType::ID_TAG,
                    TokenizedType::IDREF_TAG,
                    TokenizedType::IDREFS_TAG,
                    TokenizedType::ENTITY_TAG,
                    TokenizedType::ENTITIES_TAG,
                    TokenizedType::NMTOKEN_TAG,
                    TokenizedType::NMTOKENS_TAG,
                ],
                *input,
            ))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::StringType(string) => string.write(output),
            Self::TokenizedType(tokenized) => tokenized.write(output),
            Self::EnumeratedType(enumerated) => enumerated.write(output),
        }
    }
}

/// [55] - XmlParsingError<'src> Type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringType {
    position: span::Position,
}

impl StringType {
    const TAG: &'static str = "CDATA";
}

impl<'src> XmlElement<'src> for StringType {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        expect_bytes(input, Self::TAG)?;
        Ok(Self { position })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}", Self::TAG)
    }
}

/// [56] - Tokenized Type
///
/// https://www.w3.org/TR/xml/#NT-TokenizedType
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenizedType {
    Id(span::Position),
    IdRef(span::Position),
    IdRefs(span::Position),
    Entity(span::Position),
    Entities(span::Position),
    NmToken(span::Position),
    NmTokens(span::Position),
}

impl TokenizedType {
    const ID_TAG: &'static str = "ID";
    const IDREF_TAG: &'static str = "IDREF";
    const IDREFS_TAG: &'static str = "IDREFS";
    const ENTITY_TAG: &'static str = "ENTITY";
    const ENTITIES_TAG: &'static str = "ENTITIES";
    const NMTOKEN_TAG: &'static str = "NMTOKEN";
    const NMTOKENS_TAG: &'static str = "NMTOKENS";
}

impl<'src> XmlElement<'src> for TokenizedType {
    fn position(&self) -> span::Position {
        match self {
            Self::Id(position) => *position,
            Self::IdRef(position) => *position,
            Self::IdRefs(position) => *position,
            Self::Entity(position) => *position,
            Self::Entities(position) => *position,
            Self::NmToken(position) => *position,
            Self::NmTokens(position) => *position,
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        if let Some(stripped) = input.strip_prefix(Self::ID_TAG) {
            *input = stripped;
            Ok(Self::Id(position))
        } else if let Some(stripped) = input.strip_prefix(Self::IDREF_TAG) {
            *input = stripped;
            Ok(Self::IdRef(position))
        } else if let Some(stripped) = input.strip_prefix(Self::IDREFS_TAG) {
            *input = stripped;
            Ok(Self::IdRefs(position))
        } else if let Some(stripped) = input.strip_prefix(Self::ENTITY_TAG) {
            *input = stripped;
            Ok(Self::Entity(position))
        } else if let Some(stripped) = input.strip_prefix(Self::ENTITIES_TAG) {
            *input = stripped;
            Ok(Self::Entities(position))
        } else if let Some(stripped) = input.strip_prefix(Self::NMTOKEN_TAG) {
            *input = stripped;
            Ok(Self::NmToken(position))
        } else if let Some(stripped) = input.strip_prefix(Self::NMTOKENS_TAG) {
            *input = stripped;
            Ok(Self::NmTokens(position))
        } else {
            Err(XmlParsingError::unexpected(
                &[
                    Self::ID_TAG,
                    Self::IDREF_TAG,
                    Self::IDREFS_TAG,
                    Self::ENTITY_TAG,
                    Self::ENTITIES_TAG,
                    Self::NMTOKEN_TAG,
                    Self::NMTOKENS_TAG,
                ],
                *input,
            ))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::Id(_) => write!(output, "{}", Self::ID_TAG),
            Self::IdRef(_) => write!(output, "{}", Self::IDREF_TAG),
            Self::IdRefs(_) => write!(output, "{}", Self::IDREFS_TAG),
            Self::Entity(_) => write!(output, "{}", Self::ENTITY_TAG),
            Self::Entities(_) => write!(output, "{}", Self::ENTITIES_TAG),
            Self::NmToken(_) => write!(output, "{}", Self::NMTOKEN_TAG),
            Self::NmTokens(_) => write!(output, "{}", Self::NMTOKENS_TAG),
        }
    }
}

/// [57] - Enumerated Type
///
/// https://www.w3.org/TR/xml/#NT-EnumeratedType
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnumeratedType<'src> {
    NotationType(NotationType<'src>),
    Enumeration(Enumeration<'src>),
}

impl<'src> XmlElement<'src> for EnumeratedType<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::NotationType(notation) => notation.position(),
            Self::Enumeration(enumeration) => enumeration.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(NotationType::OPENING_TAG) {
            Ok(Self::NotationType(NotationType::parse(input)?))
        } else {
            Ok(Self::Enumeration(Enumeration::parse(input)?))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::NotationType(notation) => notation.write(output),
            Self::Enumeration(enumeration) => enumeration.write(output),
        }
    }
}

/// [58] - Notation Type
///
/// https://www.w3.org/TR/xml/#NT-NotationType
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NotationType<'src> {
    pub first: Name<'src>,
    pub others: Vec<Name<'src>>,
}

impl<'src> NotationType<'src> {
    const OPENING_TAG: &'static str = "NOTATION";
}

impl<'src> XmlElement<'src> for NotationType<'src> {
    fn position(&self) -> span::Position {
        self.first.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        expect_whitespaces(input)?;
        expect_bytes(input, "(")?;
        skip_whitespaces(input);

        let first = Name::parse(input)?;
        let mut others = Vec::new();

        loop {
            skip_whitespaces(input);
            match input.first_char() {
                /* vertical bar, new name in notation */
                Some('|') => {
                    input.bump();
                    skip_whitespaces(input);
                    others.push(Name::parse(input)?);
                }
                /* Closed parens, we are done */
                Some(')') => {
                    input.bump();
                    break;
                }
                _ => return Err(XmlParsingError::unexpected(&["|", ")"], *input)),
            }
        }

        Ok(Self { first, others })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} ({}", Self::OPENING_TAG, self.first)?;
        for other in self.others.iter() {
            write!(output, " {other}")?;
        }
        write!(output, ")")
    }
}

/// [59] - Enumeration
///
/// https://www.w3.org/TR/xml/#NT-Enumeration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enumeration<'src> {
    pub first: NmToken<'src>,
    pub others: Vec<NmToken<'src>>,
}

impl<'src> XmlElement<'src> for Enumeration<'src> {
    fn position(&self) -> span::Position {
        self.first.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, "(")?;
        skip_whitespaces(input);

        let first = NmToken::parse(input)?;
        let mut others = Vec::new();

        loop {
            skip_whitespaces(input);
            match input.first_char() {
                /* vertical bar, new nm token in enumeration */
                Some('|') => {
                    input.bump();
                    skip_whitespaces(input);
                    others.push(NmToken::parse(input)?);
                }
                /* Closed parens, we are done */
                Some(')') => {
                    input.bump();
                    break;
                }
                _ => return Err(XmlParsingError::unexpected(&["|", ")"], *input)),
            }
        }

        Ok(Self { first, others })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "({}", self.first)?;
        for other in self.others.iter() {
            write!(output, " {other}")?;
        }
        write!(output, ")")
    }
}

/// [60] - Default Declaration
///
/// https://www.w3.org/TR/xml/#NT-DefaultDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefaultDecl<'src> {
    Required(DefaultDeclRequired),
    Implied(DefaultDeclImplied),
    Value(DefaultDeclValue<'src>),
}

impl<'src> DefaultDecl<'src> {
    const REQUIRED_TAG: &'static str = "#REQUIRED";
    const IMPLIED_TAG: &'static str = "#IMPLIED";
    const FIXED_TAG: &'static str = "#FIXED";
}

impl<'src> XmlElement<'src> for DefaultDecl<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::Required(DefaultDeclRequired { position }) => *position,
            Self::Implied(DefaultDeclImplied { position }) => *position,
            Self::Value(DefaultDeclValue { position, .. }) => *position,
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        let position = input.pos();
        if let Some(stripped) = input.strip_prefix(Self::REQUIRED_TAG) {
            *input = stripped;
            Ok(Self::Required(DefaultDeclRequired { position }))
        } else if let Some(stripped) = input.strip_prefix(Self::IMPLIED_TAG) {
            *input = stripped;
            Ok(Self::Implied(DefaultDeclImplied { position }))
        } else {
            let fixed = if let Some(stripped) = input.strip_prefix(Self::FIXED_TAG) {
                *input = stripped;
                expect_whitespaces(input)?;
                true
            } else {
                false
            };
            let attribute_value = AttValue::parse(input)?;
            Ok(Self::Value(DefaultDeclValue {
                position,
                fixed,
                attribute_value,
            }))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::Required(_) => output.write_all(Self::REQUIRED_TAG.as_bytes()),
            Self::Implied(_) => output.write_all(Self::IMPLIED_TAG.as_bytes()),
            Self::Value(DefaultDeclValue {
                fixed, attribute_value, ..
            }) => {
                if *fixed {
                    write!(output, "{} ", Self::FIXED_TAG)?;
                }
                attribute_value.write(output)
            }
        }
    }
}

/// [60a] - Default Declaration Required
///
/// Required Variant of the Default Declaration
///
/// https://www.w3.org/TR/xml/#NT-DefaultDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefaultDeclRequired {
    position: span::Position,
}

/// [60b] - Default Declaration Implied
///
/// Implied Variant of the Default Declaration
///
/// https://www.w3.org/TR/xml/#NT-DefaultDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefaultDeclImplied {
    position: span::Position,
}

/// [60c] - Default Declaration Value
///
/// Value Variant of the Default Declaration
///
/// https://www.w3.org/TR/xml/#NT-DefaultDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefaultDeclValue<'src> {
    position: span::Position,
    fixed: bool,
    attribute_value: AttValue<'src>,
}

/// [66] - Character Reference
///
/// https://www.w3.org/TR/xml/#NT-CharRef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharRef {
    position: span::Position,
    char_point: u64,
}

impl<'src> CharRef {
    const OPENING_TAG: &'static str = "&#";
    const CLOSING_TAG: &'static str = ";";
}

impl<'src> XmlElement<'src> for CharRef {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let (char_point, position) = match input.first_char() {
            Some('x') => {
                input.bump();
                let nums = expect_string::<HexadecimalDigits, HexadecimalDigits>(input)?;
                (
                    u64::from_str_radix(nums.str(), 16).map_err(|_| XmlParsingError::unexpected(&["hex digits"], *input))?,
                    nums.pos(),
                )
            }
            _ => {
                let nums = expect_string::<DecimalDigits, DecimalDigits>(input)?;
                (
                    u64::from_str_radix(nums.str(), 10).map_err(|_| XmlParsingError::unexpected(&["digits"], *input))?,
                    nums.pos(),
                )
            }
        };
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(CharRef { position, char_point })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "&#x{:x};", self.char_point)
    }
}

impl<'src> std::fmt::Display for CharRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&#x{:x};", self.char_point)
    }
}

/// [67] - Reference
///
/// https://www.w3.org/TR/xml/#NT-Reference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Reference<'src> {
    EntityRef(EntityRef<'src>),
    CharRef(CharRef),
}

impl<'src> Reference<'src> {
    const OPENING_TAG: &'static str = "&";
}

impl<'src> XmlElement<'src> for Reference<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::EntityRef(entity) => entity.position(),
            Self::CharRef(character) => character.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(CharRef::OPENING_TAG) {
            Ok(Self::CharRef(CharRef::parse(input)?))
        } else if input.str().starts_with(EntityRef::OPENING_TAG) {
            Ok(Self::EntityRef(EntityRef::parse(input)?))
        } else {
            Err(XmlParsingError::unexpected(
                &[CharRef::OPENING_TAG, EntityRef::OPENING_TAG],
                *input,
            ))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::EntityRef(entity) => entity.write(output),
            Self::CharRef(character) => character.write(output),
        }
    }
}

impl<'src> std::fmt::Display for Reference<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CharRef(char_ref) => char_ref.fmt(f),
            Self::EntityRef(entity) => entity.fmt(f),
        }
    }
}

/// [68] - Entity Reference
///
/// https://www.w3.org/TR/xml/#NT-EntityRef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityRef<'src> {
    pub name: Name<'src>,
}

impl<'src> EntityRef<'src> {
    const OPENING_TAG: &'static str = "&";
    const CLOSING_TAG: &'static str = ";";
}

impl<'src> XmlElement<'src> for EntityRef<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let name = Name::parse(input)?;
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(Self { name })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.name, Self::CLOSING_TAG)
    }
}

impl<'src> std::fmt::Display for EntityRef<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(Self::OPENING_TAG)?;
        f.write_str(self.name.str())?;
        f.write_str(Self::CLOSING_TAG)?;
        Ok(())
    }
}

/// [69] - Parameter Entity Reference
///
/// https://www.w3.org/TR/xml/#NT-PEReference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PEReference<'src> {
    pub name: Name<'src>,
}

impl<'src> PEReference<'src> {
    const OPENING_TAG: &'static str = "%";
    const CLOSING_TAG: &'static str = ";";
}

impl<'src> XmlElement<'src> for PEReference<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        let name = Name::parse(input)?;
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(Self { name })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.name, Self::CLOSING_TAG)
    }
}

impl<'src> std::fmt::Display for PEReference<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(Self::OPENING_TAG)?;
        f.write_str(self.name.str())?;
        f.write_str(Self::CLOSING_TAG)?;
        Ok(())
    }
}

/// [70] - Entity Declaration
///
/// https://www.w3.org/TR/xml/#NT-EntityDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityDecl<'src> {
    GEDecl(GEDecl<'src>),
    PEDecl(PEDecl<'src>),
}

impl<'src> EntityDecl<'src> {
    const OPENING_TAG: &'static str = "<!ENTITY";
    #[allow(unused)]
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for EntityDecl<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::GEDecl(decl) => decl.position(),
            Self::PEDecl(decl) => decl.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        /* Look for the % char to dissociate the type */
        let mut temp = *input;
        expect_bytes(&mut temp, Self::OPENING_TAG)?;
        expect_whitespaces(&mut temp)?;
        if temp.str().starts_with("%") {
            Ok(Self::PEDecl(PEDecl::parse(input)?))
        } else {
            Ok(Self::GEDecl(GEDecl::parse(input)?))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::GEDecl(decl) => decl.write(output),
            Self::PEDecl(decl) => decl.write(output),
        }
    }
}

/// [71] - G Entity Declaraion
///
/// https://www.w3.org/TR/xml/#NT-GEDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GEDecl<'src> {
    pub name: Name<'src>,
    pub entity_def: EntityDef<'src>,
}

impl<'src> GEDecl<'src> {
    const OPENING_TAG: &'static str = "<!ENTITY";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for GEDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;
        expect_whitespaces(input)?;
        let entity_def = EntityDef::parse(input)?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(Self { name, entity_def })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} {} ", Self::OPENING_TAG, self.name)?;
        self.entity_def.write(output)?;
        write!(output, " {}", Self::CLOSING_TAG)?;
        Ok(())
    }
}

/// [72] - P Entity Declaration
///
/// https://www.w3.org/TR/xml/#NT-PEDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PEDecl<'src> {
    pub name: Name<'src>,
    pub pe_def: PEDef<'src>,
}

impl<'src> PEDecl<'src> {
    const OPENING_TAG: &'static str = "<!ENTITY";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for PEDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        expect_whitespaces(input)?;
        expect_bytes(input, "%")?;
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;
        expect_whitespaces(input)?;
        let pe_def = PEDef::parse(input)?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG)?;
        Ok(Self { name, pe_def })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} % {} ", Self::OPENING_TAG, self.name)?;
        self.pe_def.write(output)?;
        write!(output, " {}", Self::CLOSING_TAG)?;
        Ok(())
    }
}

/// [73] - Entity Definition
///
/// https://www.w3.org/TR/xml/#NT-EntityDef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityDef<'src> {
    EntityValue(EntityValue<'src>),
    External(EntityDefExternal<'src>),
}

impl<'src> XmlElement<'src> for EntityDef<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::EntityValue(value) => value.position,
            Self::External(external) => external.position,
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(ExternalID::SYSTEM_TAG) || input.str().starts_with(ExternalID::PUBLIC_TAG) {
            let position = input.pos();
            let id = ExternalID::parse(input)?;
            let mut temp = *input;
            let skipped = skip_whitespaces(&mut temp);
            let decl = if skipped.pos() != temp.pos() && temp.str().starts_with(NDataDecl::TAG) {
                Some(NDataDecl::parse(input)?)
            } else {
                None
            };
            Ok(Self::External(EntityDefExternal { position, id, decl }))
        } else {
            Ok(Self::EntityValue(EntityValue::parse(input)?))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::EntityValue(value) => value.write(output),
            Self::External(EntityDefExternal { id, decl, .. }) => {
                id.write(output)?;
                if let Some(decl) = &decl {
                    decl.write(output)?;
                }
                Ok(())
            }
        }
    }
}

/// [73a] - Entity Definition External
///
/// External Variant of the Entity Definition
///
/// https://www.w3.org/TR/xml/#NT-EntityDef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityDefExternal<'src> {
    position: span::Position,
    id: ExternalID<'src>,
    decl: Option<NDataDecl<'src>>,
}

/// [74] - P Entity Definition
///
/// https://www.w3.org/TR/xml/#NT-PEDef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PEDef<'src> {
    EntityValue(EntityValue<'src>),
    ExternalID(ExternalID<'src>),
}

impl<'src> XmlElement<'src> for PEDef<'src> {
    fn position(&self) -> span::Position {
        match self {
            Self::EntityValue(value) => value.position(),
            Self::ExternalID(id) => id.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if input.str().starts_with(ExternalID::SYSTEM_TAG) || input.str().starts_with(ExternalID::PUBLIC_TAG) {
            Ok(Self::ExternalID(ExternalID::parse(input)?))
        } else {
            Ok(Self::EntityValue(EntityValue::parse(input)?))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::EntityValue(value) => value.write(output),
            Self::ExternalID(id) => id.write(output),
        }
    }
}

/// [75] - External Id
///
/// https://www.w3.org/TR/xml/#NT-ExternalID
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternalID<'src> {
    System(ExternalIDSystem<'src>),
    Public(ExternalIDPublic<'src>),
}

impl<'src> ExternalID<'src> {
    const SYSTEM_TAG: &'static str = "SYSTEM";
    const PUBLIC_TAG: &'static str = "PUBLIC";
}
impl<'src> XmlElement<'src> for ExternalID<'src> {
    fn position(&self) -> span::Position {
        match self {
            ExternalID::System(ExternalIDSystem { system }) => system.position(),
            ExternalID::Public(ExternalIDPublic { pubid, .. }) => pubid.position(),
        }
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        if let Some(stripped) = input.strip_prefix(Self::SYSTEM_TAG) {
            *input = stripped;
            skip_whitespaces(input);
            let system = SystemLiteral::parse(input)?;
            Ok(Self::System(ExternalIDSystem { system }))
        } else if let Some(stripped) = input.strip_prefix(Self::PUBLIC_TAG) {
            *input = stripped;
            skip_whitespaces(input);
            let pubid = PubidLiteral::parse(input)?;
            skip_whitespaces(input);
            let system = SystemLiteral::parse(input)?;
            Ok(Self::Public(ExternalIDPublic { pubid, system }))
        } else {
            Err(XmlParsingError::unexpected(&[Self::SYSTEM_TAG, Self::PUBLIC_TAG], *input))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            ExternalID::System(ExternalIDSystem { system }) => {
                write!(output, "{} ", Self::SYSTEM_TAG)?;
                system.write(output)?;
            }
            ExternalID::Public(ExternalIDPublic { pubid, system }) => {
                write!(output, "{} ", Self::PUBLIC_TAG)?;
                pubid.write(output)?;
                write!(output, " ")?;
                system.write(output)?;
            }
        }
        Ok(())
    }
}

/// [75a] - External Id System
///
/// System variant of the External Id
///
/// https://www.w3.org/TR/xml/#NT-ExternalID
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternalIDSystem<'src> {
    system: SystemLiteral<'src>,
}

/// [75b] - External Id Public
///
/// Public variant of the External Id
///
/// https://www.w3.org/TR/xml/#NT-ExternalID
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternalIDPublic<'src> {
    pubid: PubidLiteral<'src>,
    system: SystemLiteral<'src>,
}

/// [76] - Notation Data Declaration
///
/// https://www.w3.org/TR/xml/#NT-NDataDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NDataDecl<'src> {
    pub name: Name<'src>,
}

impl<'src> NDataDecl<'src> {
    const TAG: &'static str = "NDATA";
}

impl<'src> XmlElement<'src> for NDataDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input)?;
        expect_bytes(input, Self::TAG)?;
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;
        Ok(Self { name })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, " {} {}", Self::TAG, self.name)
    }
}

/// [80] - Encoding Declaration
///
/// https://www.w3.org/TR/xml/#NT-EncodingDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EncodingDecl<'src> {
    pub position: span::Position,
    pub encoding: span::Span<'src>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for EncodingDecl<'src> {
    fn position(&self) -> span::Position {
        self.position
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input)?;

        let position = input.pos();
        expect_bytes(input, "encoding")?;
        skip_whitespaces(input);
        expect_bytes(input, "=")?;
        skip_whitespaces(input);
        let quote = QuoteKind::parse(input)?;
        let encoding = expect_string::<LatinAlphabet, ExtendedLatinAlphabet>(input)?;
        expect_bytes(input, quote.to_str())?;
        Ok(Self {
            position,
            encoding,
            quote,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, " encoding={}{}{}", self.quote, self.encoding, self.quote)
    }
}

/// [82] - Notation Declaration
///
/// https://www.w3.org/TR/xml/#NT-NotationDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NotationDecl<'src> {
    name: Name<'src>,
    id: NotationDeclID<'src>,
}

impl<'src> NotationDecl<'src> {
    const OPENING_TAG: &'static str = "<!NOTATION";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for NotationDecl<'src> {
    fn position(&self) -> span::Position {
        self.name.pos()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG)?;
        expect_whitespaces(input)?;
        let name = Name::parse(input)?;
        expect_whitespaces(input)?;
        if input.str().starts_with(ExternalID::SYSTEM_TAG) || input.str().starts_with(ExternalID::PUBLIC_TAG) {
            let external_id = ExternalID::parse(input)?;
            skip_whitespaces(input);
            expect_bytes(input, Self::CLOSING_TAG)?;
            Ok(Self {
                name,
                id: NotationDeclID::ExternalID(external_id),
            })
        } else {
            let public_id = PublicID::parse(input)?;
            skip_whitespaces(input);
            expect_bytes(input, Self::CLOSING_TAG)?;
            Ok(Self {
                name,
                id: NotationDeclID::PublicID(public_id),
            })
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} {} ", Self::OPENING_TAG, self.name)?;
        match &self.id {
            NotationDeclID::ExternalID(external_id) => {
                external_id.write(output)?;
            }
            NotationDeclID::PublicID(public_id) => {
                public_id.write(output)?;
            }
        }
        write!(output, " {}", Self::CLOSING_TAG)?;
        Ok(())
    }
}

/// [82a] - Notation Declaration ID
///
/// Either a PublicID, or an ExternalID.
///
/// https://www.w3.org/TR/xml/#NT-NotationDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NotationDeclID<'src> {
    ExternalID(ExternalID<'src>),
    PublicID(PublicID<'src>),
}

/// [83] - Public ID
///
/// https://www.w3.org/TR/xml/#NT-PublicID
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PublicID<'src> {
    pub literal: PubidLiteral<'src>,
}

impl<'src> PublicID<'src> {
    const TAG: &'static str = "PUBLIC";
}

impl<'src> XmlElement<'src> for PublicID<'src> {
    fn position(&self) -> span::Position {
        self.literal.position()
    }
    fn parse(input: &mut span::Span<'src>) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::TAG)?;
        expect_whitespaces(input)?;
        let literal = PubidLiteral::parse(input)?;
        Ok(Self { literal })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} ", Self::TAG)?;
        self.literal.write(output)?;
        Ok(())
    }
}
