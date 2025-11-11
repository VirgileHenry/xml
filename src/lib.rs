mod error;
mod parsing;

use error::XmlParsingError;
use parsing::*;
use std::fmt::{Pointer, Write};

pub fn parse_xml<'src>(input: &'src str) -> Result<Document<'src>, XmlParsingError<'src>> {
    let mut input = input;
    Document::parse(&mut input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QuoteKind {
    Single,
    Double,
}

impl QuoteKind {
    fn parse<'src>(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        match input.chars().next() {
            Some('"') => {
                *input = &input['"'.len_utf8()..];
                Ok(QuoteKind::Double)
            }
            Some('\'') => {
                *input = &input['\''.len_utf8()..];
                Ok(QuoteKind::Single)
            }
            Some(_) => {
                let expected = &["\"", "'"];
                Err(XmlParsingError::unexpected(expected, input))
            }
            None => {
                let expected = &["\"", "'"];
                Err(XmlParsingError::unexpected(expected, "EOF"))
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
    fn try_parse(input: &mut &str) -> Option<Self> {
        match input.as_bytes().first() {
            Some(0x3F) => {
                *input = &input[1..];
                Some(RepetitionOperator::ZeroOrOne)
            }
            Some(0x2A) => {
                *input = &input[1..];
                Some(RepetitionOperator::ZeroOrMore)
            }
            Some(0x2B) => {
                *input = &input[1..];
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
        f.write_char(self.char())
    }
}

trait XmlElement<'src>: Sized {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>>;
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let prolog = Prolog::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        let element = Element::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut misc = Vec::new();
        loop {
            skip_whitespaces(input);
            if input.starts_with(Comment::OPENING_TAG) || input.starts_with(PI::OPENING_TAG) {
                misc.push(Miscellaneous::parse(input).map_err(|e| e.add_ctx::<Self>())?);
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
pub struct Name<'src>(&'src str);

impl<'src> XmlElement<'src> for Name<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        Ok(Self(expect_string::<NameStartChar, NameChar>(input)?))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.0.as_bytes())
    }
}

impl<'src> std::ops::Deref for Name<'src> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'src> std::fmt::Display for Name<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

/// [8] - Nm Token
///
/// https://www.w3.org/TR/xml/#NT-Nmtoken
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NmToken<'src>(&'src str);

impl<'src> XmlElement<'src> for NmToken<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        Ok(Self(expect_string::<NameChar, NameChar>(input)?))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.0.as_bytes())
    }
}

impl<'src> std::ops::Deref for NmToken<'src> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'src> std::fmt::Display for NmToken<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

/// [9] - Entity Value
///
/// https://www.w3.org/TR/xml/#NT-EntityValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityValue<'src> {
    pub literal: Vec<EntityValueElem<'src>>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for EntityValue<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut literal = Vec::new();

        match quote {
            QuoteKind::Single => loop {
                match input.chars().next() {
                    Some('&') => literal.push(EntityValueElem::Reference(
                        Reference::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    Some('%') => literal.push(EntityValueElem::PEReference(
                        PEReference::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    Some('\'') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<%&]"], "<")),
                    Some(_) => literal.push(EntityValueElem::CharSlice(expect_string::<
                        SingleQuotedEntityValueCharacters,
                        SingleQuotedEntityValueCharacters,
                    >(input)?)),
                    None => return Err(XmlParsingError::unclosed::<Self>("'")),
                }
            },
            QuoteKind::Double => loop {
                match input.chars().next() {
                    Some('&') => literal.push(EntityValueElem::Reference(
                        Reference::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    Some('%') => literal.push(EntityValueElem::PEReference(
                        PEReference::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    Some('"') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<%&]"], "<")),
                    Some(_) => literal.push(EntityValueElem::CharSlice(expect_string::<
                        DoubleQuotedEntityValueCharacters,
                        DoubleQuotedEntityValueCharacters,
                    >(input)?)),
                    None => return Err(XmlParsingError::unclosed::<Self>("\"")),
                }
            },
        }

        Ok(Self { literal, quote })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.quote.to_str().as_bytes())?;
        for elem in self.literal.iter() {
            match elem {
                EntityValueElem::CharSlice(slice) => output.write_all(slice.as_bytes())?,
                EntityValueElem::Reference(reference) => reference.write(output)?,
                EntityValueElem::PEReference(reference) => reference.write(output)?,
            }
        }
        output.write_all(self.quote.to_str().as_bytes())?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EntityValueElem<'src> {
    CharSlice(&'src str),
    Reference(Reference<'src>),
    PEReference(PEReference<'src>),
}

/// [10] - Attribute Value
///
/// https://www.w3.org/TR/xml/#NT-AttValue
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttValue<'src> {
    pub literal: Vec<AttValueElem<'src>>,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for AttValue<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut literal = Vec::new();

        match quote {
            QuoteKind::Single => loop {
                match input.chars().next() {
                    Some('&') => literal.push(AttValueElem::Reference(
                        Reference::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    Some('\'') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<&]"], "<")),
                    Some(_) => literal.push(AttValueElem::CharSlice(
                        expect_string::<SingleQuotedAttValueCharacters, SingleQuotedAttValueCharacters>(input)
                            .map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    None => return Err(XmlParsingError::unclosed::<Self>("'")),
                }
            },
            QuoteKind::Double => loop {
                match input.chars().next() {
                    Some('&') => literal.push(AttValueElem::Reference(
                        Reference::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    Some('"') => break,
                    Some('<') => return Err(XmlParsingError::unexpected(&["[^<&]"], "<")),
                    Some(_) => literal.push(AttValueElem::CharSlice(
                        expect_string::<DoubleQuotedAttValueCharacters, DoubleQuotedAttValueCharacters>(input)
                            .map_err(|e| e.add_ctx::<Self>())?,
                    )),
                    None => return Err(XmlParsingError::unclosed::<Self>("\"")),
                }
            },
        }
        expect_bytes(input, quote.to_str())?;

        Ok(Self { literal, quote })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.quote.to_str().as_bytes())?;
        for elem in self.literal.iter() {
            match elem {
                AttValueElem::CharSlice(slice) => output.write_all(slice.as_bytes())?,
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
                AttValueElem::CharSlice(slice) => f.write_str(slice)?,
                AttValueElem::Reference(reference) => reference.fmt(f)?,
            }
        }
        f.write_str(self.quote.to_str())?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttValueElem<'src> {
    CharSlice(&'src str),
    Reference(Reference<'src>),
}

/// [11] - System Literal
///
/// https://www.w3.org/TR/xml/#NT-SystemLiteral
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SystemLiteral<'src> {
    pub literal: &'src str,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for SystemLiteral<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        let next_quote_pos = input
            .find(quote.to_char())
            .ok_or_else(|| XmlParsingError::unclosed::<Self>(quote.to_str()))
            .map_err(|e| e.add_ctx::<Self>())?;
        let (literal, rest) = input.split_at(next_quote_pos);
        // fixme: check literal is legal characters only
        *input = &rest[1..];
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
    pub literal: &'src str,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for PubidLiteral<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        let literal = expect_string::<PubidChar, PubidChar>(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, quote.to_str()).map_err(|e| e.add_ctx::<Self>())?;
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
pub struct CharData<'src>(&'src str);

impl<'src> XmlElement<'src> for CharData<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        Ok(Self(expect_string::<CharDataCharSet, CharDataCharSet>(input)?))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        output.write_all(self.0.as_bytes())
    }
}

impl<'src> std::ops::Deref for CharData<'src> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// [15] - Comment
///
/// https://www.w3.org/TR/xml/#NT-Comment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment<'src> {
    pub comment: &'src str,
}

impl<'src> Comment<'src> {
    const OPENING_TAG: &'static str = "<!--";
    const CLOSING_TAG: &'static str = "-->";
}

impl<'src> XmlElement<'src> for Comment<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        skip_whitespaces(input);
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        // Fixme: "--" shall not appear in comments
        let comment_end = input
            .find(Self::CLOSING_TAG)
            .ok_or_else(|| XmlParsingError::unclosed::<Self>(Self::CLOSING_TAG))
            .map_err(|e| e.add_ctx::<Self>())?;
        let (comment, rest) = input.split_at(comment_end);
        *input = &rest[Self::CLOSING_TAG.len()..];
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
pub struct PI<'src> {
    pub target: Name<'src>,
    pub instruction: Option<&'src str>,
}

impl<'src> PI<'src> {
    const OPENING_TAG: &'static str = "<?";
    const CLOSING_TAG: &'static str = "?>";
}

impl<'src> XmlElement<'src> for PI<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let target = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        if is_litteral_xml(target.0) {
            return Err(XmlParsingError::invalid_target(target.0));
        }

        let instruction = if input.starts_with(Self::CLOSING_TAG) {
            None /* Got closing delimiter, no instruction */
        } else {
            skip_whitespaces(input);
            let instruction_end = input
                .find(Self::CLOSING_TAG)
                .ok_or_else(|| XmlParsingError::unclosed::<Self>(Self::CLOSING_TAG))
                .map_err(|e| e.add_ctx::<Self>())?;
            let (instruction, rest) = input.split_at(instruction_end);
            *input = &rest[Self::CLOSING_TAG.len()..];
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
    data: &'src str,
}

impl<'src> CDSect<'src> {
    const OPENING_TAG: &'static str = "<![CDATA[";
    const CLOSING_TAG: &'static str = "]]>";
}

impl<'src> XmlElement<'src> for CDSect<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        /* The usage of find is required, as the data can be anything as per spec */
        let end_pos = input
            .find(Self::CLOSING_TAG)
            .ok_or_else(|| XmlParsingError::unclosed::<Self>(Self::CLOSING_TAG))
            .map_err(|e| e.add_ctx::<Self>())?;
        let (data, rest) = input.split_at(end_pos);
        *input = &rest[Self::CLOSING_TAG.len()..];
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let declaration = if input.starts_with(XmlDeclaration::OPENING_TAG) {
            Some(XmlDeclaration::parse(input).map_err(|e| e.add_ctx::<Self>())?)
        } else {
            None
        };

        let mut misc = Vec::new();
        loop {
            skip_whitespaces(input);
            if input.starts_with(Comment::OPENING_TAG) || input.starts_with(PI::OPENING_TAG) {
                misc.push(Miscellaneous::parse(input).map_err(|e| e.add_ctx::<Self>())?);
            } else {
                break;
            }
        }

        let doc_type_decl = if input.starts_with(DoctypeDecl::OPENING_TAG) {
            Some(DoctypeDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?)
        } else {
            None
        };

        loop {
            skip_whitespaces(input);
            if input.starts_with(Comment::OPENING_TAG) || input.starts_with(PI::OPENING_TAG) {
                misc.push(Miscellaneous::parse(input).map_err(|e| e.add_ctx::<Self>())?);
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        skip_whitespaces(input);
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        /* Parse the version */
        let version = VersionInfo::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        /* Parse encoding if present */
        let mut temp = *input;
        skip_whitespaces(&mut temp);
        let encoding = if temp.starts_with("encoding") {
            Some(EncodingDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?)
        } else {
            None
        };

        /* Parse standalone if present */
        let mut temp = *input;
        skip_whitespaces(&mut temp);
        let standalone = if temp.starts_with("standalone") {
            Some(SDDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?)
        } else {
            None
        };

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;

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
    pub major: usize,
    pub minor: usize,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for VersionInfo {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, "version").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, "=").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);

        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let major = expect_string::<DecimalDigits, DecimalDigits>(input).map_err(|e| e.add_ctx::<Self>())?;
        let major = major
            .parse()
            .map_err(|_| XmlParsingError::unexpected(&["uint"], major))
            .map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, ".").map_err(|e| e.add_ctx::<Self>())?;
        let minor = expect_string::<DecimalDigits, DecimalDigits>(input).map_err(|e| e.add_ctx::<Self>())?;
        let minor = minor
            .parse()
            .map_err(|_| XmlParsingError::unexpected(&["uint"], minor))
            .map_err(|e| e.add_ctx::<Self>())?;

        expect_bytes(input, quote.to_str()).map_err(|e| e.add_ctx::<Self>())?;

        Ok(Self { major, minor, quote })
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
    Pi(PI<'src>),
}

impl<'src> XmlElement<'src> for Miscellaneous<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(Comment::OPENING_TAG) {
            Ok(Miscellaneous::Comment(
                Comment::parse(input).map_err(|e| e.add_ctx::<Self>())?,
            ))
        } else if input.starts_with(PI::OPENING_TAG) {
            Ok(Miscellaneous::Pi(PI::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            Err(XmlParsingError::unexpected(&["<!--", "<?"], input))
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        let external_id = if input.starts_with(ExternalID::SYSTEM_TAG) || input.starts_with(ExternalID::PUBLIC_TAG) {
            Some(ExternalID::parse(input).map_err(|e| e.add_ctx::<Self>())?)
        } else {
            None
        };
        skip_whitespaces(input);
        let int_subset = if input.starts_with("[") {
            expect_bytes(input, "[").map_err(|e| e.add_ctx::<Self>())?;
            let subset = IntSubset::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            expect_bytes(input, "]").map_err(|e| e.add_ctx::<Self>())?;
            skip_whitespaces(input);
            Some(subset)
        } else {
            None
        };
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
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
pub enum DeclSeparator<'src> {
    PEReference(PEReference<'src>),
    Space,
}

impl<'src> XmlElement<'src> for DeclSeparator<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(PEReference::OPENING_TAG) {
            Ok(Self::PEReference(PEReference::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Self::Space)
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::PEReference(pe_ref) => pe_ref.write(output),
            Self::Space => write!(output, " "),
        }
    }
}

/// [28b] - Int Subset
///
/// https://www.w3.org/TR/xml/#NT-intSubset
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntSubset<'src> {
    pub elements: Vec<IntSubsetElement<'src>>,
}

impl<'src> XmlElement<'src> for IntSubset<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let mut elements = Vec::new();

        loop {
            match input.chars().next() {
                /* Spaces or "%" make a decl separator */
                Some(' ') | Some('\t') | Some('\r') | Some('\n') | Some('%') => elements.push(IntSubsetElement::DeclSep(
                    DeclSeparator::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                )),
                /* "<" are the start of a markup declaration */
                Some('<') => elements.push(IntSubsetElement::MarkupDecl(
                    MarkupDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?,
                )),
                /* "]" is the expected character after the int subset */
                Some(']') => break,
                Some(_) => return Err(XmlParsingError::unexpected(&["space", "]"], input)),
                None => return Err(XmlParsingError::unexpected(&["space", "]"], "EOF")),
            }
        }

        Ok(Self { elements })
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntSubsetElement<'src> {
    MarkupDecl(MarkupDecl<'src>),
    DeclSep(DeclSeparator<'src>),
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
    PI(PI<'src>),
    Comment(Comment<'src>),
}

impl<'src> XmlElement<'src> for MarkupDecl<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(ElementDecl::OPENING_TAG) {
            Ok(Self::ElementDecl(ElementDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else if input.starts_with(AttListDecl::OPENING_TAG) {
            Ok(Self::AttListDecl(AttListDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else if input.starts_with(EntityDecl::OPENING_TAG) {
            Ok(Self::EntityDecl(EntityDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else if input.starts_with(NotationDecl::OPENING_TAG) {
            Ok(Self::NotationDecl(
                NotationDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?,
            ))
        } else if input.starts_with(PI::OPENING_TAG) {
            Ok(Self::PI(PI::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else if input.starts_with(Comment::OPENING_TAG) {
            Ok(Self::Comment(Comment::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            let expected = &[
                ElementDecl::OPENING_TAG,
                AttListDecl::OPENING_TAG,
                EntityDecl::OPENING_TAG,
                NotationDecl::OPENING_TAG,
                PI::OPENING_TAG,
                Comment::OPENING_TAG,
            ];
            Err(XmlParsingError::unexpected(expected, input))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::ElementDecl(element) => element.write(output),
            Self::AttListDecl(att_list_decl) => att_list_decl.write(output),
            Self::EntityDecl(decl) => decl.write(output),
            Self::NotationDecl(decl) => decl.write(output),
            Self::PI(pi) => pi.write(output),
            Self::Comment(comment) => comment.write(output),
        }
    }
}

/// [32] - Standalone Declaration
///
/// https://www.w3.org/TR/xml/#NT-SDDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SDDecl {
    pub standalone: bool,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for SDDecl {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, "standalone").map_err(|e| e.add_ctx::<Self>())?;

        skip_whitespaces(input);
        expect_bytes(input, "=").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);

        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let standalone = if let Some(stripped) = input.strip_prefix("yes") {
            *input = stripped;
            true
        } else if let Some(stripped) = input.strip_prefix("no") {
            *input = stripped;
            false
        } else {
            return Err(XmlParsingError::unexpected(&["yes", "no"], input));
        };
        expect_bytes(input, quote.to_str()).map_err(|e| e.add_ctx::<Self>())?;
        Ok(Self { standalone, quote })
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
    Element {
        s_tag: STag<'src>,
        content: Content<'src>,
        e_tag: ETag<'src>,
    },
}

impl<'src> Element<'src> {
    const OPENING_TAG: &'static str = "<";

    fn parse_start_or_empty(input: &mut &'src str) -> Result<Result<STag<'src>, EmptyElemTag<'src>>, XmlParsingError<'src>> {
        /* Since start and empty tags are A LOT alike, parse anyway and make the decision later */
        expect_bytes(input, STag::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut attributes = Vec::new();
        loop {
            /* Check for terminator */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.starts_with(STag::CLOSING_TAG) | temp.starts_with(EmptyElemTag::CLOSING_TAG) {
                break;
            }
            /* No terminator, parse next attribute */
            expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
            attributes.push(Attribute::parse(input).map_err(|e| e.add_ctx::<Self>())?);
        }

        skip_whitespaces(input);
        if let Some(stripped) = input.strip_prefix(STag::CLOSING_TAG) {
            *input = stripped;
            Ok(Ok(STag { name, attributes }))
        } else {
            expect_bytes(input, EmptyElemTag::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Err(EmptyElemTag { name, attributes }))
        }
    }
}

impl<'src> XmlElement<'src> for Element<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        match Self::parse_start_or_empty(input)? {
            Ok(s_tag) => {
                let content = Content::parse(input).map_err(|e| e.add_ctx::<Self>())?;
                let e_tag = ETag::parse(input).map_err(|e| e.add_ctx::<Self>())?;
                Ok(Self::Element { s_tag, content, e_tag })
            }
            Err(empty_elem_tag) => Ok(Self::EmptyElemTag(empty_elem_tag)),
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::EmptyElemTag(empty) => empty.write(output),
            Self::Element { s_tag, content, e_tag } => {
                s_tag.write(output)?;
                content.write(output)?;
                e_tag.write(output)?;
                Ok(())
            }
        }
    }
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut attributes = Vec::new();
        loop {
            /* Check for terminator */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.starts_with(Self::CLOSING_TAG) {
                break;
            }
            /* No terminator, parse next attribute */
            expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
            attributes.push(Attribute::parse(input).map_err(|e| e.add_ctx::<Self>())?);
        }

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;

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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, "=").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        let value = AttValue::parse(input).map_err(|e| e.add_ctx::<Self>())?;
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;

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
    pub first_chars: Option<CharData<'src>>,
    pub content: Vec<(ContentElement<'src>, Option<CharData<'src>>)>,
}

impl<'src> XmlElement<'src> for Content<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        let first = if input.starts_with(ContentElement::OPENING_CHARS) {
            None
        } else {
            Some(CharData::parse(input).map_err(|e| e.add_ctx::<Self>())?)
        };

        let mut content = Vec::new();
        loop {
            /* The ETag start indicates we are done here */
            if input.starts_with(ETag::OPENING_TAG) {
                break;
            }
            /* Otherwise, we have remaining content to parse! */
            let element = if input.starts_with(CDSect::OPENING_TAG) {
                ContentElement::CDSect(CDSect::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            } else if input.starts_with(PI::OPENING_TAG) {
                ContentElement::PI(PI::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            } else if input.starts_with(Comment::OPENING_TAG) {
                ContentElement::Comment(Comment::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            } else if input.starts_with(Element::OPENING_TAG) {
                ContentElement::Element(Element::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            } else if input.starts_with(Reference::OPENING_TAG) {
                ContentElement::Reference(Reference::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            } else {
                return Err(XmlParsingError::unexpected(
                    &[
                        CDSect::OPENING_TAG,
                        PI::OPENING_TAG,
                        Comment::OPENING_TAG,
                        Element::OPENING_TAG,
                        Reference::OPENING_TAG,
                    ],
                    input,
                ));
            };

            let chars = if input.starts_with(ContentElement::OPENING_CHARS) {
                None
            } else {
                Some(CharData::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            };

            content.push((element, chars));
        }

        Ok(Self {
            first_chars: first,
            content,
        })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        if let Some(chars) = &self.first_chars {
            output.write_all(chars.as_bytes())?;
        }
        for (element, chars) in self.content.iter() {
            match element {
                ContentElement::Element(elem) => elem.write(output)?,
                ContentElement::Reference(reference) => reference.write(output)?,
                ContentElement::CDSect(cd_sect) => cd_sect.write(output)?,
                ContentElement::PI(pi) => pi.write(output)?,
                ContentElement::Comment(comment) => comment.write(output)?,
            }
            if let Some(chars) = chars {
                output.write_all(chars.as_bytes())?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContentElement<'src> {
    Element(Element<'src>),
    Reference(Reference<'src>),
    CDSect(CDSect<'src>),
    PI(PI<'src>),
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut attributes = Vec::new();
        loop {
            /* Check for terminator */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.starts_with(Self::CLOSING_TAG) {
                break;
            }
            /* No terminator, parse next attribute */
            expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
            attributes.push(Attribute::parse(input).map_err(|e| e.add_ctx::<Self>())?);
        }

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;

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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let content_spec = ContentSpec::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
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
    Empty,
    Any,
    Mixed(),
    Children(ElementContentChildren<'src>),
}

impl<'src> XmlElement<'src> for ContentSpec<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if let Some(prefixed) = input.strip_prefix("EMPTY") {
            *input = prefixed;
            Ok(Self::Empty)
        } else if let Some(prefixed) = input.strip_prefix("ANY") {
            *input = prefixed;
            Ok(Self::Any)
        } else {
            // Fixme
            unimplemented!()
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            ContentSpec::Empty => output.write_all("EMPTY".as_bytes()),
            ContentSpec::Any => output.write_all("ANY".as_bytes()),
            ContentSpec::Mixed() => unimplemented!(),
            ContentSpec::Children(children) => children.write(output),
        }
    }
}

/// [47] - Children
///
/// https://www.w3.org/TR/xml/#NT-children
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElementContentChildren<'src> {
    Choice {
        choice: ElementContentChoice<'src>,
        repetition: Option<RepetitionOperator>,
    },
    Seq {
        seq: ElementContentSeq<'src>,
        repetition: Option<RepetitionOperator>,
    },
}

impl<'src> XmlElement<'src> for ElementContentChildren<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, "(").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        /* Fixme: will be moved into a vec anyway ? maybe something to gain here */
        let mut cps = Vec::new();
        cps.push(ElementContentParticle::parse(input).map_err(|e| e.add_ctx::<Self>())?);
        skip_whitespaces(input);
        match input.chars().next() {
            /* Closing parens right after the first elem, it's a one element sequence */
            Some(')') => {
                *input = &input[')'.len_utf8()..];
                let repetition = RepetitionOperator::try_parse(input);
                Ok(Self::Seq {
                    seq: ElementContentSeq { sequence: cps },
                    repetition,
                })
            }
            /* A comma indicates a sequence of more than one element */
            Some(',') => loop {
                *input = &input[','.len_utf8()..];
                skip_whitespaces(input);
                cps.push(ElementContentParticle::parse(input).map_err(|e| e.add_ctx::<Self>())?);
                skip_whitespaces(input);
                /* If we have a closing parens, terminate the sequence */
                if input.as_bytes().first().cloned() == Some(0x29) {
                    *input = &input[1..];
                    let repetition = RepetitionOperator::try_parse(input);
                    break Ok(Self::Seq {
                        seq: ElementContentSeq { sequence: cps },
                        repetition,
                    });
                }
                /* Otherwise, keep munching at the sequence */
            },
            /* A vertical bar indicate a choice of multiple elements */
            Some('|') => loop {
                *input = &input['|'.len_utf8()..];
                skip_whitespaces(input);
                cps.push(
                    ElementContentParticle::parse(input)
                        .map_err(|e| e.add_ctx::<Self>())
                        .map_err(|e| e.add_ctx::<Self>())?,
                );
                skip_whitespaces(input);
                /* If we have a closing parens, terminate the sequence */
                if input.as_bytes().first().cloned() == Some(0x29) {
                    *input = &input[1..];
                    let repetition = RepetitionOperator::try_parse(input);
                    break Ok(Self::Choice {
                        choice: ElementContentChoice { choices: cps },
                        repetition,
                    });
                }
                /* Otherwise, keep munching at the sequence */
            },
            _ => Err(XmlParsingError::unexpected(&[")", ",", "|"], input)),
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            ElementContentChildren::Choice { choice, repetition } => {
                choice.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
            ElementContentChildren::Seq { seq, repetition } => {
                seq.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
        }
    }
}

/// [48] - Content Particle
///
/// https://www.w3.org/TR/xml/#NT-cp
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElementContentParticle<'src> {
    Name {
        name: Name<'src>,
        repetition: Option<RepetitionOperator>,
    },
    Choice {
        choice: ElementContentChoice<'src>,
        repetition: Option<RepetitionOperator>,
    },
    Seq {
        seq: ElementContentSeq<'src>,
        repetition: Option<RepetitionOperator>,
    },
}

impl<'src> XmlElement<'src> for ElementContentParticle<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with('(') {
            /* Since a content particle that is not a name is basically a children, use it and unpack */
            Ok(match ElementContentChildren::parse(input)? {
                ElementContentChildren::Seq { seq, repetition } => Self::Seq { seq, repetition },
                ElementContentChildren::Choice { choice, repetition } => Self::Choice { choice, repetition },
            })
        } else {
            let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            let repetition = RepetitionOperator::try_parse(input);
            Ok(Self::Name { name, repetition })
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            ElementContentParticle::Name { name, repetition } => match repetition {
                Some(repetition) => write!(output, "{name}{repetition}"),
                None => output.write_all(name.0.as_bytes()),
            },
            ElementContentParticle::Choice { choice, repetition } => {
                choice.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
            ElementContentParticle::Seq { seq, repetition } => {
                seq.write(output)?;
                if let Some(repetition) = repetition {
                    write!(output, "{repetition}")?;
                }
                Ok(())
            }
        }
    }
}

/// [49] - Choice
///
/// https://www.w3.org/TR/xml/#NT-choice
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ElementContentChoice<'src> {
    pub choices: Vec<ElementContentParticle<'src>>,
}

impl<'src> ElementContentChoice<'src> {
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
pub struct ElementContentSeq<'src> {
    pub sequence: Vec<ElementContentParticle<'src>>,
}

impl<'src> ElementContentSeq<'src> {
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;

        let mut definitions = Vec::new();

        loop {
            /* Check if we may have a terminator after spaces */
            let mut temp = *input;
            skip_whitespaces(&mut temp);
            if temp.starts_with(Self::CLOSING_TAG) {
                break;
            }
            /* Otherwise, keep parsing the attribute definition list */
            definitions.push(AttDef::parse(input).map_err(|e| e.add_ctx::<Self>())?);
        }

        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;

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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let attribute_type = AttributeType::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let default_decl = DefaultDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?;
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(StringType::TAG) {
            Ok(Self::StringType(StringType::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else if input.starts_with(TokenizedType::ID_TAG)
            || input.starts_with(TokenizedType::IDREF_TAG)
            || input.starts_with(TokenizedType::IDREFS_TAG)
            || input.starts_with(TokenizedType::ENTITY_TAG)
            || input.starts_with(TokenizedType::ENTITIES_TAG)
            || input.starts_with(TokenizedType::NMTOKEN_TAG)
            || input.starts_with(TokenizedType::NMTOKENS_TAG)
        {
            Ok(Self::TokenizedType(
                TokenizedType::parse(input).map_err(|e| e.add_ctx::<Self>())?,
            ))
        } else if input.starts_with(NotationType::OPENING_TAG) || input.starts_with("(") {
            Ok(Self::EnumeratedType(
                EnumeratedType::parse(input).map_err(|e| e.add_ctx::<Self>())?,
            ))
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
                input,
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
pub struct StringType;

impl StringType {
    const TAG: &'static str = "CDATA";
}

impl<'src> XmlElement<'src> for StringType {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::TAG).map_err(|e| e.add_ctx::<Self>())?;
        Ok(Self)
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
    Id,
    IdRef,
    IdRefs,
    Entity,
    Entities,
    NmToken,
    NmTokens,
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if let Some(stripped) = input.strip_prefix(Self::ID_TAG) {
            *input = stripped;
            Ok(Self::Id)
        } else if let Some(stripped) = input.strip_prefix(Self::IDREF_TAG) {
            *input = stripped;
            Ok(Self::IdRef)
        } else if let Some(stripped) = input.strip_prefix(Self::IDREFS_TAG) {
            *input = stripped;
            Ok(Self::IdRefs)
        } else if let Some(stripped) = input.strip_prefix(Self::ENTITY_TAG) {
            *input = stripped;
            Ok(Self::Entity)
        } else if let Some(stripped) = input.strip_prefix(Self::ENTITIES_TAG) {
            *input = stripped;
            Ok(Self::Entities)
        } else if let Some(stripped) = input.strip_prefix(Self::NMTOKEN_TAG) {
            *input = stripped;
            Ok(Self::NmToken)
        } else if let Some(stripped) = input.strip_prefix(Self::NMTOKENS_TAG) {
            *input = stripped;
            Ok(Self::NmTokens)
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
                input,
            ))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::Id => write!(output, "{}", Self::ID_TAG),
            Self::IdRef => write!(output, "{}", Self::IDREF_TAG),
            Self::IdRefs => write!(output, "{}", Self::IDREFS_TAG),
            Self::Entity => write!(output, "{}", Self::ENTITY_TAG),
            Self::Entities => write!(output, "{}", Self::ENTITIES_TAG),
            Self::NmToken => write!(output, "{}", Self::NMTOKEN_TAG),
            Self::NmTokens => write!(output, "{}", Self::NMTOKENS_TAG),
        }
    }
}

/// [57] - Enumerated Type
///
/// https://www.w3.org/TR/xml/#NT-EnumeratedType
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EnumeratedType<'src> {
    Notation(NotationType<'src>),
    Enumeration(Enumeration<'src>),
}

impl<'src> XmlElement<'src> for EnumeratedType<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(NotationType::OPENING_TAG) {
            Ok(Self::Notation(NotationType::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            Ok(Self::Enumeration(Enumeration::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::Notation(notation) => notation.write(output),
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, "(").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);

        let first = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        let mut others = Vec::new();

        loop {
            skip_whitespaces(input);
            match input.chars().next() {
                /* vertical bar, new name in notation */
                Some('|') => {
                    *input = &input['|'.len_utf8()..];
                    skip_whitespaces(input);
                    others.push(
                        Name::parse(input)
                            .map_err(|e| e.add_ctx::<Self>())
                            .map_err(|e| e.add_ctx::<Self>())?,
                    );
                }
                /* Closed parens, we are done */
                Some(')') => {
                    *input = &input[')'.len_utf8()..];
                    break;
                }
                _ => return Err(XmlParsingError::unexpected(&["|", ")"], input)),
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, "(").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);

        let first = NmToken::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        let mut others = Vec::new();

        loop {
            skip_whitespaces(input);
            match input.chars().next() {
                /* vertical bar, new nm token in enumeration */
                Some('|') => {
                    *input = &input['|'.len_utf8()..];
                    skip_whitespaces(input);
                    others.push(NmToken::parse(input).map_err(|e| e.add_ctx::<Self>())?);
                }
                /* Closed parens, we are done */
                Some(')') => {
                    *input = &input[')'.len_utf8()..];
                    break;
                }
                _ => return Err(XmlParsingError::unexpected(&["|", ")"], input)),
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
    Required,
    Implied,
    Value { fixed: bool, attribute_value: AttValue<'src> },
}

impl<'src> DefaultDecl<'src> {
    const REQUIRED_TAG: &'static str = "#REQUIRED";
    const IMPLIED_TAG: &'static str = "#IMPLIED";
    const FIXED_TAG: &'static str = "#FIXED";
}

impl<'src> XmlElement<'src> for DefaultDecl<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if let Some(stripped) = input.strip_prefix(Self::REQUIRED_TAG) {
            *input = stripped;
            Ok(Self::Required)
        } else if let Some(stripped) = input.strip_prefix(Self::IMPLIED_TAG) {
            *input = stripped;
            Ok(Self::Implied)
        } else {
            let fixed = if let Some(stripped) = input.strip_prefix(Self::FIXED_TAG) {
                *input = stripped;
                expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
                true
            } else {
                false
            };
            let attribute_value = AttValue::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Self::Value { fixed, attribute_value })
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::Required => output.write_all(Self::REQUIRED_TAG.as_bytes()),
            Self::Implied => output.write_all(Self::IMPLIED_TAG.as_bytes()),
            Self::Value { fixed, attribute_value } => {
                if *fixed {
                    write!(output, "{} ", Self::FIXED_TAG)?;
                }
                attribute_value.write(output)
            }
        }
    }
}

/// [66] - Character Reference
///
/// https://www.w3.org/TR/xml/#NT-CharRef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharRef(u64);

impl<'src> CharRef {
    const OPENING_TAG: &'static str = "&#";
    const CLOSING_TAG: &'static str = ";";
}

impl<'src> XmlElement<'src> for CharRef {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let character_point = match input.chars().next() {
            Some('x') => {
                *input = &input['x'.len_utf8()..];
                let nums = expect_string::<HexadecimalDigits, HexadecimalDigits>(input).map_err(|e| e.add_ctx::<Self>())?;
                u64::from_str_radix(nums, 16).map_err(|_| XmlParsingError::unexpected(&["hex digits"], input))?
            }
            _ => {
                let nums = expect_string::<DecimalDigits, DecimalDigits>(input).map_err(|e| e.add_ctx::<Self>())?;
                u64::from_str_radix(nums, 10).map_err(|_| XmlParsingError::unexpected(&["digits"], input))?
            }
        };
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        Ok(CharRef(character_point))
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "&#x{:x};", self.0)
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(CharRef::OPENING_TAG) {
            Ok(Self::CharRef(CharRef::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else if input.starts_with(EntityRef::OPENING_TAG) {
            Ok(Self::EntityRef(EntityRef::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            Err(XmlParsingError::unexpected(
                &[CharRef::OPENING_TAG, EntityRef::OPENING_TAG],
                input,
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        Ok(Self { name })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.name, Self::CLOSING_TAG)
    }
}

impl<'src> std::fmt::Display for EntityRef<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(Self::OPENING_TAG)?;
        f.write_str(&self.name)?;
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        Ok(Self { name })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{}{}{}", Self::OPENING_TAG, self.name, Self::CLOSING_TAG)
    }
}

impl<'src> std::fmt::Display for PEReference<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(Self::OPENING_TAG)?;
        f.write_str(&self.name)?;
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        /* Look for the % char to dissociate the type */
        let mut temp = *input;
        expect_bytes(&mut temp, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(&mut temp).map_err(|e| e.add_ctx::<Self>())?;
        if temp.starts_with("%") {
            Ok(Self::PEDecl(PEDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            Ok(Self::GEDecl(GEDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?))
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let entity_def = EntityDef::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, "%").map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let pe_def = PEDef::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
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
    External {
        id: ExternalID<'src>,
        decl: Option<NDataDecl<'src>>,
    },
}

impl<'src> XmlElement<'src> for EntityDef<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(ExternalID::SYSTEM_TAG) || input.starts_with(ExternalID::PUBLIC_TAG) {
            let id = ExternalID::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            let mut temp = *input;
            let skipped = skip_whitespaces(&mut temp);
            let decl = if skipped > 0 && temp.starts_with(NDataDecl::TAG) {
                Some(NDataDecl::parse(input).map_err(|e| e.add_ctx::<Self>())?)
            } else {
                None
            };
            Ok(Self::External { id, decl })
        } else {
            Ok(Self::EntityValue(EntityValue::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::EntityValue(value) => value.write(output),
            Self::External { id, decl } => {
                id.write(output)?;
                if let Some(decl) = &decl {
                    decl.write(output)?;
                }
                Ok(())
            }
        }
    }
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if input.starts_with(ExternalID::SYSTEM_TAG) || input.starts_with(ExternalID::PUBLIC_TAG) {
            Ok(Self::ExternalID(ExternalID::parse(input).map_err(|e| e.add_ctx::<Self>())?))
        } else {
            Ok(Self::EntityValue(EntityValue::parse(input).map_err(|e| e.add_ctx::<Self>())?))
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
    System {
        system: SystemLiteral<'src>,
    },
    Public {
        pubid: PubidLiteral<'src>,
        system: SystemLiteral<'src>,
    },
}

impl<'src> ExternalID<'src> {
    const SYSTEM_TAG: &'static str = "SYSTEM";
    const PUBLIC_TAG: &'static str = "PUBLIC";
}
impl<'src> XmlElement<'src> for ExternalID<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        if let Some(stripped) = input.strip_prefix(Self::SYSTEM_TAG) {
            *input = stripped;
            skip_whitespaces(input);
            let system = SystemLiteral::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Self::System { system })
        } else if let Some(stripped) = input.strip_prefix(Self::PUBLIC_TAG) {
            *input = stripped;
            skip_whitespaces(input);
            let pubid = PubidLiteral::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            skip_whitespaces(input);
            let system = SystemLiteral::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Self::Public { pubid, system })
        } else {
            Err(XmlParsingError::unexpected(&[Self::SYSTEM_TAG, Self::PUBLIC_TAG], input))
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            ExternalID::System { system } => {
                write!(output, "{} ", Self::SYSTEM_TAG)?;
                system.write(output)?;
            }
            ExternalID::Public { pubid, system } => {
                write!(output, "{} ", Self::PUBLIC_TAG)?;
                pubid.write(output)?;
                write!(output, " ")?;
                system.write(output)?;
            }
        }
        Ok(())
    }
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, Self::TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
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
    pub encoding: &'src str,
    pub quote: QuoteKind,
}

impl<'src> XmlElement<'src> for EncodingDecl<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_bytes(input, "encoding").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        expect_bytes(input, "=").map_err(|e| e.add_ctx::<Self>())?;
        skip_whitespaces(input);
        let quote = QuoteKind::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        let encoding = expect_string::<LatinAlphabet, ExtendedLatinAlphabet>(input)?;
        expect_bytes(input, quote.to_str()).map_err(|e| e.add_ctx::<Self>())?;
        Ok(Self { encoding, quote })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, " encoding={}{}{}", self.quote, self.encoding, self.quote)
    }
}

/// [82] - Notation Declaration
///
/// https://www.w3.org/TR/xml/#NT-NotationDecl
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NotationDecl<'src> {
    ExternalID {
        name: Name<'src>,
        external_id: ExternalID<'src>,
    },
    PublicID {
        name: Name<'src>,
        public_id: PublicID<'src>,
    },
}

impl<'src> NotationDecl<'src> {
    const OPENING_TAG: &'static str = "<!NOTATION";
    const CLOSING_TAG: &'static str = ">";
}

impl<'src> XmlElement<'src> for NotationDecl<'src> {
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::OPENING_TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let name = Name::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        if input.starts_with(ExternalID::SYSTEM_TAG) || input.starts_with(ExternalID::PUBLIC_TAG) {
            let external_id = ExternalID::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            skip_whitespaces(input);
            expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Self::ExternalID { name, external_id })
        } else {
            let public_id = PublicID::parse(input).map_err(|e| e.add_ctx::<Self>())?;
            skip_whitespaces(input);
            expect_bytes(input, Self::CLOSING_TAG).map_err(|e| e.add_ctx::<Self>())?;
            Ok(Self::PublicID { name, public_id })
        }
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        match self {
            Self::ExternalID { name, external_id } => {
                write!(output, "{} {} ", Self::OPENING_TAG, name)?;
                external_id.write(output)?;
                write!(output, " {}", Self::CLOSING_TAG)?;
            }
            Self::PublicID { name, public_id } => {
                write!(output, "{} {} ", Self::OPENING_TAG, name)?;
                public_id.write(output)?;
                write!(output, " {}", Self::CLOSING_TAG)?;
            }
        }
        Ok(())
    }
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
    fn parse(input: &mut &'src str) -> Result<Self, XmlParsingError<'src>> {
        expect_bytes(input, Self::TAG).map_err(|e| e.add_ctx::<Self>())?;
        expect_whitespaces(input).map_err(|e| e.add_ctx::<Self>())?;
        let literal = PubidLiteral::parse(input).map_err(|e| e.add_ctx::<Self>())?;
        Ok(Self { literal })
    }
    fn write<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        write!(output, "{} ", Self::TAG)?;
        self.literal.write(output)?;
        Ok(())
    }
}
