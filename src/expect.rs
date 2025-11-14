use crate::*;
use error::name_of;

#[derive(Debug, Clone, Copy)]
pub struct ExpectXmlError {
    element: &'static str,
    position: span::Position,
    expected: &'static str,
    obtained: &'static str,
}

impl ExpectXmlError {
    fn new<E, Ex, Ob>(position: span::Position) -> Self {
        ExpectXmlError {
            element: name_of::<E>(),
            position,
            expected: name_of::<Ex>(),
            obtained: name_of::<Ob>(),
        }
    }
}

impl std::fmt::Display for ExpectXmlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Expected element {} at line {}, col {} to be {}, found {}",
            self.element, self.position.line, self.position.column, self.expected, self.obtained
        )
    }
}

/// Utility trait to unpack XML DOM trees.
pub trait ExpectXml<E> {
    fn expect(&self) -> Result<&E, ExpectXmlError>;
    fn expect_mut(&mut self) -> Result<&mut E, ExpectXmlError>;
}

impl<'src> ExpectXml<CharSlice<'src>> for EntityValueElem<'src> {
    fn expect(&self) -> Result<&CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(reference.position())),
            Self::PEReference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, PEReference>(reference.name.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(reference.position())),
            Self::PEReference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, PEReference>(reference.name.pos())),
        }
    }
}

impl<'src> ExpectXml<Reference<'src>> for EntityValueElem<'src> {
    fn expect(&self) -> Result<&Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, Reference, CharSlice>(res.pos())),
            Self::Reference(reference) => Ok(reference),
            Self::PEReference(reference) => Err(ExpectXmlError::new::<Self, Reference, PEReference>(reference.name.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, Reference, CharSlice>(res.pos())),
            Self::Reference(reference) => Ok(reference),
            Self::PEReference(reference) => Err(ExpectXmlError::new::<Self, Reference, PEReference>(reference.name.pos())),
        }
    }
}

impl<'src> ExpectXml<PEReference<'src>> for EntityValueElem<'src> {
    fn expect(&self) -> Result<&PEReference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, PEReference, CharSlice>(res.pos())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, PEReference, Reference>(reference.position())),
            Self::PEReference(reference) => Ok(reference),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PEReference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, PEReference, CharSlice>(res.pos())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, PEReference, Reference>(reference.position())),
            Self::PEReference(reference) => Ok(reference),
        }
    }
}

impl<'src> ExpectXml<CharSlice<'src>> for AttValueElem<'src> {
    fn expect(&self) -> Result<&CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(reference.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(reference.position())),
        }
    }
}

impl<'src> ExpectXml<Reference<'src>> for AttValueElem<'src> {
    fn expect(&self) -> Result<&Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, Reference, CharSlice>(res.pos())),
            Self::Reference(reference) => Ok(reference),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, Reference, CharSlice>(res.pos())),
            Self::Reference(reference) => Ok(reference),
        }
    }
}

impl<'src> ExpectXml<Comment<'src>> for Miscellaneous<'src> {
    fn expect(&self) -> Result<&Comment<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Ok(comment),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Comment, Pi>(pi.target.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Comment<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Ok(comment),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Comment, Pi>(pi.target.pos())),
        }
    }
}

impl<'src> ExpectXml<Pi<'src>> for Miscellaneous<'src> {
    fn expect(&self) -> Result<&Pi<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Pi, Comment>(comment.comment.pos())),
            Self::Pi(pi) => Ok(pi),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Pi<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Pi, Comment>(comment.comment.pos())),
            Self::Pi(pi) => Ok(pi),
        }
    }
}

impl<'src> ExpectXml<PEReference<'src>> for DeclSep<'src> {
    fn expect(&self) -> Result<&PEReference<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Ok(pe_ref),
            Self::Spaces(spaces) => Err(ExpectXmlError::new::<Self, PEReference, Spaces>(spaces.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PEReference<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Ok(pe_ref),
            Self::Spaces(spaces) => Err(ExpectXmlError::new::<Self, PEReference, Spaces>(spaces.pos())),
        }
    }
}

impl<'src> ExpectXml<Spaces<'src>> for DeclSep<'src> {
    fn expect(&self) -> Result<&Spaces<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Err(ExpectXmlError::new::<Self, Spaces, PEReference>(pe_ref.name.pos())),
            Self::Spaces(spaces) => Ok(spaces),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Spaces<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Err(ExpectXmlError::new::<Self, Spaces, PEReference>(pe_ref.name.pos())),
            Self::Spaces(spaces) => Ok(spaces),
        }
    }
}

impl<'src> ExpectXml<MarkupDecl<'src>> for IntSubsetElement<'src> {
    fn expect(&self) -> Result<&MarkupDecl<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(decl_sep.position())),
            Self::MarkupDecl(markup) => Ok(markup),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut MarkupDecl<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(decl_sep.position())),
            Self::MarkupDecl(markup) => Ok(markup),
        }
    }
}

impl<'src> ExpectXml<DeclSep<'src>> for IntSubsetElement<'src> {
    fn expect(&self) -> Result<&DeclSep<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Ok(decl_sep),
            Self::MarkupDecl(markup) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(markup.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut DeclSep<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Ok(decl_sep),
            Self::MarkupDecl(markup) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(markup.position())),
        }
    }
}

impl<'src> ExpectXml<ElementDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&ElementDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Ok(element),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, ElementDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, ElementDecl, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, ElementDecl, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, ElementDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, ElementDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ElementDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Ok(element),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, ElementDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, ElementDecl, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, ElementDecl, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, ElementDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, ElementDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<AttListDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&AttListDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, AttListDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Ok(att_list),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, AttListDecl, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, AttListDecl, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, AttListDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, AttListDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut AttListDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, AttListDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Ok(att_list),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, AttListDecl, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, AttListDecl, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, AttListDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, AttListDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<EntityDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&EntityDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, EntityDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, EntityDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Ok(entity),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, EntityDecl, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, EntityDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, EntityDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EntityDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, EntityDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, EntityDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Ok(entity),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, EntityDecl, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, EntityDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, EntityDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<NotationDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&NotationDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, NotationDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, NotationDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, NotationDecl, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Ok(notation),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, NotationDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, NotationDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut NotationDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, NotationDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, NotationDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, NotationDecl, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Ok(notation),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, NotationDecl, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, NotationDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Pi<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&Pi<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, Pi, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, Pi, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, Pi, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, Pi, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Pi, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Pi<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, Pi, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, Pi, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, Pi, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, Pi, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Pi, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Comment<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&Comment<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, Comment, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, Comment, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, Pi, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, Comment, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Comment, Pi>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Comment<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, Comment, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, Comment, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, Pi, EntityDecl>(entity.position())),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, Comment, NotationDecl>(notation.name.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Comment, Pi>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
}

impl<'src> ExpectXml<EmptyElemTag<'src>> for Element<'src> {
    fn expect(&self) -> Result<&EmptyElemTag<'src>, ExpectXmlError> {
        match self {
            Self::EmptyElemTag(empty) => Ok(empty),
            Self::Element(element) => Err(ExpectXmlError::new::<Self, EmptyElemTag, NonEmptyElement>(
                element.s_tag.name.pos(),
            )),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EmptyElemTag<'src>, ExpectXmlError> {
        match self {
            Self::EmptyElemTag(empty) => Ok(empty),
            Self::Element(element) => Err(ExpectXmlError::new::<Self, EmptyElemTag, NonEmptyElement>(
                element.s_tag.name.pos(),
            )),
        }
    }
}

impl<'src> ExpectXml<NonEmptyElement<'src>> for Element<'src> {
    fn expect(&self) -> Result<&NonEmptyElement<'src>, ExpectXmlError> {
        match self {
            Self::EmptyElemTag(empty) => Err(ExpectXmlError::new::<Self, NonEmptyElement, EmptyElemTag>(empty.name.pos())),
            Self::Element(element) => Ok(element),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut NonEmptyElement<'src>, ExpectXmlError> {
        match self {
            Self::EmptyElemTag(empty) => Err(ExpectXmlError::new::<Self, NonEmptyElement, EmptyElemTag>(empty.name.pos())),
            Self::Element(element) => Ok(element),
        }
    }
}

impl<'src> ExpectXml<Element<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&Element<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Ok(element),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Element, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Element, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Element, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Element, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Element<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Ok(element),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Element, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Element, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Element, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Element, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Reference<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&Reference<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Reference, Element>(element.position())),
            Self::Reference(reference) => Ok(reference),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Reference, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Reference, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Reference, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Reference<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Reference, Element>(element.position())),
            Self::Reference(reference) => Ok(reference),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Reference, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Reference, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Reference, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<CDSect<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&CDSect<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, CDSect, Element>(element.position())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CDSect, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Ok(cd_sect),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, CDSect, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, CDSect, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CDSect<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, CDSect, Element>(element.position())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CDSect, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Ok(cd_sect),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, CDSect, Pi>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, CDSect, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Pi<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&Pi<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Pi, Element>(element.position())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Pi, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Pi, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Pi, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Pi<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Pi, Element>(element.position())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Pi, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Pi, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Pi, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Comment<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&Comment<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Comment, Element>(element.position())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Comment, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Comment, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Comment, Pi>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Comment<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Comment, Element>(element.position())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Comment, Reference>(reference.position())),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Comment, CDSect>(cd_sect.data.pos())),
            Self::Pi(pi) => Err(ExpectXmlError::new::<Self, Pi, Comment>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
}

impl<'src> ExpectXml<ContentSpecEmpty> for ContentSpec<'src> {
    fn expect(&self) -> Result<&ContentSpecEmpty, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Ok(empty),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, ContentSpecEmpty, ContentSpecAny>(any.position)),
            Self::Mixed(mixed) => Err(ExpectXmlError::new::<Self, ContentSpecEmpty, Mixed>(mixed.position())),
            Self::Children(children) => Err(ExpectXmlError::new::<Self, ContentSpecEmpty, Children>(children.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ContentSpecEmpty, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Ok(empty),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, ContentSpecEmpty, ContentSpecAny>(any.position)),
            Self::Mixed(mixed) => Err(ExpectXmlError::new::<Self, ContentSpecEmpty, Mixed>(mixed.position())),
            Self::Children(children) => Err(ExpectXmlError::new::<Self, ContentSpecEmpty, Children>(children.position())),
        }
    }
}

impl<'src> ExpectXml<ContentSpecAny> for ContentSpec<'src> {
    fn expect(&self) -> Result<&ContentSpecAny, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Err(ExpectXmlError::new::<Self, ContentSpecAny, ContentSpecEmpty>(empty.position)),
            Self::Any(any) => Ok(any),
            Self::Mixed(mixed) => Err(ExpectXmlError::new::<Self, ContentSpecAny, Mixed>(mixed.position())),
            Self::Children(children) => Err(ExpectXmlError::new::<Self, ContentSpecAny, Children>(children.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ContentSpecAny, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Err(ExpectXmlError::new::<Self, ContentSpecAny, ContentSpecEmpty>(empty.position)),
            Self::Any(any) => Ok(any),
            Self::Mixed(mixed) => Err(ExpectXmlError::new::<Self, ContentSpecAny, Mixed>(mixed.position())),
            Self::Children(children) => Err(ExpectXmlError::new::<Self, ContentSpecAny, Children>(children.position())),
        }
    }
}

impl<'src> ExpectXml<Mixed<'src>> for ContentSpec<'src> {
    fn expect(&self) -> Result<&Mixed<'src>, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Err(ExpectXmlError::new::<Self, Mixed, ContentSpecEmpty>(empty.position)),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, Mixed, ContentSpecAny>(any.position)),
            Self::Mixed(mixed) => Ok(mixed),
            Self::Children(children) => Err(ExpectXmlError::new::<Self, Mixed, Children>(children.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Mixed<'src>, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Err(ExpectXmlError::new::<Self, Mixed, ContentSpecEmpty>(empty.position)),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, Mixed, ContentSpecAny>(any.position)),
            Self::Mixed(mixed) => Ok(mixed),
            Self::Children(children) => Err(ExpectXmlError::new::<Self, Mixed, Children>(children.position())),
        }
    }
}

impl<'src> ExpectXml<Children<'src>> for ContentSpec<'src> {
    fn expect(&self) -> Result<&Children<'src>, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Err(ExpectXmlError::new::<Self, Children, ContentSpecEmpty>(empty.position)),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, Children, ContentSpecAny>(any.position)),
            Self::Mixed(mixed) => Err(ExpectXmlError::new::<Self, Children, Mixed>(mixed.position())),
            Self::Children(children) => Ok(children),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Children<'src>, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Err(ExpectXmlError::new::<Self, Children, ContentSpecEmpty>(empty.position)),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, Children, ContentSpecAny>(any.position)),
            Self::Mixed(mixed) => Err(ExpectXmlError::new::<Self, Children, Mixed>(mixed.position())),
            Self::Children(children) => Ok(children),
        }
    }
}

impl<'src> ExpectXml<ChildrenChoice<'src>> for Children<'src> {
    fn expect(&self) -> Result<&ChildrenChoice<'src>, ExpectXmlError> {
        match self {
            Self::Choice(choice) => Ok(choice),
            Self::Seq(seq) => Err(ExpectXmlError::new::<Self, ChildrenChoice, ChildrenSeq>(seq.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ChildrenChoice<'src>, ExpectXmlError> {
        match self {
            Self::Choice(choice) => Ok(choice),
            Self::Seq(seq) => Err(ExpectXmlError::new::<Self, ChildrenChoice, ChildrenSeq>(seq.position())),
        }
    }
}

impl<'src> ExpectXml<ChildrenSeq<'src>> for Children<'src> {
    fn expect(&self) -> Result<&ChildrenSeq<'src>, ExpectXmlError> {
        match self {
            Self::Choice(choice) => Err(ExpectXmlError::new::<Self, ChildrenSeq, ChildrenChoice>(choice.position())),
            Self::Seq(seq) => Ok(seq),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ChildrenSeq<'src>, ExpectXmlError> {
        match self {
            Self::Choice(choice) => Err(ExpectXmlError::new::<Self, ChildrenSeq, ChildrenChoice>(choice.position())),
            Self::Seq(seq) => Ok(seq),
        }
    }
}

impl<'src> ExpectXml<CpName<'src>> for Cp<'src> {
    fn expect(&self) -> Result<&CpName<'src>, ExpectXmlError> {
        match self {
            Self::Name(name) => Ok(name),
            Self::Choice(choice) => Err(ExpectXmlError::new::<Self, CpName, CpChoice>(choice.position())),
            Self::Seq(seq) => Err(ExpectXmlError::new::<Self, CpName, CpSeq>(seq.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CpName<'src>, ExpectXmlError> {
        match self {
            Self::Name(name) => Ok(name),
            Self::Choice(choice) => Err(ExpectXmlError::new::<Self, CpName, CpChoice>(choice.position())),
            Self::Seq(seq) => Err(ExpectXmlError::new::<Self, CpName, CpSeq>(seq.position())),
        }
    }
}

impl<'src> ExpectXml<CpChoice<'src>> for Cp<'src> {
    fn expect(&self) -> Result<&CpChoice<'src>, ExpectXmlError> {
        match self {
            Self::Name(name) => Err(ExpectXmlError::new::<Self, CpChoice, ChildrenChoice>(name.name.position)),
            Self::Choice(choice) => Ok(choice),
            Self::Seq(seq) => Err(ExpectXmlError::new::<Self, CpChoice, CpSeq>(seq.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CpChoice<'src>, ExpectXmlError> {
        match self {
            Self::Name(name) => Err(ExpectXmlError::new::<Self, CpChoice, ChildrenChoice>(name.name.position)),
            Self::Choice(choice) => Ok(choice),
            Self::Seq(seq) => Err(ExpectXmlError::new::<Self, CpChoice, CpSeq>(seq.position())),
        }
    }
}

impl<'src> ExpectXml<CpSeq<'src>> for Cp<'src> {
    fn expect(&self) -> Result<&CpSeq<'src>, ExpectXmlError> {
        match self {
            Self::Name(name) => Err(ExpectXmlError::new::<Self, CpName, ChildrenChoice>(name.name.position)),
            Self::Choice(choice) => Err(ExpectXmlError::new::<Self, CpSeq, CpChoice>(choice.position())),
            Self::Seq(seq) => Ok(seq),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CpSeq<'src>, ExpectXmlError> {
        match self {
            Self::Name(name) => Err(ExpectXmlError::new::<Self, CpName, ChildrenChoice>(name.name.position)),
            Self::Choice(choice) => Err(ExpectXmlError::new::<Self, CpSeq, CpChoice>(choice.position())),
            Self::Seq(seq) => Ok(seq),
        }
    }
}

impl<'src> ExpectXml<MixedEmpty> for Mixed<'src> {
    fn expect(&self) -> Result<&MixedEmpty, ExpectXmlError> {
        match self {
            Self::MixedEmpty(empty) => Ok(empty),
            Self::MixedContent(content) => Err(ExpectXmlError::new::<Self, MixedEmpty, MixedContent>(content.position)),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut MixedEmpty, ExpectXmlError> {
        match self {
            Self::MixedEmpty(empty) => Ok(empty),
            Self::MixedContent(content) => Err(ExpectXmlError::new::<Self, MixedEmpty, MixedContent>(content.position)),
        }
    }
}

impl<'src> ExpectXml<MixedContent<'src>> for Mixed<'src> {
    fn expect(&self) -> Result<&MixedContent<'src>, ExpectXmlError> {
        match self {
            Self::MixedEmpty(empty) => Err(ExpectXmlError::new::<Self, MixedContent, MixedEmpty>(empty.position)),
            Self::MixedContent(content) => Ok(content),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut MixedContent<'src>, ExpectXmlError> {
        match self {
            Self::MixedEmpty(empty) => Err(ExpectXmlError::new::<Self, MixedContent, MixedEmpty>(empty.position)),
            Self::MixedContent(content) => Ok(content),
        }
    }
}

impl<'src> ExpectXml<StringType> for AttributeType<'src> {
    fn expect(&self) -> Result<&StringType, ExpectXmlError> {
        match self {
            Self::StringType(string) => Ok(string),
            Self::TokenizedType(tokenized) => Err(ExpectXmlError::new::<Self, StringType, TokenizedType>(tokenized.position())),
            Self::EnumeratedType(enumerated) => {
                Err(ExpectXmlError::new::<Self, StringType, EnumeratedType>(enumerated.position()))
            }
        }
    }
    fn expect_mut(&mut self) -> Result<&mut StringType, ExpectXmlError> {
        match self {
            Self::StringType(string) => Ok(string),
            Self::TokenizedType(tokenized) => Err(ExpectXmlError::new::<Self, StringType, TokenizedType>(tokenized.position())),
            Self::EnumeratedType(enumerated) => {
                Err(ExpectXmlError::new::<Self, StringType, EnumeratedType>(enumerated.position()))
            }
        }
    }
}

impl<'src> ExpectXml<TokenizedType> for AttributeType<'src> {
    fn expect(&self) -> Result<&TokenizedType, ExpectXmlError> {
        match self {
            Self::StringType(string) => Err(ExpectXmlError::new::<Self, TokenizedType, StringType>(string.position)),
            Self::TokenizedType(tokenized) => Ok(tokenized),
            Self::EnumeratedType(enumerated) => Err(ExpectXmlError::new::<Self, TokenizedType, EnumeratedType>(
                enumerated.position(),
            )),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut TokenizedType, ExpectXmlError> {
        match self {
            Self::StringType(string) => Err(ExpectXmlError::new::<Self, TokenizedType, StringType>(string.position)),
            Self::TokenizedType(tokenized) => Ok(tokenized),
            Self::EnumeratedType(enumerated) => Err(ExpectXmlError::new::<Self, TokenizedType, EnumeratedType>(
                enumerated.position(),
            )),
        }
    }
}

impl<'src> ExpectXml<EnumeratedType<'src>> for AttributeType<'src> {
    fn expect(&self) -> Result<&EnumeratedType<'src>, ExpectXmlError> {
        match self {
            Self::StringType(string) => Err(ExpectXmlError::new::<Self, EnumeratedType, StringType>(string.position)),
            Self::TokenizedType(tokenized) => Err(ExpectXmlError::new::<Self, EnumeratedType, TokenizedType>(
                tokenized.position(),
            )),
            Self::EnumeratedType(enumerated) => Ok(enumerated),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EnumeratedType<'src>, ExpectXmlError> {
        match self {
            Self::StringType(string) => Err(ExpectXmlError::new::<Self, EnumeratedType, StringType>(string.position)),
            Self::TokenizedType(tokenized) => Err(ExpectXmlError::new::<Self, EnumeratedType, TokenizedType>(
                tokenized.position(),
            )),
            Self::EnumeratedType(enumerated) => Ok(enumerated),
        }
    }
}

impl<'src> ExpectXml<NotationType<'src>> for EnumeratedType<'src> {
    fn expect(&self) -> Result<&NotationType<'src>, ExpectXmlError> {
        match self {
            Self::NotationType(notation) => Ok(notation),
            Self::Enumeration(enumeration) => Err(ExpectXmlError::new::<Self, NotationType, Enumeration>(enumeration.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut NotationType<'src>, ExpectXmlError> {
        match self {
            Self::NotationType(notation) => Ok(notation),
            Self::Enumeration(enumeration) => Err(ExpectXmlError::new::<Self, NotationType, Enumeration>(enumeration.position())),
        }
    }
}

impl<'src> ExpectXml<Enumeration<'src>> for EnumeratedType<'src> {
    fn expect(&self) -> Result<&Enumeration<'src>, ExpectXmlError> {
        match self {
            Self::NotationType(notation) => Err(ExpectXmlError::new::<Self, Enumeration, NotationType>(notation.position())),
            Self::Enumeration(enumeration) => Ok(enumeration),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Enumeration<'src>, ExpectXmlError> {
        match self {
            Self::NotationType(notation) => Err(ExpectXmlError::new::<Self, Enumeration, NotationType>(notation.position())),
            Self::Enumeration(enumeration) => Ok(enumeration),
        }
    }
}

impl<'src> ExpectXml<DefaultDeclRequired> for DefaultDecl<'src> {
    fn expect(&self) -> Result<&DefaultDeclRequired, ExpectXmlError> {
        match self {
            Self::Required(required) => Ok(required),
            Self::Implied(implied) => Err(ExpectXmlError::new::<Self, DefaultDeclRequired, DefaultDeclImplied>(
                implied.position,
            )),
            Self::Value(value) => Err(ExpectXmlError::new::<Self, DefaultDeclRequired, DefaultDeclValue>(
                value.position,
            )),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut DefaultDeclRequired, ExpectXmlError> {
        match self {
            Self::Required(required) => Ok(required),
            Self::Implied(implied) => Err(ExpectXmlError::new::<Self, DefaultDeclRequired, DefaultDeclImplied>(
                implied.position,
            )),
            Self::Value(value) => Err(ExpectXmlError::new::<Self, DefaultDeclRequired, DefaultDeclValue>(
                value.position,
            )),
        }
    }
}

impl<'src> ExpectXml<DefaultDeclImplied> for DefaultDecl<'src> {
    fn expect(&self) -> Result<&DefaultDeclImplied, ExpectXmlError> {
        match self {
            Self::Required(required) => Err(ExpectXmlError::new::<Self, DefaultDeclImplied, DefaultDeclRequired>(
                required.position,
            )),
            Self::Implied(implied) => Ok(implied),
            Self::Value(value) => Err(ExpectXmlError::new::<Self, DefaultDeclImplied, DefaultDeclValue>(
                value.position,
            )),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut DefaultDeclImplied, ExpectXmlError> {
        match self {
            Self::Required(required) => Err(ExpectXmlError::new::<Self, DefaultDeclImplied, DefaultDeclRequired>(
                required.position,
            )),
            Self::Implied(implied) => Ok(implied),
            Self::Value(value) => Err(ExpectXmlError::new::<Self, DefaultDeclImplied, DefaultDeclValue>(
                value.position,
            )),
        }
    }
}

impl<'src> ExpectXml<DefaultDeclValue<'src>> for DefaultDecl<'src> {
    fn expect(&self) -> Result<&DefaultDeclValue<'src>, ExpectXmlError> {
        match self {
            Self::Required(required) => Err(ExpectXmlError::new::<Self, DefaultDeclValue, DefaultDeclRequired>(
                required.position,
            )),
            Self::Implied(implied) => Err(ExpectXmlError::new::<Self, DefaultDeclValue, DefaultDeclImplied>(
                implied.position,
            )),
            Self::Value(value) => Ok(value),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut DefaultDeclValue<'src>, ExpectXmlError> {
        match self {
            Self::Required(required) => Err(ExpectXmlError::new::<Self, DefaultDeclValue, DefaultDeclRequired>(
                required.position,
            )),
            Self::Implied(implied) => Err(ExpectXmlError::new::<Self, DefaultDeclValue, DefaultDeclImplied>(
                implied.position,
            )),
            Self::Value(value) => Ok(value),
        }
    }
}

impl<'src> ExpectXml<EntityRef<'src>> for Reference<'src> {
    fn expect(&self) -> Result<&EntityRef<'src>, ExpectXmlError> {
        match self {
            Self::EntityRef(entity) => Ok(entity),
            Self::CharRef(char_ref) => Err(ExpectXmlError::new::<Self, EntityRef, CharRef>(char_ref.position)),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EntityRef<'src>, ExpectXmlError> {
        match self {
            Self::EntityRef(entity) => Ok(entity),
            Self::CharRef(char_ref) => Err(ExpectXmlError::new::<Self, EntityRef, CharRef>(char_ref.position)),
        }
    }
}

impl<'src> ExpectXml<CharRef> for Reference<'src> {
    fn expect(&self) -> Result<&CharRef, ExpectXmlError> {
        match self {
            Self::CharRef(char_ref) => Ok(char_ref),
            Self::EntityRef(entity) => Err(ExpectXmlError::new::<Self, CharRef, EntityRef>(entity.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CharRef, ExpectXmlError> {
        match self {
            Self::CharRef(char_ref) => Ok(char_ref),
            Self::EntityRef(entity) => Err(ExpectXmlError::new::<Self, CharRef, EntityRef>(entity.position())),
        }
    }
}

impl<'src> ExpectXml<GEDecl<'src>> for EntityDecl<'src> {
    fn expect(&self) -> Result<&GEDecl<'src>, ExpectXmlError> {
        match self {
            Self::GEDecl(ge) => Ok(ge),
            Self::PEDecl(pe) => Err(ExpectXmlError::new::<Self, PEDecl, GEDecl>(pe.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut GEDecl<'src>, ExpectXmlError> {
        match self {
            Self::GEDecl(ge) => Ok(ge),
            Self::PEDecl(pe) => Err(ExpectXmlError::new::<Self, PEDecl, GEDecl>(pe.position())),
        }
    }
}

impl<'src> ExpectXml<PEDecl<'src>> for EntityDecl<'src> {
    fn expect(&self) -> Result<&PEDecl<'src>, ExpectXmlError> {
        match self {
            Self::PEDecl(pe) => Ok(pe),
            Self::GEDecl(ge) => Err(ExpectXmlError::new::<Self, GEDecl, PEDecl>(ge.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PEDecl<'src>, ExpectXmlError> {
        match self {
            Self::PEDecl(pe) => Ok(pe),
            Self::GEDecl(ge) => Err(ExpectXmlError::new::<Self, GEDecl, PEDecl>(ge.position())),
        }
    }
}

impl<'src> ExpectXml<EntityValue<'src>> for EntityDef<'src> {
    fn expect(&self) -> Result<&EntityValue<'src>, ExpectXmlError> {
        match self {
            Self::EntityValue(entity) => Ok(entity),
            Self::External(external) => Err(ExpectXmlError::new::<Self, EntityValue, EntityDefExternal>(external.position)),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EntityValue<'src>, ExpectXmlError> {
        match self {
            Self::EntityValue(entity) => Ok(entity),
            Self::External(external) => Err(ExpectXmlError::new::<Self, EntityValue, EntityDefExternal>(external.position)),
        }
    }
}

impl<'src> ExpectXml<EntityDefExternal<'src>> for EntityDef<'src> {
    fn expect(&self) -> Result<&EntityDefExternal<'src>, ExpectXmlError> {
        match self {
            Self::External(external) => Ok(external),
            Self::EntityValue(entity) => Err(ExpectXmlError::new::<Self, EntityDefExternal, EntityValue>(entity.position)),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EntityDefExternal<'src>, ExpectXmlError> {
        match self {
            Self::External(external) => Ok(external),
            Self::EntityValue(entity) => Err(ExpectXmlError::new::<Self, EntityDefExternal, EntityValue>(entity.position)),
        }
    }
}

impl<'src> ExpectXml<EntityValue<'src>> for PEDef<'src> {
    fn expect(&self) -> Result<&EntityValue<'src>, ExpectXmlError> {
        match self {
            Self::EntityValue(entity) => Ok(entity),
            Self::ExternalID(external) => Err(ExpectXmlError::new::<Self, EntityValue, ExternalID>(external.position())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EntityValue<'src>, ExpectXmlError> {
        match self {
            Self::EntityValue(entity) => Ok(entity),
            Self::ExternalID(external) => Err(ExpectXmlError::new::<Self, EntityValue, ExternalID>(external.position())),
        }
    }
}

impl<'src> ExpectXml<ExternalID<'src>> for PEDef<'src> {
    fn expect(&self) -> Result<&ExternalID<'src>, ExpectXmlError> {
        match self {
            Self::ExternalID(external) => Ok(external),
            Self::EntityValue(entity) => Err(ExpectXmlError::new::<Self, ExternalID, EntityValue>(entity.position)),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ExternalID<'src>, ExpectXmlError> {
        match self {
            Self::ExternalID(external) => Ok(external),
            Self::EntityValue(entity) => Err(ExpectXmlError::new::<Self, ExternalID, EntityValue>(entity.position)),
        }
    }
}

impl<'src> ExpectXml<ExternalIDSystem<'src>> for ExternalID<'src> {
    fn expect(&self) -> Result<&ExternalIDSystem<'src>, ExpectXmlError> {
        match self {
            Self::System(system) => Ok(system),
            Self::Public(public) => Err(ExpectXmlError::new::<Self, ExternalIDSystem, ExternalIDPublic>(
                public.pubid.position(),
            )),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ExternalIDSystem<'src>, ExpectXmlError> {
        match self {
            Self::System(system) => Ok(system),
            Self::Public(public) => Err(ExpectXmlError::new::<Self, ExternalIDSystem, ExternalIDPublic>(
                public.pubid.position(),
            )),
        }
    }
}

impl<'src> ExpectXml<ExternalIDPublic<'src>> for ExternalID<'src> {
    fn expect(&self) -> Result<&ExternalIDPublic<'src>, ExpectXmlError> {
        match self {
            Self::System(system) => Err(ExpectXmlError::new::<Self, ExternalIDPublic, ExternalIDSystem>(
                system.system.position(),
            )),
            Self::Public(public) => Ok(public),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ExternalIDPublic<'src>, ExpectXmlError> {
        match self {
            Self::System(system) => Err(ExpectXmlError::new::<Self, ExternalIDPublic, ExternalIDSystem>(
                system.system.position(),
            )),
            Self::Public(public) => Ok(public),
        }
    }
}

impl<'src> ExpectXml<ExternalID<'src>> for NotationDeclID<'src> {
    fn expect(&self) -> Result<&ExternalID<'src>, ExpectXmlError> {
        match self {
            Self::PublicID(pid) => Err(ExpectXmlError::new::<Self, ExternalID, PublicID>(pid.position())),
            Self::ExternalID(extid) => Ok(extid),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ExternalID<'src>, ExpectXmlError> {
        match self {
            Self::PublicID(pid) => Err(ExpectXmlError::new::<Self, ExternalID, PublicID>(pid.position())),
            Self::ExternalID(extid) => Ok(extid),
        }
    }
}

impl<'src> ExpectXml<PublicID<'src>> for NotationDeclID<'src> {
    fn expect(&self) -> Result<&PublicID<'src>, ExpectXmlError> {
        match self {
            Self::ExternalID(extid) => Err(ExpectXmlError::new::<Self, PublicID, ExternalID>(extid.position())),
            Self::PublicID(pid) => Ok(pid),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PublicID<'src>, ExpectXmlError> {
        match self {
            Self::ExternalID(extid) => Err(ExpectXmlError::new::<Self, PublicID, ExternalID>(extid.position())),
            Self::PublicID(pid) => Ok(pid),
        }
    }
}
