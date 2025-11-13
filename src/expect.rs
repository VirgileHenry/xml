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
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::PEReference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, PEReference>(reference.name.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
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
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, PEReference, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::PEReference(reference) => Ok(reference),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PEReference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError::new::<Self, PEReference, CharSlice>(res.pos())),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, PEReference, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::PEReference(reference) => Ok(reference),
        }
    }
}

impl<'src> ExpectXml<CharSlice<'src>> for AttValueElem<'src> {
    fn expect(&self) -> Result<&CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CharSlice, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
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
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Comment, PI>(pi.target.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Comment<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Ok(comment),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Comment, PI>(pi.target.pos())),
        }
    }
}

impl<'src> ExpectXml<PI<'src>> for Miscellaneous<'src> {
    fn expect(&self) -> Result<&PI<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, PI, Comment>(comment.comment.pos())),
            Self::PI(pi) => Ok(pi),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PI<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, PI, Comment>(comment.comment.pos())),
            Self::PI(pi) => Ok(pi),
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
            Self::DeclSep(decl_sep) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(match decl_sep {
                DeclSep::PEReference(pe_ref) => pe_ref.name.pos(),
                DeclSep::Spaces(spaces) => spaces.pos(),
            })),
            Self::MarkupDecl(markup) => Ok(markup),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut MarkupDecl<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(match decl_sep {
                DeclSep::PEReference(pe_ref) => pe_ref.name.pos(),
                DeclSep::Spaces(spaces) => spaces.pos(),
            })),
            Self::MarkupDecl(markup) => Ok(markup),
        }
    }
}

impl<'src> ExpectXml<DeclSep<'src>> for IntSubsetElement<'src> {
    fn expect(&self) -> Result<&DeclSep<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Ok(decl_sep),
            Self::MarkupDecl(markup) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(match markup {
                MarkupDecl::AttListDecl(att_list_decl) => att_list_decl.name.pos(),
                MarkupDecl::PI(pi) => pi.target.pos(),
                MarkupDecl::Comment(comment) => comment.comment.pos(),
                MarkupDecl::EntityDecl(entity) => match entity {
                    EntityDecl::GEDecl(ge) => ge.name.pos(),
                    EntityDecl::PEDecl(pe) => pe.name.pos(),
                },
                MarkupDecl::ElementDecl(elem) => elem.name.pos(),
                MarkupDecl::NotationDecl(notation) => notation.name.pos(),
            })),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut DeclSep<'src>, ExpectXmlError> {
        match self {
            Self::DeclSep(decl_sep) => Ok(decl_sep),
            Self::MarkupDecl(markup) => Err(ExpectXmlError::new::<Self, DeclSep, MarkupDecl>(match markup {
                MarkupDecl::AttListDecl(att_list_decl) => att_list_decl.name.pos(),
                MarkupDecl::PI(pi) => pi.target.pos(),
                MarkupDecl::Comment(comment) => comment.comment.pos(),
                MarkupDecl::EntityDecl(entity) => match entity {
                    EntityDecl::GEDecl(ge) => ge.name.pos(),
                    EntityDecl::PEDecl(pe) => pe.name.pos(),
                },
                MarkupDecl::ElementDecl(elem) => elem.name.pos(),
                MarkupDecl::NotationDecl(notation) => notation.name.pos(),
            })),
        }
    }
}

impl<'src> ExpectXml<ElementDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&ElementDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Ok(element),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, ElementDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, ElementDecl, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, ElementDecl, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, ElementDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, ElementDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut ElementDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Ok(element),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, ElementDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, ElementDecl, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, ElementDecl, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, ElementDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, ElementDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<AttListDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&AttListDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, AttListDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Ok(att_list),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, AttListDecl, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, AttListDecl, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, AttListDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, AttListDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut AttListDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, AttListDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Ok(att_list),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, AttListDecl, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, AttListDecl, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, AttListDecl, PI>(pi.target.pos())),
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
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, EntityDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, EntityDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EntityDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, EntityDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, EntityDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Ok(entity),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, EntityDecl, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, EntityDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, EntityDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<NotationDecl<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&NotationDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, NotationDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, NotationDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, NotationDecl, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Ok(notation),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, NotationDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, NotationDecl, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut NotationDecl<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, NotationDecl, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, NotationDecl, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, NotationDecl, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Ok(notation),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, NotationDecl, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, NotationDecl, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<PI<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&PI<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, PI, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, PI, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, PI, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, PI, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, PI, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PI<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, PI, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, PI, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, PI, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, PI, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, PI, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Comment<'src>> for MarkupDecl<'src> {
    fn expect(&self) -> Result<&Comment<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, Comment, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, Comment, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, PI, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, Comment, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Comment, PI>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Comment<'src>, ExpectXmlError> {
        match self {
            Self::ElementDecl(element) => Err(ExpectXmlError::new::<Self, Comment, ElementDecl>(element.name.pos())),
            Self::AttListDecl(att_list) => Err(ExpectXmlError::new::<Self, Comment, AttListDecl>(att_list.name.pos())),
            Self::EntityDecl(entity) => Err(ExpectXmlError::new::<Self, PI, EntityDecl>(match entity {
                EntityDecl::GEDecl(ge) => ge.name.pos(),
                EntityDecl::PEDecl(pe) => pe.name.pos(),
            })),
            Self::NotationDecl(notation) => Err(ExpectXmlError::new::<Self, Comment, NotationDecl>(notation.name.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Comment, PI>(pi.target.pos())),
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
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Element, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Element, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Element, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Element, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Element<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Ok(element),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Element, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Element, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Element, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Element, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Reference<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&Reference<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Reference, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Ok(reference),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Reference, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Reference, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Reference, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Reference<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Reference, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Ok(reference),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Reference, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Reference, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, Reference, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<CDSect<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&CDSect<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, CDSect, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CDSect, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Ok(cd_sect),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, CDSect, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, CDSect, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut CDSect<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, CDSect, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, CDSect, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Ok(cd_sect),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, CDSect, PI>(pi.target.pos())),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, CDSect, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<PI<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&PI<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, PI, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, PI, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, PI, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, PI, Comment>(comment.comment.pos())),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut PI<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, PI, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, PI, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, PI, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Ok(pi),
            Self::Comment(comment) => Err(ExpectXmlError::new::<Self, PI, Comment>(comment.comment.pos())),
        }
    }
}

impl<'src> ExpectXml<Comment<'src>> for ContentElement<'src> {
    fn expect(&self) -> Result<&Comment<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Comment, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Comment, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Comment, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, Comment, PI>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut Comment<'src>, ExpectXmlError> {
        match self {
            Self::Element(element) => Err(ExpectXmlError::new::<Self, Comment, Element>(match element {
                Element::Element(elem) => elem.s_tag.name.pos(),
                Element::EmptyElemTag(empty) => empty.name.pos(),
            })),
            Self::Reference(reference) => Err(ExpectXmlError::new::<Self, Comment, Reference>(match reference {
                Reference::CharRef(cr) => cr.span.pos(),
                Reference::EntityRef(er) => er.name.pos(),
            })),
            Self::CDSect(cd_sect) => Err(ExpectXmlError::new::<Self, Comment, CDSect>(cd_sect.data.pos())),
            Self::PI(pi) => Err(ExpectXmlError::new::<Self, PI, Comment>(pi.target.pos())),
            Self::Comment(comment) => Ok(comment),
        }
    }
}

impl<'src> ExpectXml<EmptyContentSpec> for ContentSpec<'src> {
    fn expect(&self) -> Result<&EmptyContentSpec, ExpectXmlError> {
        match self {
            Self::Empty(empty) => Ok(empty),
            Self::Any(any) => Err(ExpectXmlError::new::<Self, EmptyContentSpec, AnyContentSpec>(any.position)),
            Self::Mixed() => unimplemented!(),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut EmptyContentSpec, ExpectXmlError> {
        match self {
            Self::EmptyElemTag(empty) => Err(ExpectXmlError::new::<Self, NonEmptyElement, EmptyElemTag>(empty.name.pos())),
            Self::Element(element) => Ok(element),
        }
    }
}
