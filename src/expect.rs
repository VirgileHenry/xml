use crate::error::name_of;

#[derive(Debug, Clone, Copy)]
pub struct ExpectXmlError {
    element: &'static str,
    position: crate::span::Position,
    expected: &'static str,
    obtained: &'static str,
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

impl<'src> ExpectXml<crate::CharSlice<'src>> for crate::EntityValueElem<'src> {
    fn expect(&self) -> Result<&crate::CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: match reference {
                    crate::Reference::CharRef(cr) => cr.span.pos(),
                    crate::Reference::EntityRef(er) => er.name.pos(),
                },
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::Reference>(),
            }),
            Self::PEReference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: reference.name.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: match reference {
                    crate::Reference::CharRef(cr) => cr.span.pos(),
                    crate::Reference::EntityRef(er) => er.name.pos(),
                },
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::Reference>(),
            }),
            Self::PEReference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: reference.name.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
        }
    }
}

impl<'src> ExpectXml<crate::Reference<'src>> for crate::EntityValueElem<'src> {
    fn expect(&self) -> Result<&crate::Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: res.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Reference(reference) => Ok(reference),
            Self::PEReference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: reference.name.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: res.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Reference(reference) => Ok(reference),
            Self::PEReference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: reference.name.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
        }
    }
}

impl<'src> ExpectXml<crate::PEReference<'src>> for crate::EntityValueElem<'src> {
    fn expect(&self) -> Result<&crate::PEReference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: res.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Reference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: match reference {
                    crate::Reference::CharRef(cr) => cr.span.pos(),
                    crate::Reference::EntityRef(er) => er.name.pos(),
                },
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::Reference>(),
            }),
            Self::PEReference(reference) => Ok(reference),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::PEReference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: res.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Reference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: match reference {
                    crate::Reference::CharRef(cr) => cr.span.pos(),
                    crate::Reference::EntityRef(er) => er.name.pos(),
                },
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::Reference>(),
            }),
            Self::PEReference(reference) => Ok(reference),
        }
    }
}

impl<'src> ExpectXml<crate::CharSlice<'src>> for crate::AttValueElem<'src> {
    fn expect(&self) -> Result<&crate::CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: match reference {
                    crate::Reference::CharRef(cr) => cr.span.pos(),
                    crate::Reference::EntityRef(er) => er.name.pos(),
                },
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::Reference>(),
            }),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::CharSlice<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Ok(res),
            Self::Reference(reference) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: match reference {
                    crate::Reference::CharRef(cr) => cr.span.pos(),
                    crate::Reference::EntityRef(er) => er.name.pos(),
                },
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::Reference>(),
            }),
        }
    }
}

impl<'src> ExpectXml<crate::Reference<'src>> for crate::AttValueElem<'src> {
    fn expect(&self) -> Result<&crate::Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: res.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Reference(reference) => Ok(reference),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::Reference<'src>, ExpectXmlError> {
        match self {
            Self::CharSlice(res) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: res.pos(),
                expected: name_of::<crate::CharSlice>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Reference(reference) => Ok(reference),
        }
    }
}

impl<'src> ExpectXml<crate::Comment<'src>> for crate::Miscellaneous<'src> {
    fn expect(&self) -> Result<&crate::Comment<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Ok(comment),
            Self::PI(pi) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: pi.target.pos(),
                expected: name_of::<crate::Comment>(),
                obtained: name_of::<crate::PI>(),
            }),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::Comment<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Ok(comment),
            Self::PI(pi) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: pi.target.pos(),
                expected: name_of::<crate::Comment>(),
                obtained: name_of::<crate::PI>(),
            }),
        }
    }
}

impl<'src> ExpectXml<crate::PI<'src>> for crate::Miscellaneous<'src> {
    fn expect(&self) -> Result<&crate::PI<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: comment.comment.pos(),
                expected: name_of::<crate::PI>(),
                obtained: name_of::<crate::Comment>(),
            }),
            Self::PI(pi) => Ok(pi),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::PI<'src>, ExpectXmlError> {
        match self {
            Self::Comment(comment) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: comment.comment.pos(),
                expected: name_of::<crate::PI>(),
                obtained: name_of::<crate::Comment>(),
            }),
            Self::PI(pi) => Ok(pi),
        }
    }
}

impl<'src> ExpectXml<crate::PEReference<'src>> for crate::DeclSeparator<'src> {
    fn expect(&self) -> Result<&crate::PEReference<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Ok(pe_ref),
            Self::Spaces(spaces) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: spaces.pos(),
                expected: name_of::<crate::PEReference>(),
                obtained: name_of::<crate::Spaces>(),
            }),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::PEReference<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Ok(pe_ref),
            Self::Spaces(spaces) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: spaces.pos(),
                expected: name_of::<crate::PEReference>(),
                obtained: name_of::<crate::Spaces>(),
            }),
        }
    }
}

impl<'src> ExpectXml<crate::Spaces<'src>> for crate::DeclSeparator<'src> {
    fn expect(&self) -> Result<&crate::Spaces<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: pe_ref.name.pos(),
                expected: name_of::<crate::Spaces>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Spaces(spaces) => Ok(spaces),
        }
    }
    fn expect_mut(&mut self) -> Result<&mut crate::Spaces<'src>, ExpectXmlError> {
        match self {
            Self::PEReference(pe_ref) => Err(ExpectXmlError {
                element: name_of::<Self>(),
                position: pe_ref.name.pos(),
                expected: name_of::<crate::Spaces>(),
                obtained: name_of::<crate::PEReference>(),
            }),
            Self::Spaces(spaces) => Ok(spaces),
        }
    }
}
