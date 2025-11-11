use super::utils::fatal_error;

pub type TestDeclarations<'src> = std::collections::HashMap<&'src str, super::test_declaration::TestDeclaration<'src>>;

pub struct TestDeclaration<'src> {
    pub name: &'src str,
    pub path: &'src str,
}

impl<'src> TestDeclaration<'src> {
    pub fn test_declarations_from_int_subset(int_subset: &'src xml::IntSubset) -> TestDeclarations<'src> {
        int_subset
            .elements
            .iter()
            .map(|e| match e {
                xml::IntSubsetElement::MarkupDecl(decl) => TestDeclaration::from_markup_decl(decl),
                xml::IntSubsetElement::DeclSep(_) => None,
            })
            .filter(Option::is_some)
            .map(Option::unwrap)
            .map(|tc| (tc.name, tc))
            .collect::<std::collections::HashMap<_, _>>()
    }

    pub fn from_markup_decl(decl: &'src xml::MarkupDecl<'src>) -> Option<Self> {
        match decl {
            xml::MarkupDecl::EntityDecl(xml::EntityDecl::GEDecl(entity)) => Some(TestDeclaration {
                name: &entity.name,
                path: match &entity.entity_def {
                    xml::EntityDef::External {
                        id: xml::ExternalID::System { system },
                        ..
                    } => system.literal,
                    _ => fatal_error("Expected system literal for entity definition"),
                },
            }),
            xml::MarkupDecl::Comment(_) => None,
            _ => fatal_error("Expected entity definition"),
        }
    }

    pub fn run(&self) {
        println!("Running test {} at {}", self.name, self.path);
    }
}
