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

        let test_case_conf_file = match std::fs::read_to_string(format!("{}/{}", super::TEST_SUITE_LOC, self.path)) {
            Ok(file) => file,
            Err(e) => fatal_error(&format!("Failed to open test suite conf file: {e}")),
        };
        let test_case_conf = match xml::parse_xml(&test_case_conf_file) {
            Ok(xml) => xml,
            Err(e) => fatal_error(&format!("Failed to parse xml test suite conf: {e}")),
        };

        match test_case_conf.element {
            xml::Element::EmptyElemTag(_) => fatal_error("Unexpected empty element for test case"),
        }
    }
}
