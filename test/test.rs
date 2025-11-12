use super::utils::fatal_error;

pub struct Test<'src, 'tc_decl> {
    pub profile: Option<xml::AttValue<'src>>,
    pub base: xml::AttValue<'src>,
    pub tests: Vec<&'tc_decl super::test_declaration::TestDeclaration<'src>>,
}

impl<'src, 'tc_decl> Test<'src, 'tc_decl> {
    pub fn test_suite_from_element(
        element: &'src xml::Element,
        test_declarations: &'tc_decl super::test_declaration::TestDeclarations<'src>,
    ) -> Vec<Self> {
        match element {
            xml::Element::EmptyElemTag(_) => {
                fatal_error(&format!(
                    "Unexpected empty test suite at {}/xmlconf.xml",
                    super::TEST_SUITE_LOC
                ));
            }
            xml::Element::Element { s_tag, content, .. } => {
                if &*s_tag.name != "TESTSUITE" {
                    fatal_error(&format!("Expected \"TESTSUITE\" for content, found \"{}\"", s_tag.name));
                }
                println!("");
                println!("Found test suite in XML with attributes:");
                for attribute in s_tag.attributes.iter() {
                    println!("- {}: {}", attribute.name, attribute.value);
                }
                let tests = content
                    .content
                    .iter()
                    .map(|(content_elem, _)| match content_elem {
                        xml::ContentElement::Element(elem) => super::test::Test::from_element(elem, &test_declarations),
                        xml::ContentElement::Comment(_) => None,
                        other => fatal_error(&format!("Unexpected content element in test suite: {other:?}")),
                    })
                    .filter(Option::is_some)
                    .map(Option::unwrap)
                    .collect::<Vec<_>>();
                tests
            }
        }
    }

    pub fn from_element(
        element: &'src xml::Element,
        test_declarations: &'tc_decl super::test_declaration::TestDeclarations<'src>,
    ) -> Option<Self> {
        match element {
            xml::Element::EmptyElemTag(_) => {
                fatal_error("Unexpected empty test suite");
            }
            xml::Element::Element { s_tag, content, .. } => {
                if &*s_tag.name != "TESTCASES" {
                    fatal_error(&format!("Expected \"TESTSUITE\" for content, found \"{}\"", s_tag.name));
                }
                let profile = s_tag
                    .attributes
                    .iter()
                    .find(|attr| &*attr.name == "PROFILE")
                    .map(|attr| attr.value.clone());
                let base = match s_tag.attributes.iter().find(|attr| &*attr.name == "xml:base") {
                    Some(base) => base.value.clone(),
                    None => fatal_error("Attribute \"xml:base\" not found in test case!"),
                };
                let tests = content
                    .content
                    .iter()
                    .map(|(elem, _)| match elem {
                        xml::ContentElement::Reference(xml::Reference::EntityRef(entity)) => {
                            match test_declarations.get(&*entity.name) {
                                Some(test_decl) => test_decl,
                                None => fatal_error("Entity reference for test not found in test decl!"),
                            }
                        }
                        _ => fatal_error("Unexpected element in test case!"),
                    })
                    .collect();
                Some(Self { profile, base, tests })
            }
        }
    }

    pub fn run(&self) {
        println!("");
        match &self.profile {
            Some(profile) => println!("Running tests {} at {}", profile, self.base),
            None => println!("Running tests Misc at {}", self.base),
        }

        for test in self.tests.iter() {
            test.run();
        }
    }
}
