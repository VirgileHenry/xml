//! Run the offical [XML test suite](https://www.w3.org/XML/Test/).

mod test;
mod test_declaration;
mod utils;

const TEST_SUITE_LOC: &'static str = "./test/xmlconf";

fn main() {
    println!("Running XML test suite at {TEST_SUITE_LOC}");

    let test_suite_conf_file = match std::fs::read_to_string(format!("{TEST_SUITE_LOC}/xmlconf.xml")) {
        Ok(file) => file,
        Err(e) => utils::fatal_error(&format!("Failed to open test suite conf file: {e}")),
    };
    let test_suite_conf = match xml::parse_xml(&test_suite_conf_file) {
        Ok(xml) => xml,
        Err(e) => utils::fatal_error(&format!("Failed to parse xml test suite conf: {e}")),
    };

    /* The test cases are defined in the doc type int data set */
    let doctype_decl = match test_suite_conf.prolog.doc_type_decl {
        Some(doctype) => doctype,
        None => utils::fatal_error("No doctype declaration found in test suite"),
    };
    let int_subset = match doctype_decl.int_subset {
        Some(subset) => subset,
        None => utils::fatal_error("No int subset in doctype"),
    };

    let test_declarations = test_declaration::TestDeclaration::test_declarations_from_int_subset(&int_subset);
    let tests = test::Test::test_suite_from_element(&test_suite_conf.element, &test_declarations);

    println!("");
    println!("Found tests:");

    for test in tests.iter() {
        match &test.profile {
            Some(profile) => println!("- {} at {}", profile, test.base),
            None => println!("- Misc at {}", test.base),
        }
    }

    println!("");
    println!("Running all tests");

    tests.iter().for_each(test::Test::run);
}
