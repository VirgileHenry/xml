fn main() {
    let raw_xml = include_str!("./tiger.svg");
    let parsed = xml::parse_xml(raw_xml).unwrap();
    let mut output = std::fs::File::create("out.xml").unwrap();
    parsed.write_xml(&mut output).unwrap();
}
