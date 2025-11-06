fn main() -> Result<(), Box<dyn std::error::Error>> {
    let raw_xml = include_str!("./tiger.svg");
    let parsed = xml::parse_xml(raw_xml)?;
    let mut output = std::fs::File::create("out.xml")?;
    parsed.write_xml(&mut output)?;
    Ok(())
}
