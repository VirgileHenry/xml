pub fn fatal_error(error: &str) -> ! {
    const COLOR_RED: &str = "\x1b[31m";
    const COLOR_RESET: &str = "\x1b[0m";

    eprintln!("{}{}{}", COLOR_RED, error, COLOR_RESET);
    std::process::exit(1)
}
