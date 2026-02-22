fn build_greeting() -> &'static str {
    "holo CLI"
}

fn main() {
    println!("{}", build_greeting());
}

#[cfg(test)]
mod tests {
    use super::build_greeting;

    #[test]
    fn build_greeting_returns_expected_banner() {
        assert_eq!(build_greeting(), "holo CLI");
    }
}
