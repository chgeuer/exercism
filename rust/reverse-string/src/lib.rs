pub fn reverse(input: &str) -> String {
    let mut result : String = String::with_capacity(input.len());

    for c in input.chars() {
        result.insert(0, c);
    }

    result
}