pub fn brackets_are_balanced(input: &str) -> bool {
    let mut stack = String::with_capacity(input.len() / 2);
    for c in input.chars() {
        match c {
            '(' | '[' | '{' => stack.push(c),
            ')' => {
                match stack.pop() {
                    Some('(') => (),
                    _ => return false,
                }
            },
            ']' => {
                match stack.pop() {
                    Some('[') => (),
                    _ => return false,
                }
            },
            '}' => {
                match stack.pop() {
                    Some('{') => (),
                    _ => return false,
                }
            },
            _ => (),
        }
    }

    stack.len() == 0
}
