pub fn brackets_are_balanced(input: &str) -> bool {
    let opener = |c: char| match c {
        ')' => Some('('),
        ']' => Some('['),
        '}' => Some('{'),
        _ => panic!("Wrong input {}", c),
    };

    let mut stack = String::with_capacity(input.len() / 2);
    for c in input.chars() {
        match c {
            '(' | '[' | '{' => stack.push(c),
            ')' | ']' | '}' => {
                if stack.pop() != opener(c) {
                    return false;
                }
            },
            _ => (),
        }
    }

    stack.len() == 0
}
