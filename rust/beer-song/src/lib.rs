use std::i32::{MIN, MAX};

fn pluralize_bottle(n: i32, sentence_beginning: bool) -> String {
    match (n, sentence_beginning) {
        (0, true) => "No more bottles".to_string(),
        (0, false) => "no more bottles".to_string(),
        (1, _) => "1 bottle".to_string(),
        (2i32..=MAX, _) => format!("{} bottles", n),
        (MIN..=-1i32, _) => panic!(""),
    }
}

fn one_or_it(n: i32) -> String {
    match n {
        1 => "it".to_string(),
        _ => "one".to_string(),
    }
}

pub fn verse(n: i32) -> String {
    match n {
        0 => "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n".to_string(),
        (1i32..=99i32) => format!(
            "{} of beer on the wall, {} of beer.\nTake {} down and pass it around, {} of beer on the wall.\n", 
            pluralize_bottle(n, true), pluralize_bottle(n, false), one_or_it(n), pluralize_bottle(n-1, false)),
        _ => panic!(""),
    }
}

pub fn sing(start: i32, end: i32) -> String {
    (end..=start)
        .rev()
        .map(verse)
        .collect::<Vec<String>>()
        .join("\n")
}
