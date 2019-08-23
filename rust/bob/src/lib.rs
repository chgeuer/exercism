extern crate regex;
use regex::Regex;

pub fn reply(message: &str) -> &str {
    let message = message.trim();

    let is_empty = message == "";
    let is_yelling = Regex::new(r"[A-Za-z]").unwrap().is_match(message)
        && message == message.clone().to_uppercase();
    let is_question = message.ends_with('?');

    match (is_empty, is_yelling, is_question) {
        (true, _, _) => "Fine. Be that way!", // you address him without actually saying anything.
        (_, true, false) => "Whoa, chill out!", // you YELL AT HIM (in all capitals).
        (_, true, true) => "Calm down, I know what I'm doing!", //  you yell a question at him.
        (_, _, true) => "Sure.",              //  ask him a question, such as "How are you?".
        (_, _, _) => "Whatever.",             // anything else.
    }
}
