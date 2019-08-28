pub fn build_proverb(list: &[&str]) -> String {
    if list.len() == 0 {
        return String::new();
    }

    let mut verses = (0..list.len() - 1)
        .map(|i| format!("For want of a {} the {} was lost.", list[i], list[i + 1]))
        .collect::<Vec<String>>();
    verses.push(format!("And all for the want of a {}.", list[0]));
    verses.join("\n")
}
