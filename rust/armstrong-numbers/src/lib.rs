pub fn is_armstrong_number(num: u32) -> bool {
    let string = format!("{}", num);
    let exponent = string.len() as u32;
    let armstrong = string
        .chars()
        .map(|x| x.to_digit(10).unwrap())
        .map(|x| x.pow(exponent))
        .fold(0, |acc, x| acc + x);

    num == armstrong
}