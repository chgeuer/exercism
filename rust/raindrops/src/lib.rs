pub fn raindrops(n: u32) -> String {
    let mut plingplangplong = String::with_capacity(15);

    if n % 3 == 0 { plingplangplong.push_str("Pling"); }
    if n % 5 == 0 { plingplangplong.push_str("Plang"); }
    if n % 7 == 0 { plingplangplong.push_str("Plong"); }
    
    match plingplangplong.as_str() {
        "" => n.to_string(),
        _ => plingplangplong,
    }
}