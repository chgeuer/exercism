pub fn raindrops(n: u32) -> String {
    let div_by = |x| n % x == 0;
    let n2str = |x, v| if div_by(x) { v } else { "" };

    let plingplangplong = &[ 
        n2str(3, "Pling"), 
        n2str(5, "Plang"), 
        n2str(7, "Plong"),
    ];
    let plingplangplong = plingplangplong.join("");

    match plingplangplong.as_str() {
        "" => n.to_string(),
        _ => plingplangplong,
    }
}