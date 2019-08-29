pub fn factors(n: u64) -> Vec<u64> {
    // actual algorithm from https://www.geeksforgeeks.org/print-all-prime-factors-of-a-given-number/
    
    let mut n = n;
    let mut result = Vec::new();

    while n % 2 == 0 {
        result.push(2);
        n = n / 2;
    }

    for i in (3..=(n as f64).sqrt() as u64).step_by(2) {
        while n % i == 0 {
            result.push(i);
            n = n / i;
        }
    }

    if n > 2 {
        result.push(n);
    }

    result
}
