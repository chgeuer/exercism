pub fn nth(n: u32) -> u32 {
    let mut primes = vec![2u32];

    if primes.len() <= (n as usize) {
        for _ in (primes.len() as u32)..(n + 1) {
            let last_found_prime = primes[primes.len() - 1];
            for candidate_prime in last_found_prime.. {
                let dividable = |&existing_prime| candidate_prime % existing_prime == 0;
                let found_divisor = primes.iter().any(dividable);
                if !found_divisor {
                    primes.push(candidate_prime);
                    break;
                }
            }
        }
    }

    primes[n as usize]
}
