pub fn collatz(n: u64) -> Option<u64> {
    compute_collatz(n, 0)
}

fn compute_collatz(n: u64, count: u64) -> Option<u64> {
    match n {
        0 => None,
        1 => Some(count),
        n => match n % 2 {
            0 => compute_collatz(n / 2, count + 1),
            1 => compute_collatz(3 * n + 1, count + 1),
            _ => unreachable!(),
        },
    }
}
