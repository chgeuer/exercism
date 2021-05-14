use rand::Rng;

pub fn private_key(p: u64) -> u64 {
    let mut rng = rand::thread_rng();
    rng.gen_range(2..p)
}

use num_bigint::{BigInt};

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    let p_bigint = BigInt::from(p);
    let g_bigint = BigInt::from(g);
    let a_bigint = BigInt::from(a);
    let pk = g_bigint.modpow(&a_bigint, &p_bigint);
    let (sign, digits) = pk.to_u64_digits();

    assert_eq!(sign, num_bigint::Sign::Plus);
    assert_eq!(digits.len(), 1);

    digits[0]
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    public_key(p, b_pub, a)
}
