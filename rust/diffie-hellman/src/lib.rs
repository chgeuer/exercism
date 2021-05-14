use rand::Rng;

pub fn private_key(p: u64) -> u64 {
    rand::thread_rng().gen_range(2..p)
}

use num_bigint::BigInt;

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    modpow(p, g, a)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    modpow(p, b_pub, a)
}

fn modpow(p: u64, g: u64, a: u64) -> u64 {
    let (sign, digits) = BigInt::from(g)
        .modpow(&BigInt::from(a), &BigInt::from(p))
        .to_u64_digits();

    assert_ne!(sign, num_bigint::Sign::Minus);
    assert_eq!(digits.len(), 1);

    digits[0]
}
