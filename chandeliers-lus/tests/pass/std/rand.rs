use chandeliers_sem::*;
use rand::Rng;

const MOD: i64 = 10001;

#[derive(Default, Debug)]
struct RandomInt {
    rng: rand::rngs::ThreadRng,
}

impl RandomInt {
    fn update_mut(&mut self) -> ty!(int) {
        lit!(self.rng.gen())
    }
}

chandeliers_lus::decl! {
    extern node RandomInt() returns (r : int);
    extern const MOD : int;

    node sum(inc : int) returns (s : int);
    let
        s = (inc + (0 fby s)) % MOD;
    tel;

    node randsum() returns (r, s : int);
    let
        r = RandomInt() % MOD;
        s = sum(r);
    tel;
}

fn main() {
    let mut randsum = randsum::default();
    let mut sum = 0;
    for _ in 0..100 {
        let (r, s) = randsum.update_mut();
        sum = (sum + r.unwrap()) % MOD;
        assert_eq!(sum, s.unwrap());
    }
}
