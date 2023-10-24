use chandeliers_sem::traits::*;
use chandeliers_sem::*;
use rand::Rng;

const MOD: i64 = 10001;

#[derive(Default, Debug)]
struct RandomInt {
    rng: rand::rngs::ThreadRng,
}

impl Step for RandomInt {
    type Input = ();
    type Output = i64;
    fn step(&mut self, _: ()) -> ty!(int) {
        self.rng.gen::<i64>().embed()
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
        let (r, s) = randsum.step(()).trusted();
        sum = (sum + r) % MOD;
        assert_eq!(sum, s);
    }
}
