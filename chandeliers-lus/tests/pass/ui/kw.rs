//! Names that are also Rust reserved keywords, but are valid when properly
//! sanitized (`Ident::new_raw`).

use chandeliers_std::cast::float_of_int;

chandeliers_lus::decl! {
    extern node float_of_int(i : int) returns (f : float);

    node fn(unsafe : int) returns (return : float);
    var trait : bool;
    let
        trait = true fby (not trait);
        return = if trait then float_of_int(unsafe) else (float_of_int(unsafe) + 0.0 fby return);
    tel;

    const as : int = 0;
    #[export]
    const break : int = 0;
    const continue : int = 0;
    const enum : int = 0;
    const fn : int = 0;
    const for : int = 0;
    const impl : int = 0;
    const in : int = 0;
    const loop : int = 0;
    const match : int = 0;
    const mod : int = 0;
    const mut : int = 0;
    const pub : int = 0;
    const ref : int = 0;
    const return : int = 0;
    const struct : int = 0;
    const trait : int = 0;
    const type : int = 0;
    const unsafe : int = 0;
    const use : int = 0;
    const where : int = 0;
    const while : int = 0;
    const async : int = 0;
    const await : int = 0;
    const dyn : int = 0;
    const abstract : int = 0;
    const become : int = 0;
    const box : int = 0;
    const do : int = 0;
    const final : int = 0;
    const macro : int = 0;
    const override : int = 0;
    const priv : int = 0;
    const typeof : int = 0;
    const unsized : int = 0;
    const virtual : int = 0;
    const yield : int = 0;
}

const TEST: i64 = r#break;

fn main() {}

// Missing from the above list from
// https://doc.rust-lang.org/reference/keywords.html#strict-keywords:
// # Reserved by Lustre
//   -> const, else, extern, false, else, if, let, true
// # Cannot be raw identifiers
//   -> crate, self, Self, super
// # Break the parser
//   -> move, static
