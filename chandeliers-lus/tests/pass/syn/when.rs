/*
chandeliers_lus::decl! {
    node dep_input(b : bool; x: int when b; y: int whenot b) returns (m : int);
    let
        m = merge b x y;
    tel;

    node dep_output() returns (b : bool; o : int when b);
    let
        b = true fby not b;
        o = (1 fby o + 1) when b;
    tel;

    node dep_mixed(b : bool) returns (o : int whenot b);
    let
        o = -1 whenot b;
    tel;

    node foo() returns (m : int);
    var b : bool; o1 : int when b; o2: int whenot b;
    let
        (b, o1) = dep_output();
        o2 = dep_mixed(b);
        m = merge b o1 o2;
    tel;
}
*/

fn main() {}
