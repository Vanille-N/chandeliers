# Chandeliers-Std

Standard library for the Chandeliers suite (Lustre-in-Rust).

---

This crates provides definitions of Lustre nodes written in the style of Candle
(see `chandeliers-sem`). These nodes usually cannot be primitively expressed
in pure Lustre.

## Provided

### Type conversions

In module `cast`:
```rs
extern node float_of_int(i : int) returns (f : float);
extern node ceil(f : float) returns (i : int);
extern node floor(f : float) returns (i : int);
```

### Randomness

In module `rand`:
```rs
extern node random_int() returns (i : int);
extern node random_float() returns (f : float);
extern node random_bool() returns (b : bool);
```

<!--
### IO

In module `io`:
```rs
extern node print_int(i : int) returns ();
extern node print_float(f : float) returns ();
extern node print_bool(b : bool) returns ();

extern node read_int() returns (i : int);
extern node read_float() returns (f : float);
extern node read_bool() returns (b : bool);
```
FIXME: string printing too
-->

