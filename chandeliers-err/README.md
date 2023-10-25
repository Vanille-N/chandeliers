# Chandeliers-Err

Error messages for the Chandeliers suite.


---

Error messages raised by the static analyzer of Chandeliers are bubbled up
to be emitted by Rustc.

This crate provides
- a unified representation of errors as `Vec<(String, Option<Span>)>`
- utilities to produce predefined error messages.

The interface of this crate is highly unstable, and the errors are tailor-made
for compilation errors encountered when compiling Lustre programs, so it is
very unlikely that any project outside of the Chandeliers suite would need
anything that this crate provides.
