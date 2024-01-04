//! Trying to use a function call in a `const` context.
//! Due to historical reasons this is a duplicate of `const/call`.
chandeliers_lus::decl! {
    const X : float = test(5);
}

fn main() {}
