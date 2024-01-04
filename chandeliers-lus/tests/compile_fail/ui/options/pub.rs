//! Not enough visibility on `PRIVATE` and `private`.
//! Need `pub` for them to escape their module.
#![allow(unused_imports)]

mod vis {
    chandeliers_lus::decl! {
        #[export]
        node private() returns ();
        let tel;

        #[export]
        const PRIVATE: int = 0;

        #[pub]
        node public() returns ();
        let tel;

        #[pub]
        const PUBLIC : int = 0;
    }
}

use vis::private;
use vis::PRIVATE;

use vis::public;
use vis::PUBLIC;

fn main() {}
