#![allow(unused_imports)]

mod vis {
    chandeliers_lus::decl! {
        node private() returns ();
        let tel;

        const PRIVATE: int = 0;

        #[export]
        node public() returns ();
        let tel;

        #[export]
        const PUBLIC : int = 0;
    }
}

use vis::private;
use vis::PRIVATE;

use vis::public;
use vis::PUBLIC;

fn main() {}
