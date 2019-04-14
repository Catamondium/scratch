macro_rules! Callsig {
   (fn $name:ident($($params:ident: $types:ty),*)  -> $ret:ty $body:block) => {
           fn $name($($params: $types),*) -> $ret {
               let params = ($($params),*);
               let ret: $ret = $body;
               println!("{}{:?} -> {:?}", stringify!($name), params, ret);
               ret
               }
   };
   
   (fn $name:ident($($params:ident: $types:ty),*) $body:block) => {
       Callsig! {
           fn $name($($params: $types),*) -> () $body
       }
   };
}

Callsig! {
    fn add(lhs: i32, rhs: i32) -> i32 {
        lhs + rhs
    }
}

Callsig! {
    fn call() {
        println!("{}", 55);
    }
}


fn main() {
    add(1, 2);
    call();
}