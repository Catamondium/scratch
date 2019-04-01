use std::collections::HashMap;

macro_rules! myvec {
    ($($e:expr),*) => {{
        let mut temp = Vec::new();
        $(
            temp.push($e);
        )*
        temp
    }};
}

macro_rules! mymap {
    ($($l:expr => $r:expr),*) => {{
        let mut temp = HashMap::new();
        $(
            temp.insert(stringify!($l), $r);
        )*
        temp
    }};
}

fn main() {
    let v = myvec![1, 2, 3, 4];
    println!("{:?}", v);

    let m = mymap!{
        a => 1,
        b => 2,
        c => 3
    };

    println!("{:#?}", m);
}