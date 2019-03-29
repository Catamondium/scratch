#[allow(dead_code)]
#[allow(unused_variables)]

type Flt = f32; // f32 alias
struct Pvec(f32, f32); // tuple struct
struct Time { // normal struct
    hrs: u32,
    mins: u32
}

fn func(t: &mut (u32, u32)) // takes mutable reference
-> (u32, u32){ // returns tuple
    t.0 += 1; // increment 0th component
    *t // dereference
}

fn main() {
    let mut x = (5, 4); // tuples
    func(&mut x);
    let (y, z) = x; // destructuring
    println!("{} {}", y, z);
}
