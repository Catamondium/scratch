#[allow(dead_code)]
#[allow(unused_variables)]

type Flt = f32; // f32 alias
struct Pvec(f32, f32); // tuple struct
struct Time { // normal struct
    hrs: u32,
    mins: u32
}

impl Time { // repeatable block, has Time method decls
    fn abs(&self) -> u32 { // explicit self
        let ret = (self.hrs * 60) + self.mins;
        ret
    }
}
use std::fmt; // namespace
// Trait implementation for printing
impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write formatted into buffer
        write!(f, "{:02}:{:02}", self.hrs, self.mins)
    }
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

    let start = Time {hrs: 1, mins: 30};
    // Time supports std::fmt::Display trait
    println!("{}", start);
}
