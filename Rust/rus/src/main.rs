use std::fmt; // namespace

type Flt = f32; // f32 alias
struct Pvec(Flt, f32); // tuple struct
struct Time {
    // normal struct
    hrs: u32,
    mins: u32,
}

impl Time {
    // repeatable block, has Time method decls
    fn abs(&self) -> u32 {
        // explicit self
        let ret = (self.hrs * 60) + self.mins;
        ret
    }
}

// Trait implementation for printing
impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write formatted into buffer
        write!(f, "{:02}:{:02}", self.hrs, self.mins)
    }
}

fn func(t: &mut (u32, u32)) -> (u32, u32) {
    // returns tuple
    t.0 += 1; // increment 0th component
    *t // dereference
}

fn slicing() {
    // slice => ranged reference into thingy
    // does act like a borrow according to it's reference tag
    fn getword(s: &String) -> &str {
        let bytes = s.as_bytes();

        // enumerate(), iterate w/ index & element
        for (i, &item) in bytes.iter().enumerate() {
            if item == b' ' {
                return &s[0..i]; // not inclusive of i
            }
        }
        &s[..] // whole thing
    }

    let string = String::from("first second third");
    let word = getword(&string);
    //string.clear(); // illegal under borrowing
    println!("{}", word);
}

fn flowctrl() {
    let var = true; // immutable boolean, init required
                    // flowctrl are expressions, having values
    let n = if var { 50 } else { 0 };
    println!("n: {}", n);

    let mut i = 0;
    // infinite until break, break can also return
    // break may also use labels in nesting
    loop {
        print!("*");
        i += 1;
        if i > 5 {
            print!("\n");
            break;
        }
    }

    while i > 0 {
        // unparenthisised condition
        print!("^");
        i -= 1;
    }
    println!("\ni == {}", i);

    let arr = [1, 2, 3, 4]; // array
    for e in arr.iter() {
        // for x:agg, iterator pattern
        println!("Val is: {}", e);
    }

    for n in (0..=6).rev() {
        // range, `..=` includes 6
        println!("Ranged: {}", n);
    }
}

fn main() {
    let mut x = (5, 4); // tuples
    func(&mut x);
    let (y, z) = x; // destructuring
    println!("{} {}", y, z);
    //let r = x; // move, x is invalid now
    //println!("{:#?}", x); // compile error

    let Pvec(h, v) = Pvec(2.5, 4.3); // Tuple-struct destructuring
    println!("Pvec({}, {})", h, v);

    let start = Time { hrs: 1, mins: 30 };
    // Time supports std::fmt::Display trait
    println!("{} -> {}mins", start, start.abs());

    let hour_up = Time {
        hrs: start.hrs + 1,
        ..start
    }; // change hrs+1, copy rest from start
    println!("One hour later... {}", hour_up);

    let Time {
        hrs: first,
        mins: last,
    } = hour_up; // Struct destructuring
    println!("first: {}, last: {}", first, last);

    flowctrl();
    slicing();
}
