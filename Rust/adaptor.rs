use std::iter::Iterator;
struct MyMap<I, F> {
    iter: I,
    f: F,
}

impl<I, F> MyMap<I, F> {
    fn new(iter: I, f: F) -> MyMap<I, F> {
        MyMap { iter, f }
    }
}

impl<B, I: Iterator, F> Iterator for MyMap<I, F>
where
    F: FnMut(I::Item) -> B,
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(x) => Some((self.f)(x)),
            None => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

fn main() {
    let iter = [1, 2, 3].into_iter().cycle();
    let f = |x| x * 5;
    let mappy = MyMap::new(iter.clone(), f);

    println!("Fn\n---");
    for i in mappy.take(6) {
        println!("{}", i)
    }

    let mut n: i64 = 0;
    {
        let fmut = |x| {
            n += 1;
            x + n
        };
        let mutmappy = MyMap::new(iter.clone(), fmut);
        println!("FnMut\n---");
        for i in mutmappy.take(6) {
            println!("{}", i)
        }
    }
}
