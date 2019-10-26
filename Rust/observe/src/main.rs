#![feature(weak_ptr_eq, weak_counts)]

use std::cell::RefCell;
use std::error::Error;
use std::rc::{Rc, Weak};

type ShMut<T> = Rc<RefCell<T>>;
type WkMut<T> = Weak<RefCell<T>>;

trait Observer {
    // or subscriber
    fn update(&mut self);
}

trait Subject {
    // or publisher
    fn attach(&mut self, ob: &ShMut<dyn Observer>);
    fn detach(&mut self, ob: &ShMut<dyn Observer>);
    fn notify(&self);
}

struct Button(i64, Vec<WkMut<dyn Observer>>);

impl Button {
    fn new() -> Self {
        Button(0, Vec::new())
    }

    fn press(&mut self) {
        self.0 += 1;
        self.notify();
    }

    fn value(&self) -> &i64 {
        &self.0
    }

    fn watching(&self) -> usize {
        self.1.len()
    }
}

impl Subject for Button {
    fn attach(&mut self, ob: &ShMut<dyn Observer>) {
        let ptr = Rc::downgrade(ob);
        self.1.push(ptr);
    }

    fn detach(&mut self, ob: &ShMut<dyn Observer>) {
        let ptr = Rc::downgrade(ob);
        let v = &mut self.1;

        v.retain(|item| !item.ptr_eq(&ptr) && item.upgrade().is_some())
    }

    fn notify(&self) {
        for ptr in self.1.iter() {
            if let Some(ob) = ptr.upgrade() {
                ob.borrow_mut().update();
            };
        }
    }
}

struct Submitter(i64);

impl Submitter {
    fn new() -> Self {
        Submitter(0)
    }

    fn value(&self) -> &i64 {
        &self.0
    }
}

impl Observer for Submitter {
    fn update(&mut self) {
        self.0 += 1;
        println!("S {:p}\tN: {}", self, self.0)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut butt = Button::new();

    let mut observers = Vec::new();
    const P_GRP: usize = 5;
    for _ in 0..P_GRP {
        let ptr: ShMut<dyn Observer> = Rc::new(RefCell::new(Submitter::new()));
        butt.attach(&ptr);
        observers.push(ptr);
    }

    let test: ShMut<dyn Observer> = Rc::new(RefCell::new(Submitter::new()));
    let wkptr = Rc::downgrade(&test);
    butt.attach(&test);
    butt.press();

    {
        let ptr: ShMut<dyn Observer> = Rc::new(RefCell::new(Submitter::new()));
        println!("Scoped: {:p}", &Rc::downgrade(&ptr));
        butt.attach(&ptr);
        butt.press();
    }

    // Scoped & test still referenciable
    assert_eq!(butt.watching(), P_GRP + 2);

    if let Some(rcptr) = wkptr.upgrade() {
        butt.detach(&rcptr);
    }

    // Scoped & test eliminated
    assert_eq!(butt.watching(), P_GRP);

    butt.press();
    println!("B Presses: {}", butt.value());

    Ok(())
}
