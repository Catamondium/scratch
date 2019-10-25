#![feature(weak_ptr_eq, weak_counts)]

use std::error::Error;
use std::rc::{Rc, Weak};

trait Observer {
    // or subscriber
    fn update(&mut self);
}

trait Subject {
    // or publisher
    fn attach(&mut self, ob: &Rc<dyn Observer>);
    fn detach(&mut self, ob: &Rc<dyn Observer>);
    fn notify(&self);
}

struct Button(i64, Vec<Weak<dyn Observer>>);

impl Button {
    fn new() -> Self {
        Button(0, Vec::new())
    }

    fn press(&mut self) {
        self.0 += 1;
    }

    fn value(&self) -> i64 {
        self.0
    }
}

impl Subject for Button {
    fn attach(&mut self, ob: &Rc<dyn Observer>) {
        let ptr = Rc::downgrade(ob);
        self.1.push(ptr);
    }

    fn detach(&mut self, ob: &Rc<dyn Observer>) {
        let ptr = Rc::downgrade(ob);
        let mut v = self.1;

        v.retain(|&item| item.ptr_eq(&ptr) && item.upgrade().is_some())
    }

    fn notify(&self) {
        for ptr in self.1 {
            if let Some(ob) = ptr.upgrade() {
                ob.update();
            };
        }
    }
}

struct Submitter(i64);

impl Submitter {
    fn new() -> Self {
        Submitter(0)
    }

    fn value(&self) -> i64 {
        self.0
    }
}

impl Observer for Submitter {
    fn update(&mut self) {
        self.0 += 1;
        println!("Submitter:\t@{:p}\nPresses: {}", self, self.0)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut butt = Button::new();

    let observers = Vec::new();
    for _ in 0..5 {
        let ptr: Rc<dyn Observer> = Rc::new(Submitter::new());
        observers.push(ptr);
        butt.attach(&ptr);
    }
    butt.press();

    {
        let ptr: Rc<dyn Observer> = Rc::new(Submitter::new());
        println!("Scoped: @{:p}", ptr);
        butt.attach(&ptr);
        butt.press();
    }

    butt.press();

    Ok(())
}
