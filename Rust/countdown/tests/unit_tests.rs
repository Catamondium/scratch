use countdown::*;
use std::cell::RefCell;
use std::time::Duration;
use std::iter::FromIterator;
use std::io::{Result as IoResult, Write};

struct SpySleeper {
    // Interior mutability lets us observe immutable calls
    pub calls: RefCell<usize>,
}

impl Sleeper for SpySleeper {
    fn sleep(&self) {
        *self.calls.borrow_mut() += 1;
    }
}

struct OperationSpy {
    pub calls: RefCell<Vec<&'static str>>,
}

impl Sleeper for OperationSpy {
    fn sleep(&self) {
        self.calls.borrow_mut().push("sleep")
    }
}

impl Write for OperationSpy {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        self.calls.borrow_mut().push("write");
        Ok(buf.len())
    }

    fn flush(&mut self) -> IoResult<()> {
        Ok(())
    }
}

// Would've been duration measuring mock
// Thinking a static/closure will do just as well
//struct SpyTime {
//    sleeping_time: Duration
//}
//
//impl Sleeper for SpyTime {
//    fn sleep(&self) {
//        
//    }
//}

#[test]
fn correct_print() {
    let mut buffer = Vec::new();
    let mut spy = SpySleeper {calls: RefCell::new(0)};

    countdown(&mut buffer, &mut spy);

    let got: String = buffer.iter().map(|&x| x as char).collect::<String>();
    let want = "3
2
1
Go!";

    assert_eq!(got, want, "Incorrect print")
}