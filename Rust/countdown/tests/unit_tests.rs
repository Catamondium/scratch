use countdown::*;
use std::cell::RefCell;
use std::default::Default;
use std::io::{sink, Result as IoResult, Write};
use std::rc::Rc;
use std::time::Duration;

struct SpySleeper {
    // Interior mutability lets us observe immutable calls
    pub calls: RefCell<usize>,
}

impl Sleeper for SpySleeper {
    fn sleep(&self) {
        *self.calls.borrow_mut() += 1;
    }
}

struct OpSleeper {
    pub calls: Rc<RefCell<Vec<Operation>>>,
}

#[derive(Debug, Eq, PartialEq)]
enum Operation {
    Sleep,
    Write,
}

impl Sleeper for OpSleeper {
    fn sleep(&self) {
        self.calls.borrow_mut().push(Operation::Sleep)
    }
}

struct OpWriter {
    pub calls: Rc<RefCell<Vec<Operation>>>,
}

impl Write for OpWriter {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        self.calls.borrow_mut().push(Operation::Write);
        Ok(buf.len())
    }

    fn flush(&mut self) -> IoResult<()> {
        Ok(())
    }
}

#[test]
fn printing() {
    let mut buffer = Vec::new();
    let mut spy = SpySleeper {
        calls: Default::default(),
    };

    countdown(&mut buffer, &mut spy).unwrap();

    let got: String = buffer.iter().map(|&x| x as char).collect::<String>();
    let want = "3
2
1
Go!";

    assert_eq!(got, want, "Incorrect print");

    assert_eq!(spy.calls.into_inner(), 4, "Too few sleeps");
}

#[test]
fn ordering() {
    let calls = Rc::new(RefCell::new(Vec::new()));

    let mut opslp = OpSleeper {
        calls: calls.clone(),
    };

    let mut opwrt = OpWriter {
        calls: calls.clone(),
    };

    countdown(&mut opwrt, &mut opslp).unwrap();

    let want = Rc::new(RefCell::new(vec![
        Operation::Sleep,
        Operation::Write,
        Operation::Write,
        Operation::Sleep,
        Operation::Write,
        Operation::Write,
        Operation::Sleep,
        Operation::Write,
        Operation::Write,
        Operation::Sleep,
        Operation::Write,
    ]));

    assert_eq!(want, calls, "Bad ordering");
}

static mut ELAPSED: Duration = Duration::from_secs(0);

fn spytime(x: Duration) {
    unsafe {
        ELAPSED += x;
    }
}

#[test]
fn configurable() {
    let duration = Duration::from_secs(1);

    let mut sleeper = ConfigSleeper {
        duration,
        method: Box::new(spytime),
    };

    countdown(&mut sink(), &mut sleeper).unwrap();

    unsafe {
        assert_eq!(Duration::from_secs(4), ELAPSED, "Bad timing");
    }
}
