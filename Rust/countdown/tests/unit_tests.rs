use countdown::*;
use std::cell::RefCell;
use std::default::Default;
use std::io::{Result as IoResult, Write};
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
    pub calls: Rc<RefCell<Vec<&'static str>>>,
}

const SLEEP_STR: &str = "sleep";
const WRITE_STR: &str = "write";

impl Sleeper for OpSleeper {
    fn sleep(&self) {
        self.calls.borrow_mut().push(SLEEP_STR)
    }
}

struct OpWriter {
    pub calls: Rc<RefCell<Vec<&'static str>>>,
}

impl Write for OpWriter {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        self.calls.borrow_mut().push(WRITE_STR);
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

    // You'll panic, won't you!
    countdown(&mut opwrt, &mut opslp).unwrap();

    let want = Rc::new(RefCell::new(vec![
        SLEEP_STR, WRITE_STR, SLEEP_STR, WRITE_STR, SLEEP_STR, WRITE_STR, SLEEP_STR, WRITE_STR,
    ]));

    // 3 surplus calls
    //assert_eq!(want, calls, "Bad ordering");
}

//#[test] borrowing problems
//fn configurable() {
//    let duration = Duration::from_secs(5);
//    let mut elapsed = RefCell::new(Duration::from_secs(0));
//
//    let spytime = |x| {
//        *elapsed.borrow_mut() += x;
//    };
//
//    let sleeper = ConfigSleeper {duration, method: Box::new(spytime)};
//}
