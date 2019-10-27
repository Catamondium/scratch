use std::io::{Result as IoResult, Write};
use std::thread::sleep;
use std::time::Duration;

// From Go Unit testing example

pub trait Sleeper {
    fn sleep(&self);
}

pub struct ConfigSleeper {
    pub duration: Duration,
    pub method: Box<dyn Fn(Duration)>,
}

impl Sleeper for ConfigSleeper {
    fn sleep(&self) {
        (self.method)(self.duration);
    }
}

pub fn countdown(out: &mut impl Write, sleeper: &mut impl Sleeper) -> IoResult<()> {
    for i in (1..=3).rev() {
        sleeper.sleep();
        writeln!(out, "{}", i)?;
    }
    sleeper.sleep();
    write!(out, "Go!")?;
    Ok(())
}
