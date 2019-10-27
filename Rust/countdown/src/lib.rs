use std::io::{Result as IoResult, Write};
use std::time::Duration;

// From Go Unit testing example

/// A thing that can pause for a moment
pub trait Sleeper {
    fn sleep(&self);
}

/// Configurable sleeper
/// sleep for given Duration duration
/// using function method
pub struct ConfigSleeper {
    pub duration: Duration,
    pub method: Box<dyn Fn(Duration)>,
}

impl Sleeper for ConfigSleeper {
    fn sleep(&self) {
        (self.method)(self.duration);
    }
}

/// Countdown example for TTD
pub fn countdown(out: &mut impl Write, sleeper: &mut impl Sleeper) -> IoResult<()> {
    for i in (1..=3).rev() {
        sleeper.sleep();
        writeln!(out, "{}", i)?;
    }
    sleeper.sleep();
    write!(out, "Go!")?;
    Ok(())
}
