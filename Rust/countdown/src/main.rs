use countdown::*;
use std::io::{stdout, Result as IoResult};
use std::thread::sleep;
use std::time::Duration;

fn main() -> IoResult<()> {
    let mut sleeper = ConfigSleeper {
        duration: Duration::from_secs(1),
        method: Box::new(sleep),
    };

    countdown(&mut stdout(), &mut sleeper)?;

    Ok(())
}
