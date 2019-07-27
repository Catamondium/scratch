use std::env;
use std::io::{BufRead, BufReader, Result as ioResult};
use std::net::TcpStream;

fn main() -> ioResult<()> {
    let port = env::args().nth(1).unwrap_or("11235".to_string());

    let stream = TcpStream::connect(&format!("localhost:{}", port))?;
    let reader = BufReader::new(stream);
    for line in reader.lines().filter_map(|x| x.ok()) {
        println!("{}", line);
    }

    Ok(())
}