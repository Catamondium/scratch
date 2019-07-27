use std::env;
use std::io::{LineWriter, Result as ioResult, Write};
use std::net::TcpListener;
fn main() -> ioResult<()> {
    let port = env::args().nth(1).unwrap_or("11235".to_string());
    let server = TcpListener::bind(&format!("localhost:{}", port))?;

    for socket in server.incoming().filter_map(|x| x.ok()) {
        let mut writer = LineWriter::new(socket);
        writeln!(writer, "Hello world")?;
    }

    Ok(())
}