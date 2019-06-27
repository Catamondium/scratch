use std::env;

fn trans_ip(ip: &str) -> String {
    ip.split('.')
        .map(|seg| match seg.parse::<u8>() {
            Ok(i) => format!("{:08b}", i),
            Err(_) => "{ERR}".to_string(),
        })
        .collect::<Vec<String>>()
        .join(".")
}

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", trans_ip(&arg));
    }
}
