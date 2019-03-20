use tokio::io::{copy, stdout};
use tokio::prelude::{future::lazy, Future};

fn main() {
    tokio::run(lazy(|| {
        tokio::spawn(
            tokio::fs::File::open("/Users/bigb/vimwiki/index.md")
                .and_then(|file| {
                    // do something with the file ...
                    copy(file, stdout())
                })
                .and_then(|(n, _, _)| {
                    println!("Printed {} bytes", n);
                    Ok(())
                })
                .map_err(|e| {
                    // handle errors
                    eprintln!("IO error: {:?}", e);
                }),
        );

        Ok(())
    }));
}
