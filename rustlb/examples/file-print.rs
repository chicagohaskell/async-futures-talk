use tokio::io::{copy, stdout};
use tokio::prelude::Future;

fn main() {
    let task = tokio::fs::File::open("/Users/bigb/vimwiki/index.md")
        .and_then(|file| {
            // do something with the file ...
            let task = copy(file, stdout())
                .map(|(n, _, _)| {
                    println!("Copied {} bytes", n);
                })
                .map_err(|err| println!("IO error: {:?}", err));

            tokio::spawn(task);
            Ok(())
        })
        .map_err(|e| {
            // handle errors
            eprintln!("IO error: {:?}", e);
        });
    tokio::run(task);
}
