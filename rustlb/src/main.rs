#![feature(async_await, await_macro, futures_api)]

use std::io;
use std::net::Shutdown;

use futures::executor::{self, ThreadPool};
use futures::prelude::*;
use futures::task::SpawnExt;
use futures::StreamExt;

use romio::{TcpListener, TcpStream};

fn main() -> io::Result<()> {
    executor::block_on(
        async move {
            let mut threadpool = ThreadPool::new()?;

            let mut listener = TcpListener::bind(&"127.0.0.1:8080".parse().unwrap())?;
            let mut incoming = listener.incoming();

            println!("Listening on 127.0.0.1:8080");

            while let Some(stream) = await!(incoming.next()) {
                let stream = stream?;
                let addr = stream.peer_addr()?;

                let upstreams = vec![
                    String::from("127.0.0.1:7878"),
                    String::from("127.0.0.1:7879"),
                ];

                threadpool
                    .spawn(
                        async move {
                            println!("Accepting stream from: {}", addr);

                            // Panic on error
                            await!(load_balance(stream, &upstreams[0])).unwrap();

                            println!("Closing stream from: {}", addr);
                        },
                    )
                    .unwrap();
            }

            Ok(())
        },
    )
}

async fn load_balance(stream: TcpStream, upstream: &str) -> io::Result<()> {
    let upstream = await!(TcpStream::connect(&upstream.parse().unwrap()))?;
    let (mut reader, mut writer) = stream.split();
    let (mut ureader, mut uwriter) = upstream.split();

    println!("Making request to upstream");
    await!(reader.copy_into(&mut uwriter)).unwrap();

    println!("returning response");
    await!(ureader.copy_into(&mut writer)).unwrap();

    Ok(())
}
