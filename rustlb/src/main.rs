
#![feature(async_await, await_macro, futures_api)]

use std::io;

use futures::StreamExt;
use futures::executor::{self, ThreadPool};
use futures::prelude::*;
use futures::task::{SpawnExt};

use romio::{TcpListener, TcpStream};

fn main() -> io::Result<()> {

    executor::block_on(async move {
        let mut threadpool = ThreadPool::new()?;

        let mut listener = TcpListener::bind(&"127.0.0.1:8080".parse().unwrap())?;
        let mut incoming = listener.incoming();

        println!("Listening on 127.0.0.1:8080");

        while let Some(stream) = await!(incoming.next()) {
            let stream = stream?;
            let addr = stream.peer_addr()?;

            let upstreams = vec![String::from("127.0.0.1:7878"), String::from("127.0.0.1:7879")];

            threadpool.spawn(async move {
                println!("Accepting stream from: {}", addr);

                // Panic on error
                await!(load_balance(stream, &upstreams[0])).unwrap();

                println!("Closing stream from: {}", addr);
            }).unwrap();
        }

        Ok(())
    })
}

async fn load_balance(mut stream: TcpStream, upstream: &str) -> io::Result<()> {
    let mut upstream = await!(TcpStream::connect(&upstream.parse().unwrap()))?;
    let mut buf = Vec::with_capacity(100);

    println!("Making requst");

    // Read into buffer
    let read = await!(stream.read_exact(&mut buf))?;

    println!("Got request: {:?}, {:?}", read, buf);

    // Make request
    await!(upstream.write_all(&buf))?;

    // Clear the buffer
    buf.clear();

    // Read the response from upstream
    let read = await!(upstream.read_to_end(&mut buf))?;

    println!("Got response: {:?} {:?}", read, buf);

    // write response back
    await!(stream.write_all(&buf))?;
    Ok(())
}
