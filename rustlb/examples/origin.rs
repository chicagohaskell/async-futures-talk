#![feature(async_await, await_macro, futures_api)]

use std::io;

use tokio::net::{TcpListener, TcpStream};
use tokio::prelude::*;

use rand::seq::SliceRandom;

const SHAKESPEARE: &[&[u8]] = &[
    b"Now is the winter of our discontent\nMade glorious summer by this sun of York.\n",
    b"Some are born great, some achieve greatness\nAnd some have greatness thrust upon them.\n",
    b"Friends, Romans, countrymen - lend me your ears!\nI come not to praise Caesar, but to bury him.\n",
    b"The evil that men do lives after them\nThe good is oft interred with their bones.\n",
    b"                  It is a tale\nTold by an idiot, full of sound and fury\nSignifying nothing.\n",
    b"Ay me! For aught that I could ever read,\nCould ever hear by tale or history,\nThe course of true love never did run smooth.\n",
    b"I have full cause of weeping, but this heart\nShall break into a hundred thousand flaws,\nOr ere I'll weep.-O Fool, I shall go mad!\n",
    b"                  Each your doing,\nSo singular in each particular,\nCrowns what you are doing in the present deed,\nThat all your acts are queens.\n",
];

fn main() {
    let listener = TcpListener::bind(&"127.0.0.1:7878".parse().unwrap()).unwrap();

    tokio::run_async(
        async {
            let mut incoming = listener.incoming();

            println!("Listening on 127.0.0.1:7878");

            while let Some(stream) = await!(incoming.next()) {
                let stream = stream.unwrap();
                let addr = stream.peer_addr().unwrap();

                println!("Accepting stream from: {}", addr);

                // Panic on error
                await!(recite_shakespeare(stream)).unwrap();

                println!("Closing stream from: {}", addr);
            }
        },
    );
}

async fn recite_shakespeare(mut stream: TcpStream) -> io::Result<()> {

    let &quote = SHAKESPEARE.choose(&mut rand::thread_rng()).unwrap();

    let mut buf = Vec::new();

    await!(stream.read_async(&mut buf)).unwrap();

    println!("Read bytes: {:?}", String::from_utf8(buf).unwrap());

    // Construct the response
    let mut response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length:{}\r\nContent-Type: text/ascii\r\n\r\n",
        quote.len()
    )
    .into_bytes();

    response.extend_from_slice(quote);

    await!(stream.write_all_async(&response)).unwrap();

    Ok(())
}
