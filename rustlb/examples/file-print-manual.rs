use std::path::Path;

use tokio::fs::{file::MetadataFuture, file::OpenFuture, File};
use tokio::io::{self, Copy, Stdout};
use tokio::prelude::*;

use futures::try_ready;

/// The current state of our future
enum CopyState<P> {
    Opening(OpenFuture<P>),
    Parsing(MetadataFuture),
    Copying(Copy<File, Stdout>, u64),
}

struct CopyFuture<P>
where
    P: AsRef<Path> + Send + 'static,
{
    state: CopyState<P>,
    out: Stdout,
}

impl<P> CopyFuture<P>
where
    P: AsRef<Path> + Send + 'static,
{
    fn new(state: CopyState<P>, out: Stdout) -> Self {
        Self { state, out }
    }
}

impl<P> Future for CopyFuture<P>
where
    P: AsRef<Path> + Send + 'static,
{
    type Item = ();
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        use self::CopyState::*;

        // we loop because we can immediately start polling the next state, instead of being
        // forgotten?
        loop {
            match self.state {
                Opening(ref mut o) => match o.poll() {
                    Ok(Async::Ready(file)) => {
                        self.state = Parsing(file.metadata());
                    }
                    Ok(Async::NotReady) => return Ok(Async::NotReady),
                    Err(e) => return Err(e),
                },
                Parsing(ref mut m) => {
                    let (file, metadata) = try_ready!(m.poll());
                    self.state = Copying(io::copy(file, io::stdout()), metadata.len());
                }
                Copying(ref mut c, size) => {
                    let mut n = 0;

                    while n <= size {
                        let result = try_ready!(c.poll());
                        n = result.0;
                    }

                    return Ok(Async::Ready(()));
                }
            }
        }
    }
}

fn main() {
    let copy = CopyFuture::new(
        CopyState::Opening(File::open("/Users/bigb/vimwiki/index.md")),
        io::stdout(),
    );

    tokio::run(copy.map_err(|err| println!("IO error: {:?}", err)));
}
