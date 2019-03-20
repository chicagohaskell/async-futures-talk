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

struct CopyFuture<P> {
    state: CopyState<P>,
    out: Stdout,
}

impl<P> CopyFuture<P> {
    fn new(state: CopyState<P>, out: Stdout) -> Self {
        Self { state, out }
    }
}

impl<P> Future for CopyFuture<P> {
    type Item = ();
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        use self::CopyState::*;

        // we loop because ?
        loop {
            match self.state {
                Opening(mut f) => {
                    let file = try_ready!(f.poll());
                    self.state = Parsing(file.metadata());
                }
                Parsing(mut m) => {
                    let (file, metadata) = try_ready!(m.poll());
                    self.state = Copying(io::copy(file, self.out), metadata.len());
                }
                Copying(mut c, size) => {
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
