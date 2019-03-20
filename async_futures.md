---
author: Bhargav Voleti & Matthew Wraith
title: Futures in Haskell and Rust
date: March 27th, 2019
theme: solarized
---


## Introduction to Haskell and Rust generally

Goals of the languages:

- Rust, zero cost abstractions and memory safety
- Haskell, correctness and expressivity


Main differences:

- Rust cares a lot about performance, so the type system tells you where the values live in the machine (stack vs heap)
- Haskell doesn't necessarily care about performance


### Futures generally

- Concisely talk about some future values
- More often than not, you're streaming stuff



## Compare architecture

- futures is an abstraction, tokio is backend to futures
- async uses STM for coordination
- execution model

### futures-rs vs async generally

futures are an abstraction over things that need to be polled, tokio is
a backend async is more all inclusive, based on STM

Look at this section: [A closer look at futures](https://tokio.rs/docs/getting-started/futures/)



### How things are executed

executors in rust STM in Haskell



### macros vs heavy runtime

futures-rs uses macros, and async relies on Haskell's STM runtime.



## Examples

- API comparison
- Reading file pipe to stdout
- Concurrent HTTP client to single file
- Chat server


### API comparison

- Streams
- combinators over futures, streams
- and_then
- concurrency

```haskell
Async a
instance Functor Async

async  :: IO a -> IO (Async a)
wait   :: Async a -> IO a
poll   :: Async a -> IO (Maybe (Either SomeException a))
```

```haskell
-- TODO io-streams interface
```

Use `await!` macro to wait on `Future`.

```rust
pub enum Poll<T> {
    Ready(T),
    Pending,
}
pub trait Future {
    type Item;
    type Error;
    fn poll(&mut self) -> Result<Self::Item, Self::Error>;
}
```



map concurrently:

```haskell
mapConcurrently :: Traversible t
                => (a -> IO b) -> t a -> IO (t b)
```



### Web crawler

```haskell
main = mapConcurrently get urls >>= concat >>= print
```

### File web server

TODO full example




## Summary

High level differences
