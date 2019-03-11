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

---

### Futures generally

Concisely talk about some future values

---

## Compare architecture

- futures-rs is an abstraction, tokio is backend to futures-rs
- async uses STM for coordination
- execution model

---

### futures-rs vs async generally

futures are an abstraction over things that need to be polled, tokio is
a backend async is more all inclusive, based on STM

---

### How things are executed

executors in rust STM in Haskell

---

### macros vs heavy runtime

futures-rs uses macros, and async relies on Haskell's STM runtime.

---

## Examples

- API comparison
- Socket server
- HTTP client
- File IO

---

### API comparison

```haskell
Async a
instance Functor Async

async  :: IO a -> IO (Async a)
wait   :: Async a -> IO a
poll   :: Async a -> IO (Maybe (Either SomeException a))
```

Use `await!` macro to wait on `Future`.

```rust
pub enum Poll<T> {
    Ready(T),
    Pending,
}
pub trait Future {
    type Output;
    fn poll(self: Pin<&mut Self>, waker: &Waker) -> Poll<Self::Output>;
}
```

---

map concurrently:

```haskell
mapConcurrently :: Traversible t
                => (a -> IO b) -> t a -> IO (t b)
```

---

### File web server

TODO full example

---

### Web crawler

```haskell
main = mapConcurrently get urls >>= concat >>= print
```

---

## Summary

High level differences
