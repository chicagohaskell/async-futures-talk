# Async vs Futures Talk

## Introduction to Haskell and Rust generally

Goals of the languages:

  - Rust, zero cost abstractions and memory safety
  - Haskell, correctness and expressivity

## Futures generally

Concisely talk about some future values

## Compare architecture

### futures-rs vs async generally

futures are an abstraction over things that need to be polled, tokio is
a backend async is more all inclusive, based on STM

### How things are executed

executors in rust STM in Haskell

### macros vs heavy runtime

## Examples

### API comparison

1.  map concurrently

    ``` haskell
    mapConcurrently :: Traversible t => (a -> IO b) -> t a -> IO (t b)
    ```

2.  race

3.  errors

### Socket server

### HTTP client fetches

### File IO

## Summary

High level differences
