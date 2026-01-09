# 3. Move to raw mempack use instead of binary

Date: 2026-01-09

## Status

Accepted

amends [2. Use binary for working with serialization values](0002-use-binary-for-working-with-serialization-values.md)

## Context

Binary package requires additional constraints to be kept in the codebase and increases footprint.

Binary package suits well for incremental reading of the infinite streams of data.

Current format is structured as a chunked format and works best if chunk is loaded directly into the memory as a single structure.

`mempack` library is imlemented to support this scenario well.

We already use and define `mempack` instances.

Instances what were missing in `mempack` were already implemented in `mempack-scls` package

## Decision

We will drop uses of `binary` library and use `mempack` alone

## Consequences

The codebase becomes simpler and more efficient.

We had to implement additional wrappers to make MemPack fast, because mempack relied on the fact that size calculation is fast, but it was not a case in some scenarios.
