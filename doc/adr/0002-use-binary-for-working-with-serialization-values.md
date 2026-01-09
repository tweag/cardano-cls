# 2. Use binary for working with serialization values

Date: 2026-01-09

## Status

Accepted

Amended by [3. Move to raw mempack use instead of binary](0003-move-to-raw-mempack-use-instead-of-binary.md)

## Context

In haskell ecosystem there are many existing libraries that can be used for working with binary files.
Parser libraries: parsec, attoparsec, megaparsec; generic libraries: binary, cereal; custom format serializers: serialise, store. 
We need to make a good choice that will allow us to have fast and efficient serializer, without reimplementation.

## Decision

We will use `binary` library as a wrapper for the `bytestring` access.

We find it not the best choice, while binary library is quite nice it may not work best for our scenario because it's optimized for streaming writing and reading when we do not know the size of data in advance. However the file format is optimized and structured in the way that we know either exact sizes or the boundaries. As a result we may work with more efficient structures that removes some checks.

## Consequences

We implemented and reused a lot of the existing functionality that was required for reading and writing binary format.

