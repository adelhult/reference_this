# reference_this

`reference_this` is a simple tool
that reads a folder of files (mainly .gleam) and copies the
content into a gleam file as strings. This is useful if you, for
instance, wish to build a custom website to display the examples/tutorials
for a gleam library.

[![Package Version](https://img.shields.io/hexpm/v/reference_this)](https://hex.pm/packages/reference_this)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/reference_this/)

```sh
gleam add --dev reference_this
```

```sh
gleam run -m reference_this "my_examples_dir" "examples.gleam"
```
