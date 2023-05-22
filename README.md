# chibicaml

[chibicc](https://github.com/rui314/chibicc) but written in OCaml.

Code is very imperative because I like writing code that way, fight me.

## Workflow

Usually when developing all I need is the tests:
```console
dune runtest -w
```

Sometimes it goes wrong and debugging is easiest with `dune exec`:
```console
dune exec bin/chibic.exe 'some bad input'
```
