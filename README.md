# QLisp (temp)

A small toy lisp I wrote heavily following [this guide][risp].

Planned for eventual integration with my [quantum computing project][quacs].

To build the interpreter/REPL for yourself, run
```bash
cargo build --release --bin q
```
Built executables can then be used to interpret files, run standalone
expressions, and launch the REPL.
```
$ q --help
QLisp interpreter

Usage: q [OPTIONS] [FILE]

Arguments:
  [FILE]  Run a file as a script. Overridden by `-c`

Options:
  -c, --command <COMMAND>  Program passed as a string
  -h, --help               Print help information
  -V, --version            Print version information
```

See [qlisp.vim][qlisp.vim] for basic syntax highlighting in Vim.

## To do
- [x] add a string type
- [x] add a CLI and read executable expressions from files
- [ ] add namespaces and importing definitions from other files
- [ ] add (nd)arrays
- [ ] add hash maps
- [ ] add various math/statistics functions
- [ ] add file I/O with hash map serialization to `json`, `toml`, `yaml`, ...

## Known bugs
- single-item lists have their contents evaluated twice, e.g. `((print "{}" 0))`

[risp]: https://stopa.io/post/222
[quacs]: https://gitlab.com/whooie/quacs/-/tree/rustlib
[qlisp.vim]: https://gitlab.com/whooie/qlisp.vim

