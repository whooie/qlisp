# QLisp (temp)

A small toy lisp, strongly inspired by [this guide][risp].

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
- [x] add various math/statistics functions
- [x] add pattern-matching args to lambdas
- [x] add namespaces and importing definitions from other files
- [ ] add hash maps
- [ ] add (nd)arrays
- [ ] add file I/O with hash map serialization to `json`, `toml`, `yaml`, ...

## Known bugs / things I'm not happy about
- ~~single-item lists have their contents evaluated twice, e.g.
  `((print "{}" 0))`~~
- evaluation of imported functions is messy and involves cloning the functions'
  environments for each call outside of an iterator routine (e.g. `map`)
- the system for printing help text in the REPL is clunky
- there's probably much more cloning than is absolutely necessary

[risp]: https://stopa.io/post/222
[quacs]: https://gitlab.com/whooie/quacs/-/tree/rustlib
[qlisp.vim]: https://gitlab.com/whooie/qlisp.vim

