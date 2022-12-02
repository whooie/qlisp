# QLisp (temp)

A small toy lisp I wrote heavily following [this guide][risp].

Planned for eventual integration with my [quantum computing project][quacs].

To run the REPL (currently the only way to interact with the language) for
yourself, run
```bash
cargo run --release --bin q
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

