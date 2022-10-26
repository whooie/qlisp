# QLisp (temp)

A small toy lisp I wrote, heavily following [this guide][risp], whose source
code is currently also present, but will be removed.

Planned for eventual integration with my [quantum computing project][quacs].

To run the REPL (currently the only way to interact with the language) for
yourself, run
```bash
cargo run --release --bin q
```

## To do
- expand the list of built-in functions:
    - basic stuff, like `print`, `format`, ...
    - iterator stuff, like `map`, `filter`, `fold`, `split`, ...
- add a string type
- add (nd)arrays
- add hash maps
- add various math/statistics functions
- add a CLI and read executable expressions from files
- *maybe* add looping


[risp]: https://stopa.io/post/222
[quacs]: https://gitlab.com/whooie/quacs/-/tree/rustlib

