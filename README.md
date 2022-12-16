# QLisp (temp)

A small toy lisp, strongly inspired by [this guide][risp].

Planned for eventual integration with my [quantum computing project][quacs].

To build the interpreter/REPL for yourself, run
```bash
cargo build --release --bin q
```
Built executables can then be used to interpret files, run standalone
expressions, and run the REPL.
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

## Features
- Written in 100% Rust with Rust-like built-ins for transforming lists of data
- Lambda functions have pattern-matching arguments, e.g.
  ```lisp
  ; Euclidean distance function in 2 dimensions
  (defn dist2d ((x1 y1) (x2 y2))
    (sqrt (+
      (** (- x2 x1) 2)
      (** (- y2 y1) 2))))

  ; the following calls to `dist2d` are equivalent:
  ; (a)
  (def p1 (5 2))
  (def p2 (1 6))
  (dist2d p1 p2) ; evaluates to 5
  ; (b)
  (let (x1 y1) (6 2))
  (let (x2 y2) (3 7))
  (dist2d (x1 y1) (x2 y2))

  ; points in 3 dimensions are disallowed
  (def p_3d (3 5 7))
  (dist2d p1 p_3d) ; error!
  ```
- Everything returns a value. This gives an easy way to check intermediates
  and propagate values up through scope hierarchies without affecting local
  environments:
  ```lisp
  ; bind the same value to two variables
  (def b (def a 10))
  (= a b 10) ; true

  ; print statements can be inserted in the middle of other expressions
  (defn fibb (n)
    (if (<= (println "{}" n) 1) ; track recursive evaluations of `fibb`
      1
      (+ (fibb (- n 1))
         (fibb (- n 2)))))

  ; create a module and bring a piece of it out into the surrounding environment
  (let (_ hbar)
    (module phys (
      (def h 6.626070040e-34)
      (/ h 2 PI))))
  (println "{:.5e}; {:.5e}" phys::h hbar) ; 6.62607e-34;  1.05457e-34
  ```

## To do
- [x] add a string type
- [x] add a CLI and read executable expressions from files
- [x] add various math/statistics functions
- [x] add pattern-matching args to lambdas
- [x] add namespaces and importing definitions from other files
- [x] add basic file I/O
    - [ ] maybe implement a way to stream data from a file
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
- ~~`formatx` doesn't support `e`/`E` formatting~~

[risp]: https://stopa.io/post/222
[quacs]: https://gitlab.com/whooie/quacs/-/tree/rustlib
[qlisp.vim]: https://gitlab.com/whooie/qlisp.vim

