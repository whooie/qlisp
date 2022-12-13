use std::borrow::Cow;
use phf::phf_map;
use reedline as reed;
use crate::{
    qerr,
    qerr_fmt,
    qbool,
    qlist,
    qstr,
    qsymbol,
    lang::{
        PROTECTED,
        QResult,
        QErr,
        QExp,
        QEnv,
        QEnvEntry,
        tokenize,
        parse,
    },
};

macro_rules! println_flush {
    ( $fmt:literal, $( $val:expr ),* ) => {
        println!($fmt, $( $val, )*);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
    }
}

static HELP_TEXT: phf::Map<&'static str, (&'static str, &'static str)>
    = phf_map! {
/*
 * special functions
 */

        "def" => (
            "Assign a value to a symbol.",
"
Assign a value to a symbol and store it in the local environment.
Alias: `:=`

Expected form:
(def <symbol> <value expression>)

Example:
q>> (def a (+ 1 2 3)) ; assign `6` to `a`
q>> a ; evaluates to 6
q>> (+ 5 (def b (* 10 11))) ; evalues to 115
q>> b ; evaluates to 110
",
        ),

        "let" => (
            "Assign multiple values to multiple symbols.",
"
Assign multiple values to multiple symbols, recusively unpacking lists if
necessary. The top-level value expression must be a list.
Alias: `*:=`

Expected form:
(let (<symbols>...) <value expression>)

Example:
q>> (let (a b (c d)) (0 1 (2 (range 3 6))))
q>> a ; evaluates to 0
q>> b ; evaluates to 1
q>> c ; evaluates to 2
q>> d ; evaluates to (3 4 5)
",
        ),

        "fn" => (
            "Construct an anonymous function.",
"
Construct an anonymous (lambda) function.
Alias: `@:`

Expected form:
(fn (<args>...) <body expression>)

Example:
q>> ; define a function of a, b that computes 5 * b + a and immediately call it
q>> ((fn (a b) (+ a (* 5 b))) 8 2) ; evaluates to 18
",
        ),

        "defn" => (
            "Construct a function and assign it to a symbol.",
"
Portmanteau of 'def' and 'fn': Construct a function and assign it to a symbol in
the local environment.
Alias: `@:=`

Expected form:
(defn <symbol> (<args>...) <body expression>)

Example:
q>> ; recursive definition of the factorial operation
q>> (defn factorial (n) (if (<= n 1) 1 (* (factorial (- n 1)) n)))
q>> (factorial 5) ; evaluates to 120
",
        ),

        "if" => (
            "Condition evaluation.",
"
Conditional expression: Evaluates one of two expressions based on the true/false
value of a test expression.

Expected form:
(if <test expression> <true expression> <false expression>)

Example:
q>> (def a true)
q>> (if a 1 0) ; evaluates to 1
q>> (def b false)
q>> (if b 1 0) ; evaluates to 0
",
        ),

        "module" => (
            "Construct a module and assign it to a symbol.",
"
Construct a module and assign it to a symbol in the local environment. Evaluated
data is returned as a list ordered by evaluation.

Expected form:
(module <symbol> (<expressions...>))

Example:
q>> ; use a namespace (module) to hold a set of constants and functions
q>> ; elements of a module cannot be changed from the outside
q>> (module my-module ((def a 10) (defn add-to-a (b) (+ a b))))
q>> my-module::a ; evaluates to 10
q>> ; functions inside a module are unaffected by outer environments
q>> (my-module::add-to-a 10) ; evaluates to 20
q>> (def a 15)
q>> (my-module::add-to-a 10) ; evaluates to 20
",
        ),

        "use" => (
            "Load an external file as a module.",
"
Load an external file as a module and assign it to a symbol in the local
environment. External files are specified as a symbolic path, where each element
in the path should correspond to a directory except for the final element, which
should correspond to a file name. The final element should not have a file
extension; instead the directory corresponding to the final parent in the path
will be searched for files with stems equal to the final element and ending with
'.qlisp', or '.qlsp'. All paths are treated as relative to the file being
executed or the current working directory if the REPL is being used. Use 'super'
in a path to specify a parent directory (equivalent to '..'). A symbol may be
also be specified to store the module under; the default symbol is the final
path element.

Expected form:
(use <path> [ symbol ])

Example:
q>> ; load ../mod1/mod2/child.qlisp and store it as child-mod
q>> (use super::mod1::mod2::child child-mod)
",
        ),

        "use-all" => (
            "Load the contents of an external file.",
"
Load the contents of an external file as a module and bring all of its contents
into the local environment.External files are specified as a symbolic path,
where each element in the path should correspond to a directory except for the
final element, which should correspond to a file name. The final element should
not have a file extension; instead the directory corresponding to the final
parent in the path will be searched for files with stems equal to the final
element and ending with '.qlisp', or '.qlsp'. All paths are treated as relative
to the file being executed or the current working directory if the REPL is being
used. Use 'super' in a path to specify a parent directory (equivalent to '..').
A symbol may be also be specified to store the module under; the default symbol
is the final path element.
Alias: `use*`

Expected form:
(use-all <path>)

Example:
q>> ; load the contents of ../mod1/mod2/child.qlisp into the local environment
q>> (use-all super::mod1::mod2::child)
",
        ),

        "interact" => (
            "Pause execution and launch the REPL.",
"
Immedately pause execution of a program and launch the REPL. Execution will
continue when the REPL is exited.

Expected form:
(interact)

Example:
q>> ; suppose this is in the middle of a file being executed
q>> (def a 10)
q>> (defn foo (b) (+ a b))
q>> ; launch the REPL here
q>> ; the environment will hold whatever definitions have been made up to this
q>> ; point in the program
q>> (interact)
q>> ; continue doing other things after the REPL exits.
",
        ),

        "isdef" => (
            "Check whether a symbol is defined.",
"
Check whether a set of definitions is present. Returns `true` if all symbols
passed as arguments exist with a value, `false` otherwise.
Alias: `?:=`

Expected form:
(isdef <symbols>...)

Example:
q>> (def a 10)
q>> (def foo (b) (+ a b))
q>> (module my-module ((def c 15)))
q>> (isdef a foo my-module::c) ; evaluates to true
",
        ),

        "del" => (
            "Remove a definition.",
"
Remove a definition from the local environment. Any value including modules may
be removed, but only values will be returned.
Alias: `!-`

Expected form:
(del <symbols>...)

Example:
q>> (def a 10)
q>> a ; evaluates to 10
q>> (del a) ; evaluates to 10
q>> a ; causes an error
",
        ),

/*
 * systems
 */

        "format" => (
            "Substitute values for patterns in a str.",
"
Substitute values for patterns in a format string.
Alias: `$`

Expected form:
(format <format string> <values>...)

Example:
q>> (format \"hello, {}!\" \"John\") ; evaluates to \"hello, John!\"
q>> (format \"{:.5}\" 3.141592653589793238) ; evaluates to \"3.14159\"
",
        ),

        "print" => (
            "Print a formatted str to STDOUT.",
"
Substitute values for patterns in a format string and print to STDOUT. If a
single value is passed, the value is returned; if multiple values are passed,
they are all returned in a list.
Alias: `$-`

Expected form:
(print <format string> <values>...)

Example:
q>> (print \"hello {}!\" \"John\") ; prints `hello John!` and evaluates to \"John\"
q>> ; passed values are returned, so `print` can be inserted in the middle of
q>> ; other expressions
q>> (+ (print \"{}\" (* 2 3)) 5) ; prints `6` and evaluates to 11
",
        ),

        "println" => (
            "Print a formatted str to STDOUT with a newline appended.",
"
Substitute values for patterns in a format string and print to STDOUT with
newline appended. If a single value is passed, the value is returned; if
multiple values are passed, they are all returned in a list.
Alias: `$_`

Expected form:
(println <format string> <values>...)

Example:
q>> (println \"hello {}!\" \"John\") ; prints `hello John!\\n` and evaluates to \"John\"
q>> ; passed values are returned, so `print` can be inserted in the middle of
q>> ; other expressions
q>> (+ (println \"{}\" (* 2 3)) 5) ; prints `6\\n` and evaluates to 11
",
        ),

        "halt" => (
            "Halt execution of a program with an error message.",
"
Substitute values for patterns in a format string and return the result as an
error.
Alias: `!!`

Expected form:
(halt <format string> <values>...)

Example:
q>> (defn a -1)
q>> (if (> a 0) (* a 2) (halt \"expected a positive value, but got {}\" a))
",
        ),

        "istype" => (
            "Test the type of a value.",
"
Returns `true` if a value is of a given type or list of types, `false`
otherwise. Types must be specified as strs. Available type names are: `bool`,
`int`, `float`, `complex`, `list`, `str`, `function`, `any`.
Alias: `~?`

Expected form:
(istype <type or list of types> <value>)

Example:
q>> (def z 5i)
q>> (istype \"complex\" z) ; evaluates to true
q>> (def reals (\"bool\" \"int\" \"float\"))
q>> (if (istype reals z) 0 1) ; evaluates to 1
",
        ),

        "type" => (
            "Get the type(s) of one or more values.",
"
Get the type(s) of one or more values as strs. If one value is passed, the
result is returned as a single string, otherwise a list of strings is returned.
Possible return values are \"bool\", \"int\", \"float\", \"complex\", \"list\", \"str\",
\"function\".
Alias: `?~`

Expected form:
(type <values>...)

Example:
q>> (type true) ; evaluates to \"bool\"
q>> (type true 1 1.0 1i) ; evaluates to (\"bool\" \"int\" \"float\" \"complex\")
",
        ),

/*
 * type-casting
 */

        "bool" => (
            "Boolean type.",
"
Primitive Boolean type. Can be used as a function to cast other values to this
type or parse strs as such. If a single list is passed, operates on the contents
of the list instead of the list itself.

Expected form:
(bool <values>...)

Example:
q>> (bool 0 1.0 1i) ; evaluates to (false true true)
q>> (bool \"false\") ; evaluates to false
",
        ),

        "int" => (
            "Integer type.",
"
Signed 64-bit integer type. Can be used as a function to cast other values to
this type or parse strs as such. If a single list is passed, operates on the
contents of the list instead of the list itself.

Expected form:
(int <values>...)

Example:
q>> (int true false 5.5) ; evaluates to (1 0 5)
q>> (int \"8\") ; evaluates to 8
"
        ),

        "float" => (
            "Floating-point type.",
"
64-bit floating-point type. Can be used as a function to cast other values to
this type or parse strs as such. If a single list is passed, operates on the
contents of the list instead of the list itself.

Expected form:
(float <values>...)

Example:
q>> (float true 5) ; evaluates to (0.0 5.0)
q>> (float \"3.14159\") ; evaluates to 3.14159
"
        ),

        "complex" => (
            "Complex floating-point type.",
"
128-bit complex floating-point type. Can be used as a function to cast other
values to this type or parse strs as such. If a single list is passed, operates
on the contents of the list instead of the list itself.

Expected form:
(complex <values>...)

Example:
q>> (complex true 5 5.0) ; evaluates to (1.0+0i 5.0+0i 5.0+0i)
q>> (complex \"2+8i\") ; evaluates to 2.0+8.0i
",
        ),

        "list" => (
            "List type.",
"
List type. Can be used as a function to pack all arguments into a list.

Expected form:
(list <values>...)

Example:
q>> (list true 5 5.0 5i \"hello\") ; evaluates to (true 5 5.0 5i \"hello\")
",
        ),

        "str" => (
            "String type.",
"
String type. Can be used as a function to cast all arguments to strs using a
default formatter.

Expected form:
(str <values>...)

Example:
q>> (str 5.0) ; evaluates to \"5\"
q>> (str \"hello\" 1.0546 10i) ; evaluates to (\"hello\" \"1.0546\" \"0+10i\")
"
        ),

/*
 * arithmetic
 */

        "add" => (
            "Addition operation.",
"
Addition operation. Supports any number of arguments, retuning the total sum. If
no arguments are passed, returns 0. The type of the returned value is the most
general of all argument types. If only a single list is passed, this function is
applied to its contents.
Alias: `+`

Expected form:
(add <numbers>...)

Example:
q>> (add 5 5 5) ; evaluates to 15
q>> (add 5 5 5.0) ; evaluates to 15.0
q>> (add 5 5.0 5.0+0i) ; evaluates to 15.0+0i
",
        ),

        "sub" => (
            "subtraction operation.",
"
Subtraction operation. Requires at least one argument, subtracting all following
arguments from the first. The type of the returned value is the most general of
all argument types. If only a single list is passed, this function is applied to
its contents.
Alias: `-`

Expected form:
(sub <numbers>...)

Example:
q>> (sub 5 5 5) ; evaluates to -10
q>> (sub 5 5 5.0) ; evaluates to -10.0
q>> (sub 5 5.0 5.0+0i) ; evaluates to -10.0+0i
"
        ),

        "mul" => (
            "Multiplication operation.",
"
Multiplication operation. Supports any number of arguments, returning the total
product. If no arguments are passed, returns 1. The type of the returned value
is the most general of all argument types. If only a single list is passed, this
function is applied to its contents.
Alias: `*`

Expected form:
(mul <numbers>...)

Example:
q>> (mul 5 5 5) ; evaluates to 125
q>> (mul 5 5 5.0) ; evaluates to 125.0
q>> (mul 5 5.0 5.0+0i) ; evaluates to 125.0+0i
"
        ),

        "div" => (
            "Division operation.",
"
Division operation. Requires at least one argument, dividing the first by all
following arguments. The type of the returned value is either a float or a
complex, depending on the types of the arguments. If only a single list is
passed, this function is applied to its contents.
Alias: `/`

Expected form:
(div <numbers>...)

Example:
q>> (div 5 5 5) ; evaluates to 0.2
q>> (div 5 5 5.0) ; evaluates to 0.2
q>> (div 5 5.0 5.0+0i) ; evaluates to 0.2+0i
"
        ),

        "idiv" => (
            "Integer division operation.",
"
Integer division operation, where a floor operator is applied after each
pairwise operation. Requires at least one argument, integer-dividing the first
by all following arguments. Does not accept complex-valued arguments. The type
of the returned value is always an int. If only a single list is passed, this
function is applied to its contents.
Alias: `//`

Expected form:
(idiv <numbers>...)

Example:
q>> (idiv 5 2) ; evaluates to 2
q>> (idiv -5 2.0) ; evaluates to -3
q>> (idiv 5 2.0) ; evaluates to 2
q>> (idiv 5 2.0 2.2) ; evaluates to 0
"
        ),

/*
 * boolean comparisons
 */

        "and" => (
            "Logical AND operator.",
"
Logical AND operator, short-circuited and extended to n >= 0 inputs. Returns
`true` if all inputs are booleans with positive value or if there are no inputs.
Alias: `&&`

Expected form:
(and <inputs>...)

Example:
q>> (and true true true) ; evaluates to true
q>> (and true true false) ; evaluates to false
q>> (and) ; evaluates to true
",
        ),

        "or" => (
            "Logical OR operator.",
"
Logical OR operator, short-circuited and extended to n >= 0 inputs. Returns
`true` if all evaluated inputs are booleans and at least one of them has
positive value.
Alias: `||`

Expected form:
(or <inputs>...)

Example:
q>> (or true true true) ; evaluates to true
q>> (or true false false) ; evaluates to true
q>> (or) ; evaluates to false
",
        ),

        "xor" => (
            "Logical XOR operator.",
"
Logical XOR operator, short-circuited and extended to n >= 0 inputs. Returns
`true` if exactly one input is `true`, `false` otherwise.
Alias: `^`

Expected form:
(xor <inputs>...)

Example:
q>> (xor true false false) ; evaluates to true
q>> (xor true true false) ; evaluates to false
q>> (xor) ; evaluates to false
",
        ),

        "eq" => (
            "Equality comparison operator.",
"
Equality comparison operator. Not to be confused with the `def`/`:=` assignment
keyword. Returns `false` if there exists at least 1 argument that is not equal
to the rest. Returns `true` if no arguments are passed. If only a single list is
passed, operates on the contents of the list instead of the list itself. Note
that two numbers of different types (e.g. 5 and 5.0) are *not* considered equal,
even if their numerical values are the same. To perform such a comparison, cast
the values to the same type, e.g. `(= (float 5 5.0))`.
Alias: `=`

Expected form:
(eq <args>...)

Example:
q>> (def a 5)
q>> (eq a 5) ; evaluates to true
q>> (eq a 5 5.0) ; evaluates to false
q>> (eq (float a 5 5.0)) ; evaluates to true
"
        ),

        "neq" => (
            "Non-equality comparison operator.",
"
Non-equality comparison operator. Returns `false` if at least two arguments are
equal. Returns `true` if no arguments are passed. If only a single list is
passed, operates on the contents of the list instead of the list itself. Note
that two numbers of different types (e.g. 5 and 5.0) are *not* considered equal,
even if their numerical values are the same. To perform such a comparison, cast
the values to the same type, e.g. `(!= (float 5 5.0))`.
Alias: `!=`

Expected form:
(neq <args>...)

Example:
q>> (def a 5)
q>> (neq a 5) ; evaluates to false
q>> (neq a 5.0) ; evaluates to true
q>> (neq (float a 5.0)) ; evaluates to false
",
        ),

        "gt" => (
            "Greater-than comparison operator.",
"
Greater-than comparison operator. Returns `true` if the arguments are in
monotonic, descending order, i.e. every element is greater than the one
following it. Returns `true` if no arguments are passed. If only a single list
is passed, operates on the contents of the list instead of the list itself.
Alias: `>`

Expected form:
(gt <args>...)

Example:
q>> (gt 5 4) ; evaluates to true
q>> (gt 5 5 4) ; evaluates to false
q>> (gt \"c\" \"b\" \"a\") ; evaluates to true
",
        ),

        "geq" => (
            "Greater-than-or-equal-to comparison operator.",
"
Greater-than-or-equal-to comparison operator. Returns `true` if the arguments
are in non-increasing order, i.e. there exists no element that is less than the
one following it. Returns `true` if no arguments are passed. If only a single
list is passed, operates on the contents of the list instead of the list itself.
Alias: `>=`

Expected form:
(geq <args>...)

Example:
q>> (geq 5 4) ; evaluates to true
q>> (geq 5 5 4) ; evaluates to true
q>> (geq \"c\" \"b\" \"a\") ; evaluates to true
",
        ),

        "lt" => (
            "Less-than comparison operator.",
"
Less-than comparison operator. Returns `true` if the arguments are in monotonic,
ascending order, i.e. every element is less than the one following it. Returns
`true` if no arguments are passed. If only a single list is passed, operates on
the contents of the list instead of the list itself.
Alias: `<`

Expected form:
(lt <args>...)

Example:
q>> (gt 5 6) ; evaluates to true
q>> (gt 5 5 6) ; evaluates to false
q>> (gt \"a\" \"b\" \"c\") ; evaluates to true
",
        ),

        "leq" => (
            "Less-than-or-equal-to comparison operator.",
"
Less-than-or-equal-to comparison operator. Returns `true` if the arguments are
in non-decreasing order, i.e. there exists no element that is greater than the
one following it. Returns `true` if no arguments are passed. If only a single
list is passed, operates on the contents of the list instead of the list
itself.
Alias: `<=`

Expected form:
(leq <args>...)

Example:
q>> (leq 5 6) ; evaluates to true
q>> (leq 5 5 6) ; evaluates to true
q>> (leq \"a\" \"b\" \"c\") ; evaluates to true
",
        ),

/*
 * boolean accumulators
 */

        "all" => (
            "Logical AND accumulator.",
"
Logical AND accumulator on a list input: Returns `true` if all items in a list
are `true`, `false` otherwise.
Alias: `&&*`

Expected form:
(all (<values>...))

Example:
q>> (def numbers (range 0 5)) ; (0 1 2 3 4)
q>> (all (map (fn (n) (< n 10)) numbers)) ; evaluates to true
q>> (all (map (fn (n) (< n 2)) numbers)) ; evaluates to false
",
        ),

        "any" => (
            "Logical OR accumulator.",
"
Logical OR accumulator on a list input: Returns `true` if at least one item in
a list is `true`, `false` otherwise.
Alias: `||*`

Expected form:
(any (<values>...))

Example:
q>> (def numbers (range 0 5)) ; (0 1 2 3 4)
q>> (any (map (fn (n) (> n 10)) numbers)) ; evaluates to false
q>> (any (map (fn (n) (> n 2)) numbers)) ; evaluates to true
",
        ),

        "xany" => (
            "Logical XOR accumulator",
"
Logical XOR accumulator on a list input: Returns `true` if exactly one item in
a list is `true`, `false` otherwise.
Alias: `^*`

Expected form:
(xany (<values>...))

Example:
q>> (def numbers (range 0 5)) ; (0 1 2 3 4)
q>> (xany (map (fn (n) (> n 2)) numbers)) ; evaluates to false
q>> (xany (map (fn (n) (= n 2)) numbers)) ; evaluates to true
",
        ),

/*
 * iterable creation
 */

        "range" => (
            "Construct a list of integers on a semi-open interval.",
"
Construct a list of integers on a semi-open interval.
Alias: `..`

Expected form:
(range <start> <stop>)

Example:
q>> (range 5 15) ; evaluates to (5 6 7 8 9 10 11 12 13 14)
q>> (range 5 -5) ; evaluates to (5 4 3 2 1 0 -1 -2 -3 -4)
",
        ),

        "range-inc" => (
            "Construct a list of integers on a closed interval.",
"
Construct a list of integers on a closed interval.
Alias: `..=`

Expected form:
(range-inc <start> <stop>)

Example:
q>> (range-inc 5 15) ; evaluates to (5 6 7 8 9 10 11 12 13 14 15)
q>> (range-inc 5 -5) ; evaluates to (5 4 3 2 1 0 -1 -2 -3 -4 -5)
",
        ),

        "repeat" => (
            "Repeat a list or str `n` times.",
"
Construct a new list or str by repeating the contents of another `n` times.
Alias: `#=`

Expected form:
(repeat <n> <list or str>)

Example:
q>> (repeat 3 (1 2 3)) ; evaluates to (1 2 3 1 2 3 1 2 3)
q>> (repeat 2 \"hello\") ; evaluates to \"hellohello\"
",
        ),

/*
 * iterable accumulation
 */

        "length" => (
            "Find the length of a list or str.",
"
Return the number of items in a list or characters in a str.
Alias: `#`

Expected form:
(length <list or str>)

Example:
q>> (length (range 0 10)) ; evaluates to 10
q>> (length \"hello\") ; evaluates to 5
",
        ),

        "fold" => (
            "Fold elements of a list or str into an accumulator.",
"
Iterate over a list or str, folding each element into an accumulator by applying
a function. The function should take two arguments with the first being the
accumulator.
Alias: `@.`

Expected form:
(fold <start> <function> <list or str>)

Example:
q>> (defn f (acc x) (if (= (% 2 x) 0) (+ acc x) (* acc x)))
q>> (fold 1 f (range 0 4)) ; evaluates to 9
",
        ),

        "min" => (
            "Find the minimum of a list or str.",
"
Find the minimum of a list or str using the `<` function.
Alias: `<:`

Expected form:
(min <list or str>)

Example:
q>> (min (range 5 -5)) ; evaluates to -4
q>> (min \"hello world\") ; evaluates to \" \"
",
        ),

        "max" => (
            "Find the maximum of a list or str.",
"
Find the maximum of a list or str using the `>` function.
Alias: `:>`

Expected form:
(max <list or str>)

Example:
q>> (max (range 5 -5)) ; evaluates to 5
q>> (max \"hello world\") ; evaluates to \"w\"
",
        ),

        "select-by" => (
            "Select an element of a list or str by a comparison function.",
"
Select an extremum element through element-wise comparisons with an accumulator
using a comparison function. The function should take two arguments, with the
first being the trial element and the second being the accumulator, and return
`true` if the trial element is to replace the accumulator, `false` otherwise.
The initial value of the accumulator is set to be the first element of the list
or str. To set a different initial value, use `fold` instead.
Alias: `*@.`

Expected form:
(select-by <comparison function> <list or str>)

Example:
q>> (select-by < (range 5 -5)) ; evaluates -4
q>> ; find the maximum number in a list divisible by 3
q>> (defn maxdiv3 (x acc) (and (= (mod 3 x) 0) (> x acc)))
q>> (select-by maxdiv3 (0 5 3 7 5 6 6 9 7 3 5 12 3 10)) ; evaluates to 12
",
        ),

/*
 * iterable slicing and access
 */

        "get" => (
            "Get the element of a list or str at an index.",
"
Return the item of a list or character of a str at an index. Indicies must be
non-negative integers.
Alias: `.`

Expected form:
(get <index> <list or str>)

Example:
q>> (get 3 (range-inc 1 10)) ; evaluates to 4
q>> (get 4 \"hello world\") ; evaluates to \"o\"
",
        ),

        "set" => (
            "Change the value of an item in a list.",
"
Return a copy of a list where values at given indices have been set to new ones.
Each argument following the list must be a list of length 2 where the first item
is the index and the second is the value.
Alias: `.:=`

Expected form:
(set <list> (<index> <value>)...)

Example:
q>> (set (range 0 10) (0 3) (7 10)) ; evaluates to (3 1 2 3 4 5 6 10 8 9)
",
        ),

        "slice" => (
            "Slice a list or str over a semi-open range of indices.",
"
Return a slice of a list or substring of a str over a semi-open range of
indices.
Alias: `--`

Expected form:
(slice <start> <stop> <list or str>)

Example:
q>> (slice 2 7 (range 10 0)) ; evaluates to (8 7 6 5 4)
q>> (slice 2 7 \"hello world\") ; evaluates to \"llo w\"
",
        ),

        "slice-inc" => (
            "Slice a list or str over a closed range of indices.",
"
Return a slice of a list or substring of a str over a closed range of indices.
Alias: `--=`

Expected form:
(slice-inc <start> <stop> <list or str>)

Example:
q>> (slice-inc 2 7 (range 10 0)) ; evaluates to (8 7 6 5 4 3)
q>> (slice-inc 2 7 \"hello world\") ; evaluates to \"llo wo\"
",
        ),

        "slice-by" => (
            "Step through a semi-open slice.",
"
Portmanteau of slice and step-by: Return a slice of a list or substring of a str
over a semi-open range of indices with a given step size.
Alias: `~~`

Expected form:
(slice-by <start> <stop> <step size> <list or str>)

Example:
q>> (slice-by 2 7 2 (range 10 0)) ; evaluates to (8 6 4)
q>> (slice-by 2 7 2 \"hello world\") ; evaluates to \"low\"
",
        ),

        "slice-inc-by" => (
            "Step through a closed slice.",
"
Portmanteau of slice-inc and step-by: Return a slice of a list or substring of a
str over a closed range of indices with a given step size.
Alias: `~~=`

Expected form:
(slice-inc-by <start> <stop> <step size> <list or str>)

Example:
q>> (slice-by 2 8 2 (range 10 0)) ; evaluates to (8 6 4 2)
q>> (slice-by 2 8 2 \"hello world\") ; evaluates to \"lowr\"
",
        ),

        "pick" => (
            "Select multiple elements from a list or str",
"
Select items from a list or characters from a str at specific indices and return
them in a new list or str.
Alias: `.*`

Expected form:
(pick (<index>...) <list or str>)

Example:
q>> (pick (0 5 3 3 6) (range 10 0)) ; evaluates to (10 5 7 7 4)
q>> (pick (0 5 3 3 6) \"hello world\") ; evaluates to \"h llw\"
",
        ),

        "first" => (
            "Get the first element in a list or str.",
"
Get the item in a list or character in a str at index 0. Equivalent to
(get 0 ...).
Alias: `.-`

Expected form:
(first <list or str>)

Example:
q>> (first (range 0 10)) ; evaluates to 0
q>> (first \"hello world\") ; evaluates to \"h\"
",
        ),

        "take" => (
            "Take the first `n` element of a list or str.",
"
Take the first `n` items in a list or characters in a str, discarding the rest.
Alias: `~.`

Expected form:
(take <n> <list or str>)

Example:
q>> (take 5 (range 0 10)) ; evaluates to (0 1 2 3 4)
q>> (take 5 \"hello world\") ; evaluates to \"hello\"
",
        ),

        "take-while" => (
            "Take elements of a list or str satisfying a predicate.",
"
Iterate from the start of a list or str, taking all items or characters for
which a function returns `true`, discarding all after and including the first
for which the function returns `false`. The function must return a bool for all
inputs.
Alias: `~.@`

Expected form:
(take-while <predicate function> <list or str>)

Example:
q>> (take-while (fn (n) (< n 3)) (0 1 3 2 2 5)) ; evaluates to (0 1)
q>> (take-while (fn (c) (!= c \" \") \"hello world\") ; evaluates to \"hello\"
",
        ),

        "last" => (
            "Get the last element of a list or str.",
"
Get the item in a list or character in a str at index N - 1, where N is the
length of the list or str. Equivalent to (get (- N 1) ...).
Alias: `-.`

Expected form:
(last <list or str>)

Example:
q>> (last (range 0 10)) ; evaluates to 9
q>> (last \"hello world\") ; evaluates to \"d\"
",
        ),

        "skip" => (
            "Discard the first `n` elements of a list or str.",
"
Discard the first `n` items in a list or characters in a str, keeping the rest.
Alias: `.~`

Expected form:
(skip <n> <list or str>)

Example:
q>> (skip 5 (range 0 10)) ; evaluates to (5 6 7 8 9)
q>> (skip 5 \"hello world\") ; evaluates to \" world\"
",
        ),

        "skip-while" => (
            "Skip elements of a list or str satisfying a predicate.",
"
Iterate from the start of a list or str, skipping all items or characters for
which a function returns `true`, keeping all after and including the first for
which the function returns `false`. The function must return a bool for all
inputs.
Alias: `.~@`

Expected form:
(skip-while <predicate function> <list or str>)

Example:
q>> (skip-while (fn (n) (< n 3)) (0 1 3 2 2 5)) ; evaluates to (3 2 2 5)
q>> (skip-while (fn (c) (!= c \" \")) \"hello world\") ; evaluates to \" world\"
",
        ),

/*
 * iterable transformations
 */

        "step-by" => (
            "Step through a list or str.",
"
Step through a list or str with a given step size.
Alias `~`

Expected form:
(step-by <step size> <list or str>)

Example:
q>> (step-by 3 (range 0 15)) ; evaluates to (0 3 6 9 12)
q>> (step-by 3 \"hello world\") ; evaluates to \"hlwl\"
",
        ),

        "enumerate" => (
            "Convert elements in a list or str to (index, element) pairs.",
"
Convert each item in a list or character in a str to a two-item list containing
the item or character and its index.
Alias: `##`

Expected form:
(enumerate <list or str>)

Example:
q>> (enumerate (1 1.0 1i)) ; evaluates to ((0 true) (1 1) (2 1.0) (3 0.0+1.0i))
q>> (enumerate \"hello\") ; evaluates to ((0 \"h\") (1 \"e\") (2 \"l\") (3 \"l\") (4 \"o\"))
",
        ),

        "reverse" => (
            "Reverse the order of a list or str.",
"
Reverse the order of a list or str.
Alias: `<>`

Expected form:
(reverse <list or str>)

Example:
q>> (reverse (range 0 10)) ; evaluates to (9 8 7 6 5 4 3 2 1 0)
q>> (reverse \"hello world\") ; evaluates to \"dlrow olleh\"
",
        ),

        "cycle" => (
            "Shift elements in a list or str, wrapping at the ends.",
"
Shift the positions of items in a list or characters in a str by a constant
offset n, wrapping around the ends; i.e. move the item at index k to new index
(k + n) % N, where N is the length of the list or str.
Alias: `<#>`

Expected form:
(cycle <offset> <list or str>)

Example:
q>> (cycle 2 (range 0 10)) ; evaluates to (8 9 0 1 2 3 4 5 6 7)
q>> (cycle 2 \"hello world\") ; evaluates to \"ldhello wor\"
",
        ),

        "map" => (
            "Apply a function to each element of a list or str.",
"
Apply a function to each item of a list or character of a str and return the
results in a list.
Alias: `@`

Expected form:
(map <function> <list or str>)

Example:
q>> (map (fn (n) (* n n)) (range 0 8)) ; evaluates to (0 1 4 9 25 36 49 64)
q>> (map (fn (c) (= c \"l\")) \"hello\") ; evaluates to (false false true true false)
",
        ),

        "filter" => (
            "Take only elements of a list or str satisfying a predicate.",
"
Apply a function to each item of a list or character of a str and discard all
for which the function returns `false`. The function must return a bool for all
inputs.
Alias: `@!`

Expected form:
(filter <predicate function> <list or str>)

Example:
q>> (filter (fn (n) (= (% 3 n) 0)) (range 0 10)) ; evaluates to (0 3 6 9)
q>> (filter (fn (c) (= c \"l\")) \"hello world\") ; evaluates to \"lll\"
",
        ),

        "unique" => (
            "Reduce a list or str to only its unique elements.",
"
Reduce a list or str down to only unique items or characters. If two elements
are equal, the element nearer to the start of the list or str is kept; order is
otherwise maintained.
Alias: `*!=`

Expected form:
(unique <list or str>)

Example:
q>> (unique (true 1 1 5i false true)) ; evaluates to (true 1 5i false)
q>> (unique \"hello world\") ; evaluates to \"helo wrd\"
",
        ),

        "flatten" => (
            "Recursively unpack nested lists.",
"
Recursively unpack nested lists so that all items lie next to each other in a
single list.
Alias `__`

Expected form:
(flatten <list>)

Example:
q>> (flatten ((1 2 3) (4 5 6) (7 8 9))) ; evaluates to (1 2 3 4 5 6 7 8 9)
",
        ),

        "sort" => (
            "Sort the elements of a list or str.",
"
Sort a list or str in ascending order using the `<` function. This sort is
stable and performs in O(n log(n)) time.
Alias: `<*`

Expected form:
(sort <list or str>)

Example:
q>> (sort (4 7 3 5 5 2 7 9 4 6 8)) ; evaluates to (2 3 4 4 5 5 6 7 7 8 9)
q>> (sort \"hello world\") ; evaluates to \" dehllloorw\"
",
        ),

        "sort-by" => (
            "Sort the elements of a list or str by a comparison function.",
"
Sort a list or str using a given comparison function. The function should take
two arguments, x and y, and return `true` if x should come before y, `false`
otherwise.
Alias: `<@`

Expected form:
(sort-by <comparison function> <list or str>)

Example:
q>> (sort-by > (4 7 3 5 5 2 7 9 4 6 8)) ; evaluates to (9 8 7 7 6 5 5 4 4 3 2)
q>> (defn listmax (a b) (> (max a) (max b)))
q>> (sort-by listmax ((1 5) (6 2) (0 10))) ; evaluates to ((0 10) (6 2) (1 5))
",
        ),

        "permute" => (
            "Perform a series of cyclic permutations on a list or str.",
"
Perform a series of permutations on a list X. Permutations are described
by n-item lists of indices (i_1 i_2 ... i_n) where the presence of an index i_k
indicates that the element in X at i_k is to be replaced by the element at
i_{k - 1}. The element at i_1 will be replaced by the element at i_n. Each
permutation can contain each available index at most once.
Alias: `.~.`

Expected form:
(permute <list> <permutations>...)

Example:
q>> ; simple swaps
q>> (permute (range 0 5) (0 4) (2 3)) ; evaluates to (4 1 3 2 0)
q>> ; cyclic permutations
q>> (permute (\"a\" \"b\" \"c\") (0 1 2)) ; evaluates to (\"c\" \"a\" \"b\")
q>> ; non-total cyclic permutations
q>> (permute (range 0 10) (0 2 4 6)) ; evaluates to (6 1 0 3 2 5 4 7 8 9)
",
        ),

/*
 * iterable division
 */

        "split-at" => (
            "Divide a list or str into two pieces.",
"
Divide a list or str into two pieces with the first containing the first `n`
items or characters and the second containing the rest.
Alias: `|.`

Expected form:
(split-at <n> <list or str>)

Example:
q>> (split-at 7 (range 0 10)) ; evaluates to ((0 1 2 3 4 5 6) (7 8 9))
q>> (split-at 7 \"hello world\") ; evaluates to (\"hello w\" \"orld\")
",
        ),

        "split-on" => (
            "Divide a list or str into pieces.",
"
Iterate from the start of a list or str, splitting into slices or substrings on
each item or character for which a function returns `true`. Matched elements are
discarded.
Alias: `|@`

Expected form:
(split-on <matching function> <list or str>)

Example:
q>> (defn div3 (n) (= (mod 3 n) 0))
q>> (split-on div3 (range 0 5)) ; evaluates to (() (1 2) (4))
q>> (defn is_space (c) (= c \" \"))
q>> (split-on is_space \"hello world\"); evaluates to (\"hello\" \"world\")
",
        ),

        "split-on-inc" => (
            "Divide a list or str into pieces, keeping split elements.",
"
Iterate from the start of a list or str, splitting into slices or substrings on
each item or character for which a function returns `true`. Matched elements are
contained in the previous slice or substring.
Alias: `|@=`

Expected forms:
(split-on-inc <matching function> <list or str>)

Example:
q>> (defn div3 (n) (= (mod 3 n) 0))
q>> (split-on-inc div3 (range 0 5)) ; evaluates to ((0) (1 2 3) (4))
q>> (defn is_space (c) (= c \" \"))
q>> (split-on-inc is_space \"hello world\"); evaluates to (\"hello \" \"world\")
",
        ),

/*
 * iterable addition
 */

        "append" => (
            "Append elements to the end of a list or str.",
"
Append one or more items, one by one, to the end of a list or strs to the end
of another str.
Alias: `+.`

Expected form:
(append <list or str> <items>...)

Example:
q>> (append (0 1 2) true 5i \"abc\") ; evaluates to (0 1 2 true 5i \"abc\")
q>> (append \"hello \" \"wo\" \"rld\") ; evaluates to \"hello world\"
",
        ),

        "prepend" => (
            "Prepend element to the beginning of a list or str.",
"
Prepend one or more items, one by one, to the beginning of a list or strings to
the beginning of another string. Note that this process inverts the order of
arguments following the list or str.
Alias: `.+`

Expected form:
(prepend <list or str> <items>...)

Example:
q>> (prepend (0 1 2) true 5i \"abc\") ; evaluates to (5i true 0 1 2)
q>> (prepend \"hello \" \"wo\" \"rld\") ; evaluates to \"rldwohello \"
",
        ),

        "insert" => (
            "Insert one or more elements into a list or str.",
"
Insert one or more items into a list or strings into another string at a given
index. The order of arguments following the list or str is preserved.
Alias: `+.+`

Expected form:
(insert <index> <list or str> <items>...)

Example:
q>> (insert 3 (range 0 5) true false) ; evaluates to (0 1 2 true false 3 4)
q>> (insert 3 \"hello world\" \"abc\" \"def\") ; evaluates to \"helabcdeflo world\"
",
        ),

        "join" => (
            "Concatenate lists or strs.",
"
Concatenate two or more lists or strs in the order they are passed. Arguments
must be either all lists or all strs. If only a single list is passed, operates
on the contents of the list instead of the list itself.
Alias: `++`

Expected form:
(join <lists or strs>...)

Example:
q>> (join (1 2 3) (4 5 6) (7 8 9)) ; evaluates to (1 2 3 4 5 6 7 8 9)
q>> (join \"hello\" \" \" \"world\") ; evaluates to \"hello world\"
",
        ),

        "join-with" => (
            "Concatenate lists or strs with an item placed at each join.",
"
Concatenate two or more lists or strs in the order they are passed, inserting an
extra item at each join. Arguments after the item just be either all lists or
all strs. If only a single list is passed, operates on the contents of the list
instead of the list itself.
Alias `+*+`

Expected form:
(join-with <item> <lists or strs>...)

Example:
q>> (join-with 0 (1 2 3) (4 5 6) (7 8 9)) ; evaluates to (1 2 3 0 4 5 6 0 7 8 9)
q>> (join-with \" \" \"hello\" \"world\") ; evaluates to \"hello world\"
",
        ),

        "zip" => (
            "Combine lists or strs element-wise.",
"
Combine lists or strs element-wise, returning a list of lists containing
elements drawn from each argument. The length of the returned list is limited to
that of the shortest argument. strs are unpacked into lists of one
character-long strs. If only a single list is passed, operates on the contents
of the list instead of the list itself.
Alias: `:~:`

Expected form:
(zip <lists or strs>...)

Example:
q>> (zip (1 2) (3 4) (5 6 7)) ; evaluates to ((1 3 5) (2 4 6))
q>> (zip ((1 3 5) (2 4 6))) ; evaluates to ((1 2) (3 4) (5 6))
q>> (zip (0 1 2) \"hello world\") ; evaluates to ((0 \"h\") (1 \"e\") (2 \"l\"))
",
        ),

        "cart" => (
            "Cartesian product.",
"
Compute the Cartesian product of two or more lists or strs. The returned value
is a list of lists. str arguments are unpacked into lists of one character-long
strs. If only a single list is passed, operates on the contents of the list
instead of the list itself.
Alias: `:*:`

Expected form:
(cart <lists or strs>...)

Example:
q>> (cart (1 2) (3 4) (5 6)) ; evaluates to ((1 3 5) (1 3 6) (1 4 5) [...])
q>> (cart (1 2) \"ab\") ; evaluates to ((1 \"a\") (1 \"b\") (2 \"a\") (2 \"b\"))
",
        ),

/*
 * iterable testing
 */

        "contains" => (
            "Determine whether a list or str contains a particular element.",
"
Returns `true` if a given list or str contains a given item or substring.
Alias: `*=`

Expected form:
(contains <list or str> <item or substring>)

Example:
q>> (contains (range 0 10) 5) ; evaluates to true
q>> (contains \"hello world\" \"o w\") ; evaluates to true
q>> (contains \"hello world\" \"a\") ; evaluates to false
",
        ),

        "index-of" => (
            "Find the first occurrence of an element in a list or str",
"
Returns the index of the first occurrence of an item or substring in a list or
str. If the list or str does not contain the element, -1 is returned.
Alias: `#*=`

Expected form:
(index-of <list or str> <item or substring>)

Example:
q>> (index-of (range 10 0) 9) ; evaluates to 1
q>> (index-of \"hello world\" \"l\") ; evaluates to 2
q>> (index-of \"hello world\" \"a\") ; evaluates to -1
",
        ),

/*
 * element-wise math
 */

        "neg" => (
            "Logical and arithmetic negative.",
"
Logical and arithmetic negative: For each input, returns the logical negation
for booleans and additive inverse for other numerical types. If a single list is
passed, operates on the contents of the list instead of the list itself.
Alias: `!`

Expected form:
(neg <values>...)

Example:
q>> (neg true) ; evaluates to false
q>> (neg -5) ; evaluates to 5
q>> (neg false 1.0546) ; evaluates to (true -1.0546)
",
        ),

        "recip" => (
            "Scalar multiplicative inverse.",
"
Scalar multiplicative inverse: For each input x, returns the multiplicative
inverse 1/x. If a single list is passed, operates on the contents of the list
instead of the list itself. Output values are always floats, complexes, or lists
thereof.
Alias: `1/`

Expected form:
(recip <values>...)

Example:
q>> (recip 2) ; evaluates to 0.5
q>> (recip 1+1i) ; evaluates to 0.5+0.5i
",
        ),

        "abs" => (
            "Absolute value operation.",
"
Absolute value operation for single numbers or a list of numbers. If a single
list is passed, operates on the contents of the list instead of the list itself.
Alias: `|.|`

Expected form:
(abs <values>...)

Example:
q>> (abs -5) ; evaluates to 5
q>> (abs (-4 3.2 3+4i)) ; evaluates to (4 3.2 5.0)
",
        ),

        "sqrt" => (
            "Square-root operation.",
"
Square-root operation for single numbers or a list of numbers. If a single list
is passed, operates on the contents of the list instead of the list itself.
Output values are always floats, complexes, or lists thereof.

Expected form:
(sqrt <values>...)

Example:
q>> (sqrt 2) ; evaluates to 1.4142135623720951
q>> (sqrt (-1 -1+0i)) ; evaluates to (NaN 0+1i)
",
        ),

        "cbrt" => (
            "Cube-root operation.",
"
Cube-root operation for single numbers or a list of numbers. If a single list
is passed, operates on the contents of the list instead of the list itself.
Output values are always floats, complexes, or lists thereof.

Expected form:
(cbrt <values>...)

Example:
q>> (cbrt 2) ; evaluates to 1.2599210498948734
q>> (cbrt (-1 -1+0i)) ; evaluates to (-1, 0.5+0.8660254037844386i)
",
        ),

        "exp" => (
            "Exponential function.",
"
Exponential function for single numbers or a list of numbers. If a single list
is passed, operates on the contents of the list instead of the list itself.
Output values are always floats, complexes, or lists thereof.
Alias: `e**`

Expected form:
(exp <values>...)

Example:
q>> (exp 1) ; evaluates to 2.718281828459045
q>> (exp (* 1i PI)) ; evaluates to -1+0i
",
        ),

        "floor" => (
            "Floor operation.",
"
Floor operation for single numbers or a list of numbers. If a single list is
passed, operates on the contents of the list instead of the list itself. Output
values are always ints or lists thereof.
Alias: `~_`

Expected form:
(floor <values>...)

Example:
q>> (floor 5.2) ; evaluates to 5
q>> (floor (2.25 -5.2 -7.9)) ; evaluates to (2 -6 -8)
",
        ),

        "ceil" => (
            "Ceiling operation.",
"
Ceiling operation for single numbers or a list of numbers. If a single list is
passed, operates on the contents of the list instead of the list itself. Output
values are always ints or lists thereof.
Alias: `~^`

Expected form:
(ceil <values>...)

Example:
q>> (ceil 5.2) ; evaluates to 6
q>> (ceil (2.25 -5.2 -7.9)) ; evaluates to (3 -5 -7)
",
        ),

        "round" => (
            "Round to the nearest int.",
"
Round real numbers to the nearest integer. If a single list is passed, operates
on the contents of the list instead of the list itself. Output values are always
ints or lists thereof.
Alias: `~:`

Expected form:
(round <values>...)

Example:
q>> (round 5.5 6.5 -7.5) ; evaluates to (6 7 -8)
q>> (round (4.2 -7.8 1.9) ; evaluates to (4 -8 2)
",
        ),

        "ln" => (
            "Natural logarithm function.",
"
Natural logarithm function. For complex input z, this function satisfies
    -π ≤ arg(ln(z)) ≤ π.
If a single list is passed, operates on the contents of the list instead of the
list itself. Outputs are always floats, complexes, or lists thereof.

Expected form:
(ln <values>...)

Example:
q>> (ln 2) ; evaluates to 0.6931471805599453
q>> (ln 1j) ; evaluates to 0+1.5707963267948966i
q>> (ln (1 2j)) ; evaluates to (0 0.6931471805599453+1.5707963267948966i)
",
        ),

        "sin" => (
            "Sine function.",
"
Sine function. If a single list is passed, operates on the contents of the list
instead of the list itself. Outputs are always floats, complexes, or lists
thereof.

Expected form:
(sin <values>...)

Example:
q>> (sin (0 (/ PI 2))) ; evaluates to (0 1)
",
        ),

        "cos" => (
            "Cosine function.",
"
Cosine function. If a single list is passed, operates on the contents of the
list instead of the list itself. Outputs are always floats, complexes, or lists
thereof.

Expected form:
(cos <values>...)

Example:
q>> (cos (0 (/ PI 2))) ; evaluates to (1 0)
",
        ),

        "tan" => (
            "Tangent function.",
"
Tangent function. If a single list is passed, operates on the contents of the
list instead of the list itself. Outputs are always floats, complexes, or lists
thereof.

Expected form:
(tan <values>...)

Example:
q>> (tan (0 (/ PI 4))) ; evaluates to (0 1)
",
        ),

        "arcsin" => (
            "Arcsine function.",
"
Arcsine function. If a single list is passed, operates on the contents of the
list instead of the list itself. Outputs are always floats, complexes, or lists
thereof.
Alias: `asin`

Expected form:
(arcsin <values>...)

Example:
q>> (arcsin (0 1)) ; evaluates to (0 1.5707963267948966)
",
        ),

        "arccos" => (
            "Arccosine function.",
"
Arccosine function. If a single list is passed, operates on the contents of the
list instead of the list itself. Outputs are always floats, complexes, or lists
thereof.
Alias: `acos`

Expected form:
(arccos <values>...)

Example:
q>> (arccos (0 1)) ; evaluates to (1.5707963267948966 0)
",
        ),

        "arctan" => (
            "Arctangent function.",
"
Arctangent function. If a single list is passed, operates on the contents of the
list instead of the list itself. Outputs are always floats, complexes, or lists
thereof.
Alias: `atan`

Expected form:
(arctan <values>...)

Example:
q>> (arctan (0 1)) ; evaluates to (0 0.7853981633974483)
",
        ),

        "arctan2" => (
            "Four-quadrant arctangent function.",
"
Four-quadrant arctangent function. Expects arguments to be two-item lists with
the first being the y-coordinate. If a single list of length not equal to 2 is
passed, operates on the contents of the list instead of the list itself. Outputs
are always floats, complexes, or lists thereof.
Alias: `atan2`

Expected form:
(arctan2 <values>...)

Example:
q>> (arctan2 (1 1)) ; evaluates to 0.7853981633974483
q>> (arctan2 (-1 -1)) ; evaluates to -2.356194490192345
",
        ),

        "sinh" => (
            "Hyperbolic sine function.",
"
Hyperbolic sine function. If a single list is passed, operates on the contents
of the list instead of the the list itself. Outputs are always floats,
complexes, or lists thereof.

Expected form:
(sinh <values>...)

Example:
q>> (sinh (0 1)) ; evaluates to (0 1.1752011936438014)
",
        ),

        "cosh" => (
            "Hyperbolic cosine function.",
"
Hyperbolic cosine function. If a single list is passed, operates on the contents
of the list instead of the list itself. Outputs are always floats, complexes,
or lists thereof.

Expected form:
(cosh <values>...)

Example:
q>> (cosh (0 1)) ; evaluates to (1 1.5430806348152437)
",
        ),

        "tanh" => (
            "Hyperbolic tangent function.",
"
Hyperbolic tangent function. If a single list is passed, operates on the
contents of the list instead of the list itself. Outputs are always floats,
complexes, or lists thereof.

Expected form:
(tanh <values>...)

Example:
q>> (tanh (0 1)) ; evaluates to (0 0.7615941559557649)
",
        ),

        "arsinh" => (
            "Area hyperbolic sine function.",
"
Area hyperbolic sine function. If a single list is passed, operates on the
contents of the list instead of the list itself. Outputs are always floats,
complexes, or lists thereof.
Alias: `asinh`

Expected form:
(arsinh <values>...)

Example:
q>> (arsinh (0 1)) ; evaluates to (0 0.8813735870195429)
",
        ),

        "arcosh" => (
            "Area hyperbolic cosine function.",
"
Area hyperbolic cosine function. If a single list is passed, operates on the
contents of the list instead of the list itself. Outputs are always floats,
complexes, or lists thereof.
Alias: `acosh`

Expected form:
(arcosh <values>...)

Example:
q>> (arcosh (0 1)) ; evaluates to (NaN 0)
",
        ),

        "artanh" => (
            "Area hyperbolilc tangent function.",
"
Area hyperbolic tangent function. If a single list is passed, operates on the
contents of the list instead of the list itself. Outputs are always floats,
complexes, or lists thereof.
Alias: `atanh`

Expected form:
(artanh <values>...)

Example:
q>> (artanh (0 1)) ; evaluates to (0 inf)
",
        ),

        "arg" => (
            "Complex number argument.",
"
Returns the argument (polar angle) of a complex number. Returned values are
restricted to the range (-π, +π]. If a single list is passed, operates on the
contents of the list instead of the list itself. Outputs are always floats or
lists thereof.

Expected form:
(arg <values>...)

Example:
q>> (arg 1i) ; evaluates to 1.5707963267948966
q>> (arg (0 -1)) ; evaluates to (NaN 3.141592653589793)
",
        ),

        "cis" => (
            "Cis function.",
"
Cis (cosine-i-sine) function. If a single list is passed, operates on the
contents of the list instead of the list itself. Outputs are always complexes or
lists thereof.
Alias: `e**i`

Expected form:
(cis <values>...)

Example:
q>> (cis (0 (/ PI 2) PI)) ; evaluates to (1+0i 0+1i -1+0i)
",
        ),

        "conj" => (
            "Complex conjugate operation.",
"
Complex conjugate operation. If a single list is passed, operates on the
contents of the list instead of the list itself.
Alias: `~z`

Expected form:
(conj <values>...)

Example:
q>> (conj (1+1i 5)) ; evaluates to (1-1i 5)
",
        ),

        "real" => (
            "Real part of a number.",
"
Get the real part of a number. If a single list is passed, operates on the
contents of the list instead of the list itself.
Alias: `Re`

Expected form:
(real <values>...)

Example:
q>> (real (1+1i 1+2i 2+2i)) ; evaluates to (1.0 1.0 2.0)
",
        ),

        "imag" => (
            "Imaginary part of a number.",
"
Get the imaginary part of a number. If a single list is passed, operates on the
contents of the list instead of the list itself.
Alias: `Im`

Expected form:
(imag <values>...)

Example:
q>> (imag (1+1i 1+2i 2+2i)) ; evaluates to (1.0 2.0 2.0)
",
        ),


/*
 * parameterized element-wise math
 */

        "mod" => (
            "Modulo operator.",
"
Modulo operator for single numbers or lists of numbers. Returned values are
non-negative.
Alias: `%`

Expected form:
(mod <value> <modulo>)

Example:
q>> (mod -5.5 2.5) ; evaluates to 2.0
q>> (mod (range 5 15) 10) ; evaluates to (5 6 7 8 9 0 1 2 3 4)
q>> (mod 10 (range 5 15)) ; evaluates to (0 4 3 2 1 0 10 10 10 10)
q>> (mod (3 4 5) (3 4 5)) ; evluates to (0 0 0)
",
        ),

        "log" => (
            "Logarithm operation.",
"
Logarithm operation for single numbers or lists of numbers. Returned values are
always floats, complexes, or lists thereof.

Expected form:
(log <base> <value>)

Example:
q>> (log 2 128) ; evaluates to 7.0
q>> (log 2 (1 2 4 8 16 32)) ; evaluates to (0.0 1.0 2.0 3.0 4.0 5.0)
q>> (log (2 E 10) 100) ; evaluates to (6.643856189774725, 4.605170185988092, 2)
q>> (log (4 5 6) (4 5 6)) ; evaluates to (1.0 1.0 1.0)
",
        ),

        "pow" => (
            "Exponentiation with arbitrary base.",
"
Exponentiation with arbitrary base.
Alias: `**`

Expected form:
(pow <base> <exponent>)

Example:
q>> (pow 2 8) ; evaluates to 256
q>> (pow 2 (0 1 2 3 4 5)) ; evaluates to (1 2 4 8 16 32)
q>> (pow (1 2 3 4 5) 2) ; evaluates to (1 4 9 16 25)
q>> (pow (1 2 3 4) (1 2 3 4)) ; evaluates to (1 4 27 256)
",
        ),

        "shl" => (
            "Bit-shift left.",
"
Bit-shift left for single numbers or lists of numbers. Overflow is prevented by
automatically reducing the shift size mod 64. Arguments must be bools, ints, or
lists thereof.
Alias: `<<`

Expected form:
(shl <value> <shift size>)

Example:
q>> (shl 1 7) ; evaluates to 128
q>> (shl 1 (1 2 3 4 5)) ; evaluates to (2 4 8 16 32)
q>> (shl (1 2 3 4 5) 2) ; evaluates to (4 8 12 16 20)
q>> (shl (1 2 3 4 5) (1 2 3 4 5)) ; evaluates to (2 8 24 64 160)
",
        ),

        "shr" => (
            "Bit-shift right.",
"
Bit-shift right for single numbers or lists of numbers. Overflow is prevented by
automatically reducing the shift size mod 64. Arguments must be bools, ints, or
lists thereof.
Alias: `>>`

Expected form:
(shr <value> <shift size>)

Example:
q>> (shr 128 7) ; evaluates to 1
q>> (shr 128 (1 2 3 4 5)) ; evaluates to (64 32 16 8 4)
q>> (shr (16 32 64 128 256) 2) ; evaluates to (4 8 16 32 64)
q>> (shr (16 32 64 128 256) (4 5 6 7 8)) ; evaluates to (1 1 1 1 1)
",
        ),

/*
 * list -> list math
 */

        "convolve" => (
            "Discrete-valued convolution.",
"
Discrete-valued convolution operation between two lists of numbers. Returns
values obtained from all points of non-zero overlap; the returned list contains
N + M - 1 values, where N and M are the lengths of the two argument lists.
Alias: `<:>`

Expected form:
(convolve <list1> <list2>)

Example:
q>> (convolve (1 2 3 4 5) (6 7 8)) ; evaluates to (6 19 40 61 82 67 40)
",
        ),

        "hist" => (
            "Count elements of a data set in a histogram.",
"
Count elements of a data set in a histogram. Returns a list containing pairs
where, in each pair, the first element is a value and the second is the number
of times it occurs in the data set.
Alias: `|#|`

Expected form:
(hist <data>)

Example:
q>> (hist (\"a\" \"b\" \"c\" \"a\")) ; evaluates to ((\"a\" 2) (\"b\" 1) (\"c\" 1))
",
        ),

        "hist-prob" => (
            "Count elements of a data set as probabilities in a histogram.",
"
Count elements of a data set as probabilities in a histogram. Returns a list
containins pairs where, in each pair, the first element is a value and the
second is the number of times it occurs in the data set, normalized to the
length of the data set.
Alias: `|p|`

Expected form:
(hist-prob <data>)

Example:
q>> (hist-prob (\"a\" \"b\" \"c\" \"a\")) ; evaluates to ((\"a\" 0.5) (\"b\" 0.25) (\"c\" 0.25))
",
        ),

        "covariance" => (
            "Covariance matrix for N-dimensional data.",
"
Calculates the NxN covariance matrix for N-dimensional data, passed as a list of
N-item lists of numbers.
Alias: `Cov`

Expected form:
(covariance <data>)

Example:
q>> (covariance ((1 2) (1 1) (2 5) (2 4))) ; evaluates to ((0.25 0.75) (0.75 2.5))
",
        ),

        "correlation" => (
            "Pearson correlation matrix for N-dimensional data.",
"
Calculates the NxN Pearson correlation matrix for N-dimensional data, passed as
a list of N-item lists of numbers.
Alias: `Corr`

Expected form:
(correlation <data>)

Example:
q>> (correlation ((1 2) (1 1) (2 5) (2 4))) ; evaluates to ((1 0.948) (0.948 1))
",
        ),

        // fft
        // ifft

/*
 * list -> value math
 */

        "mean" => (
            "Mean of a data set.",
"
Finds the mean of a (scalar) data set.

Expected form:
(mean <data>)

Example:
q>> (mean (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 4.384615384615385
",
        ),

        "variance" => (
            "Variance of a data set.",
"
Finds the variance of a (scalar) data set.
Alias: `Var`

Expected form:
(variance <data>)

Example:
q>> (variance (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 6.236686390532544
",
        ),

        "stddev" => (
            "Standard deviation of a data set.",
"
Finds the standard deviation of a (scalar) data set.
Alias: `Std`

Expected form:
(stddev <data>)

Example:
q>> (stddev (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 2.497335858576604
",
        ),

/*
 * list+1 -> value math
 */

        "pnorm" => (
            "P-norm of a N-dimensional value.",
"
Finds the p-norm of an N-dimensional value (passed as a list of numbers) for
given p.
Alias: `|+|`

Expected form:
(pnorm <p> <value>)

Example:
q>> (pnorm 2 (3 4)) ; evaluates to 5.0
q>> (pnorm 1 (3 4)) ; evaluates to 7.0
",
        ),

        "moment" => (
            "N-th moment of a data set.",
"
Finds the N-th moment of a (scalar) data set for given N.

Expected form:
(moment <N> <data>)

Example:
q>> (moment 0 (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 1.0
q>> (moment 1 (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 4.384615384615385
q>> (moment 2 (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 25.46153846153846
",
        ),


/*
 * special-arg math
 */

        // sample

/*
 * REPL-only
 */

        "vars" => (
            "List all defined symbols in an environment.",
"
List all defined symbols as strings within the local environment or any loaded
modules. REPL-only.

Expected form:
(vars [<modules>...])

Example:
q>> (vars) ; list all symbols in the local environment
q>> (module my-module (...))
q>> (vars my-module) ; list all symbols in my-module
",
        ),

        "defs" => (
            "List all non-default definitions in an environment.",
"
List all user-defined symbols as strings within the local environment or any
loaded modules. REPL-only.

Expected form:
(defs [<modules>...])

Example:
q>> (defs) ; list all user-defined symbols in the local environment
q>> (module my-module (...))
q>> (defs my-module) ; list all user-defined symbols in my-module
",
        ),

    };

static FUNCTION_SYMBOLS: phf::Map<&'static str, &'static str> = phf_map! {
    // special functions -- keyword-like
    ":="    => "def",
    "*:="   => "let",
    "@:"    => "fn",
    "@:="   => "defn",
    "=>"    => "if",
    // systems
    "$"     => "format",
    "$-"    => "print",
    "$_"    => "println",
    "!!"    => "halt",
    "~?"    => "istype",
    "?~"    => "type",
    // arithmetic
    "+"     => "add",
    "-"     => "sub",
    "*"     => "mul",
    "/"     => "div",
    "//"    => "idiv",
    // boolean comparisons
    "&&"    => "and",
    "||"    => "or",
    "^"     => "xor",
    "="     => "eq",
    "!="    => "neq",
    ">"     => "gt",
    ">="    => "geq",
    "<"     => "lt",
    "<="    => "leq",
    // boolean accumulators
    "&&*"   => "all",
    "||*"   => "any",
    "^*"    => "xany",
    // iterable creation
    ".."    => "range",
    "..="   => "range-inc",
    "#="    => "repeat",
    // iterable accumulation
    "#"     => "length",
    "@."    => "fold",
    "<<"    => "min",
    ">>"    => "max",
    "*@."   => "select-by",
    // iterable slicing and access
    "."     => "get",
    ".:="   => "set",
    "--"    => "slice",
    "--="   => "slice-inc",
    "~~"    => "slice-by",
    "~~="   => "slice-inc-by",
    ".*"    => "pick",
    ".-"    => "first",
    "~."    => "take",
    "~.@"   => "take-while",
    "-."    => "last",
    ".~"    => "skip",
    ".~@"   => "skip-while",
    // iterable transformation
    "~"     => "step-by",
    "##"    => "enumerate",
    "<>"    => "reverse",
    "<#>"   => "cycle",
    "@"     => "map",
    "@!"    => "filter",
    "*!="   => "unique",
    "__"    => "flatten",
    "<*"    => "sort",
    "<@"    => "sort-by",
    ".~."   => "permute",
    // iterable division
    "|."    => "split-at",
    "|@"    => "split-on",
    "|@="   => "split-on-inc",
    // iterable addition
    "+."    => "append",
    ".+"    => "prepend",
    "+.+"   => "insert",
    "++"    => "join",
    "::"    => "zip",
    ":*:"   => "cart",
    // iterable testing
    "*="    => "contains",
    "#*="   => "index-of",
    // element-wise math
    "!"     => "neg",
    "1/"    => "recip",
    "|.|"   => "abs",
    "e**"   => "exp",
    "~_"    => "floor",
    "~^"    => "ceil",
    "~:"    => "round",
    "e**i"  => "cis",
    "~z"    => "conj",
    "Re"    => "real",
    "Im"    => "imag",
    // parameterized element-wise math
    "%"     => "mod",
    "**"    => "pow",
    // list -> list math
    "{*}"   => "convolve",
    "|#|"   => "histogram",
    "|p|"   => "histogram-prob",
    "Cov"   => "covariance",
    "Corr"  => "correlation",
    "{F}"   => "fft",
    "{iF}"  => "ifft",
    // list -> value math
    "{E}"   => "mean",
    "Var"   => "variance",
    "Std"   => "stddev",
    // list+1 -> value math
    "|+|"   => "pnorm",
    "{En}"  => "moment",
    // special-arg math
    "?."    => "sample",
};

enum ReplOut {
    Print(QExp),
    NoPrint(QExp),
}

impl ReplOut {
    fn as_print(exp: QExp) -> Self { Self::Print(exp) }

    fn as_noprint(exp: QExp) -> Self { Self::NoPrint(exp) }

    fn unpack(self) -> QExp {
        return match self {
            ReplOut::Print(exp) => {
                if let qstr!(s) = &exp {
                    println_flush!("\"{}\"", s);
                } else {
                    println_flush!("{}", exp);
                }
                exp
            },
            ReplOut::NoPrint(exp) => exp,
        };
    }

    fn unpack_unchecked(self) -> QExp {
        return match self {
            ReplOut::Print(exp) | ReplOut::NoPrint(exp) => exp
        };
    }

    fn print(self) -> ReplOut {
        return match self {
            ReplOut::Print(_) => self,
            ReplOut::NoPrint(exp) => ReplOut::Print(exp),
        };
    }

    fn noprint(self) -> ReplOut {
        return match self {
            ReplOut::Print(exp) => ReplOut::NoPrint(exp),
            ReplOut::NoPrint(_) => self,
        };
    }
}

trait ReplEnv {
    const NOPRINT: &'static [&'static str];

    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut>;

    fn repl_eval_forms(&mut self, args: &[QExp]) -> QResult<Vec<ReplOut>>;

    fn repl_parse_eval(&mut self, expr: String) -> QResult<Vec<ReplOut>>;

    fn repl_eval_help(&mut self, args: &[QExp]) -> QResult<QExp>;

    fn repl_eval_vars(&mut self, args: &[QExp]) -> QResult<QExp>;

    fn repl_eval_defs(&mut self, args: &[QExp]) -> QResult<QExp>;

    fn repl_func(&mut self, exp: &QExp, args: &[QExp])
        -> Option<QResult<ReplOut>>;
}

impl<'a> ReplEnv for QEnv<'a> {
    const NOPRINT: &'static [&'static str] = &[
        "help",     "?",
        "def",      ":=",
        "defn",     "@:=",
        "print",    "$-",
        "println",  "$_",
        "halt",     "!!",
        "use",
        "use-all",  "use*",
        "interact",
    ];

    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut> {
        return match exp {
            qlist!(list) => {
                if let Some(first_form) = list.first() {
                    match self.repl_func(first_form, &list[1..]) {
                        Some(res) => res,
                        None => {
                            match first_form {
                                qsymbol!(s) => {
                                    if Self::NOPRINT.contains(&s.as_ref()) {
                                        self.eval(exp)
                                            .map(ReplOut::as_noprint)
                                    } else {
                                        self.eval(exp)
                                            .map(ReplOut::as_print)
                                    }
                                },
                                _ => self.eval(exp).map(ReplOut::as_print),
                            }
                        },
                    }
                } else {
                    self.eval(exp).map(ReplOut::as_print)
                }
            },
            _ => self.eval(exp).map(ReplOut::as_print),
        };
    }

    fn repl_parse_eval(&mut self, expr: String) -> QResult<Vec<ReplOut>> {
        let exps: Vec<QExp> = parse(&tokenize(expr)?)?;
        let evaled: Vec<ReplOut> = self.repl_eval_forms(&exps)?;
        return Ok(evaled);
    }

    fn repl_eval_forms(&mut self, args: &[QExp]) -> QResult<Vec<ReplOut>> {
        return args.iter().map(|x| self.repl_eval(x)).collect();
    }

    fn repl_eval_help(&mut self, args: &[QExp]) -> QResult<QExp> {
        if args.is_empty() {
            let mut topics: Vec<(&'static str, &'static str)>
                = (&HELP_TEXT).into_iter()
                .map(|(t, (s, _))| (*t, *s))
                .collect();
            topics.sort_by(|l, r| l.0.cmp(r.0));
            println_flush!("{}", "-".repeat(80));
            println!(
"Help: Print information on various topics.\n\
Type `(help <topics>...)` for more information on one or more specific topic.\n\
Available topics:"
            );
            for (topic, text) in topics.into_iter() {
                println_flush!("{:>14}    {}", topic, text);
            }
            println_flush!("{}", "-".repeat(80));
            return Ok(qbool!(true));
        }
        for arg in args.iter() {
            println_flush!("{}", "-".repeat(80));
            match arg {
                qstr!(s) | qsymbol!(s) => {
                    if let Some((_, help_text)) = HELP_TEXT.get(s) {
                        println_flush!("Help for {}:\n{}", s, help_text);
                    } else if let Some(unalias) = FUNCTION_SYMBOLS.get(s) {
                        if let Some((_, help_text)) = HELP_TEXT.get(unalias) {
                            println_flush!(
                                "Help for {} ({}):\n{}",
                                s, unalias, help_text
                            );
                        } else {
                            panic!("missing alias");
                        }
                    } else {
                        return Err(qerr_fmt!(
                            "help: no help text for symbol '{}'", s));
                    }
                },
                _ => {
                    return Err(qerr!("help: args must be symbols or strs"));
                },
            }
        }
        println_flush!("{}", "-".repeat(80));
        return Ok(qbool!(true));
    }

    fn repl_eval_vars(&mut self, args: &[QExp]) -> QResult<QExp> {
        return if args.is_empty() {
            Ok(qlist!(
                self.symbols().into_iter().map(|s| qstr!(s)).collect()
            ))
        } else {
            let res: Vec<QExp>
                = args.iter()
                .map(|qk| match qk {
                    qsymbol!(s) => match self.get_ok(s) {
                        Ok(QEnvEntry::Mod(m)) => Ok(m.symbols()),
                        Ok(QEnvEntry::Exp(_))
                            => Err(qerr!("vars: args must be loaded modules")),
                        Err(e) => Err(e.prepend_source("vars")),
                    },
                    _ => Err(qerr!("vars: args must be symbols")),
                })
                .collect::<QResult<Vec<Vec<String>>>>()?
                .into_iter()
                .map(|ss| qlist!(ss.into_iter().map(|s| qstr!(s)).collect()))
                .collect();
            if res.len() == 1 {
                Ok(res.into_iter().next().unwrap())
            } else {
                Ok(qlist!(res))
            }
        };
    }

    fn repl_eval_defs(&mut self, args: &[QExp]) -> QResult<QExp> {
        return if args.is_empty() {
            Ok(qlist!(
                self.symbols()
                    .into_iter()
                    .filter_map(|s| {
                        (!PROTECTED.contains(&s.as_ref()))
                            .then_some(qstr!(s))
                    })
                    .collect()
            ))
        } else {
            let res: Vec<QExp>
                = args.iter()
                .map(|qk| match qk {
                    qsymbol!(s) => match self.get_ok(s) {
                        Ok(QEnvEntry::Mod(m)) => {
                            Ok(
                                m.symbols()
                                .into_iter()
                                .filter_map(|s| {
                                    (!PROTECTED.contains(&s.as_ref()))
                                        .then_some(s)
                                })
                                .collect()
                            )
                        },
                        Ok(QEnvEntry::Exp(_))
                            => Err(qerr!("defs: args must be loaded modules")),
                        Err(e) => Err(e.prepend_source("vars")),
                    },
                    _ => Err(qerr!("defs: args must be symbols")),
                })
                .collect::<QResult<Vec<Vec<String>>>>()?
                .into_iter()
                .map(|ss| qlist!(ss.into_iter().map(|s| qstr!(s)).collect()))
                .collect();
            if res.len() == 1 {
                Ok(res.into_iter().next().unwrap())
            } else {
                Ok(qlist!(res))
            }
        };
    }

    fn repl_func(&mut self, exp: &QExp, args: &[QExp])
        -> Option<QResult<ReplOut>>
    {
        return if let qsymbol!(s) = exp {
            match s.as_ref() {
                "help" | "?"
                    => Some(self.repl_eval_help(args).map(ReplOut::as_noprint)),
                "vars"
                    => Some(self.repl_eval_vars(args).map(ReplOut::as_print)),
                "defs"
                    => Some(self.repl_eval_defs(args).map(ReplOut::as_print)),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct QPrompt { }

impl reed::Prompt for QPrompt {
    fn render_prompt_left(&self) -> Cow<str> { Cow::from("q") }

    fn render_prompt_right(&self) -> Cow<str> { Cow::from("") }

    fn render_prompt_indicator(&self, prompt_mode: reed::PromptEditMode)
        -> Cow<str>
    {
        return match prompt_mode {
            reed::PromptEditMode::Default => Cow::from(">> "),
            reed::PromptEditMode::Emacs => Cow::from(">> "),
            reed::PromptEditMode::Vi(vimode) => match vimode {
                    reed::PromptViMode::Normal => Cow::from("|> "),
                    reed::PromptViMode::Insert => Cow::from(">> "),
            },
            reed::PromptEditMode::Custom(_) => Cow::from("c> "),
        };
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        return Cow::from("... ");
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reed::PromptHistorySearch,
    ) -> Cow<str>
    {
        return Cow::from("history: ");
    }
}

pub fn run_repl(env: &mut QEnv) {
    let mut line_editor = reed::Reedline::create();
    let prompt = QPrompt { };
    println!(
        "Welcome to the QLisp interpreter REPL. Type `(help)` for more \
        information."
    );
    loop {
        match line_editor.read_line(&prompt) {
            Ok(reed::Signal::Success(s)) => {
                let (expr, _comment): (String, String)
                    = match s.split_once(';') {
                        Some((e, c)) => (e.to_string(), c.to_string()),
                        None => (s, "".to_string()),
                    };
                if ["quit", "exit"].contains(&expr.trim()) {
                    break;
                }
                match env.repl_parse_eval(expr) {
                    Ok(res) => {
                        res.into_iter().for_each(|r| { r.unpack(); });
                    },
                    Err(e) => match e {
                        QErr::Reason(msg)
                            => { println_flush!("Error: {}", msg); },
                    },
                }
            },
            Ok(reed::Signal::CtrlD) => {
                break;
            },
            Ok(reed::Signal::CtrlC) => {
                continue;
            },
            x => { println_flush!("Error: {:?}", x); },
        }
    }
}

