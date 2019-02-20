# User Documentation

Techelson's user documentation uses [mdbook]. If you don't have [rust] installed, you can [install]
it with

```
curl https://sh.rustup.rs -sSf | sh
```

Next, install `mdbook` and `mdbook-linkcheck`:

```
cargo install mdbook mdbook-linkcheck
```

That's it. Then:
- `mdbook build` will build the book in `book/`
- `mdbook build -o` will do the same and open the book in your browser
- `mdbook serve` will deploy the book's website locally (check the output for information)

> **NB**: the additional `mdbook-linkcheck` makes sure all the (hyper)links in the book actually
> point to something.

## Contributing

The architecture of the book should be pretty straightforward. If you need to add examples created
a new directory in `rsc` with an appropriate name `<dir>`, if none exist. Techelson contracts go in
`rsc/<dir>/contracts/.`: test script `test.sh` will make sure all contracts pass techelson's
parsing.

Next, testcases go to `rsc/<dir>/okay/.` or `rsc/<dir>/error/.` depending on what the output should
be. If fact, the testcases in there **ignored** by `test.sh`. The test script checks for files
ending with `.techel.output`. The first line of these files must be a bash command starting with
`"$ "`, and all lines after that represent the expected output of this command.

> **Warning**: currently, `test.sh` assumes the command starts `"$ techelson ..."` since the
> primary goal of `*.techel.output` files is to check the output of a techelson run.

[mdbook]: https://github.com/rust-lang-nursery/mdBook (mdBook github repository)
[rust]: https://www.rust-lang.org (Rust's official website)
[install]: https://www.rust-lang.org/tools/install (Rust's install guidelines)