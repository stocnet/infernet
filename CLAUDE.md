# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project overview

`infernet` is an R package (part of the
[stocnet](https://github.com/stocnet) ecosystem) providing the
*inferential layer* for network analysis: conditional uniform graph
(CUG) and quadratic assignment procedure (QAP) tests of network
statistics, and multiple regression QAP (MRQAP). It combines a formula
front end with a range of estimator range, missing-data handling, and
treatments for multimodal, multilevel, and cognitive social structure
networks. It builds on [manynet](https://stocnet.github.io/manynet/)
(network classes and coercion) and
[netrics](https://stocnet.github.io/netrics/) (measures), so its
functions accept matrices, edgelists, [igraph](https://r.igraph.org/),
[network](https://statnet.org/),
[tidygraph](https://tidygraph.data-imaginist.com), or `stocnet` objects,
and one-mode or two-mode networks alike. Plotting results belongs in
[autograph](https://stocnet.github.io/autograph/).

Full package documentation — common dev commands, file organization, the
[`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
pipeline, function body and parallelism conventions, console messaging,
dependency practice, test conventions, `NEWS.md` conventions, and
branching/CI — lives in
[.github/CONTRIBUTING.md](https://stocnet.github.io/infernet/CONTRIBUTING.md).
Read it before adding or restructuring functions, or when you need the
exact `devtools`/testing commands for this repo.

Note that [migraph](https://stocnet.github.io/migraph/) still carries
older copies of
[`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
and the `test_*()` family, which
[infernet](https://stocnet.github.io/infernet/) supersedes. A fix made
in [migraph](https://stocnet.github.io/migraph/) needs porting here; a
fix made here does not need porting back.

## Where to make changes

- Make all changes on the `develop` branch. `develop` is the working
  branch; `main` is the release branch.
- Do not commit to `main` directly. Do not open a new feature branch for
  a fix unless the user asks.
- Keep changes reviewable. Leave them as an uncommitted working-tree
  diff on `develop`, or as local commits on `develop`. Commit only when
  the user asks.
- Do not push to `origin/develop` without explicit approval. `develop`
  is shared.
