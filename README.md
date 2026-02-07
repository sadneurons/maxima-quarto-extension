# Maxima Quarto Knitr Extension

Quarto extension that adds a Maxima knitr engine with feature parity expectations for R/Python workflows. It supports symbolic math, LaTeX output, session persistence, caching, and plot capture from both gnuplot and the Maxima `draw` package.

## Highlights

- Maxima chunks in Quarto (` ```{maxima}`` blocks)
- LaTeX/MathML/code-generation output formats
- Session persistence across chunks
- Plot capture with `gnuplot` and `draw` backends
- knitr-compatible options (`eval`, `echo`, `results`, `engine.path`, `engine.env`, etc.)

## Install in a Quarto Project

```bash
quarto add path/to/maxima/extension
```

Or copy `_extensions/maxima` into your project.

In your document, source the engine once:

```r
source("_extensions/maxima/knitr-engine.R")
```

## Quick Start

Run the regression suite:

```bash
cd /home/tenebris/maxima-quarto-extension
./tests/run_regressions.sh
```

Render the showcase PDF:

```bash
cd /home/tenebris/maxima-quarto-extension/examples
TEXMFVAR=/tmp/texmf-var TEXMFCACHE=/tmp/texmf-cache quarto render maxima_showcase.qmd --to pdf
```

## Documentation

The full technical reference and option list lives here:

- `_extensions/maxima/README.md`

## Structure

- `_extensions/maxima/` - Engine implementation (`knitr-engine.R`), Lua glue, metadata
- `tests/` - Regression tests for knitr parity and session/TeX behavior
- `examples/` - Canonical showcase: `examples/maxima_showcase.qmd`

## Notes

- Generated artifacts are not committed.
- The showcase is the primary release-quality example.
