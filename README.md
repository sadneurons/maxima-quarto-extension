# Maxima Quarto Knitr Extension

Standalone repository extracted from prototyping work in `MathBSc`.

## Structure

- `_extensions/maxima/` - Quarto extension source (`knitr-engine.R`, Lua filter, metadata)
- `tests/` - Regression test runner for knitr parity and session/TeX behavior
- `examples/` - Runnable example and validation documents

## Quick Start

```bash
cd /home/tenebris/maxima-quarto-extension
./tests/run_regressions.sh
```

## Notes

- Prototype PDFs and generated artifacts were intentionally not copied.
- Documentation can be rewritten from scratch in this repository.
