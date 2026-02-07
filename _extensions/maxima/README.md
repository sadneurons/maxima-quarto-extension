# Maxima Extension for Quarto

This extension provides native Maxima code block support in Quarto documents with extensive features for symbolic computation, plotting, and session management. Feature parity with R and Python knitr engines.

## Installation

To use this extension in a Quarto project:

```bash
quarto add path/to/maxima/extension
```

Or, copy the `_extensions/maxima` directory to your project.

## Basic Usage

Add the extension to your document's YAML front matter:

```yaml
---
title: "My Document"
format: pdf
---
```

Then source the engine in a setup chunk:

```r
source("_extensions/maxima/knitr-engine.R")
```

Use Maxima code blocks:

````markdown
```{maxima}
#| tex: true
#| results: asis
#| echo: false
integrate(x^2, x);
```
````

## Features & Options

## Option Reference

All supported chunk options in one place:

- `bfloat` - High-precision floating point output
- `cache` - Enable/disable caching
- `cache.path` - Cache directory prefix
- `dev` - Alias for `fig.dev`
- `echo` - Show/hide code
- `engine.env` - Environment variables for the Maxima process
- `engine.opts` - Extra CLI arguments passed to Maxima
- `engine.path` - Path to Maxima executable
- `error` - Continue on errors (`true`) or stop (`false`, default)
- `eval` - Execute/skip execution
- `fig.align` - Figure alignment (`left`, `center`, `right`)
- `fig.cap` - Figure caption (alt syntax)
- `fig-cap` - Figure caption
- `fig.dev` - Figure device (`png`, `pdf`, `svg`, `eps`)
- `fig.path` - Figure output path prefix
- `fig.show` - Figure display (`asis`, `hold`, `hide`)
- `fig.width` - Output width multiplier (used for display)
- `format` - Output format (`tex`, `mathml`, `fortran`, `c`, `ascii`, `string`)
- `fpprec` - Precision for `bfloat`
- `include` - Run but include neither code nor output
- `init` - Initialization commands
- `label` - Chunk label used for figure filenames and IDs
- `load` - Load Maxima packages/files
- `message` - Show/hide informational messages
- `out.height` - Output figure height
- `out.width` - Output figure width
- `plot` - Enable plot capture
- `plot_backend` - Plot backend (`gnuplot`, `draw`)
- `plot_height` - Plot height in pixels
- `plot_width` - Plot width in pixels
- `results` - Output handling (`asis`, `markup`, `hide`, `hold`)
- `session` - Persist Maxima state across chunks
- `simplify` - Simplify output
- `ratsimp` - Rational simplification
- `factor` - Factor expressions
- `expand` - Expand expressions
- `trigsimp` - Trigonometric simplification
- `radcan` - Radical canonical form
- `fullratsimp` - Full rational simplification
- `trigreduce` - Trig reduction
- `tex` - LaTeX output
- `warning` - Show/hide warnings

### 1. Output Formats

Control how Maxima outputs results:

- `tex: true` - LaTeX output (default, best for PDF/HTML math)
- `format: "mathml"` - MathML output
- `format: "fortran"` - Fortran code generation
- `format: "c"` - C code generation
- `format: "ascii"` - Plain ASCII output
- `format: "string"` - String representation

**Example:**
````markdown
```{maxima}
#| format: "fortran"
integrate(x^2 + 2*x + 1, x);
```
````

### 2. Results Handling

Control how results are displayed (matching R/Python knitr engines):

- `results: "asis"` - Output directly as-is (for LaTeX/Markdown)
- `results: "markup"` - Wrap output in code blocks (default)
- `results: "hide"` - Suppress output (only show code if echo=true)
- `results: "hold"` - Hold all output until end of chunk

**Example:**
````markdown
```{maxima}
#| results: hide
/* Hidden computation */
a: 42 * 2;
```

```{maxima}
#| results: asis
#| tex: true
integrate(sin(x), x);
```
````

### 3. Warning and Message Control

Filter warnings and informational messages:

- `warning: true/false` - Show/hide warnings (default: true)
- `message: true/false` - Show/hide messages (default: true)

**Example:**
````markdown
```{maxima}
#| warning: false
#| message: false
load("draw");  /* Load message will be hidden */
```
````

### 4. Automatic Simplification

Apply simplification functions automatically:

- `simplify: true` - General simplification
- `ratsimp: true` - Rational simplification
- `factor: true` - Factor expressions
- `expand: true` - Expand expressions
- `trigsimp: true` - Trigonometric simplification
- `radcan: true` - Radical canonical form
- `fullratsimp: true` - Full rational simplification
- `trigreduce: true` - Reduce trig expressions

**Example:**
````markdown
```{maxima}
#| tex: true
#| trigsimp: true
sin(x)^2 + cos(x)^2;
```
````

### 5. Numerical vs Symbolic Math

- `float: true` - Convert to floating point
- `bfloat: true` - High-precision floating point
- `fpprec: 50` - Set floating point precision (use with bfloat)

**Example:**
````markdown
```{maxima}
#| bfloat: true
#| fpprec: 100
sqrt(2);
```
````

### 6. Session Persistence

Keep variables and definitions across chunks:

- `session: true` - Use default session
- `session: "name"` - Use named session

**Example:**
````markdown
```{maxima}
#| session: true
a: 5;
b: 10;
```

```{maxima}
#| session: true
a + b;  /* Uses values from previous chunk */
```
````

### 7. Loading Packages

Load Maxima packages or custom files:

- `load: "package"` - Load single package
- `load: ["pkg1", "pkg2"]` - Load multiple packages

**Example:**
````markdown
```{maxima}
#| load: ["draw", "descriptive"]
draw2d(explicit(sin(x), x, 0, 2*%pi));
```
````

### 8. Custom Initialization

Run commands before your code:

- `init: "command"` - Single initialization command
- `init: ["cmd1", "cmd2"]` - Multiple commands

**Example:**
````markdown
```{maxima}
#| init: "ratprint: false"
integrate(1/(x^2 + 1), x);
```
````

### 9. Plot Support

Generate and embed plots with full control:

**Basic Plotting Options:**
- `plot: true` - Enable plotting
- `plot_width: 800` - Plot width in pixels for PNG/SVG; used to derive size for PDF/EPS
- `plot_height: 600` - Plot height in pixels for PNG/SVG; used to derive size for PDF/EPS
- `plot_backend: "gnuplot"` - Backend: `"gnuplot"` (default) or `"draw"`

**Figure Format Options:**
- `fig.dev: "png"` - PNG format (default)
- `fig.dev: "pdf"` - PDF format (vector graphics)
- `fig.dev: "svg"` - SVG format (web-friendly vector)
- `fig.dev: "eps"` - EPS format (PostScript)
- `dev: "png"` - Alternative to `fig.dev`

**Figure Display and Paths:**
- `fig.show: "asis"` - Display figures inline (default)
- `fig.show: "hold"` - Defer all figures until end of chunk
- `fig.show: "hide"` - Run plot code but suppress figure output
- `fig.path: "path/"` - Prefix output figure paths (directory must be writable)

Notes:
- When `fig.path` is not set, output goes to `*_files/figure-pdf/` based on the input filename.

**Figure Sizing and Alignment:**
- `fig.align: "center"` - Alignment: "left", "center", or "right"
- `out.width: "80%"` - Output width (percentage or pixels)
- `out.height: "400px"` - Output height
- `fig.width: 8` - Figure width multiplier (1-10)

**Figure Captions and Labels:**
- `fig-cap: "My Caption"` - Figure caption
- `fig.cap: "My Caption"` - Alternative caption syntax
- `label: "myplot"` - Figure identifier (used as-is in the output)

**Example with gnuplot (PNG):**
````markdown
```{maxima}
#| label: fig-sine
#| plot: true
#| fig.dev: png
#| plot_width: 1000
#| plot_height: 600
#| fig-cap: "Sine wave"
#| fig.align: center
#| out.width: "80%"
plot2d(sin(x), [x, -10, 10]);
```
````

**Example with vector formats (PDF/SVG):**
````markdown
```{maxima}
#| label: fig-cosine
#| plot: true
#| fig.dev: pdf
#| fig-cap: "Cosine in PDF format for publication"
#| fig.align: center
plot2d(cos(x), [x, 0, 2*%pi]);
```

```{maxima}
#| label: fig-tan
#| plot: true
#| fig.dev: svg
#| fig-cap: "Tangent in SVG for web"
#| out.width: "70%"
plot2d(tan(x), [x, -%pi, %pi], [y, -5, 5]);
```
````

**Example with draw package:**
````markdown
```{maxima}
#| label: fig-draw
#| plot: true
#| plot_backend: draw
#| fig.dev: png
#| plot_width: 800
#| plot_height: 600
#| fig-cap: "Draw package with custom styling"
#| fig.align: center
draw2d(
  color = red,
  explicit(sin(x), x, -10, 10),
  color = blue,
  explicit(cos(x), x, -10, 10),
  title = "Sine and Cosine",
  grid = true
);
```
````

The `draw` package offers more advanced features:
- Better color control and gradients
- Multiple coordinate systems
- Enhanced grid and axis control
- More plot types (implicit, polar, parametric, etc.)

### 10. Standard Chunk Options

Core knitr options supported by this engine:

- `echo: true/false` - Show/hide code in output
- `eval: true/false` - Execute/skip execution
- `include: false` - Run but include neither code nor output
- `error: true/false` - Continue on errors or stop rendering (default: stop)
- `warning: true/false` - Show/hide warnings
- `message: true/false` - Show/hide informational messages
- `results: "asis"|"markup"|"hide"|"hold"` - Output formatting

**Cache options:**
- `cache: true/false` - Enable/disable caching
- `cache.path: "cache/"` - Cache directory prefix (default: `cache/`)

Caching notes:
- Cache is disabled when `session` is enabled.
- Cached runs restore output and plot files into the current figure path.

**Engine options (knitr standard):**
- `engine.path: "..."` - Override the Maxima executable path
- `engine.opts: ["--flag"]` - Extra CLI arguments passed to Maxima
- `engine.env:` - Environment variables passed to the Maxima process

**Other options used by the engine:**
- `label` - Chunk label, used for plot filenames and figure identifiers
- `chunk.index` - Internal knitr index used when no label is provided

## Complete Examples

### Symbolic Calculus with Clean Output

````markdown
```{maxima}
#| tex: true
#| results: asis
#| echo: false
#| warning: false
#| simplify: true
diff(sin(x)*cos(x), x);
```
````

### Solving Equations

````markdown
```{maxima}
#| tex: true
#| results: asis
#| session: "algebra"
equations: [x + y = 5, 2*x - y = 1];
solution: solve(equations, [x, y]);
```
````

### Linear Algebra

````markdown
```{maxima}
#| load: "eigen"
#| session: "linalg"
M: matrix([1, 2], [3, 4]);
eigenvalues(M);
```
````

### High-Precision Computation

````markdown
```{maxima}
#| bfloat: true
#| fpprec: 200
%pi;
```
````

### Code Generation

````markdown
```{maxima}
#| format: "fortran"
integrate(x^3 * sin(x), x);
```
````

## Inline Maxima

Use the helper function for inline computations:

```r
# In setup chunk
maxima_inline <- function(code) {
  code <- sub(";\\s*$", "", code)
  cmd <- sprintf("maxima --very-quiet --batch-string='tex(%s);'", code)
  result <- system(cmd, intern = TRUE)
  latex <- grep("^\\$\\$", result, value = TRUE)
  if (length(latex) > 0) {
    return(gsub("\\$\\$", "", latex))
  }
  return("")
}
```

Then in text:

```markdown
The derivative is $`r maxima_inline("diff(x^2, x)")`$.
```

## Error Handling

The extension provides helpful error messages:

- Syntax errors show the problematic code location
- Undefined symbols are clearly identified
- All errors include the original code for debugging

Error behavior:

- Default (`error: false`): stop rendering on Maxima errors
- `error: true`: render the error text and continue

## Session Cleanup

To clean up temporary session files at the end of your document:

```r
cleanup_maxima_sessions()
```

## Requirements

- Maxima must be installed and available in PATH
- For plots: gnuplot must be installed
- For LaTeX output: working LaTeX installation (for PDF rendering)

## Regression Testing

Run the regression suite before release:

```bash
cd /home/tenebris/maxima-quarto-extension
./tests/run_regressions.sh
```

The suite validates critical knitr parity behavior:

- `eval: false` skips execution
- `error` behavior matches knitr expectations
- `engine.path`, `engine.opts`, and `engine.env` are honored
- `include: false` suppresses code and output
- `cache: true` skips re-execution on subsequent runs
- `fig.show` controls figure display timing
- `fig.path` controls output figure location
- Session state sharing and multi-line TeX matrix output remain stable

## Tips

1. **Combine options**: Mix and match features for powerful workflows
2. **Use sessions**: For multi-step computations or defining functions
3. **Cache expensive computations**: Use `cache: true` for slow operations
4. **Load packages once**: Use a setup chunk to load common packages

## License

MIT
