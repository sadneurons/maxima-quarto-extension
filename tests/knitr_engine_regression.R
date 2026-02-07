#!/usr/bin/env Rscript

# Regression checks for the custom Maxima knitr engine.
# Runs focused end-to-end renders against temporary QMD files.

`%||%` <- function(x, y) if (is.null(x)) y else x

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- args[startsWith(args, file_arg)]
  if (length(hit) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub(file_arg, "", hit[1])), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
engine_file <- normalizePath(
  file.path(project_dir, "_extensions", "maxima", "knitr-engine.R"),
  winslash = "/",
  mustWork = TRUE
)

quarto_bin <- Sys.getenv("QUARTO_BIN", "quarto")
work_dir <- tempfile("maxima-knitr-regression-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

on.exit({
  unlink(work_dir, recursive = TRUE, force = TRUE)
}, add = TRUE)

cat("Regression workspace:", work_dir, "\n")
cat("Using engine:", engine_file, "\n")
cat("Using Quarto:", quarto_bin, "\n\n")

render_qmd <- function(name, body, to = "gfm") {
  path <- file.path(work_dir, paste0(name, ".qmd"))
  writeLines(body, path, useBytes = TRUE)
  cmd_args <- c("render", basename(path), "--to", to)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(work_dir)
  out <- suppressWarnings(system2(
    quarto_bin,
    args = cmd_args,
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status") %||% 0L
  md_path <- file.path(work_dir, paste0(name, ".md"))
  list(status = as.integer(status), output = out, qmd = path, md = md_path)
}

assertions <- list()
failures <- character(0)

record_assert <- function(condition, label) {
  assertions[[length(assertions) + 1]] <<- list(ok = isTRUE(condition), label = label)
  if (!isTRUE(condition)) failures <<- c(failures, label)
}

chunk_setup <- sprintf(
  paste(
    "```{r}",
    "#| include: false",
    "source(\"%s\")",
    "```",
    sep = "\n"
  ),
  engine_file
)

# 1) eval: false should not execute invalid code.
case_eval_false <- paste(
  "---",
  "title: eval false",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| eval: false",
  "#| echo: true",
  "1 + ;",
  "```",
  sep = "\n"
)

r <- render_qmd("case_eval_false", case_eval_false)
record_assert(r$status == 0, "eval:false render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(grepl("1 \\+ ;", md), "eval:false should still show source code")
  record_assert(!grepl("Syntax Error", md, fixed = TRUE), "eval:false should not execute code")
} else {
  record_assert(FALSE, "eval:false markdown output should exist")
}

# 2) error defaults to stop rendering.
case_error_default <- paste(
  "---",
  "title: error default",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| echo: true",
  "1 + ;",
  "```",
  "",
  "After chunk marker",
  sep = "\n"
)

r <- render_qmd("case_error_default", case_error_default)
record_assert(r$status != 0, "default error behavior should stop render")

# 3) error:true should continue rendering and include error text.
case_error_true <- paste(
  "---",
  "title: error true",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| error: true",
  "#| echo: true",
  "1 + ;",
  "```",
  "",
  "After chunk marker",
  sep = "\n"
)

r <- render_qmd("case_error_true", case_error_true)
record_assert(r$status == 0, "error:true render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(grepl("Syntax Error", md, fixed = TRUE), "error:true should include error output")
  record_assert(grepl("After chunk marker", md, fixed = TRUE), "error:true should continue document")
} else {
  record_assert(FALSE, "error:true markdown output should exist")
}

# 4) engine.path, engine.opts, engine.env should be respected.
wrapper <- file.path(work_dir, "maxima-wrapper.sh")
writeLines(c(
  "#!/usr/bin/env bash",
  "printf '%s\\n' \"$@\" > \"$MAXIMA_WRAPPER_ARGS_LOG\"",
  "printf 'TEST_MAXIMA_ENV=%s\\n' \"$TEST_MAXIMA_ENV\" > \"$MAXIMA_WRAPPER_ENV_LOG\"",
  "exec maxima \"$@\""
), wrapper, useBytes = TRUE)
Sys.chmod(wrapper, mode = "0755")

wrapper_args_log <- file.path(work_dir, "wrapper-args.log")
wrapper_env_log <- file.path(work_dir, "wrapper-env.log")

case_engine_opts <- paste(
  "---",
  "title: engine options",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  sprintf("#| engine.path: \"%s\"", wrapper),
  "#| engine.opts: [\"--version\"]",
  "#| engine.env:",
  sprintf("#|   TEST_MAXIMA_ENV: \"%s\"", "hello-release"),
  sprintf("#|   MAXIMA_WRAPPER_ARGS_LOG: \"%s\"", wrapper_args_log),
  sprintf("#|   MAXIMA_WRAPPER_ENV_LOG: \"%s\"", wrapper_env_log),
  "1+1;",
  "```",
  sep = "\n"
)

r <- render_qmd("case_engine_opts", case_engine_opts)
record_assert(r$status == 0, "engine.path/opts/env render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(grepl("Maxima", md), "engine.opts should affect invocation (--version visible)")
} else {
  record_assert(FALSE, "engine.path/opts/env markdown output should exist")
}
record_assert(file.exists(wrapper_args_log), "wrapper args log should exist")
record_assert(file.exists(wrapper_env_log), "wrapper env log should exist")
if (file.exists(wrapper_args_log)) {
  args_text <- paste(readLines(wrapper_args_log, warn = FALSE), collapse = " ")
  record_assert(grepl("--very-quiet", args_text, fixed = TRUE), "engine invocation should include default --very-quiet")
  record_assert(grepl("--version", args_text, fixed = TRUE), "engine.opts should pass custom argument")
}
if (file.exists(wrapper_env_log)) {
  env_text <- paste(readLines(wrapper_env_log, warn = FALSE), collapse = "\n")
record_assert(grepl("TEST_MAXIMA_ENV=hello-release", env_text, fixed = TRUE), "engine.env should be passed to subprocess")
}

# 4b) include:false should suppress code and output.
case_include_false <- paste(
  "---",
  "title: include false",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| include: false",
  "#| echo: true",
  "1+1;",
  "```",
  "",
  "After include false",
  sep = "\n"
)

r <- render_qmd("case_include_false", case_include_false)
record_assert(r$status == 0, "include:false render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(!grepl("1\\+1", md), "include:false should suppress code")
  record_assert(!grepl("\\$\\$2\\$\\$", md), "include:false should suppress output")
  record_assert(grepl("After include false", md, fixed = TRUE), "include:false should not suppress surrounding content")
} else {
  record_assert(FALSE, "include:false markdown output should exist")
}

# 5) fig.show: hide should suppress image output.
case_fig_show_hide <- paste(
  "---",
  "title: fig show hide",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| plot: true",
  "#| fig.show: hide",
  "#| echo: false",
  "plot2d(sin(x), [x,0,6.28]);",
  "```",
  sep = "\n"
)

r <- render_qmd("case_fig_show_hide", case_fig_show_hide)
record_assert(r$status == 0, "fig.show:hide render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(!grepl("<img", md, fixed = TRUE), "fig.show:hide should suppress HTML image tags")
  record_assert(!grepl("!\\[", md), "fig.show:hide should suppress markdown image links")
} else {
  record_assert(FALSE, "fig.show:hide markdown output should exist")
}

# 6) fig.path should control output location and markdown path.
case_fig_path <- paste(
  "---",
  "title: fig path",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| plot: true",
  "#| fig.path: \"myfigs/\"",
  "#| echo: false",
  "plot2d(cos(x), [x,0,6.28]);",
  "```",
  sep = "\n"
)

r <- render_qmd("case_fig_path", case_fig_path)
record_assert(r$status == 0, "fig.path render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(grepl("myfigs/", md, fixed = TRUE), "fig.path should be used in output markdown")
} else {
  record_assert(FALSE, "fig.path markdown output should exist")
}

myfigs_dir <- file.path(work_dir, "myfigs")
fig_files <- if (dir.exists(myfigs_dir)) {
  list.files(myfigs_dir, pattern = "\\.(png|pdf|svg|eps)$", full.names = TRUE, ignore.case = TRUE)
} else {
  character(0)
}
record_assert(length(fig_files) > 0, "fig.path should create figure files under custom directory")

# 6b) cache:true should skip execution on second render.
cache_dir <- file.path(work_dir, "cache")
cache_counter <- file.path(work_dir, "cache-counter.txt")
cache_wrapper <- file.path(work_dir, "cache-wrapper.sh")
writeLines(c(
  "#!/usr/bin/env bash",
  "count=0",
  "if [ -f \"${MAXIMA_CACHE_COUNTER}\" ]; then",
  "  count=$(cat \"${MAXIMA_CACHE_COUNTER}\")",
  "fi",
  "count=$((count+1))",
  "echo \"$count\" > \"${MAXIMA_CACHE_COUNTER}\"",
  "exec maxima \"$@\""
), cache_wrapper, useBytes = TRUE)
Sys.chmod(cache_wrapper, mode = "0755")

case_cache <- paste(
  "---",
  "title: cache test",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| cache: true",
  sprintf("#| cache.path: \"%s/\"", cache_dir),
  sprintf("#| engine.path: \"%s\"", cache_wrapper),
  "#| engine.env:",
  sprintf("#|   MAXIMA_CACHE_COUNTER: \"%s\"", cache_counter),
  "1+1;",
  "```",
  sep = "\n"
)

r <- render_qmd("case_cache", case_cache)
record_assert(r$status == 0, "cache:true first render should succeed")
count1 <- if (file.exists(cache_counter)) as.integer(readLines(cache_counter, warn = FALSE)[1]) else NA_integer_
record_assert(isTRUE(count1 == 1), "cache:true should execute on first render")

r <- render_qmd("case_cache", case_cache)
record_assert(r$status == 0, "cache:true second render should succeed")
count2 <- if (file.exists(cache_counter)) as.integer(readLines(cache_counter, warn = FALSE)[1]) else NA_integer_
record_assert(isTRUE(count2 == 1), "cache:true should skip execution on second render")

# 7) Session sharing with TeX matrix output should remain intact.
case_session_tex <- paste(
  "---",
  "title: session tex matrix",
  "format: gfm",
  "---",
  "",
  chunk_setup,
  "",
  "```{maxima}",
  "#| session: true",
  "matrix_a: matrix([1,2], [3,4]);",
  "matrix_b: matrix([5,6], [7,8]);",
  "```",
  "",
  "```{maxima}",
  "#| session: true",
  "#| tex: true",
  "#| results: asis",
  "matrix_a . matrix_b",
  "```",
  sep = "\n"
)

r <- render_qmd("case_session_tex", case_session_tex)
record_assert(r$status == 0, "session + tex matrix render should succeed")
if (file.exists(r$md)) {
  md <- paste(readLines(r$md, warn = FALSE), collapse = "\n")
  record_assert(grepl("19&22", md, fixed = TRUE), "session variable state should carry between chunks")
  record_assert(grepl("43&50", md, fixed = TRUE), "session matrix output should include full second row")
} else {
  record_assert(FALSE, "session + tex markdown output should exist")
}

cat("Assertions:\n")
for (i in seq_along(assertions)) {
  prefix <- if (assertions[[i]]$ok) "[PASS]" else "[FAIL]"
  cat(sprintf("  %s %s\n", prefix, assertions[[i]]$label))
}

if (length(failures) > 0) {
  cat("\nRegression checks failed:\n")
  for (f in failures) cat(" -", f, "\n")
  quit(status = 1)
}

cat("\nAll Maxima knitr engine regression checks passed.\n")
