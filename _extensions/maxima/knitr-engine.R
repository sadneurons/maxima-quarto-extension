# Maxima knitr engine for Quarto
# Enhanced version with full feature support

# Session management - store persistent Maxima state
.maxima_sessions <- new.env(parent = emptyenv())

# Helper function to get or create session state file
get_session_file <- function(session_id) {
  if (!exists(session_id, envir = .maxima_sessions)) {
    # Use /tmp directly instead of tempfile() since session files need to persist across knitr chunks
    session_hash <- digest::digest(paste(session_id, Sys.time()), algo = "md5")
    session_file <- file.path("/tmp", sprintf("maxima_session_%s_%s.mac", session_id, substr(session_hash, 1, 13)))
    .maxima_sessions[[session_id]] <- list(
      file = session_file,
      initialized = FALSE
    )
  }
  .maxima_sessions[[session_id]]
}

# Helper function to format Maxima errors
format_maxima_error <- function(result, code) {
  error_msg <- paste(result, collapse = "\n")
  
  # Check for common error patterns
  if (grepl("incorrect syntax", error_msg, ignore.case = TRUE)) {
    # Extract the problematic part
    error_location <- gsub(".*incorrect syntax:\\s*", "", error_msg)
    error_location <- strsplit(error_location, "\\n")[[1]][1]
    return(sprintf("Syntax Error: %s\n\nIn code:\n%s", error_location, code))
  } else if (grepl("undefined", error_msg, ignore.case = TRUE)) {
    return(sprintf("Undefined symbol or function\n\nIn code:\n%s\n\nOutput:\n%s", code, error_msg))
  } else if (grepl("error", error_msg, ignore.case = TRUE)) {
    return(sprintf("Maxima Error:\n%s\n\nIn code:\n%s", error_msg, code))
  }
  
  return(error_msg)
}

# Helper function to mark session as initialized and return
finalize_session <- function(session_id, save_session_cmd) {
  if (!is.null(session_id) && !is.null(save_session_cmd)) {
    if (exists(session_id, envir = .maxima_sessions)) {
      .maxima_sessions[[session_id]]$initialized <- TRUE
    }
  }
}

# Extract complete TeX display-math blocks from Maxima output.
# Maxima may emit `$$ ... $$` across multiple lines (e.g. matrices).
extract_tex_blocks <- function(lines) {
  blocks <- character(0)
  current <- character(0)
  in_block <- FALSE
  
  for (line in lines) {
    dollar_hits <- gregexpr("\\$\\$", line, perl = TRUE)[[1]]
    dollar_count <- if (length(dollar_hits) == 1 && dollar_hits[1] == -1) 0 else length(dollar_hits)
    
    if (!in_block) {
      if (dollar_count > 0) {
        current <- line
        # Two (or more) $$ tokens on one line means a complete block.
        if (dollar_count >= 2) {
          blocks <- c(blocks, current)
          current <- character(0)
        } else {
          in_block <- TRUE
        }
      }
    } else {
      current <- c(current, line)
      if (dollar_count > 0) {
        blocks <- c(blocks, paste(current, collapse = "\n"))
        current <- character(0)
        in_block <- FALSE
      }
    }
  }
  
  # Return any partial block as-is so output is debuggable rather than dropped.
  if (in_block && length(current) > 0) {
    blocks <- c(blocks, paste(current, collapse = "\n"))
  }
  
  blocks
}

# Build a simple cache key for the chunk based on code and options.
build_cache_key <- function(code, init_commands, options) {
  # Avoid caching when sessions are used (stateful execution).
  if (!is.null(options$session) && options$session != FALSE) {
    return(NULL)
  }
  cache_opts <- options
  cache_opts$code <- NULL
  cache_opts$engine <- NULL
  digest::digest(
    list(code = code, init = init_commands, options = cache_opts, engine = "maxima"),
    algo = "md5"
  )
}

get_cache_root <- function(options) {
  if (!is.null(options$cache.path)) {
    return(as.character(options$cache.path)[1])
  }
  "cache/"
}

# Resolve engine.* options that may be provided globally or per-engine.
resolve_engine_option <- function(value, engine = "maxima") {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.list(value) && !is.null(names(value)) && !is.null(value[[engine]])) {
    return(value[[engine]])
  }
  value
}

resolve_engine_path <- function(options, engine = "maxima", default = "maxima") {
  path_opt <- resolve_engine_option(options$engine.path, engine)
  if (is.null(path_opt)) {
    return(default)
  }
  if (is.list(path_opt)) {
    path_opt <- unlist(path_opt, use.names = FALSE)
  }
  path_opt <- as.character(path_opt)
  if (length(path_opt) == 0 || !nzchar(path_opt[1])) {
    return(default)
  }
  path_opt[1]
}

resolve_engine_opts <- function(options, engine = "maxima") {
  opts <- resolve_engine_option(options$engine.opts, engine)
  if (is.null(opts)) {
    return(character(0))
  }
  as.character(unlist(opts, use.names = FALSE))
}

resolve_engine_env <- function(options, engine = "maxima") {
  env_opt <- resolve_engine_option(options$engine.env, engine)
  if (is.null(env_opt)) {
    return(character(0))
  }
  if (is.list(env_opt)) {
    env_opt <- unlist(env_opt, use.names = TRUE)
  }
  if (is.character(env_opt) && is.null(names(env_opt))) {
    return(env_opt)
  }
  if (!is.null(names(env_opt))) {
    return(sprintf("%s=%s", names(env_opt), as.character(env_opt)))
  }
  as.character(env_opt)
}

# Main Maxima engine
eng_maxima <- function(options) {
  # Get the code from the chunk
  code <- paste(options$code, collapse = "\n")
  original_code <- code
  
  # Respect standard knitr semantics: do not execute when eval = FALSE.
  if (isFALSE(options$eval)) {
    return(knitr::engine_output(options, original_code, character(0)))
  }
  
  # Initialize variables
  init_commands <- c()
  post_process <- NULL
  plot_file <- NULL
  
  # 1. LOAD PACKAGES OR CUSTOM INIT
  if (!is.null(options$load)) {
    packages <- if (is.character(options$load)) options$load else as.character(options$load)
    for (pkg in packages) {
      init_commands <- c(init_commands, sprintf('load("%s")', pkg))
    }
  }
  
  # Custom initialization commands
  if (!is.null(options$init)) {
    init_commands <- c(init_commands, options$init)
  }
  
  # 2. SESSION PERSISTENCE
  session_state <- NULL
  session_id <- NULL
  save_session_cmd <- NULL
  if (!is.null(options$session) && options$session != FALSE) {
    session_id <- if (is.character(options$session)) options$session else "default"
    session_state <- get_session_file(session_id)
    
    # Load previous session state
    if (session_state$initialized && file.exists(session_state$file)) {
      init_commands <- c(init_commands, sprintf('loadfile("%s")', session_state$file))
    }
    
    # We'll save at the end, after all code execution
    save_session_cmd <- sprintf('save("%s", all)', session_state$file)
  }
  
  # 3. FLOAT VS EXACT MATH
  # Only apply to result, not to definitions
  apply_float <- !is.null(options$float) && options$float == TRUE
  apply_bfloat <- !is.null(options$bfloat) && options$bfloat == TRUE
  
  if (apply_bfloat) {
    # High precision floating point
    if (!is.null(options$fpprec)) {
      init_commands <- c(init_commands, sprintf("fpprec : %d", as.integer(options$fpprec)))
    }
  }
  
  # 4. AUTOMATIC SIMPLIFICATION
  simplify_funcs <- c("simplify", "ratsimp", "factor", "expand", "trigsimp", 
                      "radcan", "fullratsimp", "trigreduce")
  for (func in simplify_funcs) {
    if (!is.null(options[[func]]) && options[[func]] == TRUE) {
      code <- sprintf("%s(%s)", func, sub(";\\s*$", "", code))
      break  # Only apply one simplification
    }
  }
  
  # 5. OUTPUT FORMAT OPTIONS
  output_format <- if (!is.null(options$format)) options$format else "tex"
  use_tex <- (!is.null(options$tex) && options$tex == TRUE) || output_format == "tex"
  
  # Handle different output formats
  format_wrapper <- switch(output_format,
    "tex" = "tex",
    "mathml" = "mathml",
    "fortran" = "fortran",
    "c" = "c",
    "string" = "string",
    "ascii" = NULL,  # Default Maxima output
    "tex"  # Default to tex
  )
  
  # Apply wrapping only if not already wrapped and if there's actual computation
  # Don't wrap assignments, simple statements, or multi-line code with load statements
  should_wrap <- !grepl(sprintf("^%s\\(", format_wrapper), code) && 
                 !grepl("^\\s*[a-zA-Z_][a-zA-Z0-9_]*\\s*:", code) &&  # Not an assignment
                 !grepl("load\\s*\\(|\\n", code)  # Not multi-statement with load or multi-line
  
  if (!is.null(format_wrapper) && should_wrap) {
    # Remove semicolon, wrap, then add it back
    code_no_semi <- sub(";\\s*$", "", code)
    
    # For float/bfloat, wrap each statement
    if (apply_float) {
      code <- sprintf("float(%s);", code_no_semi)
    } else if (apply_bfloat) {
      code <- sprintf("bfloat(%s);", code_no_semi)  
    } else {
      code <- sprintf("%s(%s);", format_wrapper, code_no_semi)
    }
  } else if (!grepl(";\\s*$", code)) {
    # Just ensure semicolon
    code <- paste0(code, ";")
  }
  
  # 6. PLOT SUPPORT
  plot_use_draw <- FALSE
  plot_files <- c()  # Track multiple plots
  if (!is.null(options$plot) && options$plot == TRUE) {
    # Use knitr's figure path conventions
    # Get the base filename from current input
    input_file <- knitr::current_input()
    if (is.null(input_file) || input_file == "") {
      base_name <- "document"
    } else {
      base_name <- sub("\\.[Qq]md$", "", basename(input_file))
    }
    
    # Create meaningful filename from chunk label or use counter
    chunk_label <- options$label
    if (is.null(chunk_label) || chunk_label == "") {
      chunk_label <- paste0("unnamed-chunk-", options$chunk.index %||% 1)
    }
    
    # Determine figure format - support multiple formats
    fig_dev <- if (!is.null(options$fig.dev)) options$fig.dev else if (!is.null(options$dev)) options$dev else "png"
    fig_ext <- switch(fig_dev,
                     "png" = "png",
                     "pdf" = "pdf",
                     "svg" = "svg",
                     "eps" = "eps",
                     "postscript" = "eps",
                     "png")  # Default fallback
    
    # Respect knitr's fig.path when provided; otherwise use Quarto-like default.
    fig_path_opt <- if (!is.null(options$fig.path)) as.character(options$fig.path)[1] else NULL
    if (!is.null(fig_path_opt) && nzchar(fig_path_opt)) {
      plot_file <- paste0(fig_path_opt, chunk_label, ".", fig_ext)
    } else {
      fig_dir <- paste0(base_name, "_files/figure-pdf")
      plot_file <- file.path(fig_dir, paste0(chunk_label, ".", fig_ext))
    }
    
    fig_dir <- dirname(plot_file)
    if (!dir.exists(fig_dir)) {
      dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    plot_width <- if (!is.null(options$plot_width)) options$plot_width else 800
    plot_height <- if (!is.null(options$plot_height)) options$plot_height else 600
    
    # Determine plot backend: gnuplot (default) or draw
    plot_backend <- if (!is.null(options$plot_backend)) options$plot_backend else "gnuplot"
    
    if (plot_backend == "draw") {
      # Load draw package
      init_commands <- c(init_commands, 'load("draw")')
      plot_use_draw <- TRUE
      
      # Get gnuplot terminal for draw package
      draw_terminal <- switch(fig_dev,
                             "png" = "png",
                             "pdf" = "pdfcairo",
                             "svg" = "svg",
                             "eps" = "epslatex",
                             "png")
      
      # For draw package, check if code contains draw2d or draw3d
      if (grepl("draw[23]d\\s*\\(", code)) {
        # Check if it already has terminal/file_name settings
        has_terminal <- grepl("terminal\\s*=", code)
        has_filename <- grepl("file_name\\s*=", code)
        
        if (!has_terminal && !has_filename) {
          # draw package adds extension automatically in some cases, so remove it from our path
          file_base <- sub(sprintf("\\.%s$", fig_ext), "", plot_file)
          
          # Replace draw2d( or draw3d( with draw2d(terminal=..., file_name=..., dimensions=...,
          code <- gsub("(draw[23]d)\\s*\\(", 
                      sprintf("\\1(terminal=%s, file_name=\"%s\", dimensions=[%d,%d], ",
                              draw_terminal, file_base, plot_width, plot_height),
                      code)
          
          # Update plot_file to what draw will actually create
          if (fig_dev == "png" || fig_dev == "svg") {
            plot_file <- paste0(file_base, ".", fig_ext)
          } else {
            plot_file <- file_base  # PDF/EPS may handle differently
          }
        }
      }
    } else {
      # Use gnuplot (default)
      gnuplot_term <- switch(fig_dev,
                            "png" = sprintf("png size %d,%d", plot_width, plot_height),
                            "pdf" = sprintf("pdfcairo size %dcm,%dcm", plot_width/100, plot_height/100),
                            "svg" = sprintf("svg size %d,%d", plot_width, plot_height),
                            "eps" = sprintf("postscript eps color size %dcm,%dcm", plot_width/100, plot_height/100),
                            sprintf("png size %d,%d", plot_width, plot_height))
      
      init_commands <- c(init_commands, 
                        sprintf('set_plot_option([gnuplot_term, "%s"])', gnuplot_term),
                        sprintf('set_plot_option([gnuplot_out_file, "%s"])', plot_file))
    }
    
    plot_files <- c(plot_files, plot_file)
  }
  
  # Combine all commands
  full_code <- c(init_commands, code)
  
  # Save session state if requested (at the very end)
  if (!is.null(save_session_cmd)) {
    full_code <- c(full_code, save_session_cmd)
  }
  
  # Strip trailing semicolons from all lines (paste will add them back)
  full_code <- gsub(";\\s*$", "", full_code)
  
  # Build the maxima command
  batch_string <- paste(full_code, collapse = "; ")
  # Add trailing semicolon - required by maxima to complete parsing
  batch_string <- paste0(batch_string, ";")
  # Replace newlines with spaces to keep everything on one line
  batch_string <- gsub("\n", " ", batch_string)
  
  # Cache handling (skip execution when possible).
  cache_enabled <- isTRUE(options$cache)
  cache_key <- if (cache_enabled) build_cache_key(code, init_commands, options) else NULL
  cache_root <- if (!is.null(cache_key)) get_cache_root(options) else NULL
  cache_file <- if (!is.null(cache_root) && !is.null(cache_key)) {
    dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
    file.path(cache_root, paste0("maxima_", cache_key, ".rds"))
  } else {
    NULL
  }
  cache_plot_dir <- if (!is.null(cache_root) && !is.null(cache_key)) {
    file.path(cache_root, paste0("maxima_", cache_key, "_plots"))
  } else {
    NULL
  }
  
  used_cache <- FALSE
  cached_result <- NULL
  cached_output <- NULL
  cached_plot_files <- NULL
  
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    cached_output <- cached$output %||% character(0)
    cached_result <- cached$result %||% character(0)
    cached_plot_files <- cached$plot_files %||% character(0)
    used_cache <- TRUE
    
    # Restore cached plots into current target paths.
    if (length(plot_files) > 0 && length(cached_plot_files) > 0) {
      dir.create(dirname(plot_files[1]), recursive = TRUE, showWarnings = FALSE)
      for (i in seq_along(plot_files)) {
        if (i <= length(cached_plot_files) && file.exists(cached_plot_files[i])) {
          dir.create(dirname(plot_files[i]), recursive = TRUE, showWarnings = FALSE)
          file.copy(cached_plot_files[i], plot_files[i], overwrite = TRUE)
        }
      }
    }
  }
  
  # Honor standard knitr engine options for executable path/flags/environment.
  maxima_cmd <- resolve_engine_path(options)
  maxima_args <- c("--very-quiet", resolve_engine_opts(options))
  maxima_env <- resolve_engine_env(options)
  
  # 7. EXECUTE MAXIMA WITH ERROR HANDLING
  if (!used_cache) {
    result <- tryCatch({
      system2(
        maxima_cmd,
        args = maxima_args,
        input = batch_string,
        stdout = TRUE,
        stderr = TRUE,
        env = maxima_env
      )
    }, error = function(e) {
      structure(
        paste("System error executing Maxima:", e$message),
        class = "maxima_system_error"
      )
    })
  } else {
    result <- cached_result %||% character(0)
  }
  
  if (inherits(result, "maxima_system_error")) {
    if (isTRUE(options$error)) {
      return(knitr::engine_output(options, original_code, as.character(result)))
    }
    stop(as.character(result), call. = FALSE)
  }
  
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    status_msg <- c(sprintf("Maxima exited with status %s.", status), result)
    if (isTRUE(options$error)) {
      return(knitr::engine_output(options, original_code, status_msg))
    }
    stop(paste(status_msg, collapse = "\n"), call. = FALSE)
  }
  
  # 8. PROCESS OUTPUT AND FILTERING
  output <- if (used_cache) cached_output %||% character(0) else result
  
  # Always filter out session save/load file paths from output
  if (!is.null(session_state)) {
    output <- output[!grepl(session_state$file, output, fixed = TRUE)]
  }
  
  # Filter out the "false" return value from tex() function
  # tex() prints its output and returns false
  output <- output[!grepl("^\\s*false\\s*$", output)]
  
  # Filter warnings and messages if requested (BEFORE error checking)
  if (!is.null(options$warning) && options$warning == FALSE) {
    # Filter out common Maxima warnings
    output <- output[!grepl("Warning:|warning:", output, ignore.case = TRUE)]
  }
  
  if (!is.null(options$message) && options$message == FALSE) {
    # Filter out info messages, load messages, and file paths
    output <- output[!grepl("Info:|Note:|loading|read and interpret", output, ignore.case = TRUE)]
    # Filter out file paths (load output) - may have leading whitespace
    output <- output[!grepl("^\\s*/.*\\.lisp$", output)]
    output <- output[!grepl("^\\s*/usr/share/maxima", output)]
  }
  
  # Check for Maxima errors (after filtering, so errors can be hidden if warning=FALSE)
  # For plots, ignore plot-specific errors as the plot may still succeed
  if (length(plot_files) > 0) {
    # Filter out plot-related errors/warnings (plot may succeed despite these)
    output <- output[!grepl("expt: undefined|plot2d: expression evaluates|plot3d: expression evaluates|non-numeric value", output, ignore.case = TRUE)]
    has_error <- any(grepl("incorrect syntax|Syntax Error", output, ignore.case = TRUE))
  } else {
    has_error <- any(grepl("incorrect syntax|Syntax Error|^\\s*-- an error", output, ignore.case = TRUE))
  }
  
  if (has_error) {
    error_output <- format_maxima_error(output, original_code)
    finalize_session(session_id, save_session_cmd)
    if (isTRUE(options$error)) {
      return(knitr::engine_output(options, original_code, error_output))
    }
    stop(error_output, call. = FALSE)
  }
  
  # Handle plot output - Maxima returns the filename in the output
  if (length(plot_files) > 0) {
    plots_markdown <- c()
    
    for (i in seq_along(plot_files)) {
      plot_file <- plot_files[i]
      # Check if plot file was mentioned in output or actually exists
      plot_mentioned <- any(grepl(plot_file, result, fixed = TRUE))
      if (file.exists(plot_file) || plot_mentioned) {
        # File is already in the correct location (figure directory)
        # Make path relative to document root for markdown
        rel_plot_path <- plot_file
        
        # Build markdown with proper Quarto/Pandoc attributes
        fig_attrs <- c()
        
        # Add figure alignment
        if (!is.null(options$fig.align)) {
          align_val <- options$fig.align
          if (align_val %in% c("center", "left", "right")) {
            fig_attrs <- c(fig_attrs, sprintf("fig-align=\"%s\"", align_val))
          }
        }
        
        # Add output width/height
        if (!is.null(options$out.width)) {
          fig_attrs <- c(fig_attrs, sprintf("width=\"%s\"", options$out.width))
        } else if (!is.null(options$fig.width)) {
          fig_attrs <- c(fig_attrs, sprintf("width=\"%d%%\"", as.integer(options$fig.width * 10)))
        } else {
          fig_attrs <- c(fig_attrs, "width=\"80%\"")
        }
        
        if (!is.null(options$out.height)) {
          fig_attrs <- c(fig_attrs, sprintf("height=\"%s\"", options$out.height))
        }
        
        # Add label/id if specified
        if (!is.null(options$label)) {
          label_suffix <- if (length(plot_files) > 1) sprintf("-%d", i) else ""
          fig_attrs <- c(fig_attrs, sprintf("#%s%s", options$label, label_suffix))
        }
        
        # Build the attributes string
        attrs_str <- if (length(fig_attrs) > 0) {
          sprintf("{%s}", paste(fig_attrs, collapse=" "))
        } else {
          "{width=\"80%\"}"
        }
        
        # Use caption from fig-cap or fig.cap
        caption <- if (!is.null(options$fig.cap)) {
          # Handle multiple captions for multiple plots
          if (length(options$fig.cap) > 1 && i <= length(options$fig.cap)) {
            options$fig.cap[i]
          } else if (length(options$fig.cap) == 1) {
            options$fig.cap
          } else {
            sprintf("Maxima Plot %d", i)
          }
        } else if (!is.null(options[["fig-cap"]])) {
          options[["fig-cap"]]
        } else {
          sprintf("Maxima Plot %d", i)
        }
        
        # Create proper markdown figure
        plot_md <- sprintf("![%s](%s)%s", 
                          caption, 
                          rel_plot_path,
                          attrs_str)
        plots_markdown <- c(plots_markdown, plot_md)
      }
    }
    
    if (length(plots_markdown) > 0) {
      fig_show <- if (!is.null(options$fig.show)) {
        tolower(as.character(options$fig.show)[1])
      } else {
        "asis"
      }
      
      if (fig_show == "hide") {
        finalize_session(session_id, save_session_cmd)
        return(knitr::engine_output(
          options,
          if (isTRUE(options$echo)) original_code else character(0),
          character(0)
        ))
      }
      
      # Combine multiple plots with spacing
      plot_output <- paste(plots_markdown, collapse = "\n\n")
      # Handle fig.show hold by deferring plots to the end.
      if (fig_show == "hold") {
        deferred_plots <- plot_output
      } else {
        # Return with appropriate results type
        opts_copy <- options
        opts_copy$results <- "asis"  # Force asis for plots
        
        finalize_session(session_id, save_session_cmd)
        return(knitr::engine_output(
          opts_copy,
          if (isTRUE(options$echo)) original_code else character(0),
          paste0("\n\n", plot_output, "\n\n")
        ))
      }
    }
  }
  
  # Extract format-specific output
  if (output_format == "tex" || use_tex) {
    # Extract complete LaTeX output blocks delimited by $$ ... $$
    latex_blocks <- extract_tex_blocks(result)
    if (length(latex_blocks) > 0) {
      output <- latex_blocks
    }
  } else if (output_format == "mathml") {
    # Extract MathML output
    mathml_start <- grep("<math", result)
    mathml_end <- grep("</math>", result)
    if (length(mathml_start) > 0 && length(mathml_end) > 0) {
      output <- result[mathml_start[1]:mathml_end[length(mathml_end)]]
    }
  } else if (output_format %in% c("fortran", "c", "string")) {
    # Keep all output for code generation
    output <- result
  }
  
  # Filter out Maxima noise
  output <- output[!output %in% c("false", "true", "")]
  output <- output[!grepl("^\\(%[io]\\d+\\)", output)]  # Remove input/output labels
  
  # Remove empty lines
  output <- output[nzchar(trimws(output))]
  
  # Handle results option
  results_type <- if (!is.null(options$results)) options$results else "markup"
  
  if (results_type == "hide") {
    # Don't show output, only code (if echo=TRUE)
    output <- character(0)
  } else if (results_type == "hold") {
    # Hold results until end (already the default behavior)
    # No change needed
  } else if (results_type == "asis") {
    # Return output as-is for direct markdown/LaTeX inclusion
    # Already handled correctly
  } else if (results_type == "markup") {
    # Wrap output in code block (knitr default)
    # knitr::engine_output handles this automatically
  }
  
  # If plots were deferred via fig.show: hold, append them now.
  if (exists("deferred_plots", inherits = FALSE) && length(deferred_plots) > 0) {
    if (results_type == "markup" && length(output) > 0) {
      text_output <- paste(output, collapse = "\n")
      output <- c(
        paste0("```\n", text_output, "\n```"),
        "",
        deferred_plots
      )
      results_type <- "asis"
    } else {
      output <- c(output, "", deferred_plots)
      results_type <- "asis"
    }
    options$results <- results_type
  }
  
  # Honor include: false (run but do not include in output).
  if (isFALSE(options$include)) {
    finalize_session(session_id, save_session_cmd)
    return(knitr::engine_output(options, character(0), character(0)))
  }
  
  # Persist cache on successful completion.
  if (!used_cache && !is.null(cache_file)) {
    if (!is.null(cache_plot_dir) && length(plot_files) > 0) {
      dir.create(cache_plot_dir, recursive = TRUE, showWarnings = FALSE)
      cached_plot_files <- character(0)
      for (i in seq_along(plot_files)) {
        if (file.exists(plot_files[i])) {
          dest <- file.path(cache_plot_dir, basename(plot_files[i]))
          file.copy(plot_files[i], dest, overwrite = TRUE)
          cached_plot_files <- c(cached_plot_files, dest)
        }
      }
    } else {
      cached_plot_files <- character(0)
    }
    saveRDS(
      list(output = output, result = result, plot_files = cached_plot_files),
      cache_file
    )
  }
  
  # Return the output using knitr's engine_output function
  finalize_session(session_id, save_session_cmd)
  knitr::engine_output(options, original_code, output)
}

# Register the engine
knitr::knit_engines$set(maxima = eng_maxima)

# Cleanup function for session files (optional, call at end of document)
cleanup_maxima_sessions <- function() {
  for (session_id in ls(.maxima_sessions)) {
    session <- .maxima_sessions[[session_id]]
    if (file.exists(session$file)) {
      unlink(session$file)
    }
  }
  rm(list = ls(.maxima_sessions), envir = .maxima_sessions)
}
