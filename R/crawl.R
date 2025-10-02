#' Check that one or more files exist
#'
#' Non-asserting check function that verifies each element of
#' \code{filenames} names an existing file. This follows the checkmate-style
#' contract: it returns \code{TRUE} on success, and a single character string
#' describing the failure otherwise.
#'
#' Note: this function does not throw an R error; it returns a failure message
#' (character scalar) which is suitable for use with
#' \code{checkmate::makeAssertionFunction()}.
#'
#' @param filenames [character] A character vector of file paths to check.
#'   Missing values are allowed (the function will report them as failing).
#'
#' @return \itemize{
#'   \item{\code{TRUE} if every path in \code{filenames} points to an existing
#'         file}
#'   \item{A string describing which inputs do not exist}
#' }
#'
#' @examples
#' \dontrun{
#' check_files_exist(c("/path/to/existing", "/path/to/missing"))
#' }
check_files_exist <- function(filenames) {
  checks <- sapply(filenames, function(file) {
    checkmate::test_file_exists(file)
  })

  if (!all(checks)) {
    # At least one of the files doesn't exist.
    failed_checks <- Filter(rlang::is_false, checks)

    non_existent_files <- names(failed_checks) |>
      paste(collapse = ", ")
    error_message <- paste(
      "The following files do not exist:",
      non_existent_files,
      collapse = " "
    )
    error_message
  } else {
    TRUE
  }
}

#' Assertion function for files existing
#'
#' Assertion wrapper produced via \code{checkmate::makeAssertionFunction()} from
#' \code{check_files_exist}. When used, this assertion will throw an error if
#' any of the supplied file paths do not exist. On success it returns its
#' argument invisibly.
#'
#' @param filenames [character] One or more file paths to check.
#' @template add
#' @template var.name
#'
#' @return Invisibly returns its \code{filenames} argument if all files exist;
#'   otherwise throws an error with a descriptive message.
#'
#' @examples
#' \dontrun{
#' assert_files_exist(c("/path/to/file1", "/path/to/file2"))
#' }
assert_files_exist <- checkmate::makeAssertionFunction(
  check.fun = check_files_exist
)

#' Check that BLAST DB indicator files exist for each base path
#'
#' Given base paths (without extensions), this check verifies that, for every
#' base path, at least one of the following exists:
#' - \code{<base>.nsq} (nucleotide DB indicator), or
#' - \code{<base>.psq} (protein DB indicator).
#'
#' A mixture across inputs is allowed (some bases may have \code{.nsq}, others
#' \code{.psq}, some both). The check fails only for base paths that are
#' missing both.
#'
#' This function performs lightweight validation only and does not attempt to
#' validate other BLAST DB companion files (e.g., \code{.nin}/\code{.nhr} or
#' \code{.pin}/\code{.phr}).
#'
#' This is a checkmate-style check function: it returns \code{TRUE} on success
#' and a single character string describing the problem on failure.
#'
#' @param filenames [character] One or more base paths (without extension) of
#'   the BLAST databases to check. For each base path, the function will look
#'   for either \code{<base>.nsq} or \code{<base>.psq}.
#'
#' @return \itemize{
#'   \item{\code{TRUE} if every base path points to a plausible BLAST DB}
#'   \item{A string describing the failure}
#' }
#'
#' @examples
#' \dontrun{
#' check_blast_dbs_exist(c("/data/db/nt", "/data/db/prot"))
#' }
check_blast_dbs_exist <- function(filenames) {
  # Basic input sanity (non-asserting to comply with checkmate check contract)
  if (!checkmate::test_character(filenames, any.missing = FALSE, min.len = 1)) {
    return(
      "filenames must be a non-empty character vector with no missing values"
    )
  }

  # Disallow passing files with .nsq or .psq extensions. That's definitely a
  # user error (case-insensitive)
  has_forbidden_ext <- grepl("\\.(nsq|psq)$", filenames, ignore.case = TRUE)
  if (any(has_forbidden_ext)) {
    offenders <- unique(filenames[has_forbidden_ext])
    return(
      paste0(
        "Do not include file extensions (.nsq/.psq). Provide base paths only. Problem inputs: ",
        paste(offenders, collapse = ", ")
      )
    )
  }

  nsq_paths <- paste0(filenames, ".nsq")
  psq_paths <- paste0(filenames, ".psq")

  nsq_exists <- vapply(nsq_paths, checkmate::test_file_exists, logical(1))
  psq_exists <- vapply(psq_paths, checkmate::test_file_exists, logical(1))

  # Pass if each base path has at least one of .nsq or .psq
  exists_any <- nsq_exists | psq_exists
  if (all(exists_any)) {
    return(TRUE)
  }

  # Return an informative error message showing the base paths missing both
  missing_bases <- filenames[!exists_any]
  paste0(
    "No .nsq or .psq file found for base paths: ",
    paste(missing_bases, collapse = ", ")
  )
}

#' Assertion that BLAST DB indicator files exist
#'
#' Assertion wrapper generated via \code{checkmate::makeAssertionFunction()} from
#' \code{check_blast_dbs_exist}. This assertion will throw an error if any of
#' the supplied base paths are missing both \code{.nsq} and \code{.psq}.
#' On success it returns its argument invisibly.
#'
#' @param filenames [character] One or more base paths (without extension) of
#'   the BLAST databases to check.
#' @template add
#' @template var.name
#'
#' @return Invisibly returns its \code{filenames} argument if all base paths
#'   have at least one of \code{.nsq} or \code{.psq}; otherwise throws an error
#'   with an informative message.
#'
#' @examples
#' \dontrun{
#' assert_blast_dbs_exist(c("/data/db/nt", "/data/db/prot"))
#' }
assert_blast_dbs_exist <- checkmate::makeAssertionFunction(
  check.fun = check_blast_dbs_exist
)

#' Find a command in PATH or a specific directory
#'
#' Like \code{Sys.which} but accepts a scalar \code{command} and an optional
#' single \code{directory} argument. If \code{directory} is supplied the call
#' checks for the command in that directory only (by prepending the directory
#' to the command before calling \code{Sys.which}).
#'
#' The caller must check if the returned string is empty (meaning the command
#' was not found).
#'
#' @param command \code{[character(1)]}\cr
#'   The command name to look for.
#' @param directory \code{[character(1) | NULL]}\cr
#'   Optional directory to search in.
#'   If non-\code{NULL}, the directory is prepended to \code{command} and the
#'   presence of that path is tested. May be \code{NULL}.
#'
#' @return A single string: the path to the command if found, or the empty
#'   string (\code{""}) if not found.
#'
#' @examples
#' \dontrun{
#' sys_which("blastn")
#' sys_which("blastn", directory = "/usr/local/bin")
#' }
#'
#' @export
sys_which <- function(command, directory = NULL) {
  checkmate::assert_string(command, min.chars = 1)
  checkmate::assert_string(directory, min.chars = 1, null.ok = TRUE)

  # If directory is not null, then the user wants to check for the command in
  # a specific directory.
  if (!rlang::is_null(directory)) {
    command <- file.path(directory, command)
  }

  result <- Sys.which(command)

  result[[1]]
}

#' Parse BLAST short-format specifiers into a character vector
#'
#' Accepts a string containing BLAST "short" format specifiers (space- or
#' non-word-separated) and returns a character vector of individual
#' specifiers. If \code{format_specifiers} is \code{NULL}, empty, or equal to
#' the BLAST "std" keyword, a default set of specifiers is returned.
#'
#' The function validates that the resulting specifiers are known/allowed by
#' checking membership against accepted BLAST specifiers.
#'
#' @param format_specifiers \code{[character(1) | NULL]}\cr
#'   A single string of specifiers, or \code{NULL} to use the default set.
#'
#' @return A character vector of validated format specifier names.
parse_short_format_specifiers <- function(format_specifiers = NULL) {
  checkmate::assert_string(format_specifiers, null.ok = TRUE)

  # In some cases we want to use the default value
  if (
    rlang::is_null(format_specifiers) ||
      format_specifiers == "" ||
      # This comes from BLAST
      format_specifiers == "std"
  ) {
    format_specifiers <- "qaccver saccver pident length mismatch gapopen qstart qend sstart send evalue bitscore"
  }

  splits <- format_specifiers |>
    strsplit("\\W+") |>
    checkmate::assert_list(types = "character", len = 1)

  result <- splits[[1]] |>
    # There should be at least one format specifier, and any present should be non-empty
    checkmate::assert_character(min.len = 1, min.chars = 1) |>
    # There should only be valid specifiers
    checkmate::assert_subset(
      choices = .format_specifiers$format_specifiers$Short
    )

  result
}

#' Run a system process (wrapper for processx::run)
#'
#' Thin wrapper around \code{processx::run} to make calls easier to mock in
#' tests. The function forwards \code{command}, \code{args} and
#' \code{error_on_status}.
#'
#' @param command \code{[character(1)]}\cr
#'   Command to run.
#' @param args \code{[character]}\cr
#'   Command arguments (vector).
#' @param error_on_status \code{[logical(1)]}
#'   If \code{TRUE} an error is thrown when the process exits with a non-zero
#'   status. Defaults to \code{FALSE}.
#'
#' @return The list returned by \code{processx::run} (containing
#'   \code{status}, \code{stdout}, \code{stderr}, etc).
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' run_process("echo", c("hello"))
#' }
run_process <- function(command, args, error_on_status = FALSE) {
  processx::run(
    command = command,
    args = args,
    error_on_status = error_on_status
  )
}

#' Read a BLAST TSV file (wrapper for readr::read_tsv)
#'
#' Thin wrapper around \code{readr::read_tsv} used to make file reading easier
#' to mock in tests.
#'
#' @param file \code{[character(1)]}\cr
#'   Path to the TSV file.
#' @param col_names \code{[logical | character]}\cr
#'   See \code{readr::read_tsv}.
#' @param col_types \code{[cols | NULL | character]}\cr
#'   Column type specification passed \code{readr::read_tsv}.
#'
#' @return A tibble with the parsed TSV contents.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' read_blast_tsv("blast_output.tsv", col_names = TRUE)
#' }
read_blast_tsv <- function(file, col_names, col_types) {
  readr::read_tsv(file = file, col_names = col_names, col_types = col_types)
}

# TODO: somewhere need to document the col types and the long specifiers

#' Crawl BLAST databases with a set of query files
#'
#' Run a BLAST executable for every combination of query file and BLAST DB
#' base path and return the combined parsed results as a tibble. This function
#' constructs the appropriate command-line arguments (including a -outfmt 6
#' short-format specifier), invokes the BLAST executable, parses the TSV BLAST output and binds all results together.
#'
#' The function validates inputs and supports user callbacks for two
#' failure modes: when a BLAST job exits with non-zero status, and when parse
#' of the BLAST stdout fails. Parallel execution across query/DB pairs is
#' supported via the future package, so be sure to set a plan before running this function like \code{future::plan(future::multisession, workers = 2)}.
#'
#' @param blast_executable \code{[character(1)]}\cr
#'   Name of the BLAST executable to run (e.g. \code{"blastn"}, \code{"blastp"}).
#'   If \code{blast_executable_directory} is provided the executable will be
#'   looked up in that directory; otherwise the PATH is searched.
#' @param query_paths \code{[character]}\cr
#'   One or more paths to query files.
#' @param db_paths \code{[character]}\cr
#'   One or more BLAST DB base paths.
#' @param blast_executable_directory \code{[character(1) | NULL]}\cr
#'   Optional directory in which to look for \code{blast_executable}. If
#'   non-\code{NULL} the executable is resolved via \code{file.path(directory,
#'   blast_executable)}. May be \code{NULL}.
#' @param outfmt_specifiers \code{[character(1)]}\cr
#'   Space-separated string of BLAST short-format specifiers to
#'   request (the part after the leading \code{6} in \code{-outfmt}). Use
#'   \code{NULL}, empty string, or the BLAST keyword \code{"std"} to select the
#'   package default set. The default matches BLAST's common short-format
#'   columns: qaccver saccver pident length mismatch gapopen qstart
#'   qend sstart send evalue bitscore.
#' @param extra_blast_arguments \code{[character | NULL]}\cr
#'   Additional command-line arguments passed through to the BLAST command.
#'   This must not include an \code{-outfmt} argument. May be \code{NULL}.
#' @param use_long_names_in_parsed_result \code{[logical(1)]}\cr
#'   If \code{TRUE} the resulting tibble will use long, descriptive column
#'   names mapped from the short-format specifiers. If \code{FALSE} short
#'   specifier names are used as column names.
#' @param job_failed_callback \code{[function]}\cr
#'   Function called when a BLAST process exits with a non-zero status (or a
#'   NA status). Called as \code{job_failed_callback(query_path, db_path,
#'   exit_status, command, args, stderr)}. The default is a no-op function.
#' @param parse_failed_callback \code{[function]}\cr
#'   Function called when parsing the BLAST stdout fails. Called as
#'   \code{parse_failed_callback(query_path, db_path, error_condition, command,
#'   args, stderr)}. The default is a no-op function.
#'
#' @return A tibble combining the parsed BLAST hits from all successful
#'   query/DB runs. Column names correspond to the selected format specifiers
#'   (short or long names, depending on \code{use_long_names_in_parsed_result})
#'   and column types are those defined by the package format-specifier
#'   mapping. The returned tibble may have zero rows (e.g., if all jobs fail or
#'   produce no hits).
#'
#' @examples
#' \dontrun{
#' # Basic usage (assumes blastn is on PATH and DBs exist)
#' results <- crawl(
#'   blast_executable = "blastn",
#'   query_paths = c("queries/a.fasta", "queries/b.fasta"),
#'   db_paths = c("/data/db/nt", "/data/db/custom_db")
#' )
#'
#' # Provide callbacks to log failures
#' crawl(
#'   blast_executable = "blastn",
#'   query_paths = "queries/a.fasta",
#'   db_paths = "dbs/nt",
#'   job_failed_callback = function(query_path, db_path, exit_status, command, args, stderr) {
#'     message("BLAST job failed for ", query_path, " vs ", db_path, ": ", exit_status)
#'   },
#'   parse_failed_callback = function(query_path, db_path, error_condition, command, args, stderr) {
#'     message(
#'       "Failed to parse BLAST output for ",
#'       query_path,
#'       " vs ",
#'       db_path,
#'       ": ",
#'       error_condition$message
#'     )
#'   }
#' )
#' }
#'
#' @export
crawl <- function(
  blast_executable,
  query_paths,
  db_paths,
  blast_executable_directory = NULL,
  # default args for -outfmt 6
  outfmt_specifiers = "qaccver saccver pident length mismatch gapopen qstart qend sstart send evalue bitscore",
  extra_blast_arguments = NULL,
  use_long_names_in_parsed_result = FALSE,
  job_failed_callback = function(
    query_path,
    db_path,
    exit_status,
    command,
    args,
    stderr
  ) {},
  parse_failed_callback = function(
    query_path,
    db_path,
    error_condition,
    command,
    args,
    stderr
  ) {}
) {
  blast_executable <- sys_which(
    command = blast_executable,
    directory = blast_executable_directory
  )
  checkmate::assert_string(blast_executable, min.chars = 1)

  checkmate::assert_character(query_paths, min.len = 1)
  assert_files_exist(query_paths)

  checkmate::assert_character(db_paths, min.len = 1)
  assert_blast_dbs_exist(db_paths)

  checkmate::assert_character(extra_blast_arguments, null.ok = TRUE)
  # Make sure user didn't supply the outfmt arg
  user_provided_outfmt_argument <- any(grepl(
    pattern = "-outfmt",
    x = extra_blast_arguments,
    fixed = TRUE
  ))
  checkmate::assert_false(user_provided_outfmt_argument)

  # Check callbacks
  checkmate::assert_function(
    job_failed_callback,
    args = c(
      "query_path",
      "db_path",
      "exit_status",
      "command",
      "args",
      "stderr"
    )
  )
  checkmate::assert_function(
    parse_failed_callback,
    args = c(
      "query_path",
      "db_path",
      "error_condition",
      "command",
      "args",
      "stderr"
    )
  )

  short_format_specifiers <- parse_short_format_specifiers(outfmt_specifiers)

  if (use_long_names_in_parsed_result) {
    checkmate::assert_subset(
      short_format_specifiers,
      choices = names(.format_specifiers$format_specifier_short_to_long_names)
    )
    long_format_specifiers <- .format_specifiers$format_specifier_short_to_long_names[
      short_format_specifiers
    ]

    blast_result_column_types <- .format_specifiers$format_specifier_types[
      long_format_specifiers
    ]
  } else {
    blast_result_column_types <- .format_specifiers$format_specifier_types[
      short_format_specifiers
    ]
  }

  empty_blast_result_callback <- function() {
    .format_specifiers$empty_blast_table_columns |>
      dplyr::select(names(blast_result_column_types))
  }

  # This works fine even when each argument is length 1
  query_db_pairs <- expand.grid(
    query_path = query_paths,
    db_path = db_paths,
    stringsAsFactors = FALSE
  ) |>
    checkmate::assert_data_frame(
      min.rows = 1,
      types = c("character", "character")
    )
  checkmate::assert_names(
    colnames(query_db_pairs),
    must.include = c("query_path", "db_path")
  )

  blast_hits_list <- future.apply::future_mapply(
    query_db_pairs$query_path,
    query_db_pairs$db_path,
    # These two args make mapply work like lapply so we get back a list
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    # We need to pass in the helper functions directly to the function run in
    # the future so that they can be found in a multiprocessing environment.
    #
    # Do NOT attempt to resolve these with `futures.globals`, it will not work
    # properly.
    MoreArgs = list(
      run_process = run_process,
      read_blast_tsv = read_blast_tsv
    ),
    FUN = function(query_path, db_path, run_process, read_blast_tsv) {
      blast_args <- append(
        c(
          "-db",
          db_path,
          "-query",
          query_path,
          "-outfmt",
          paste(
            c("6", short_format_specifiers),
            collapse = " "
          )
        ),
        extra_blast_arguments
      )

      process_result <- run_process(
        command = blast_executable,
        # NOTE: append handles `extra_blast_arguments` being NULL
        args = blast_args,
        error_on_status = FALSE
      )

      # TODO: what to do with the stderr if it is present? This is ignoring all stderr, which has caused bugs in rCRUX.

      # Need a callback for that too...takes the query-target-stderr

      if (rlang::is_na(process_result$status) || process_result$status != 0) {
        job_failed_callback(
          query_path = query_path,
          db_path = db_path,
          exit_status = process_result$status,
          command = blast_executable,
          args = blast_args,
          stderr = process_result$stderr
        )
        empty_blast_result_callback()
      } else {
        # Try to read the BLAST data
        rlang::try_fetch(
          expr = {
            blast_hit_data <-
              # Wrapping with I() ensures that the string is treated as literal
              # data and not a file name
              I(process_result$stdout) |>
              read_blast_tsv(
                col_names = names(blast_result_column_types),
                col_types = blast_result_column_types
              )
            readr::stop_for_problems(blast_hit_data)
            # NOTE: since we specify col_names and col_types, if there are no
            # rows, this will be the good default value.
            blast_hit_data
          },
          # If there are any problems reading the data, report the error and
          # return
          # the default value.
          error = function(error_condition) {
            parse_failed_callback(
              query_path = query_path,
              db_path = db_path,
              error_condition = error_condition,
              command = blast_executable,
              args = blast_args,
              stderr = process_result$stderr
            )
            empty_blast_result_callback()
          }
        )
      }
    }
  )

  result <- dplyr::bind_rows(blast_hits_list)
  checkmate::assert_names(
    colnames(result),
    permutation.of = names(blast_result_column_types)
  )

  # NOTE: this data frame may have zero rows. The caller will need to handle it.
  result
}
