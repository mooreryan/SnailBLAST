expect_checkmate_error <- function(object, ...) {
  testthat::expect_error(object, class = "checkmateError", ...)
}

make_temp_db <- function(pattern = "db", fileext = ".nsq") {
  temp_db_full_path <- tempfile(pattern = pattern, fileext = fileext)
  writeLines("abc", temp_db_full_path)
  sub(
    pattern = paste0(fileext, "$"),
    replacement = "",
    x = temp_db_full_path
  )
}

test_that("assert_files_exist validates single existing file", {
  # Arrange
  temp_file <- tempfile()
  writeLines("test", temp_file)
  on.exit(unlink(temp_file))

  # Act & Assert
  expect_equal(assert_files_exist(temp_file), expected = temp_file)
})

test_that("assert_files_exist validates multiple existing files", {
  # Arrange
  temp_files <- replicate(3, tempfile())
  lapply(temp_files, function(f) writeLines("test", f))
  on.exit(unlink(temp_files))

  # Act & Assert
  expect_equal(assert_files_exist(temp_files), expected = temp_files)
})

test_that("assert_files_exist fails for non-existent single file", {
  # Arrange
  non_existent <- "/path/that/does/not/exist.txt"

  # Act & Assert
  expect_checkmate_error(assert_files_exist(non_existent))
})

test_that("assert_files_exist fails for multiple non-existent files", {
  # Arrange
  non_existent_files <- c("/fake/path1.txt", "/fake/path2.txt")

  # Act & Assert
  expect_checkmate_error(assert_files_exist(non_existent_files))
})

test_that("assert_files_exist fails when some files exist and some don't", {
  # Arrange
  temp_file <- tempfile()
  writeLines("test", temp_file)
  on.exit(unlink(temp_file))
  mixed_files <- c(temp_file, "/fake/nonexistent.txt")

  # Act & Assert
  expect_checkmate_error(assert_files_exist(mixed_files))
})

test_that("assert_blast_dbs_exist validates single base path with .nsq", {
  # Arrange
  base <- tempfile()
  nsq <- paste0(base, ".nsq")
  writeLines("test", nsq)
  on.exit(unlink(nsq))

  # Act & Assert
  expect_equal(assert_blast_dbs_exist(base), expected = base)
})

test_that("assert_blast_dbs_exist validates multiple base paths with .nsq", {
  # Arrange
  bases <- replicate(3, tempfile())
  nsqs <- paste0(bases, ".nsq")
  lapply(nsqs, function(f) writeLines("test", f))
  on.exit(unlink(nsqs))

  # Act & Assert
  expect_equal(assert_blast_dbs_exist(bases), expected = bases)
})

test_that("assert_blast_dbs_exist validates multiple base paths with .psq", {
  # Arrange
  bases <- replicate(2, tempfile())
  psqs <- paste0(bases, ".psq")
  lapply(psqs, function(f) writeLines("test", f))
  on.exit(unlink(psqs))

  # Act & Assert
  expect_equal(assert_blast_dbs_exist(bases), expected = bases)
})

test_that("assert_blast_dbs_exist passes when both .nsq and .psq exist for all bases", {
  # Arrange
  bases <- replicate(2, tempfile())
  nsqs <- paste0(bases, ".nsq")
  psqs <- paste0(bases, ".psq")
  lapply(c(nsqs, psqs), function(f) writeLines("test", f))
  on.exit(unlink(c(nsqs, psqs)))

  # Act & Assert
  expect_equal(assert_blast_dbs_exist(bases), expected = bases)
})

test_that("assert_blast_dbs_exist passes when mixture of .nsq and .psq across bases", {
  # Arrange
  base_nsq_only <- tempfile()
  base_psq_only <- tempfile()
  nsq <- paste0(base_nsq_only, ".nsq")
  psq <- paste0(base_psq_only, ".psq")
  writeLines("test", nsq)
  writeLines("test", psq)
  on.exit(unlink(c(nsq, psq)))

  # Act & Assert
  bases <- c(base_nsq_only, base_psq_only)
  expect_equal(assert_blast_dbs_exist(bases), expected = bases)
})

test_that("assert_blast_dbs_exist fails when none of the bases have .nsq or .psq", {
  # Arrange
  bases <- c(tempfile(), tempfile())

  # Act & Assert (new message)
  expect_checkmate_error(assert_blast_dbs_exist(bases))
})


test_that("assert_blast_dbs_exist rejects inputs with .nsq/.psq extensions (case-insensitive)", {
  # Arrange
  inputs <- c("/path/DB.NSQ", "/path/DB2.PsQ")

  # Act & Assert
  expect_checkmate_error(assert_blast_dbs_exist(inputs))
})

test_that("osenoi", {
  good_temp_db <- make_temp_db()
  bad_temp_db <- "/this/is/a/bad/db"

  expect_checkmate_error(assert_blast_dbs_exist(c(good_temp_db, bad_temp_db)))
})

test_that("sys_which finds command on PATH", {
  # Arrange
  skip_on_os("windows")

  # Act
  result <- sys_which("ls")

  # Assert
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(nchar(result) > 0)
  expect_true(file.exists(result))
})

test_that("sys_which validates input parameters", {
  # Act & Assert
  expect_checkmate_error(sys_which(""))
  expect_checkmate_error(sys_which(123))
  expect_checkmate_error(sys_which("cmd", ""))
  expect_checkmate_error(sys_which("cmd", 123))
})

test_that("sys_which finds command in specific directory", {
  skip_on_os("windows")

  # Arrange
  temp_dir <- tempdir()
  temp_script <- file.path(temp_dir, "test_script")
  writeLines("#!/bin/bash\necho test", temp_script)
  Sys.chmod(temp_script, "0755")
  on.exit(unlink(temp_script))

  # Act
  result <- sys_which("test_script", temp_dir)

  # Assert
  expect_equal(result, temp_script)
})

test_that("sys_which returns the empty string for non-existent command", {
  # Act & Assert
  expect_equal(
    sys_which("definitely_not_a_real_command_12345"),
    expected = ""
  )
})

test_that("parse_short_format_specifiers handles NULL input", {
  # Act
  result <- parse_short_format_specifiers(NULL)

  # Assert
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true(all(nchar(result) > 0))
})

test_that("parse_short_format_specifiers handles empty string", {
  # Act
  result <- parse_short_format_specifiers("")

  # Assert
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_contains(result, "qaccver")
  expect_contains(result, "saccver")
})

test_that("parse_short_format_specifiers handles 'std' keyword", {
  # Act
  result <- parse_short_format_specifiers("std")

  # Assert
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_contains(result, "qaccver")
  expect_contains(result, "saccver")
})

test_that("parse_short_format_specifiers parses valid specifiers", {
  # Act
  result <- parse_short_format_specifiers("qaccver saccver pident evalue")

  # Assert
  expect_equal(result, c("qaccver", "saccver", "pident", "evalue"))
})

test_that("parse_short_format_specifiers validates input type", {
  # Act & Assert
  expect_checkmate_error(parse_short_format_specifiers(123))
  expect_checkmate_error(parse_short_format_specifiers(c("a", "b")))
})

test_that("parse_short_format_specifiers rejects invalid specifiers", {
  # Act & Assert
  expect_checkmate_error(parse_short_format_specifiers("invalid_specifier"))
  expect_checkmate_error(parse_short_format_specifiers(
    "qaccver invalid_spec saccver"
  ))
})

test_that("crawl validates input parameters", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  # blast_executable validation
  expect_checkmate_error(crawl("", temp_query, temp_db))

  with_mocked_bindings(
    {
      # query_paths validation
      expect_checkmate_error(crawl("blastn", character(0), temp_db))
      expect_checkmate_error(crawl(
        "blastn",
        "/nonexistent/query.fasta",
        temp_db
      ))

      # db_paths validation
      expect_checkmate_error(crawl("blastn", temp_query, character(0)))
    },
    sys_which = function(...) "/usr/bin/blastn"
  )
})

test_that("crawl rejects user-provided outfmt argument", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  # Act & Assert
  expect_checkmate_error(
    crawl(
      "blastn",
      temp_query,
      temp_db,
      extra_blast_arguments = c("-outfmt", "7")
    )
  )
})

test_that("crawl validates callback functions", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  # Act & Assert
  expect_checkmate_error(
    crawl(
      "blastn",
      temp_query,
      temp_db,
      job_failed_callback = "not_a_function"
    )
  )

  expect_checkmate_error(
    crawl(
      "blastn",
      temp_query,
      temp_db,
      job_failed_callback = function(wrong_args) {}
    )
  )

  expect_checkmate_error(
    crawl(
      "blastn",
      temp_query,
      temp_db,
      parse_failed_callback = function(wrong_args) {}
    )
  )
})

test_that("crawl handles job failures", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  failed_jobs <- list()

  with_mocked_bindings(
    {
      result <- crawl(
        "blastn",
        temp_query,
        temp_db,
        job_failed_callback = function(query_path, db_path, exit_status) {
          failed_jobs <<- append(
            failed_jobs,
            # This list in list keeps it so we can check individual steps more
            # easily
            list(list(
              query = query_path,
              db = db_path,
              status = exit_status
            ))
          )
        }
      )

      expect_length(failed_jobs, 1)
      expect_equal(failed_jobs[[1]]$query, temp_query)
      expect_equal(failed_jobs[[1]]$db, temp_db)
      expect_equal(failed_jobs[[1]]$status, 1)

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 0)
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(...) list(status = 1, stdout = "")
  )
})

test_that("crawl handles parse failures gracefully", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  parse_failures <- list()

  with_mocked_bindings(
    {
      result <- crawl(
        "blastn",
        temp_query,
        temp_db,
        parse_failed_callback = function(query_path, db_path, error_condition) {
          parse_failures <<- append(
            parse_failures,
            list(list(
              query = query_path,
              db = db_path,
              error = error_condition
            ))
          )
        }
      )

      expect_length(parse_failures, 1)
      expect_equal(parse_failures[[1]]$query, temp_query)
      expect_equal(parse_failures[[1]]$db, temp_db)

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 0)
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(...) {
      list(status = 0, stdout = "invalid\ttab\tseparated\tdata")
    },
    read_blast_tsv = function(...) stop("Parse error")
  )
})

test_that("crawl processes successful BLAST results", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  mock_blast_output <- "query1\tsubject1\t95.5\t100\t2\t0\t1\t100\t1\t100\t1e-50\t200"

  with_mocked_bindings(
    {
      # browser()
      result <- crawl(
        "blastn",
        temp_query,
        temp_db,
        parse_failed_callback = function(
          query_path,
          db_path,
          error_condition
        ) {
          warning(sprintf(
            "Parsing failed for query '%s' against database '%s': %s",
            query_path,
            db_path,
            error_condition$message
          ))
        },
        job_failed_callback = function(query_path, db_path, exit_status) {
          warning("the job failed")
        }
      )

      expect_equal(
        result,
        expected = tibble::tibble(
          qaccver = "query1",
          saccver = "subject1",
          pident = 95.5,
          length = 100,
          mismatch = 2,
          gapopen = 0,
          qstart = 1,
          qend = 100,
          sstart = 1,
          send = 100,
          evalue = 1e-50,
          bitscore = 200
        )
      )
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(...) list(status = 0, stdout = mock_blast_output)
  )
})

test_that("crawl handles empty BLAST results", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  with_mocked_bindings(
    {
      result <- crawl(
        "blastn",
        temp_query,
        temp_db
      )

      expect_equal(
        result,
        # Empty blast data with default columns
        expected = tibble::tibble(
          qaccver = character(),
          saccver = character(),
          pident = double(),
          length = integer(),
          mismatch = integer(),
          gapopen = integer(),
          qstart = integer(),
          qend = integer(),
          sstart = integer(),
          send = integer(),
          evalue = double(),
          bitscore = integer()
        )
      )
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(...) list(status = 0, stdout = "")
  )
})


test_that("crawl supports custom format specifiers", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  with_mocked_bindings(
    {
      result <- crawl(
        "blastn",
        temp_query,
        temp_db,
        outfmt_specifiers = "qaccver saccver pident"
      )

      expect_equal(
        result,
        expected = tibble::tibble(
          qaccver = character(),
          saccver = character(),
          pident = double()
        )
      )
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(...) list(status = 0, stdout = "")
  )
})

test_that("crawl supports long column names", {
  # Arrange
  temp_query <- tempfile(fileext = ".fasta")
  writeLines(">test\nACGT", temp_query)
  on.exit(unlink(temp_query))

  temp_db <- make_temp_db()
  on.exit(unlink(temp_db), add = TRUE)

  with_mocked_bindings(
    {
      result <- crawl(
        "blastn",
        temp_query,
        temp_db,
        outfmt_specifiers = "qaccver saccver pident",
        use_long_names_in_parsed_result = TRUE
      )

      expect_equal(
        result,
        expected = tibble::tibble(
          query_accession_version = character(),
          subject_accession_version = character(),
          percent_identical_matches = double()
        )
      )
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(...) list(status = 0, stdout = "")
  )
})

test_that("crawl creates correct BLAST commands with outfmt specifiers and extra blast arguments", {
  # Arrange
  queries <- c(
    test_path("test_data", "real_queries_1.fasta"),
    test_path("test_data", "real_queries_2.fasta")
  )

  blast_dbs <- c(
    test_path("test_data", "real_db_1"),
    test_path("test_data", "real_db_2")
  )

  full_commands <- NULL

  with_mocked_bindings(
    {
      crawl(
        "blastn",
        queries,
        blast_dbs,
        outfmt_specifiers = "qaccver saccver pident length",
        extra_blast_arguments = c("-evalue", "1e-10", "-max_target_seqs", "5")
      )
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(command, args, ...) {
      full_commands <<- append(
        full_commands,
        list(paste(c(command, args), collapse = " "))
      )
      list(status = 0, stdout = "")
    }
  )

  expect_snapshot(cat(paste(full_commands, collapse = "\n")))
})

test_that("crawl creates correct BLAST commands with outfmt specifiers, extra blast arguments, and long names", {
  # Arrange
  queries <- c(
    test_path("test_data", "real_queries_1.fasta"),
    test_path("test_data", "real_queries_2.fasta")
  )

  blast_dbs <- c(
    test_path("test_data", "real_db_1"),
    test_path("test_data", "real_db_2")
  )

  full_commands <- NULL

  with_mocked_bindings(
    {
      crawl(
        "blastn",
        queries,
        blast_dbs,
        outfmt_specifiers = "qaccver saccver pident length",
        use_long_names_in_parsed_result = TRUE,
        extra_blast_arguments = c("-evalue", "1e-10", "-max_target_seqs", "5")
      )
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(command, args, ...) {
      full_commands <<- append(
        full_commands,
        list(paste(c(command, args), collapse = " "))
      )
      list(status = 0, stdout = "")
    }
  )

  expect_snapshot(cat(paste(full_commands, collapse = "\n")))
})

test_that("crawl handles flaky BLAST", {
  # Arrange
  temp_queries <- c(
    tempfile(fileext = ".1.fasta"),
    tempfile(fileext = ".2.fasta")
  )
  lapply(temp_queries, function(f) writeLines(">test\nACGT", f))
  on.exit(unlink(temp_queries))

  temp_blast_dbs <- c(
    make_temp_db(pattern = "db1."),
    make_temp_db(pattern = "db2.")
  )

  mock_blast_output <- "query1\tsubject1\t95.5\t100\t2\t0\t1\t100\t1\t100\t1e-50\t200"

  expected_blast_hits <- I(paste(rep(mock_blast_output, 2), collapse = "\n")) |>
    readr::read_tsv(
      col_names = c(
        "qaccver",
        "saccver",
        "pident",
        "length",
        "mismatch",
        "gapopen",
        "qstart",
        "qend",
        "sstart",
        "send",
        "evalue",
        "bitscore"
      ),
      show_col_types = FALSE
    )

  with_mocked_bindings(
    {
      result <- crawl("blastn", temp_queries, temp_blast_dbs)
      expect_equal(result, expected_blast_hits)
    },
    sys_which = function(...) "/usr/bin/blastn",
    run_process = function(command, args, ...) {
      is_query_1 <- any(grepl("\\.1\\.fasta", args))
      is_query_2 <- any(grepl("\\.2\\.fasta", args))
      is_db_1 <- any(grepl("db1", args))
      is_db_2 <- any(grepl("db2", args))

      if (is_query_1 && is_db_1) {
        list(status = 1, stdout = "", stderr = "BLAST error occurred")
      } else if (is_query_2 && is_db_2) {
        # No error, but empty blast result
        list(status = 0, stdout = "", stderr = "")
      } else {
        list(status = 0, stdout = mock_blast_output, stderr = "")
      }
    }
  )
})

##############################################################################
# Integration tests with real data ###########################################
##############################################################################

test_that("crawl integration test with real BLAST data", {
  # Skip if BLAST is not available
  skip_if(sys_which("blastn") == "", "blastn not found on PATH")

  # Arrange - use real test data files
  query_files <- c(
    test_path("test_data", "real_queries_1.fasta"),
    test_path("test_data", "real_queries_2.fasta")
  )

  db_files <- c(
    test_path("test_data", "real_db_1"),
    test_path("test_data", "real_db_2")
  )

  # Act - run the actual BLAST search
  result <- crawl(
    blast_executable = "blastn",
    query_paths = query_files,
    db_paths = db_files,
    extra_blast_arguments = c("-evalue", "1e-5")
  )

  # Assert - check the structure and content

  expeceted_types <- list(
    qaccver = "character",
    saccver = "character",
    pident = "double",
    length = "integer",
    mismatch = "integer",
    gapopen = "integer",
    qstart = "integer",
    qend = "integer",
    sstart = "integer",
    send = "integer",
    evalue = "double",
    bitscore = "integer"
  )

  checkmate::expect_data_frame(
    result,
    types = unlist(expeceted_types),
    any.missing = FALSE,
    min.rows = 1
  )

  checkmate::expect_names(
    colnames(result),
    permutation.of = names(expeceted_types)
  )

  # Should respect our evalue filter
  checkmate::expect_double(result$evalue, upper = 1e-5)

  # Read the expected results
  expected_result <- readr::read_tsv(
    test_path("test_data", "expected_real_hits.tsv"),
    col_names = c(
      "qaccver",
      "saccver",
      "pident",
      "length",
      "mismatch",
      "gapopen",
      "qstart",
      "qend",
      "sstart",
      "send",
      "evalue",
      "bitscore"
    ),
    show_col_types = FALSE
  )

  expect_equal(
    dplyr::arrange(result, .data$qaccver, .data$saccver),
    expected = dplyr::arrange(expected_result, .data$qaccver, .data$saccver)
  )
})

test_that("crawl integration test with real BLAST data (multisession plan)", {
  # Skip if BLAST is not available
  skip_if(sys_which("blastn") == "", "blastn not found on PATH")

  run_test <- function() {
    # Modify the future backend only within this function
    with(future::plan(future::multisession, workers = 2), local = TRUE)

    # Arrange - use real test data files
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    # Act - run the actual BLAST search
    result <- crawl(
      blast_executable = "blastn",
      query_paths = query_files,
      db_paths = db_files,
      extra_blast_arguments = c("-evalue", "1e-5")
    )

    # Assert - check the structure and content

    expeceted_types <- list(
      qaccver = "character",
      saccver = "character",
      pident = "double",
      length = "integer",
      mismatch = "integer",
      gapopen = "integer",
      qstart = "integer",
      qend = "integer",
      sstart = "integer",
      send = "integer",
      evalue = "double",
      bitscore = "integer"
    )

    checkmate::expect_data_frame(
      result,
      types = unlist(expeceted_types),
      any.missing = FALSE,
      min.rows = 1
    )

    checkmate::expect_names(
      colnames(result),
      permutation.of = names(expeceted_types)
    )

    # Should respect our evalue filter
    checkmate::expect_double(result$evalue, upper = 1e-5)

    # Read the expected results
    expected_result <- readr::read_tsv(
      test_path("test_data", "expected_real_hits.tsv"),
      col_names = c(
        "qaccver",
        "saccver",
        "pident",
        "length",
        "mismatch",
        "gapopen",
        "qstart",
        "qend",
        "sstart",
        "send",
        "evalue",
        "bitscore"
      ),
      show_col_types = FALSE
    )

    expect_equal(
      dplyr::arrange(result, .data$qaccver, .data$saccver),
      expected = dplyr::arrange(expected_result, .data$qaccver, .data$saccver)
    )
  }

  run_test()
})

test_that("crawl integration test with real BLAST data (multicore plan)", {
  # Multicore plan is not available on windows
  skip_on_os("windows")

  # Skip if BLAST is not available
  skip_if(sys_which("blastn") == "", "blastn not found on PATH")

  run_test <- function() {
    # Modify the future backend only within this function
    with(future::plan(future::multicore, workers = 2), local = TRUE)

    # Arrange - use real test data files
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    # Act - run the actual BLAST search
    result <- crawl(
      blast_executable = "blastn",
      query_paths = query_files,
      db_paths = db_files,
      extra_blast_arguments = c("-evalue", "1e-5")
    )

    # Assert - check the structure and content

    expeceted_types <- list(
      qaccver = "character",
      saccver = "character",
      pident = "double",
      length = "integer",
      mismatch = "integer",
      gapopen = "integer",
      qstart = "integer",
      qend = "integer",
      sstart = "integer",
      send = "integer",
      evalue = "double",
      bitscore = "integer"
    )

    checkmate::expect_data_frame(
      result,
      types = unlist(expeceted_types),
      any.missing = FALSE,
      min.rows = 1
    )

    checkmate::expect_names(
      colnames(result),
      permutation.of = names(expeceted_types)
    )

    # Should respect our evalue filter
    checkmate::expect_double(result$evalue, upper = 1e-5)

    # Read the expected results
    expected_result <- readr::read_tsv(
      test_path("test_data", "expected_real_hits.tsv"),
      col_names = c(
        "qaccver",
        "saccver",
        "pident",
        "length",
        "mismatch",
        "gapopen",
        "qstart",
        "qend",
        "sstart",
        "send",
        "evalue",
        "bitscore"
      ),
      show_col_types = FALSE
    )

    expect_equal(
      dplyr::arrange(result, .data$qaccver, .data$saccver),
      expected = dplyr::arrange(expected_result, .data$qaccver, .data$saccver)
    )
  }

  run_test()
})

test_that("crawl integration test with real BLAST data (custom format spec)", {
  # Skip if BLAST is not available
  skip_if(sys_which("blastn") == "", "blastn not found on PATH")

  # Arrange - use real test data files
  query_files <- c(
    test_path("test_data", "real_queries_1.fasta"),
    test_path("test_data", "real_queries_2.fasta")
  )

  db_files <- c(
    test_path("test_data", "real_db_1"),
    test_path("test_data", "real_db_2")
  )

  # Test with custom format specifiers
  result <- crawl(
    blast_executable = "blastn",
    query_paths = query_files,
    db_paths = db_files,
    outfmt_specifiers = "qaccver saccver pident bitscore"
  )

  checkmate::expect_names(
    colnames(result),
    permutation.of = c("qaccver", "saccver", "pident", "bitscore")
  )

  # Read the expected results
  expected_result <- readr::read_tsv(
    test_path("test_data", "expected_real_hits.tsv"),
    col_names = c(
      "qaccver",
      "saccver",
      "pident",
      "length",
      "mismatch",
      "gapopen",
      "qstart",
      "qend",
      "sstart",
      "send",
      "evalue",
      "bitscore"
    ),
    show_col_types = FALSE
  ) |>
    dplyr::select(c("qaccver", "saccver", "pident", "bitscore"))

  expect_equal(
    dplyr::arrange(result, .data$qaccver, .data$saccver),
    expected = dplyr::arrange(expected_result, .data$qaccver, .data$saccver)
  )
})

test_that("crawl integration test with real BLAST data (long col names)", {
  # Skip if BLAST is not available
  skip_if(sys_which("blastn") == "", "blastn not found on PATH")

  # Arrange - use real test data files
  query_files <- c(
    test_path("test_data", "real_queries_1.fasta"),
    test_path("test_data", "real_queries_2.fasta")
  )

  db_files <- c(
    test_path("test_data", "real_db_1"),
    test_path("test_data", "real_db_2")
  )

  # Test with long column names
  result <- crawl(
    blast_executable = "blastn",
    query_paths = query_files,
    db_paths = db_files,
    outfmt_specifiers = "qaccver saccver pident bitscore",
    use_long_names_in_parsed_result = TRUE
  )

  checkmate::expect_names(
    colnames(result),
    permutation.of = c(
      "query_accession_version",
      "subject_accession_version",
      "percent_identical_matches",
      "bit_score"
    )
  )

  # Read the expected results
  expected_result <- readr::read_tsv(
    test_path("test_data", "expected_real_hits.tsv"),
    col_names = c(
      "qaccver",
      "saccver",
      "pident",
      "length",
      "mismatch",
      "gapopen",
      "qstart",
      "qend",
      "sstart",
      "send",
      "evalue",
      "bitscore"
    ),
    show_col_types = FALSE
  ) |>
    dplyr::select(c("qaccver", "saccver", "pident", "bitscore")) |>
    dplyr::rename(
      "query_accession_version" = "qaccver",
      "subject_accession_version" = "saccver",
      "percent_identical_matches" = "pident",
      "bit_score" = "bitscore"
    )

  expect_equal(
    dplyr::arrange(
      result,
      .data$query_accession_version,
      .data$subject_accession_version
    ),
    expected = dplyr::arrange(
      expected_result,
      .data$query_accession_version,
      .data$subject_accession_version
    )
  )
})
