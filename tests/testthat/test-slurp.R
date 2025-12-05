describe("slurping up the output files", {
  # Skip if BLAST is not available
  skip_if(sys_which("blastn") == "", "blastn not found on PATH")

  it("fails when slurp is false and no output directory is given", {
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta"),
      test_path("test_data", "real_queries_3.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    testthat::expect_error(
      {
        crawl(
          blast_executable = "blastn",
          query_paths = query_files,
          db_paths = db_files,
          slurp = FALSE,
          output_directory = NULL
        )
      },
      class = "snailblast_error",
      regexp = "slurp"
    )
  })

  it("slurps the BLAST output into a tibble when slurp is true (with outdir)", {
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta"),
      test_path("test_data", "real_queries_3.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    output_directory <- tempfile()
    on.exit(unlink(output_directory), add = TRUE)

    result <- crawl(
      blast_executable = "blastn",
      query_paths = query_files,
      db_paths = db_files,
      slurp = TRUE,
      output_directory = output_directory
    )

    checkmate::expect_data_frame(result, nrows = 10, ncols = 12)
  })

  it("slurps the BLAST output into a tibble when slurp is true (no outdir)", {
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta"),
      test_path("test_data", "real_queries_3.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    result <- crawl(
      blast_executable = "blastn",
      query_paths = query_files,
      db_paths = db_files,
      slurp = TRUE,
      output_directory = NULL
    )

    checkmate::expect_data_frame(result, nrows = 10, ncols = 12)
  })

  it("doesn't slurp outfiles when slurp is false and there is an outdir", {
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta"),
      test_path("test_data", "real_queries_3.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    output_directory <- tempfile()
    on.exit(unlink(output_directory), add = TRUE)

    result <- crawl(
      blast_executable = "blastn",
      query_paths = query_files,
      db_paths = db_files,
      slurp = FALSE,
      output_directory = output_directory
    )

    testthat::expect_null(result)
    filenames <- list.files(output_directory)
    testthat::expect_equal(length(filenames), 6)
  })
})

describe("removing temporary output files", {
  it("does not remove output files if output_directory is given", {
    query_files <- c(
      test_path("test_data", "real_queries_1.fasta"),
      test_path("test_data", "real_queries_2.fasta"),
      test_path("test_data", "real_queries_3.fasta")
    )

    db_files <- c(
      test_path("test_data", "real_db_1"),
      test_path("test_data", "real_db_2")
    )

    output_directory <- tempfile()

    crawl(
      blast_executable = "blastn",
      query_paths = query_files,
      db_paths = db_files,
      slurp = TRUE,
      output_directory = output_directory
    )

    testthat::expect_equal(
      length(list.files(output_directory)),
      6
    )
  })
})
