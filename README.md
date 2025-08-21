# SnailBLAST

Parallel BLAST Search Faster than a Speeding Snail

Launches BLAST+ searches across multiple query files and subject databases using
parallel processing. Designed for workflows that need to crunch through tons of
comparisons, SnailBLAST handles job orchestration, result merging, and error
handling. Faster than a speeding snail, but just as reliable.

## Installation

You can install the development version of SnailBLAST like so:

``` r
# If you don't already have "remotes" installed, uncomment this line:
# install.packages("remotes")

remotes::install_github("mooreryan/SnailBLAST")
```

## Example

Here is a basic example that runs `blastn` on the query `a.fasta` against the DB named `db`.

``` r
SnailBLAST::crawl("blastn", "/path/to/a.fasta", "/path/to/db")
```

Here is a more involved example that shows some of the optional arguments you might use.

``` r
SnailBLAST::crawl(
  # The name of the BLAST executable you want to run. We will try to find it on
  # your PATH
  blast_executable = "blastn",
  # Paths to query files. Provide as many as you want.
  query_paths = c("/path/to/a.fasta", "/path/to/b.fasta"),
  # Paths to BLAST DB files (targets). Provide as many as you want.\
  db_paths = c("/path/to/db1", "/path/to/db2", "/path/to/db3"),

  # Note: All the arguments from here and below are *optional*.

  # Any additional arguments you want to provide to the BLAST executable. Use
  # the same arguments that you would if you were running BLAST on the command
  # line.
  extra_blast_arguments = c("-evalue", "1e-5", "-max_target_seqs", "200"),
  # These are the format specifiers that will be passed to the -outfmt 6
  # argument. Use them to control the columns of the output.
  outfmt_specifiers = "qaccver saccver pident evalue bitscore",
  # This callback runs if one of the BLAST jobs fails. That is, if the BLAST
  # command itself fails or is killed for some reason.
  job_failed_callback = function(query_path, db_path, exit_status) {
    # You can imagine replacing this with more robust logging so you can track
    # errors.
    warning(
      "BLAST job failed (",
      exit_status,
      ") for query: ",
      query_path,
      ", DB: ",
      db_path
    )
  },
  # This callback runs if the BLAST job succeeds, but the parsing of the TSV
  # result fails for some reason. If this happens, something weird has
  # definitely happened to your data....
  parse_failed_callback = function(query_path, db_path, error_condition) {
    # You can imagine replacing this with more robust logging so you can track
    # errors.
    warning(
      "Failed to parse BLAST output for query: ",
      query_path,
      ", DB: ",
      db_path
    )
  }
)
```

In both cases, you will get back a `tibble` with the BLAST hits.
