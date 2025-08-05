# File names assume that you're building from the root of the package

safe_empty_values <- c(
  "character(0)",
  "integer(0)",
  "double(0)"
)

safe_types <- c(
  "readr::col_character()",
  "readr::col_integer()",
  "readr::col_double()"
)

# Read the format specifiers CSV file containing BLAST output format definitions
format_specifiers_csv <- file.path("data-raw", "format_specifiers.csv")
if (rlang::is_installed("rprojroot")) {
  format_specifiers_csv <- file.path(
    rprojroot::find_package_root_file(),
    format_specifiers_csv
  )
}

if (!rlang::is_installed("readr")) {
  stop("You need to install readr in order to build the sysdata")
}
format_specifiers <- readr::read_csv(format_specifiers_csv, col_types = "cccc")
# Validate that the CSV has the required column names
checkmate::assert_names(
  colnames(format_specifiers),
  must.include = c("Short", "Long", "EmptyValue", "Type")
)

# Ensure that columns that will be evaluated with `eval` only contained allowed
# code
format_specifiers$EmptyValue |>
  checkmate::assert_subset(choices = safe_empty_values)
format_specifiers$Type |> checkmate::assert_subset(choices = safe_types)


# This is a tibble that has one "empty" column of the correct type for all of
# the default blast column format specifiers. You will select the columns you
# need to generate default empty blast results.
empty_blast_table_columns <- format_specifiers$EmptyValue |>
  rep(times = 2) |>
  lapply(function(x) eval(str2expression(x))) |>
  setNames(append(format_specifiers$Short, format_specifiers$Long)) |>
  tibble::as_tibble()
checkmate::assert_names(
  colnames(empty_blast_table_columns),
  permutation.of = append(format_specifiers$Short, format_specifiers$Long)
)
checkmate::assert_data_frame(empty_blast_table_columns, nrows = 0)


# Create a named list of column types for short format specifier names
# Parse the Type column (contains R type expressions like character(), numeric())
format_specifier_types <- format_specifiers$Type |>
  rep(times = 2) |>
  lapply(function(x) eval(str2expression(x))) |>
  setNames(append(format_specifiers$Short, format_specifiers$Long)) |>
  checkmate::assert_list(len = nrow(format_specifiers) * 2)

# Create a mapping from short names to long names for format specifiers
format_specifier_short_to_long_names <- format_specifiers$Long
names(format_specifier_short_to_long_names) <- format_specifiers$Short

.format_specifiers <- new.env(parent = emptyenv())
.format_specifiers$format_specifiers <- format_specifiers
.format_specifiers$empty_blast_table_columns <- empty_blast_table_columns
.format_specifiers$format_specifier_types <- format_specifier_types
.format_specifiers$format_specifier_short_to_long_names <- format_specifier_short_to_long_names


# Save all the processed data objects as internal package data
# These will be available to package functions but not exported to users
if (!rlang::is_installed("usethis")) {
  stop("You need to install usethis to build the sysdata")
}
usethis::use_data(.format_specifiers, overwrite = TRUE, internal = TRUE)
