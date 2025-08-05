test:
    Rscript --vanilla -e 'devtools::test()'

test_crawl:
    Rscript --vanilla -e 'devtools::test_active_file("tests/testthat/test-crawl.R")'

check:
    Rscript --vanilla -e 'roxygen2::roxygenize(clean = TRUE); devtools::check()'

install: check
    Rscript --vanilla -e 'devtools::install()'
