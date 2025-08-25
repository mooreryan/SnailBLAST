test:
    Rscript --vanilla -e 'devtools::test()'

dev_deps:
    Rscript --vanilla -e 'devtools::install_dev_deps()'

check: dev_deps
    Rscript --vanilla -e 'roxygen2::roxygenize(clean = TRUE); devtools::check()'

install: check
    Rscript --vanilla -e 'devtools::install(build_vignettes = TRUE)'
