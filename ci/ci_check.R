if (!normalizePath(Sys.getenv('R_LIBS_USER'), winslash = '/') %in% .libPaths()) {
  stop("Wrong lib path.")
}
install.packages('devtools', lib = Sys.getenv('R_LIBS_USER'), dependencies = TRUE)
remotes::install_deps(dependencies = TRUE, lib = Sys.getenv('R_LIBS_USER'))
devtools::check(vignettes = FALSE, check_dir = Sys.getenv('CHECK_DIR'), args = '--no-tests')