# This file is used to run code when loading the package
.onAttach <- function(libname, pkgname) {
    if (!reticulate::py_available(initialize = FALSE)) {
      packageStartupMessage(
        "Install Python from https://www.python.org/ and use `python_ready()` to enable all features."
      )
    }
  } 