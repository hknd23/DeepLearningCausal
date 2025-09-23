# File: R/setup.R

#' Ensure Python modules are available
#'
#' Call this to manually set up Python and dependencies.
#' @param modules Character vector of Python modules to check for and install if missing.
#' @param envname Name of the virtual environment to use or create.
#' @export
python_ready <- function(modules = c("keras", "tensorflow", "numpy"),
                         envname = "keras-tensorflow") {
  if (reticulate::py_available(initialize = FALSE)) {
    message("Python is already available")
  } else {
    message("Python is not available. Initializing...")
    reticulate::virtualenv_create("r-reticulate")
    reticulate::use_virtualenv("r-reticulate", required = TRUE)
  }
  message("Checking for required Python modules...")
  missing <- modules[!vapply(modules, 
                             reticulate::py_module_available, 
                             logical(1))]
  if (length(missing) > 0) {
    message("Installing missing Python modules: ", 
            paste(missing, collapse = ", "))
    reticulate::py_install(missing, envname = envname, pip = TRUE)
  } else {
    message("All required Python modules are available.")
}
  invisible(TRUE)
}
