#' Check for Python module availability and install if missing.
#'
#' Call this to manually set up Python and dependencies. The function checks if Python is available via the `reticulate` package, and if not, it creates a virtual environment and installs the specified Python modules.
#' @param modules Character vector of Python modules to check for and install if missing.
#' @param envname Name of the virtual environment to use or create. Defaults to "keras-tensorflow".
#' @return Invisibly returns TRUE if setup is complete.
#' @examples
#' \dontrun{
#' python_ready(modules = c("keras", "tensorflow", "numpy"),
#'             envname = "keras-tensorflow")
#' }
#' @export
python_ready <- function(modules = c("keras", "tensorflow", "numpy"),
                         envname = "keras-tensorflow") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("'reticulate' package required. Please install with install.packages('reticulate').")
  }
  
  if (!requireNamespace("keras3", quietly = TRUE)) {
    message("'keras3' R package not found. Please install with install.packages('keras3').")
  }
  
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    message("'tensorflow' R package not found. Please install with install.packages('tensorflow').")
  }
  
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

#' Check for required CRAN packages and prompt installation if missing.
#' @return Invisibly returns TRUE if all required packages are installed.
#' @keywords internal
check_cran_deps <- function() {
  packages <- c("reticulate", "keras3", "tensorflow")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing required R packages: ", paste(missing, collapse = ", "),
         ". Please install them with:\ninstall.packages(c(\"", 
         paste(missing, collapse = "\", \""), "\"))")
  }
  invisible(TRUE)
}

#' Check for required Python modules and prompt installation if missing.
#' @return Invisibly returns TRUE if all required modules are available.
#' @keywords internal
check_python_modules <- function() {
  py_modules <- c("tensorflow", "keras", "numpy")
  missing <- py_modules[!vapply(py_modules, reticulate::py_module_available, logical(1))]
  if (length(missing) > 0) {
    stop("Missing required Python modules: ", 
         paste(missing, collapse = ", "),
         ". Please install them in your Python environment with reticulate::py_install() or setup using python_ready().")
  }
  invisible(TRUE)
}
