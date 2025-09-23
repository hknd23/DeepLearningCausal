# This file is used to run code when loading the package
.onLoad <- function() {
  # Only initialize if Python is available and not already initialized
  if (!reticulate::py_available(initialize = FALSE)) {
    packageStartupMessage(
      "Python not installed or unavailable. Some features may be disabled.\n",
      "Install Python from https://www.python.org/ and use `python_ready()` to enable all features."
    )
  }
}
  
