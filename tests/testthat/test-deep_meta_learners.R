test_that("keras_pattc", {
  #python_ready()
  message(paste0("Python tensorflow: ", reticulate::py_module_available('tensorflow')))
  message(paste0("Python keras: ", reticulate::py_module_available('keras')))
  
  skip_if_not(reticulate::py_available(), "Python not available") 
  # Try keras3, install if missing
  if (!requireNamespace("keras3", quietly = TRUE)) {
    message("Installing keras3...")
    install.packages("keras3", repos = "https://cloud.r-project.org")
  }
  
  # Try tensorflow, install if missing
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    message("Installing tensorflow...")
    install.packages("tensorflow", repos = "https://cloud.r-project.org")
  }
  
  set.seed(1234)
  deeppattc <- pattc_deep(response.formula = support_war ~ age + female +
                            income + education +  employed + married +
                            hindu + job_loss,
                          exp.data = exp_data_full,
                          pop.data = pop_data_full,
                          treat.var = "strong_leader",
                          compl.var = "compliance",
                          algorithm = "adam",
                          hidden.layer = c(2,2),
                          ID = NULL,
                          weights = NULL,
                          cluster = NULL,
                          verbose = 0,
                          complier.epoch = 500,
                          response.epoch = 1000,
                          batch_size = 32,
                          model_type = "classification",
                          binary.preds = FALSE,
                          boot = FALSE
  )
  expect_s3_class(deeppattc, "pattc_deep")
  expect_type(deeppattc$complier_prediction, "double")
  expect_equal(nrow(deeppattc$population_counterfactuals), nrow(pop_data))
  expect_equal(nrow(deeppattc$complier_prediction), nrow(exp_data))
  
})
