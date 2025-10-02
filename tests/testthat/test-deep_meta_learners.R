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
                          compl.algorithm = "adam",
                          response.algorithm = "adam",
                          compl.hidden.layer = c(8,4,2),
                          response.hidden.layer = c(8,4,2),
                          compl.hidden_activation = "relu",
                          response.hidden_activation = "relu",
                          response.output_activation = "linear",
                          response.loss = "mean_squared_error",
                          response.metrics = "mean_absolute_error",
                          ID = NULL,
                          weights = NULL,
                          cluster = NULL,
                          compl.epoch = 5000,
                          response.epoch = 10000,
                          verbose = 0,
                          batch_size = 64,
                          model_type = "regression",
                          binary.preds = FALSE,
                          bootstrap = FALSE,
                          nboot = 1000)
  expect_s3_class(deeppattc, "pattc_deep")
  print(nrow(deeppattc$population_counterfactuals))
  #expect_equal(nrow(deeppattc$population_counterfactuals), nrow(pop_data_full))
  expect_equal(nrow(deeppattc$complier_prediction), nrow(exp_data_full))
  
})
