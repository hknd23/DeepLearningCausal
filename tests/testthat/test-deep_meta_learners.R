skip_on_cran()
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
  deeppattc <- pattc_deeplearning(response.formula = support_war ~ age + female +
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
                          response.output_activation = "sigmoid",
                          response.output_units = 1,
                          response.loss = "binary_crossentropy",
                          response.metrics = "accuracy",
                          ID = NULL,
                          weights = NULL,
                          cluster = NULL,
                          compl.epoch = 200,
                          response.epoch = 500,
                          compl.validation_split = 0.2,
                          response.validation_split = 0.2,
                          compl.patience = 20,
                          response.patience = 20,
                          compl.dropout_rate = 0.1,
                          response.dropout_rate = c(0.1, 0.1, 0.1),
                          verbose = 0,
                          batch_size = 32,
                          nboot = 1000)
  expect_s3_class(deeppattc, "pattc_deeplearning")
  #print(nrow(deeppattc$population_counterfactuals))
  #expect_equal(nrow(deeppattc$population_counterfactuals), nrow(pop_data_full))
  expect_equal(nrow(deeppattc$complier_prediction), nrow(exp_data_full))
  
})
