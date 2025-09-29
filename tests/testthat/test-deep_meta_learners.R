test_that("keras_pattc", {
  python_ready()
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
  deeppattc <- deep_pattc(response.formula = support_war ~ age + female +
                            income + education +  employed + married +
                            hindu + job_loss,
                          exp.data = exp_data,
                          pop.data = pop_data,
                          treat.var = "strong_leader",
                          compl.var = "compliance",
                          algorithm = "adam",
                          hidden.layer = c(2,2),
                          ID = NULL,
                          weights = NULL,
                          cluster = NULL,
                          epoch = 1000,
                          verbose = 1,
                          batch_size = 32,
                          model_type = "classification",
                          binary.preds = FALSE,
                          boot = FALSE
  )
  expect_s3_class(deeppattc, "deep_pattc")
  expect_type(deeppattc$predictions, "double")
  expect_equal(length(deeppattc$population_counterfactuals), nrow(pop_data))
  expect_equal(length(deeppattc$complier_prediction), nrow(exp_data))
  
})
