test_that("keras_meta_learners", {
  set.seed(1243)
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
  
})
