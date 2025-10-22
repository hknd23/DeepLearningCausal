#' hte_plot
#'
#' @description
#' Produces plot to illustrate sub-group Heterogeneous Treatment Effects (HTE)
#' of estimated CATEs from \code{metalearner_ensemble} and
#' \code{metalearner_neural}, as well as PATT-C from \code{pattc_ensemble}
#' and \code{pattc_neural}.
#'
#' @param x estimated model from \code{metalearner_ensemble},
#'  \code{metalearner_neural}, \code{pattc_ensemble}, or \code{pattc_neural}.
#' @param custom_labels character vector for the names of subgroups.
#' @param boot logical for using bootstraps to estimate confidence intervals.
#' @param n_boot number of bootstrap iterations. Only used with boot = TRUE.
#' @param cut_points numeric vector for cut-off points to generate subgroups from
#' covariates. If left blank a vector generated from median values will be used.
#' @param zero_int logical for vertical line at 0 x intercept.
#' @param ... Additional arguments 
#' @param selected_vars vector for names of covariates to use for subgroups.
#'
#' @returns \code{ggplot} object illustrating subgroup HTE and 95% confidence
#' intervals.
#' @importFrom magrittr %>%
#' @importFrom stats median sd qnorm
#' @import ggplot2
#' @export
#'
#' @examples
#' \donttest{
#' # load dataset
#' set.seed(123456)
#' xlearner_nn <- metalearner_neural(cov.formula = support_war ~ age +
#'                                   income  + employed  + job_loss,
#'                                   data = exp_data,
#'                                   treat.var = "strong_leader",
#'                                   meta.learner.type = "X.Learner",
#'                                   stepmax = 2e+9,
#'                                   nfolds = 5,
#'                                   algorithm = "rprop+",
#'                                   hidden.layer = c(3),
#'                                   linear.output = FALSE,
#'                                   binary.preds = FALSE)
#'
#' hte_plot(xlearner_nn)
#' hte_plot(xlearner_nn,
#'         selected_vars = c("age", "income"),
#'         cut_points = c(33, 3),
#'         custom_labels = c("Age <= 33", "Age > 33", "Income <= 3", "Income > 3"),
#'         n_boot = 500)
#'                                   }
hte_plot <- function(x, ...,
                     boot = TRUE,
                     n_boot = 1000,
                     cut_points = NULL,
                     custom_labels = NULL,
                     zero_int = TRUE,
                     selected_vars = NULL) {
  
  # --- Extract data depending on object class ---
  if (class(x) %in% c("metalearner_ensemble", 
                      "metalearner_neural", 
                      "metalearner_deeplearning")) {
    all_vars <- all.vars(x$formula)
    x_var_names <- all_vars[-1]
    if(is.null(x$data)){x$data<-x$test_data}
    x_vars <- x$data[, c(x_var_names), drop = FALSE]
    rownames(x_vars) <- 1:nrow(x_vars)
    y_var <- data.frame(count_diff = x$CATEs) 
    
  } else if (class(x) %in% c("pattc_ensemble", 
                             "pattc_neural",
                             "pattc_deeplearning")) {
    all_vars <- all.vars(x$formula)
    y_var <- all_vars[1]
    x_var_names <- all_vars[-1]
    compliers <- x$pop_data[which(x$pop_data[, x$compl_var] == 1), ]
    rownames(compliers) <- 1:nrow(compliers)
    x_vars <- compliers[, c(x_var_names), drop = FALSE]
    y_var <- data.frame(count_diff = x$pop_counterfactual[, 2] -
                          x$pop_counterfactual[, 1])
  }
  
  # --- Handle selected variables ---
  if (!is.null(selected_vars)) {
    missing_vars <- setdiff(selected_vars, x_var_names)
    if (length(missing_vars) > 0) {
      stop(paste("These selected_vars are not in the data:",
                 paste(missing_vars, collapse = ", ")))
    }
    x_vars <- x_vars[, selected_vars, drop = FALSE]
    x_var_names <- selected_vars
  }
  
  if (ncol(x_vars) == 0) {
    stop("No covariates selected. Please check selected_vars matches your data.")
  }
  
  # --- Cut points ---
  if (is.null(cut_points)) {
    cuts <- sapply(x_vars, median, na.rm = TRUE)
  } else {
    if (length(cut_points) != length(x_var_names)) {
      stop(paste0("length of cut_points must be ", length(x_var_names)))
    }
    cuts <- cut_points
  }
  
  # --- Initialize containers ---
  lowers <- list(); highers <- list()
  lowers_y <- list(); highers_y <- list()
  lowers_CIs <- list(); highers_CIs <- list()
  lowers_bootout <- list(); highers_bootout <- list()
  
  # --- Compute subgroups ---
  for (i in seq_along(x_var_names)) {
    lowers[[i]] <- x_vars[x_vars[, i] <= cuts[i], , drop = FALSE]
    highers[[i]] <- x_vars[x_vars[, i] > cuts[i], , drop = FALSE]
    
    lowers_y[[i]] <- y_var[as.numeric(rownames(lowers[[i]])), , drop = FALSE]
    highers_y[[i]] <- y_var[as.numeric(rownames(highers[[i]])), , drop = FALSE]
    
    if (boot) {
      lowers_boot_means <- replicate(n_boot, {
        mean(sample(lowers_y[[i]]$count_diff, length(lowers_y[[i]]$count_diff), 
                    replace = TRUE), na.rm = TRUE)
      })
      highers_boot_means <- replicate(n_boot, {
        mean(sample(highers_y[[i]]$count_diff, length(highers_y[[i]]$count_diff), 
                    replace = TRUE), na.rm = TRUE)
      })
      
      lowers_CIs[[i]] <- quantile(lowers_boot_means, c(0.025, 0.975), na.rm = TRUE)
      highers_CIs[[i]] <- quantile(highers_boot_means, c(0.025, 0.975), na.rm = TRUE)
      lowers_bootout[[i]] <- lowers_boot_means
      highers_bootout[[i]] <- highers_boot_means
    } else {
      lowers_CIs[[i]] <- quantile(lowers_y[[i]], c(0.025, 0.975), na.rm = TRUE)
      highers_CIs[[i]] <- quantile(highers_y[[i]], c(0.025, 0.975), na.rm = TRUE)
    }
  }
  
  # --- Compute means ---
  if (boot) {
    lowers_means <- sapply(lowers_bootout, mean, na.rm = TRUE)
    highers_means <- sapply(highers_bootout, mean, na.rm = TRUE)
  } else {
    lowers_means <- sapply(lowers_y, mean, na.rm = TRUE)
    highers_means <- sapply(highers_y, mean, na.rm = TRUE)
  }
  
  lowers_y_df <- data.frame(do.call(rbind, lowers_CIs),
                            means = lowers_means,
                            var_name = paste0(x_var_names, " <= ", cuts))
  highers_y_df <- data.frame(do.call(rbind, highers_CIs),
                             means = highers_means,
                             var_name = paste0(x_var_names, " > ", cuts))
  combined_df <- rbind(lowers_y_df, highers_y_df)
  sorted_df <- combined_df[order(combined_df$var_name), ]
  
  # --- Axis intercept ---
  x_int <- if (zero_int) 0 else NULL
  
  # --- Plot ---
  if (!is.null(custom_labels)) {
    if (length(custom_labels) != nrow(sorted_df)) {
      stop(paste0("length of custom_labels must be ", nrow(sorted_df),
                  " (you provided ", length(custom_labels), ")"))
    }
    ht_plot <- ggplot(sorted_df, aes(y = var_name, x = means)) +
      geom_point() +
      geom_errorbarh(aes(xmin = X2.5., xmax = X97.5.), height = 0.2) +
      theme_minimal() +
      labs(x = "", y = "") +
      geom_vline(xintercept = x_int, linetype = "dashed", color = "grey70") +
      scale_y_discrete(labels = custom_labels)
  } else {
    ht_plot <- ggplot(sorted_df, aes(y = var_name, x = means)) +
      geom_point() +
      geom_errorbarh(aes(xmin = X2.5., xmax = X97.5.), height = 0.2) +
      theme_minimal() +
      labs(x = "", y = "") +
      geom_vline(xintercept = x_int, linetype = "dashed", color = "grey70")
  }
  
  print(ht_plot)
}

#' plot.metalearner_neural
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{metalearner_neural}
#'
#' @param x \code{metalearner_neural} model object.
#' @param type "CATEs" or "predict".
#' @param conf_level numeric value for confidence level. Defaults to 0.95.
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd qnorm
#' @import ggplot2
plot.metalearner_neural <- function(x, ..., 
                                        conf_level = 0.95, 
                                        type = "CATEs")
{
  if (type == "CATEs"){
    cates <- x[["CATEs"]]
    if (!is.matrix(cates) && !is.data.frame(cates)) {
      stop("model_obj[['CATEs']] must be a matrix or data frame")
    }
    if (ncol(cates) != 1) {
      stop("This version supports only 1 group (1 column in CATEs)")
    }
    if (conf_level <= 0 || conf_level >= 1) {
      stop("conf_level must be between 0 and 1")
    }
    
    cate_vals <- cates[, 1]
    mean_cate <- mean(cate_vals, na.rm = TRUE)
    sd_cate   <- sd(cate_vals, na.rm = TRUE)
    a <- qnorm(1 - (1 - conf_level) / 2)
    
    lower <- mean_cate - a * sd_cate
    upper <- mean_cate + a * sd_cate
    color <- ifelse(lower < 0 & upper > 0, "red", "black")
    
    df_summary <- data.frame(
      Mean = mean_cate,
      Lower = lower,
      Upper = upper,
      Color = color
    )
    
    x_min <- min(lower, min(cate_vals)) - 1*sd_cate
    x_max <- max(upper, max(cate_vals)) + 1*sd_cate
    
    meta_plot <- ggplot() +
      geom_density(data = data.frame(CATE = cate_vals),
                   aes(x = CATE, y = ..density..),
                   fill = "gray90", color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "gray30") +
      geom_point(data = df_summary, aes(x = Mean, y = 0), size = 2) +
      geom_errorbarh(data = df_summary, 
                     aes(xmin = Lower, xmax = Upper,
                         y = 0, color = Color), 
                     height = 0.1) +
      scale_color_identity() +
      xlim(x_min, x_max) +
      labs(
        title = paste0("CATE Distribution with ", round(conf_level * 100), 
                       "% Confidence Interval"),
        x = "CATE",
        y = "Density"
      ) +
      theme_minimal()
  } else if (type == "predict") {
    meta_preds <-  rbind(data.frame("predictions" = x$Y_hats[,1],
                                       type = "Y_hat0"),
                            data.frame("predictions" = x$Y_hats[,2],
                                       type = "Y_hat1"))
    
    x_min <- min(meta_preds$predictions) - 1*sd(meta_preds$predictions)
    x_max <- max(meta_preds$predictions) + 1*sd(meta_preds$predictions)
    
    meta_plot <-  ggplot() +
      geom_density(data = meta_preds,  
                   aes(x = predictions, fill = type),
                   color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      xlab("Predicted Outcome") + ylab("") +
      xlim(x_min, x_max) +
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())
  }
  print(meta_plot)
}

#' plot.metalearner_ensemble
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{metalearner_ensemble}
#'
#' @param x \code{metalearner_ensemble} model object
#' @param type "CATEs" or "predict"
#' @param conf_level numeric value for confidence level. Defaults to 0.95.
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd qnorm
#' @import ggplot2
plot.metalearner_ensemble <- function(x, ...,
                                      conf_level = 0.95,
                                      type = "CATEs")
{
  if (type == "CATEs"){
    cates <- x[["CATEs"]]
    if (!is.matrix(cates) && !is.data.frame(cates)) {
      stop("model_obj[['CATEs']] must be a matrix or data frame")
    }
    if (ncol(cates) != 1) {
      stop("This version supports only 1 group (1 column in CATEs)")
    }
    if (conf_level <= 0 || conf_level >= 1) {
      stop("conf_level must be between 0 and 1")
    }
    
    cate_vals <- cates[, 1]
    mean_cate <- mean(cate_vals, na.rm = TRUE)
    sd_cate   <- sd(cate_vals, na.rm = TRUE)
    a <- qnorm(1 - (1 - conf_level) / 2)
    
    lower <- mean_cate - a * sd_cate
    upper <- mean_cate + a * sd_cate
    color <- ifelse(lower < 0 & upper > 0, "red", "black")
    
    df_summary <- data.frame(
      Mean = mean_cate,
      Lower = lower,
      Upper = upper,
      Color = color
    )
    
    x_min <- min(lower, min(cate_vals)) - 1*sd_cate
    x_max <- max(upper, max(cate_vals)) + 1*sd_cate
    
    meta_plot <- ggplot() +
      geom_density(data = data.frame(CATE = cate_vals),
                   aes(x = CATE, y = ..density..),
                   fill = "gray90", color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "gray30") +
      geom_point(data = df_summary, aes(x = Mean, y = 0), size = 2) +
      geom_errorbarh(data = df_summary, aes(xmin = Lower, xmax = Upper, y = 0, 
                                            color = Color), height = 0.1) +
      scale_color_identity() +
      xlim(x_min, x_max) +
      labs(
        title = paste0("CATE Distribution with ", 
                       round(conf_level * 100), 
                       "% Confidence Interval"),
        x = "CATE",
        y = "Density"
      ) +
      theme_minimal()
  } else if (type == "predict") {
    meta_preds <-  rbind(data.frame("predictions" = x$Y_hats[,1],
                                    type = "Y_hat0"),
                         data.frame("predictions" = x$Y_hats[,2],
                                    type = "Y_hat1"))
    
    x_min <- min(meta_preds$predictions) - 1*sd(meta_preds$predictions)
    x_max <- max(meta_preds$predictions) + 1*sd(meta_preds$predictions)
    
    meta_plot <-  ggplot() +
      geom_density(data = meta_preds,  
                   aes(x = predictions, fill = type),
                   color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      xlab("Predicted Outcomes") + ylab("") +
      xlim(x_min, x_max) +
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())
  }
  print(meta_plot)
}

#' plot.pattc_neural
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{pattc_neural}
#'
#' @param x \code{pattc_neural} model object
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd qnorm
#' @import ggplot2
plot.pattc_neural <- function(x, ...)
{
  patt_preds <-  rbind(data.frame("predictions" = x$pop_counterfactual[,1],
                                  type = "Y_hat0"),
                       data.frame("predictions" = x$pop_counterfactual[,2],
                                  type = "Y_hat1"))
  
  x_min <- min(patt_preds$predictions) - 1*sd(patt_preds$predictions)
  x_max <- max(patt_preds$predictions) + 1*sd(patt_preds$predictions)
  
  pattc_plot <-  ggplot(data = patt_preds) +
    geom_density(aes(x = predictions, fill = type),
                 color = "black",
                 alpha = 0.7, linewidth = 0.5) +
    xlab("Predicted Outcomes") + ylab("") +
    xlim(x_min, x_max) +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  print(pattc_plot)
}

#' plot.pattc_ensemble
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{pattc_ensemble}
#'
#' @param x \code{pattc_ensemble} model object
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @import ggplot2
plot.pattc_ensemble <- function(x, ...)
{
  patt_preds <-  rbind(data.frame("predictions" = x$pop_counterfactual[,1],
                                  type = "Y_hat0"),
                       data.frame("predictions" = x$pop_counterfactual[,2],
                                  type = "Y_hat1"))
  
  x_min <- min(patt_preds$predictions) - 1*sd(patt_preds$predictions)
  x_max <- max(patt_preds$predictions) + 1*sd(patt_preds$predictions)
  
  pattc_plot <-  ggplot(data = patt_preds) +
    geom_density(aes(x = predictions, fill = type),
                 color = "black",
                 alpha = 0.7, linewidth = 0.5) +
    xlab("Predicted Outcome") + ylab("") +
    xlim(x_min, x_max) +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  print(pattc_plot)
}

#' plot.pattc_deeplearning
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of predicted
#' outcomes from  \code{pattc_deeplearning}
#'
#' @param x \code{pattc_deeplearning} model object
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @import ggplot2
plot.pattc_deeplearning <- function(x, ...)
{
  patt_preds <-  rbind(data.frame("predictions" = x$pop_counterfactual[,1],
                                  type = "Y_hat0"),
                       data.frame("predictions" = x$pop_counterfactual[,2],
                                  type = "Y_hat1"))
  
  x_min <- min(patt_preds$predictions) - 1*sd(patt_preds$predictions)
  x_max <- max(patt_preds$predictions) + 1*sd(patt_preds$predictions)
  
  pattc_plot <-  ggplot(data = patt_preds) +
    geom_density(aes(x = predictions, fill = type),
                 color = "black",
                 alpha = 0.7, linewidth = 0.5) +
    xlab("Predicted Outcomes") + ylab("") +
    xlim(x_min, x_max) +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  print(pattc_plot)
}

#' conformal_plot
#'
#' @description
#' Visualizes the distribution of estimated individual treatment effects (ITEs) 
#' along with their corresponding conformal prediction intervals. 
#' The function randomly samples a proportion of observations from a fitted 
#' \code{metalearner_ensemble} or \code{metalearner_deeplearning} object and 
#' plots the conformal intervals as vertical ranges around the point estimates. 
#' This allows users to visually assess the uncertainty and variation in 
#' estimated treatment effects.
#'
#' @param x A fitted model object of class \code{metalearner_ensemble} 
#' or \code{metalearner_deeplearning} that contains a \code{conformal_interval} element.
#' @param ... Additional arguments (currently unused).
#' @param seed Random seed for reproducibility of subsampling. Default is \code{1234}.
#' @param prop Proportion of observations to randomly sample for plotting. 
#' Must be between 0 and 1. Default is \code{0.3}.
#' @param binary.outcome Logical; if \code{TRUE}, constrains the y-axis to 
#' \code{[-1, 1]} for binary outcomes. Default is \code{FALSE}.
#' @param x.labels Logical; if \code{TRUE}, displays x-axis labels for each sampled observation. 
#' Default is \code{TRUE}.
#' @param x.title Character string specifying the x-axis title. 
#' Default is \code{"Observations"}.
#' @param color Color of the conformal intervals and points. 
#' Default is \code{"steelblue"}.
#' @param break.by Numeric value determining the spacing between y-axis breaks. 
#' Default is \code{0.5}.
#'
#' @details
#' The function extracts the estimated ITEs (\code{CATEs}) and conformal intervals 
#' (\code{ITE_lower}, \code{ITE_upper}) from the model output, samples a subset 
#' of rows, and generates a \code{ggplot2} visualization. 
#' Each vertical line represents the conformal prediction interval for one observation’s 
#' treatment effect estimate. 
#' The conformal intervals are typically obtained from weighted split-conformal inference, 
#' using propensity overlap weights to adjust interval width.
#'
#' @returns A \code{ggplot} object showing sampled individual treatment effects 
#' with their weighted conformal prediction intervals.
#' @export
#' @importFrom magrittr %>%
#' @import ggplot2
conformal_plot <- function(x, ...,
                           seed=1234, prop=0.3,
                           binary.outcome=FALSE,
                           x.labels=TRUE,
                           x.title="Observations",
                           color="steelblue",
                           break.by=0.5) {
  if (class(x) %in% c("metalearner_ensemble", "metalearner_deeplearning")) {
    
    if (!"conformal_interval" %in% names(x)) {
      stop("Object does not contain 'conformal_interval'. Please ensure conformal intervals are computed.")
    }
    set.seed(seed)
    
    df<-x[["conformal_interval"]]
    df$ITE<- x$CATEs
    df_sample <- df %>%
      dplyr::mutate(row_id = rownames(df)) %>%   
      dplyr::sample_frac(prop) %>%
      dplyr::arrange(row_id)                      
    
    y_min <- min(df$ITE_lower, na.rm = TRUE)
    y_max <- max(df$ITE_upper, na.rm = TRUE)
    y_range <- y_max - y_min
    
    
    
    if (binary.outcome==TRUE) {
      y_limits <- c(-1, 1)
      y_breaks <- c(-1, -0.5, 0, 0.5, 1)
    } else {
      y_limits <- c(y_min - y_range / 5, y_max + y_range / 5)
      y_breaks <- seq(
        from = floor(y_limits[1] * 2) / 2,
        to   = ceiling(y_limits[2] * 2) / 2,
        by   = break.by
      )
    }
    # Plot coefficient with vertical intervals
    pic<- ggplot(df_sample, aes(x = factor(row_id), y = ITE)) +
      geom_pointrange(aes(ymin = ITE_lower, ymax = ITE_upper), 
                      color = color, size = 0.3) +
      labs(
        x = x.title,
        y = "Estimated ITE",
        title = paste0("Weighted Conformal Intervals for ITEs (",
                       prop*100,"% of Observations)")
      ) +
      scale_y_continuous(
        limits = y_limits,
        breaks = y_breaks
      ) +
      theme_minimal(base_size = 10) +
      theme(
        panel.grid.major.y = element_blank()
      )
    
    if (!x.labels) {
      pic <- pic + scale_x_discrete(labels = NULL)
    }
    print(pic)
  } else {stop("Invalid object: 'x' must be of class 'metalearner_ensemble' or 'metalearner_deeplearning'.")}  
}    
    
