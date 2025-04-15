#' Title
#'
#' @param model_obj
#' @param custom_labels
#' @param boot
#' @param n_boot
#' @param cut_points
#'
#' @returns
#' @export
#'
#' @examples
hte_plot <- function(model_obj,
    custom_labels = NULL,
    boot = TRUE,
    n_boot = 1000,
    cut_points = NULL)
{
    if(class(model_obj) %in% c("metalearner_ensemble",
                               "metalearner_deepneural")){
      all_vars <- all.vars(model_obj$formula)
      x_var_names <- all_vars[-1]
      x_vars <- model_obj$data[,c(x_var_names)]
      rownames(x_vars) <- 1:nrow(x_vars)
      y_var <- model_obj$CATEs
    } else if (class(model_obj) %in% c("pattc_ensemble",
                                       "metalearner_deepneural")){
      all_vars <- all.vars(model_obj$formula)
      y_var <- all_vars[1]
      x_var_names <- all_vars[-1]

      compliers <- model_obj$pop_data[which(
        model_obj$pop_data[,model_obj$compl_var]==1),]
      rownames(compliers) <- 1:nrow(compliers)
      x_vars <- compliers[,c(x_var_names)]
      y_var <- data.frame( count_diff =  model_obj$pop_counterfactual[,2] -
                            model_obj$pop_counterfactual[,1])
    }

    lowers <- list()
    highers <- list()
    lowers_y <- list()
    highers_y <- list()
    lowers_CIs <- list()
    highers_CIs <- list()
    lowers_bootout <- list()
    highers_bootout <- list()
    mean_lowers_temp <- rep(NA, n_boot)
    mean_highers_temp <- rep(NA, n_boot)

    if (is.null(cut_points)) {
      cuts <- sapply(x_vars, median,na.rm = TRUE)
    } else {
      if (length(cut_points) !=  length(x_var_names)) {
        stop(paste0("length of cut_points must be ", length(x_var_names)))
      }
      cuts <- cut_points
    }

    for (i in 1:length(x_var_names)) {
      lowers[[i]] <- x_vars[which(x_vars[,i] <= cuts[i]),]
      highers[[i]] <- x_vars[which(x_vars[,i] > cuts[i]),]
      lowers_y[[i]] <- y_var[as.numeric(rownames(lowers[[i]])),]
      highers_y[[i]] <- y_var[as.numeric(rownames(highers[[i]])),]
      if (boot) {
        lowers_boot_means <- rep(NA, n_boot)
        highers_boot_means <- rep(NA, n_boot)
        for (j in 1:n_boot){
          lowers_resample <- sample(1:length(lowers_y[[i]]),
                                    length(lowers_y[[i]]),
                                    replace=T)
          highers_resample <- sample(1:length(highers_y[[i]]),
                                     length(highers_y[[i]]),
                                     replace=T)
          lowers_boot_means[j] <- mean(lowers_y[[i]][lowers_resample],
                                       na.rm = TRUE)
          lowers_bootout[[i]] <- lowers_boot_means
          highers_boot_means[j] <- mean(highers_y[[i]][highers_resample],
                                        na.rm = TRUE)
          highers_bootout[[i]] <- highers_boot_means
        }
        lowers_CIs[[i]] <- c(quantile(lowers_bootout[[i]], c(0.025,  0.975)))
        highers_CIs[[i]] <- c(quantile(highers_bootout[[i]], c(0.025, 0.975)))
      } else if (!boot){
        lowers_CIs[[i]] <- c(quantile(lowers_y[[i]], c(0.025,  0.975)))
        highers_CIs[[i]] <- c(quantile(highers_y[[i]], c(0.025, 0.975)))
      }
    }

    names(lowers) <- paste0(x_var_names, " <= ", cuts)
    names(highers) <- paste0(x_var_names, " > ", cuts)
    names(lowers_y) <- paste0(x_var_names, " <= ", cuts)
    names(highers_y) <- paste0(x_var_names, " > ", cuts)

    if (boot){
      lowers_means <- unlist(lapply(lowers_bootout, mean, na.rm = TRUE))
      highers_means <- unlist(lapply(highers_bootout, mean, na.rm = TRUE))
    } else if (!boot) {
      lowers_means <- unlist(lapply(lowers_y, mean, na.rm = TRUE))
      highers_means <- unlist(lapply(highers_y, mean, na.rm = TRUE))
    }

    lowers_y_df <- data.frame(do.call(rbind, lowers_CIs),
                              means = lowers_means,
                              var_name = paste0(x_var_names, " <= ", cuts) )

    highers_y_df <- data.frame(do.call(rbind, highers_CIs),
                               means = highers_means,
                               var_name = paste0(x_var_names, " > ", cuts) )

    combined_df <- rbind(lowers_y_df, highers_y_df)

    sorted_df <- combined_df[order(combined_df$var_name), ]

    if (!is.null(custom_labels)) {
      if (length(custom_labels) !=  length(combined_df$var_name)) {
        stop(paste0("length of custom_labels must be ",
                    length(combined_df$var_name)))
      }
      sorted_df$var_name <- as.character(custom_labels)
    }

    ht_plot <- ggplot(sorted_df, aes(y = var_name, x = means)) +
      geom_point() +
      geom_errorbarh(aes(xmin = X2.5., xmax = X97.5.), height = 0.2) +
      theme_minimal() +
      labs(
        title = "",
        x = "",
        y = ""
      ) + geom_vline(xintercept = 0, linetype = "dashed", color = "grey70")
    return(ht_plot)
}

#' Title
#'
#' @param model_obj
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
plot.metalearner_deepneural <- function(model_obj,
                     type = "CATEs")
{
  if (type == "CATEs"){
  meta_plot <- data.frame(model_obj$CATEs) %>%
    ggplot( aes(x= model_obj.CATEs)) +
    geom_histogram(alpha = 0.6, position = 'identity')+
    xlab("CATEs (T Learner)")+ylab("")
  } else if (type == "predict") {
    meta_preds <-  rbind(data.frame("predictions" = model_obj$Y_hats[,1],
                                       type = "Y_hat0"),
                            data.frame("predictions" = model_obj$Y_hats[,2],
                                       type = "Y_hat1"))
    meta_plot <- meta_preds %>%
      ggplot( aes(x = predictions, fill = type)) +
      geom_histogram(alpha = 0.6, position = 'identity')+xlab("Predicted Outcome")+ylab("")+
      theme(legend.position = "bottom")+
      theme(legend.title=element_blank())
  }
  return(meta_plot)
}

#' Title
#'
#' @param model_obj
#' @param type
#'
#' @returns
#' @export
#'
#' @examples
plot.metalearner_ensemble <- function(model_obj,
                                        type = "CATEs")
{
  if (type == "CATEs"){
    meta_plot <- data.frame(model_obj$CATEs) %>%
      ggplot( aes(x= model_obj.CATEs)) +
      geom_histogram(alpha = 0.6, position = 'identity')+
      xlab("CATEs (T Learner)")+ylab("")
  } else if (type == "predict") {
    meta_preds <-  rbind(data.frame("predictions" = model_obj$Y_hats[,1],
                                    type = "Y_hat0"),
                         data.frame("predictions" = model_obj$Y_hats[,2],
                                    type = "Y_hat1"))
    meta_plot <- meta_preds %>%
      ggplot( aes(x = predictions, fill = type)) +
      geom_histogram(alpha = 0.6, position = 'identity')+xlab("Predicted Outcome")+ylab("")+
      theme(legend.position = "bottom")+
      theme(legend.title=element_blank())
  }
  return(meta_plot)
}
