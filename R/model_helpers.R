#' Handle output for Linear Models
#'
#' Internal function to handle the output for objects of class \code{lm}.
#'
#' @param mod A fitted linear model object of class \code{lm}.
#' @param by Required. The main predictor of interest. Behaviour will
#' differ when variable is continuous vs categorical
#' @param dat_mod The data used to fit the model, passed from thekids_model
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of model-specific output.
#'
#' @method thekids_model_output lm
#'
#' @keywords internal
#'
#' @exportS3Method thekidsbiostats::thekids_model_output
thekids_model_output.lm <- function(mod, by, dat_mod = NULL, ...) {

  if(is.null(dat_mod)) dat_mod <- mod$model

  y <- all.vars(formula(mod))[1]

  if(is.character(dat_mod[[by]])) {
    mod_desc <- dat_mod %>%
      tbl_summary(by = by,
                  type = list(where(is.numeric) ~ "continuous"),
                  statistic = list(all_continuous() ~ "{mean} ({sd}) [{N_nonmiss}]")) %>%
      modify_header(label = by) %>%
      add_p() %>%
      bold_labels() %>%
      suppressMessages() %>% suppressWarnings()

    mod_desc_plot <- dat_mod %>%
      mutate(x = factor(.[[by]])) %>%
      ggplot(aes(x = x, y = .data[[y]],
                 colour = x, group = x, fill = x)) +
      geom_violin(alpha = 0.1, width = 0.5) +
      geom_jitter(height = 0, width = 0.05, alpha = 0.3) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.05, colour = "red") +
      stat_summary(fun = mean, colour = "red", geom="point", size = 4) +
      stat_summary(fun = mean, colour = "black", geom="point", size = 3, shape = 4) +
      thekids_theme() +
      theme(plot.caption = element_text(size = 12, face = "italic"),
            legend.position = "none") +
      labs(title = "Jittered dot and violin distribution plot",
           subtitle = paste0(y, " plotted by ", by),
           caption = "Black 'X' on red circle indicates mean (±SE)",
           x = by)
  } else {
    mod_desc <- dat_mod %>%
      tbl_summary(type = list(where(is.numeric) ~ "continuous"),
                  statistic = list(all_continuous() ~ "{mean} ({sd}) [{N_nonmiss}]")) %>%
      bold_labels() %>%
      suppressMessages() %>% suppressWarnings()

    mod_desc_plot <- dat_mod %>%
      mutate(x = .[[by]]) %>%
      ggplot(aes(x = x, y = .data[[y]])) +
      geom_jitter(width = 0.05, height = 0.05, alpha = 0.6, col = thekids_colours[[3]]) +
      geom_smooth(fill = thekids_colours[[1]], col = thekids_colours[[1]]) +
      thekids_theme() +
      theme(plot.caption = element_text(size = 12, face = "italic"),
            legend.position = "none") +
      labs(title = "Dot plot with line of best bit (LOESS)",
           subtitle = paste0(y, " plotted by ", by),
           x = by)
  }

  mod_diag <- autoplot(mod)

  mod_output <- mod %>%
    tbl_regression(intercept = T,
                   estimate_fun = function(x) style_number(x, digits = 1),
                   pvalue_fun = function(x) style_number(x, digits = 3)) %>%
    modify_column_merge(
      pattern = "{estimate} ({conf.low}, {conf.high})",
      rows = !is.na(estimate)
    )


  return(list(mod_dat = dat_mod,
              mod_desc = mod_desc,
              mod_desc_plot = mod_desc_plot,
              mod_diag = mod_diag,
              mod = mod,
              mod_output = mod_output))
}

#' Handle Output for Negative Binomial Models
#'
#' Internal function to handle the output for objects of class \code{negbin}.
#'
#' @param mod A fitted negative binomial model object of class \code{negbin}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of model-specific output.
#'
#' @method thekids_model_output negbin
#'
#' @keywords internal
#'
#' @exportS3Method thekidsbiostats::thekids_model_output
thekids_model_output.negbin <- function(mod, ...) {
  print(paste("Model class:", class(mod)))
}

#' Handle Output for Quantile Regression Models
#'
#' Internal function to handle the output for objects of class \code{rq}.
#'
#' @param mod A fitted quantile regression model object of class \code{rq}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of model-specific output.
#'
#' @keywords internal
#'
#' @exportS3Method thekidsbiostats::thekids_model_output
thekids_model_output.rq <- function(mod, ...) {
  print(paste("Model class:", class(mod)))
}

#' Handle Output for Unsupported Model Types
#'
#' Internal function to handle cases where the model type is unsupported.
#'
#' @param mod A model object of an unsupported class.
#' @param ... Additional arguments (currently unused).
#'
#' @return Stops execution with an error message indicating the unsupported model type.
#'
#' @keywords internal
#'
#' @exportS3Method thekidsbiostats::thekids_model_output
thekids_model_output.default <- function(mod, ...) {
  stop("Unsupported model type: ", class(mod))
}
