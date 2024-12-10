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
  print(paste("Model class:", class(mod)))

  if(is.null(dat_mod)) dat_mod <- mod$model

  mod_desc <- dat_mod %>%
    tbl_summary(by = by,
                type = list(where(is.numeric) ~ "continuous"),
                statistic = list(all_continuous() ~ "{mean} ({sd}) [{N_nonmiss}]")) %>%
    modify_header(label = by) %>%
    add_p() %>%
    bold_labels() %>%
    suppressMessages() %>% suppressWarnings()

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
