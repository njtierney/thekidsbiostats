#' Handle output for Linear Models
#'
#' Internal function to handle the output for objects of class \code{lm}.
#'
#' @param mod A fitted linear model object of class \code{lm}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A list of model-specific output.
#'
#' @method thekids_model_output lm
#'
#' @keywords internal
#'
#' @exportS3Method thekidsbiostats::thekids_model_output
thekids_model_output.lm <- function(mod, ...) {
  print(paste("Model class:", class(mod)))
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
