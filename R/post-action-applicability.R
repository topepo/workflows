#' Add an applicability domain to a workflow
#'
#' @description
#' - `add_applicability()` specifies the type of applicability domain model that
#'   should be used.
#'
#' - `remove_applicability()` removes the applicability domain model as well as
#'   any downstream objects that might get created after the applicability
#'   domain is used for post-processing.
#'
#' - `update_applicability()` first removes the applicability domain model, then
#'   replaces it with the new one.
#'
#' @param x A workflow
#'
#' @param method The type of applicability domain model to use. Current values
#' are: "hat_values", "isolation", "pca", and "similarity".
#'
#' @param predictors Tidyselect expressions specifying the original predictors
#' of the  model. See [tidyselect::select_helpers] for the full range of
#' possible ways  to specify terms. The default is to use all predictors given
#' to the workflow.
#'
#' @param ... Options to pass to the applicability domain model.
#'
#' @return
#' `x`, updated with either a new or removed applicability postprocessor.
#'
#' @export
#' @examples
#' library(applicable)
add_applicability <- function(x, method, predictors = NULL, ...) {
  # check_dots_empty()
  method <- rlang::arg_match0(method, apd_methods)

  # validate `predictors`; if NULL go to the pre:settings
  predictors <- enquos(predictors)

  validate_applicability_available(method)
  action <- new_action_applicability(method = method, predictors = predictors, ...)
  res <- add_action(x, action, "applicability")
  res
}

# Can a post-process take the original data as an input to fit?

apd_methods <- c("similarity", "hat_values", "pca", "isolation")

#' @rdname add_applicability
#' @export
remove_applicability <- function(x) {
  validate_is_workflow(x)

  if (!has_postprocessor(x)) {
    cli_warn("The workflow has no applicability postprocessor to remove.")
  }

  actions <- x$post$actions
  actions[["applicability"]] <- NULL

  # note that the preprocessor and model fit don't need to be "untrained"
  # with new_stage_* since they are unaffected by the post-processor.
  new_workflow(
    pre = x$pre,
    fit = x$fit,
    post = new_stage_post(actions = actions),
    trained = FALSE
  )
}

#' @rdname add_applicability
#' @export
update_applicability <- function(x, method, predictors = NULL, ...) {
  check_dots_empty()
  x <- remove_applicability(x)
  add_applicability(x, method, predictors = NULL, ...)
}

validate_applicability_available <- function (method, call = caller_env()) {
  if (method == "isolation") {
    pkgs <- c("isotree", "applicable")
  } else {
    pkgs <- c("applicable")
  }
  rlang::check_installed(pkgs, call = call)
  invisible()
}

# ------------------------------------------------------------------------------

#' @export
fit.action_applicability <- function(object, workflow, data, ...) {
  # subset data

  # switch if predictors = NULL
  mold <- extract_mold0(workflow)
  apd_data <- mold$predictors
  apd_data <- apd_data[, names(apd_data) != "(Intercept)"]

  # case_weights <- extract_case_weights0(workflow)

 # Add apd code
  cl <- rlang::call2(paste0("apd_", object$method), .ns = "applicable",
                     x = expr(apd_data), !!!object$options)
  apd_model <- try(eval_tidy(cl), silent = TRUE)
  # what to do if error?

  new_workflow(
    pre = workflow$pre,
    fit = workflow$fit,
    post = new_stage_post(
      actions = workflow$post$actions,
      fit = apd_model
    )
  )
}

# ------------------------------------------------------------------------------

new_action_applicability <- function(method, predictors = NULL, ..., call = caller_env()) {
  # check_dots_empty()

  opts <- rlang::enquos(...)

  new_action_post(
    method = method,
    predictors = predictors,
    options = opts,
    subclass = "action_applicability"
  )
}

# ------------------------------------------------------------------------------

predict_applicability <- function(object, new_data, ...) {
  rlang::check_installed("applicable")
  cl <- rlang::call2("score", .ns = "applicable", object = expr(object),
                     new_data = expr(new_data))
  predictions <- eval_tidy(cl)
  apd_type <- class(object)[1]

  # The types of predictions do not have homogeneous names, so do some renaming
  rn_list <-
    list(
      "apd_pca" = list(applicability_score = "distance",
                       applicability_percentile = "distance_pctl"),
      "apd_similarity" = list(applicability_score = "similarity",
                              applicability_percentile = "similarity_pctl"),
      "apd_isolation" = list(applicability_score = "score",
                             applicability_percentile = "score_pctl"),
      "apd_hat_values" = list(applicability_score = "hat_values",
                              applicability_percentile = "hat_values_pctls")
    )
  if (!tibble::is_tibble(predictions)) {
    predictions <- tibble::as_tibble(predictions)
  }
  predictions <- dplyr::select(predictions, !!!rn_list[[apd_type]])

  predictions
}


print_postprocessor_applicability <- function(x) {
  # TODO more here
  cat_line("Applicability Domain Model")
  invisible(x)
}
