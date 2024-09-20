#' Predict from a workflow
#'
#' @description
#' This is the `predict()` method for a fit workflow object. The nice thing
#' about predicting from a workflow is that it will:
#'
#' - Preprocess `new_data` using the preprocessing method specified when the
#'   workflow was created and fit. This is accomplished using
#'   [hardhat::forge()], which will apply any formula preprocessing or call
#'   [recipes::bake()] if a recipe was supplied.
#'
#' - Call [parsnip::predict.model_fit()] for you using the underlying fit
#'   parsnip model.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param object A workflow that has been fit by [fit.workflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on. If using a recipe preprocessor, you should not call
#'   [recipes::bake()] on `new_data` before passing to this function.
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#'
#' @name predict-workflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' training <- mtcars[1:20, ]
#' testing <- mtcars[21:32, ]
#'
#' model <- linear_reg() %>%
#'   set_engine("lm")
#'
#' workflow <- workflow() %>%
#'   add_model(model)
#'
#' recipe <- recipe(mpg ~ cyl + disp, training) %>%
#'   step_log(disp)
#'
#' workflow <- add_recipe(workflow, recipe)
#'
#' fit_workflow <- fit(workflow, training)
#'
#' # This will automatically `bake()` the recipe on `testing`,
#' # applying the log step to `disp`, and then fit the regression.
#' predict(fit_workflow, testing)
predict.workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  workflow <- object

  if (!is_trained_workflow(workflow)) {
    cli_abort(c(
      "Can't predict on an untrained workflow.",
      "i" = "Do you need to call {.fun fit}?"
    ))
  }

  fit <- extract_fit_parsnip(workflow)
  new_data <- forge_predictors(new_data, workflow)

  # A tailor computes predictions so, if we have one, we don't need to call
  # predict.model_fit. Otherwise we so and might append new columns to the
  # model predictions.

  if (has_postprocessor_tailor(workflow)) {
    # use `augment()` rather than `fit()` to get all possible prediction `type`s.
    # likely, we actually want tailor to check for the existence of needed
    # columns at predict time and just use `predict()` output here.
    fit_aug <- augment(fit, new_data, opts = opts, ...)

    post <- extract_postprocessor(workflow)
    predictions <- predict(post, fit_aug)[post$columns$estimate]
  } else {
    predictions <- predict(fit, new_data, type = type, opts = opts, ...)
  }

  if (has_postprocessor_applicability(workflow)) {
    # TODO this is currently structured in a way that there can be only
    # fitted postprocessor
    apd_pred <-  predict_applicability(workflow$post$fit, new_data)
    # TODO use dplyr here?
    predictions <- cbind(predictions, apd_pred)
    if (!tibble::is_tibble(predictions)) {
      predictions <- tibble::as_tibble(predictions)
    }
  }

  predictions
}

forge_predictors <- function(new_data, workflow) {
  mold <- extract_mold(workflow)
  forged <- hardhat::forge(new_data, blueprint = mold$blueprint)
  forged$predictors
}
