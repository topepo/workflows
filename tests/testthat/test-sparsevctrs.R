skip("Sparse vector materialized errors")

test_that("sparse tibble can be passed to `fit() - recipe", {
  skip_if_not_installed("glmnet")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)
  # materialize outcome
  hotel_data$avg_price_per_room <- hotel_data$avg_price_per_room[]

  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  rec <- recipes::recipe(avg_price_per_room ~ ., data = hotel_data)

  wf_spec <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  expect_no_error(wf_fit <- fit(wf_spec, hotel_data))
})

test_that("sparse tibble can be passed to `fit() - formula", {
  skip_if_not_installed("glmnet")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_formula(avg_price_per_room ~ .) %>%
    add_model(spec)

  expect_snapshot(
    error = TRUE,
    wf_fit <- fit(wf_spec, hotel_data)
  )
})

test_that("sparse tibble can be passed to `fit() - xy", {
  skip_if_not_installed("glmnet")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)
  # materialize outcome
  hotel_data$avg_price_per_room <- hotel_data$avg_price_per_room[]

  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_variables(avg_price_per_room, everything()) %>%
    add_model(spec)

  expect_no_error(wf_fit <- fit(wf_spec, hotel_data))
})
