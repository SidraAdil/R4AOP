test_that("check_id() accepts valid ids", {
  expect_silent(check_id(1))
  expect_silent(check_id(597))
})

test_that("check_id() rejects invalid ids", {
  expect_error(check_id(-1), "positive whole number")
  expect_error(check_id(1.5), "positive whole number")
  expect_error(check_id("a"), "positive whole number")
  expect_error(check_id(c(1, 2)), "positive whole number")
})

test_that("check_range() enforces bounds", {
  expect_silent(check_range(2, 1, 3, "depth"))
  expect_error(check_range(9, 1, 3, "depth"), "between 1 and 3")
})

test_that("check_choice() allows NULL but rejects unknown values", {
  expect_silent(check_choice(NULL, c("mie", "ke", "ao"), "role"))
  expect_silent(check_choice("mie", c("mie", "ke", "ao"), "role"))
  expect_error(check_choice("MIE", c("mie", "ke", "ao"), "role"), "must be one of")
})
