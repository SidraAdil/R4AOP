test_that("ke_components() returns one row per component", {
  httptest2::with_mock_dir("fixtures/ke_components", {
    res <- ke_components(55)
    expect_s3_class(res, "tbl_df")
    expect_true(all(c("ke_id", "action", "process", "process_id",
                      "object", "object_id") %in% names(res)))
    expect_equal(unique(res$ke_id), 55L)
  })
})

test_that("ke_components() reports ontology identifiers", {
  httptest2::with_mock_dir("fixtures/ke_components_mie", {
    res <- ke_components(244)
    expect_true(any(grepl("^GO:", res$process_id)))
  })
})

test_that("ke_components() reports a missing key event clearly", {
  httptest2::with_mock_dir("fixtures/ke_components_missing", {
    expect_error(ke_components(999999), "does not exist")
  })
})

test_that("aop_components() adds the key event title and role", {
  httptest2::with_mock_dir("fixtures/aop_components", {
    res <- aop_components(38)
    expect_true(all(c("ke_title", "role") %in% names(res)))
    expect_gt(nrow(res), 0)
  })
})

test_that("ke_components() validates its input", {
  expect_error(ke_components(-1), "positive whole number")
})
