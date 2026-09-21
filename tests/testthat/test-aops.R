test_that("aop_list() returns a tibble of AOPs", {
  httptest2::with_mock_dir("fixtures/aop_list", {
    res <- aop_list(q = "liver")
    expect_s3_class(res, "tbl_df")
    expect_gt(nrow(res), 0)
    expect_true(all(c("id", "title") %in% names(res)))
  })
})

test_that("aop_get() returns one row", {
  httptest2::with_mock_dir("fixtures/aop_get", {
    res <- aop_get(1)
    expect_equal(nrow(res), 1)
    expect_equal(res$id, 1)
  })
})

test_that("aop_key_events() filters by role", {
  httptest2::with_mock_dir("fixtures/aop_kes", {
    res <- aop_key_events(1, role = "mie")
    expect_s3_class(res, "tbl_df")
  })
})

test_that("empty results keep their columns", {
  httptest2::with_mock_dir("fixtures/aop_genes", {
    aop_genes(37)                 # learns the columns
    empty <- aop_genes(1)         # AOP 1 has no genes
    expect_equal(nrow(empty), 0)
    expect_true("symbol" %in% names(empty))
  })
})
