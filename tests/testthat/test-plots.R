test_that("aop_gene_summary() returns the expected columns", {
  httptest2::with_mock_dir("fixtures/gene_summary", {
    res <- aop_gene_summary(38)
    expect_s3_class(res, "tbl_df")
    expect_true(all(c("ke_id", "ke_title", "role", "detected_by", "n_genes")
                    %in% names(res)))
    expect_type(res$n_genes, "integer")
  })
})

test_that("aop_gene_summary() returns 0 rows when an AOP has no genes", {
  httptest2::with_mock_dir("fixtures/gene_summary_empty", {
    res <- aop_gene_summary(1)
    expect_equal(nrow(res), 0)
    expect_true("ke_title" %in% names(res))
  })
})

test_that("aop_plot_genes() returns a ggplot", {
  skip_if_not_installed("ggplot2")
  httptest2::with_mock_dir("fixtures/gene_plot", {
    p <- aop_plot_genes(38)
    expect_s3_class(p, "ggplot")
  })
})

test_that("aop_gene_summary() validates its input", {
  expect_error(aop_gene_summary(-1), "positive whole number")
})
