# Generated from _main.Rmd: do not edit by hand  
testthat::test_that("gen_clust works as expected", {
  set.seed(123)
  Xclust <- gen_clust(n=1000, s=2, d = 4, eta = 1)
  testthat::expect_equal(round(apply(Xclust, 2, var)),
                         c(1, 2, 2, 2, 1, 2, 2, 2))
})

testthat::test_that("gen_block works as expected", {
  set.seed(123)
  Xblock <- gen_block(n=1000, nb=2, b = 4, eta = 1)
  testthat::expect_equal(round(apply(Xblock, 2, var)),
                         c(1, 1, 1, 1, 5, 1, 1, 1, 1, 5))
})

