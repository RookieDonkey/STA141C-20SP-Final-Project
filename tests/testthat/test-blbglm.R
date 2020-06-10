test_that("blbglm works", {
  fitglm <- blbglm(Species ~ Sepal.Length * Sepal.Width, data = iris[1:100,],
                   m = 3, B = 100, family = binomial)
  expect_s3_class(fitglm, "blbglm")
  coglm <- coef.blblm(fitglm)
  expect_equal(length(coglm), 4)
})