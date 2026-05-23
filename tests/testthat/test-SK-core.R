# test-SK-core.R — basic regression and smoke tests for the ScottKnott package.
#
# Organisation:
#   1. Class and components of the SK object
#   2. Balanced designs: formula, aov, lm inputs
#   3. Unbalanced data
#   4. summary and print
#   5. plot.SK (smoke tests)
#   6. xtable.SK

library(ScottKnott)

# ===========================================================================
# 1. Class and components
# ===========================================================================

test_that("SK returns an object of class 'SK'", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  expect_s3_class(sk, "SK")
})

test_that("SK object contains the expected components", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  expect_true(all(c("out", "info", "stat", "clus") %in% names(sk)))
})

# ===========================================================================
# 2. Balanced designs
# ===========================================================================

test_that("SK works with formula input — CRD", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  expect_s3_class(sk, "SK")
})

test_that("SK works with aov input — CRD", {
  data(CRD1)
  av <- with(CRD1, aov(y ~ x, data = dfm))
  sk <- SK(av, which = "x")
  expect_s3_class(sk, "SK")
})

test_that("SK works with lm input — CRD", {
  data(CRD1)
  lm1 <- with(CRD1, lm(y ~ x, data = dfm))
  sk <- SK(lm1, which = "x")
  expect_s3_class(sk, "SK")
})

test_that("SK works with formula input — RCBD", {
  data(RCBD)
  sk <- with(RCBD, SK(y ~ blk + tra, data = dfm, which = "tra"))
  expect_s3_class(sk, "SK")
})

test_that("SK works with aov input — RCBD", {
  data(RCBD)
  av <- with(RCBD, aov(y ~ blk + tra, data = dfm))
  sk <- SK(av, which = "tra")
  expect_s3_class(sk, "SK")
})

test_that("SK respects sig.level argument", {
  data(CRD1)
  sk_05 <- with(CRD1, SK(y ~ x, data = dfm, which = "x", sig.level = 0.05))
  sk_01 <- with(CRD1, SK(y ~ x, data = dfm, which = "x", sig.level = 0.01))
  # A stricter level must produce at least as many groups as a looser one.
  expect_gte(
    length(unique(sk_01$out$Result[, 2])),
    length(unique(sk_05$out$Result[, 2]))
  )
})

# ===========================================================================
# 3. Unbalanced data
# ===========================================================================

test_that("SK handles unbalanced data via formula — CRD", {
  data(CRD2)
  uCRD2 <- CRD2$dfm
  uCRD2[c(3, 5, 10), 3] <- NA
  sk <- SK(y ~ x, data = uCRD2, which = "x")
  expect_s3_class(sk, "SK")
})

test_that("SK handles unbalanced data via lm — CRD", {
  data(CRD2)
  uCRD2 <- CRD2$dfm
  uCRD2[c(3, 5, 10), 3] <- NA
  lm1 <- lm(y ~ x, data = uCRD2)
  sk <- SK(lm1, which = "x")
  expect_s3_class(sk, "SK")
})

# ===========================================================================
# 4. summary and print
# ===========================================================================

test_that("summary.SK produces output without error", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  expect_no_error(summary(sk))
})

test_that("print.SK produces output without error", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  expect_no_error(print(sk))
})

# ===========================================================================
# 5. plot.SK
# ===========================================================================

test_that("plot.SK runs without error — basic", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  expect_no_error(plot(sk))
})

test_that("plot.SK runs without error for all dispersion options", {
  data(CRD2)
  sk <- with(CRD2, SK(y ~ x, data = dfm, which = "x"))
  for (disp in c("mm", "sd", "ci", "cip"))
    expect_no_error(plot(sk, dispersion = disp))
})

# ===========================================================================
# 6. xtable.SK
# ===========================================================================

test_that("xtable.SK returns an object of class 'xtable'", {
  data(CRD1)
  sk <- with(CRD1, SK(y ~ x, data = dfm, which = "x"))
  xt <- xtable(sk)
  expect_s3_class(xt, "xtable")
})
