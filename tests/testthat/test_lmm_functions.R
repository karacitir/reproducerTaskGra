test_that("should return an object of class lme", {
  lmm_CORRECTNESS <- reproduce_lmm_CORRECTNESS()
  expect_is(lmm_CORRECTNESS, "lme")
})
