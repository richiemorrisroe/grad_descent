
require(testthat)
test_that("20x can be converted to an expression",
          expect_is(expression("20x"), "Expression"))
