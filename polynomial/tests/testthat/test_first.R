
require(testthat)
test_that("20x can be converted to an expression",
          expect_is(to_expression("20x"), "Expression"))
test_that("20x can be differentiated",{
    expect_is(differentiate(to_expression("20x")), "Expression")})
