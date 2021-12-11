test_that("lln_demo error checks on input hold", {
    expect_error(lln_demo("50"))
    expect_error(lln_demo(c(10, 20, 30)))
    expect_error(lln_demo(10))
})
test_that("lln_demo produces a graph", {
    expect_equal(class(lln_demo()$plot)[1], "gg")
})
