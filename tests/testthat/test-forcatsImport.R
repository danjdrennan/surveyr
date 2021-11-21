test_that("forcats::as_factor imports into NAMESPACE", {
    x <- as_factor(rep(1:5, 20))
    expect_equal(class(x), "factor")
})
