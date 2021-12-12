test_that("clt_demo handles errors correctly", {
    expect_error(clt_demo(n = c(10, 20)))
    expect_error(clt_demo(n = 1))
    expect_error(clt_demo(n = "Failing test"))
    expect_error(clt_demo(N = 0))
    expect_error(clt_demo(N = 99))
    expect_error(clt_demo(N = c(100, 200)))
    expect_error(clt_demo(N = "Failing test"))
    expect_error(clt_demo(distribution="Read the docs so this doesn't fail"))
    expect_error(clt_demo(a=c(1, 3)))
    expect_error(clt_demo(b=c(5, 20)))
    expect_error(clt_demo(s=NULL))
})
test_that("clt demo produces graph", {
    # Binormal distribution
    clt <- clt_demo(a=30, b=50, s=10)
    expect_equal(is.numeric(clt$data), TRUE)
    expect_equal(is.vector(clt$data), TRUE)
    expect_equal(class(clt$plot)[2], "ggplot")
    expect_equal(clt$popmean, 40)
    expect_equal(clt$popvariance, 100)

    # Uniform distribution
    clt_uniform <- clt_demo(distribution="uniform", a=0, b=1)
    expect_equal(is.numeric(clt_uniform$data), TRUE)
    expect_equal(is.vector(clt_uniform$data), TRUE)
    expect_equal(class(clt_uniform$plot)[2], "ggplot")
    expect_equal(clt_uniform$popmean, 0.5)
    expect_equal(clt_uniform$popvariance, 1/12)

    # Gamma distribution
    clt_gamma <- clt_demo(distribution="gamma", a=5, b=10)
    expect_equal(is.numeric(clt_gamma$data), TRUE)
    expect_equal(is.vector(clt_gamma$data), TRUE)
    expect_equal(class(clt_gamma$plot)[2], "ggplot")
    expect_equal(clt_gamma$popmean, 5 * 10)
    expect_equal(clt_gamma$popvariance, 5 * 100)
})
