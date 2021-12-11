test_that("type checks in s_plot",{
    set.seed(1)
    d <- tibble::tibble(s = rep(1:2, 50), y = rnorm(100, 30*s, 5*s))
    expect_error(s_plot("Should be data, so this fails", y))
    expect_error(s_plot(d, y, kind="hist"))
    expect_error(s_plot(d, y, s, kind = "some failure"))
    expect_error(s_plot(d, y, s, kind=c("hist", "boxs_plot")))
})
test_that("s_plot returns gg objects", {
    # The library provides examples of how to test the pixels of s_plot objects
    # The library is well-tested, so our goal is only to prove that the returned
    # objects are actually gg objects through different code paths.
    # The downside to this is we cannot verify which kinds of graphs are s_plotted
    # through the different code paths.
    set.seed(1)
    d <- tibble::tibble(s = rep(1:2, 50), y = rnorm(100, 30*s, 5*s))

    # Different test configurations
    G <- s_plot(d, y)
    expect_equal(class(G)[1], "gg")
    G <- s_plot(d, y, s)
    expect_equal(class(G)[1], "gg")
    G <- s_plot(d, y, s, kind="hist")
    expect_equal(class(G)[1], "gg")
})
