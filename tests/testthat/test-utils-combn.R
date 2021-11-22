test_that("utils::combn imports", {
    expect_equal(
        combn(2, 1),
        matrix(1:2, 1, 2)
    )
})
