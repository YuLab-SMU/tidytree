## Behaviour of offspring() that must survive any internal change.

test_that("offspring() returns a tbl_tree for one node on a tbl_tree", {
    set.seed(1)
    tb <- as_tibble(ape::rtree(8))
    r <- offspring(tb, 10)
    expect_s3_class(r, "tbl_tree")
    expect_identical(sort(r$node), sort(offspring_bulk(tb, nodes = 10)[[1]]))
})

test_that("offspring() returns an integer vector for one node on a phylo", {
    set.seed(2)
    tr <- ape::rtree(8)
    r <- offspring(tr, 10)
    expect_type(r, "integer")
})

test_that("offspring() returns a named list for several nodes", {
    set.seed(3)
    tb <- as_tibble(ape::rtree(8))
    r <- offspring(tb, c(10, 11))
    expect_type(r, "list")
    expect_identical(names(r), c("10", "11"))
    expect_s3_class(r[[1]], "tbl_tree")
})

test_that("offspring() self_include adds the node itself", {
    set.seed(4)
    tb <- as_tibble(ape::rtree(8))
    without <- offspring(tb, 10)$node
    with <- offspring(tb, 10, self_include = TRUE)$node
    expect_false(10 %in% without)
    expect_true(10 %in% with)
    expect_equal(sort(with), sort(c(10, without)))
})

test_that("offspring() tiponly keeps only tips", {
    set.seed(5)
    tb <- as_tibble(ape::rtree(10))
    r <- offspring(tb, 11, tiponly = TRUE)
    n_tip <- sum(!tb$node %in% tb$parent)
    expect_true(all(r$node <= n_tip))
})

test_that("offspring() of a tip is empty", {
    set.seed(6)
    tb <- as_tibble(ape::rtree(6))
    r <- offspring(tb, 1)
    expect_equal(nrow(r), 0L)
})

test_that("offspring() keeps the other columns of the tree", {
    set.seed(7)
    tb <- as_tibble(ape::rtree(6))
    r <- offspring(tb, 8)
    expect_true(all(c("parent", "node", "label") %in% names(r)))
})
