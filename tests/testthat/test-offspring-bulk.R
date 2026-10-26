## offspring_bulk() computes the descendants of many nodes in one post-order
## pass. It must agree with offspring() node by node.

test_that("offspring_bulk() matches offspring() for every internal node", {
    for (n in c(5, 20, 100)) {
        for (s in 1:3) {
            set.seed(s)
            tb <- as_tibble(ape::rtree(n))
            got <- offspring_bulk(tb)
            expect_gt(length(got), 0L)
            for (nd in as.integer(names(got))) {
                expect_setequal(got[[as.character(nd)]], offspring(tb, nd)$node)
            }
        }
    }
})

test_that("offspring_bulk(tips_only = TRUE) matches offspring(tiponly = TRUE)", {
    set.seed(1)
    tb <- as_tibble(ape::rtree(30))
    got <- offspring_bulk(tb, tips_only = TRUE)
    for (nd in as.integer(names(got))) {
        expect_setequal(got[[as.character(nd)]],
                        offspring(tb, nd, tiponly = TRUE)$node)
    }
})

test_that("offspring_bulk() honours the nodes argument", {
    set.seed(2)
    tb <- as_tibble(ape::rtree(20))
    internal <- unique(tb$parent[tb$parent != tb$node])
    picked <- internal[1:3]
    got <- offspring_bulk(tb, nodes = picked)
    expect_identical(names(got), as.character(picked))
    for (nd in picked) {
        expect_setequal(got[[as.character(nd)]], offspring(tb, nd)$node)
    }
})

test_that("offspring_bulk() does not include the node itself", {
    set.seed(3)
    tb <- as_tibble(ape::rtree(10))
    got <- offspring_bulk(tb)
    for (nd in as.integer(names(got))) {
        expect_false(nd %in% got[[as.character(nd)]])
    }
})

test_that("offspring_bulk() handles a ladder tree", {
    tr <- ape::stree(50, type = "left")
    tb <- as_tibble(tr)
    got <- offspring_bulk(tb)
    for (nd in as.integer(names(got))) {
        expect_setequal(got[[as.character(nd)]], offspring(tb, nd)$node)
    }
})

test_that("offspring_bulk() handles a star tree", {
    tr <- ape::stree(20, type = "star")
    tb <- as_tibble(tr)
    got <- offspring_bulk(tb)
    for (nd in as.integer(names(got))) {
        expect_setequal(got[[as.character(nd)]], offspring(tb, nd)$node)
    }
})

test_that("offspring_bulk() rejects multiPhylo", {
    expect_error(offspring_bulk(ape::rmtree(2, 5)), "multiPhylo")
})
