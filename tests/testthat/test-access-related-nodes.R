context("related_nodes")

library(ape)
set.seed(42)
# sample bifurcating tree
bi_tree <- ape::rtree(10)
bi_tree$tip.label <- paste0("t", 1:10)
# sample non-bifurcating tree
multi_tree <- ape::di2multi(bi_tree, tol=0.5)
# bifurcating tree with node names
named_bi_tree <- bi_tree
named_bi_tree$node.label <- paste0("n", 11:19)
# non-bifurcating tree with node names
named_multi_tree <- multi_tree
named_multi_tree$node.label <- paste0("n", 11:16)

empty_tbl <- tibble::data_frame(
  parent=integer(0),
  node=integer(0),
  branch.length=numeric(0),
  label=character(0),
  isTip=logical(0)
)

test_that("conversion to table is reversible", {
  expect_equal(as.phylo(as_data_frame(bi_tree)), bi_tree)
  expect_equal(as.phylo(as_data_frame(multi_tree)), multi_tree)
  expect_equal(as.phylo(as_data_frame(named_bi_tree)), named_bi_tree)
  expect_equal(as.phylo(as_data_frame(named_multi_tree)), named_multi_tree)
})

test_that("child works for bifurcating trees", {
  # a leaf has no children
  expect_equal(child(as_data_frame(bi_tree), 1), empty_tbl)
  # can find node children
  expect_equal(child(as_data_frame(bi_tree), 19)$node, 7:8)
  # can find root children
  expect_equal(child(as_data_frame(bi_tree), 11)$node, c(10,12))
})

test_that("child works for non-bifurcating trees", {
  # a leaf has no children
  expect_equal(child(as_data_frame(multi_tree), 1), empty_tbl)
  # can find node children
  expect_equal(child(as_data_frame(multi_tree), 12)$node, c(3,9,13,14))
  # can find root children
  expect_equal(child(as_data_frame(multi_tree), 11)$node, c(10,12))
})

test_that("offspring works on bifurcating trees", {
  expect_equal(offspring(as_data_frame(bi_tree), 1), empty_tbl)
  expect_equal(offspring(as_data_frame(bi_tree), 11)$node, (1:19)[-11])
  expect_equal(offspring(as_data_frame(bi_tree), 17)$node, c(4:6, 18))
})

test_that("offspring works on non-bifurcating trees", {
  expect_equal(offspring(as_data_frame(multi_tree), 1), empty_tbl)
  expect_equal(offspring(as_data_frame(multi_tree), 11)$node, (1:16)[-11])
  expect_equal(offspring(as_data_frame(multi_tree), 14)$node, c(4:8, 15:16))
})

test_that("parent works for bifurcating trees", {
  expect_equal(parent(as_data_frame(bi_tree), 11), empty_tbl)
  expect_equal(parent(as_data_frame(bi_tree), 1)$node, 15)
  expect_equal(parent(as_data_frame(bi_tree), 17)$node, 16)
})

test_that("parent works for non-bifurcating trees", {
  expect_equal(parent(as_data_frame(multi_tree), 11), empty_tbl)
  expect_equal(parent(as_data_frame(multi_tree), 8)$node, 16)
  expect_equal(parent(as_data_frame(multi_tree), 14)$node, 12)
})

test_that("ancestor works for bifurcating trees", {
  expect_equal(ancestor(as_data_frame(bi_tree), 11), empty_tbl)
  expect_equal(ancestor(as_data_frame(bi_tree), 1)$node, 11:15)
  expect_equal(ancestor(as_data_frame(bi_tree), 17)$node, c(11:13, 16))
})

test_that("ancestor works for non-bifurcating trees", {
  expect_equal(ancestor(as_data_frame(multi_tree), 11), empty_tbl)
  expect_equal(ancestor(as_data_frame(multi_tree), 8)$node, c(11,12,14,16))
  expect_equal(ancestor(as_data_frame(multi_tree), 14)$node, 11:12)
})


test_that("mrca works for bifurcating trees", {
    expect_equal(mrca(as_data_frame(multi_tree), 11, 5), empty_tbl)
    expect_equal(mrca(as_data_frame(bi_tree), 5, 7)$node, 16)
})

test_that("mrca works for non-bifurcating trees", {
  expect_equal(mrca(as_data_frame(multi_tree), 11, 5), empty_tbl)
  expect_equal(mrca(as_data_frame(multi_tree), 5, 7)$node, 14)
})


test_that("sibling works for bifurcating trees", {
  expect_equal(sibling(as_data_frame(bi_tree), 11), empty_tbl)
  expect_equal(sibling(as_data_frame(bi_tree), 1)$node, 2)
  expect_equal(sibling(as_data_frame(bi_tree), 17)$node, 19)
})

test_that("sibling works for non-bifurcating trees", {
  expect_equal(sibling(as_data_frame(multi_tree), 11), empty_tbl)
  expect_equal(sibling(as_data_frame(multi_tree), 12)$node, 10)
  expect_equal(sibling(as_data_frame(multi_tree), 3)$node, c(9,13,14))
  expect_equal(sibling(as_data_frame(multi_tree), 4)$node, 5)
})
