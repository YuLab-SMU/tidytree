context("grouping")

library(treeio)

nwk <- system.file("extdata", "sample.nwk", package="treeio")
tree <- read.tree(nwk)

d <- as_data_frame(tree)
x <- groupClade(d, c(17, 21))

test_that("group by clade", {
    expect_equal(filter(x, group == 1 & node != 17)$node, offspring(d, 17)$node)
    expect_equal(filter(x, group == 2 & node != 21)$node, offspring(d, 21)$node)
})

cls <- list(c1=c("A", "B", "C", "D", "E"),
            c2=c("F", "G", "H"),
            c3=c("L", "K", "I", "J"),
            c4="M")

y <- groupOTU(d, cls)

test_that("group by taxa", {
    expect_equal(filter(y, group == 'c1')$node,
                 filter(d, node %in% c(filter(d, node %in% 1:5)$parent, 1:5))$node)
    expect_equal(filter(y, group == 'c2')$node,
                 filter(d, node %in% c(filter(d, node %in% 6:8)$parent, 6:8))$node)
    expect_equal(filter(y, group == 'c3')$node,
                 sort(filter(d, node %in% c(filter(d, node %in% 9:12)$parent, 9:12, mrca(d, 9, 12)$node))$node))
     expect_equal(filter(y, group == 'c4')$node, 13:14)
})
