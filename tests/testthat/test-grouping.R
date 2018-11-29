context("grouping")


nwk <- '(((((((A:4,B:4):6,C:5):8,D:6):3,E:21):10,((F:4,G:12):14,H:8):13):13,((I:5,J:2):30,(K:11,L:11):2):17):4,M:56);'

tree <- read.tree(text=nwk)

d <- as_tibble(tree)
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
                 sort(filter(d, node %in% c(filter(d, node %in% 9:12)$parent, 9:12, MRCA(d, 9, 12)$node))$node))
     expect_equal(filter(y, group == 'c4')$node, 13:14)
})
