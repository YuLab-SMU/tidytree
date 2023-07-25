context("fulljoin")


set.seed(123)
tr <- ape::rtree(6)
da <- data.frame(label=tip.label(tr), value = letters[seq_len(6)])
y <- full_join(tr, da, by = 'label')

test_that("linking external data to treedata", {
    expect_true(is(y, "treedata"))
    expect_true("value" %in% get.fields(y))
})
