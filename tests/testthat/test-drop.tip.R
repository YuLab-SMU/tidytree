context("drop.tip and keep.tip")

test_that("drop.tip and keep.tip for treedata",{
  set.seed(123)
  tr <- ape::rtree(6)
  da <- data.frame(id=tip.label(tr), value = letters[seq_len(6)])
  trda <- tr %>% dplyr::left_join(da, by = c('label'='id'))
  toDrop <- c("t2", "t1")
  toKeep <- setdiff(tip.label(trda), toDrop)
  tr1 <- drop.tip(trda, toDrop)
  tr2 <- keep.tip(trda, toKeep)
  expect_equal(tr1, tr2)  
})

