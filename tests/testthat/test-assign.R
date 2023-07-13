context("assign and access label methods")

test_that("accessor and assignment label method for phylo, tbl_tree and treedata",{
    tr <- ape::rtree(4)
    tr.da <- treedata(phylo=tr)
    tr.df <- as_tibble(tr)
    
    new.lab <- c('a', 'b', 'c', 'd')

    tip.label(tr) <- new.lab
    tip.label(tr.da) <- new.lab
    tip.label(tr.df) <- new.lab
    
    expect_equal(tip.label(tr), new.lab)
    expect_equal(tip.label(tr.da), new.lab)
    expect_equal(tip.label(tr.df), new.lab)
    
    node.lab <- c('node1', 'node2', 'node3')
    node.label(tr) <- node.lab
    node.label(tr.da) <- node.lab
    node.label(tr.df) <- node.lab
    
    expect_equal(node.label(tr), node.lab)
    expect_equal(node.label(tr.da), node.lab)
    expect_equal(node.label(tr.df), node.lab)
  }
)
