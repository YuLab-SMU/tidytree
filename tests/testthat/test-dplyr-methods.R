context("dplyr-methods")

nwk <- '(((((((A:4,B:4):6,C:5):8,D:6):3,E:21):10,((F:4,G:12):14,H:8):13):13,((I:5,J:2):30,(K:11,L:11):2):17):4,M:56);'
dat <- tibble(node=c(1, 2, 3, 4, 5), group=c("A", "A", "A", "B", "B"), test=c(10, 20, 30, 40, 50))

tree <- read.tree(text=nwk) %>% treeio::as.treedata()
tree@data <- dat

test_that("select fields from treedata and return tbl_df directly ",{
    expect_equal(tree %>% select(group) %>% nrow(), tree %>% as_tibble() %>% nrow())
    expect_equal(tree %>% select(node, group) %>% filter(!is.na(group)) %>% nrow(), dat %>% nrow())
})

test_that("select fields from treedata and return treedata",{
    expect_true(inherits(tree %>% select(-group, keep.td=TRUE), "treedata"))
    expect_true(inherits(tree %>% select(-c(group, test), keep.td=TRUE), "treedata"))
    expect_equal(tree %>% select(-test, keep.td=TRUE) %>% get.fields, "group")
})

test_that("filter fields for treedata and return tbl_df directly",{
    expect_equal(tree %>% 
                   filter(group=="A", keep.td=FALSE) %>% 
                   nrow(), 
                 dat %>% 
                   filter(group=="A") %>% 
                   nrow()
    )
    expect_equal(tree %>% 
                   filter(group=="A" & test>=20, keep.td=FALSE) %>% 
                   nrow(), 
                 dat %>% 
                   filter(group=="A" & test>=20) %>% 
                   nrow()
    )
})

test_that("filter fields for treedata and return treedata", {
    expect_true(inherits(tree %>% filter(group=="A", keep.td=TRUE), "treedata"))
    tree2 <- tree %>% filter(group=="A" & test>=20, keep.td=TRUE)
    expect_equal(tree2@data %>% 
                   filter(!is.na(group)) %>%
                   nrow(),
                 dat %>% 
                   filter(group=="A" & test>=20) %>%
                   nrow() 
    )
})

test_that("mutate fields for treedata and return tbl_df", {
    expect_equal(tree %>% 
                   mutate(type="A", keep.td=FALSE) %>% 
                   nrow(),
                 tree %>% 
                   as_tibble() %>%
                   nrow()
    )

    expect_equal(tree %>%
                   mutate(test="A", keep.td=FALSE) %>%
                   colnames(),
                 c("node", "label", "isTip", colnames(dat)[-1])
    )

})

test_that("mutate fields for treedata and return treedata", {
    expect_true(inherits(tree %>% 
                  mutate(type="A", keep.td=TRUE),
                  "treedata")
    )
    tree2 <- tree %>% mutate(type="A", keep.td=TRUE)
    p <- ggtree::ggtree(tree2)
    expect_true(inherits(p, "ggtree"))
    expect_equal(tree2@extraInfo %>% nrow(), 
                 tree %>% treeio::Nnode(internal.only=FALSE) 
    )
})

test_that("left_join for treedata",{
    set.seed(123)
    df <- data.frame(label=tree@phylo$tip.label, value=abs(rnorm(length(tree@phylo$tip.label))))
    N <- tree %>% treeio::Nnode(internal.only=FALSE)
    dt <- data.frame(ind=rep(seq_len(N), 2), group=rep(c("A","B"), each=N))

    tr2 <- tree %>% left_join(df, by="label")

    tr3 <- tree %>% left_join(dt, by=c("node"="ind"))

    expect_true(inherits(tr2, "treedata"))

    expect_true(inherits(tr3, "treedata"))

    expect_equal(tree %>% 
                   as_tibble() %>% 
                   nrow(), 
                 tr3 %>% 
                   as_tibble() %>% 
                   nrow())

    expect_equal(tr3 %>% 
                   select("node", "group.y") %>%
                   tidyr::unnest("group.y") %>%
                   dplyr::rename(ind="node", group="group.y"),
                 dt %>% 
                   tibble::as_tibble() %>%
                   dplyr::arrange(ind)
    )
})
