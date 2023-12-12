# tidytree 0.4.6

+ import `methods::setClassUnion()` to fix R check (2023-12-12, Tue)
+ check before `setOldClass()`, which suppose to fix the #47 issue (2023-11-29, Wed)

# tidytree 0.4.5

+ mv tree operation functions (e.g., `parent()`, `child()`, `tree_subset()`, etc.) from the 'treeio' package (2023-08-03, Thu, #44)
+ update `valid.edge()` to avoid warning (2023-7-18, Tue)

# tidytree 0.4.4

+ update old-style 'CITATION' from `citEntry()` to `bibentry()` (2023-07-14, Fri, #38)
+ update dplyr-verb for 'tbl_tree' object to print information to avoid confusion (2023-07-13, Thu, #37, #39)
+ add accessor function of node label for 'tbl_tree', 'phylo' and 'treedata' object (2023-07-13, Thu, #37)

# tidytree 0.4.3

+ add `print()` method for 'tbl_tree' object to avoid confusion with `tbl_df` (2023-07-12, Wed)

# tidytree 0.4.2

+ fixed the `dots` issue of `left_join` (2022-12-16, Fri, #30, #31)

# tidytree 0.4.1

+ update citation with the ggtree imeta paper (2022-08-13, Sat)

# tidytree 0.4.0

+ update citation with the tree data book (2022-08-13, Sat)

# tidytree 0.3.9

+ update package startup message (2022-03-04, Fri)

# tidytree 0.3.8

+ add the CRC book in startup message (2022-02-17, Thu)

# tidytree 0.3.7

+ update startup message to randomly print two citation items of the ggtree package suite (2022-01-10, Mon)

# tidytree 0.3.6

+ use `yulab.utils::str_wrap` to print tree (2021-10-09, Sat)

# tidytree 0.3.5

+ add `select`, `filter`, `mutate`, `left_join`, `unnest`, `pull` and `rename` verbs for `treedata` object (2021-08-22, Sun; @xiangpin, #19)
+ update `print` and `show` methods for `treedata`
  - with `options(show_data_for_treedata=TRUE)` to control whether show associated data (2021-08-20, Fri; @xiangpin, #18)

# tidytree 0.3.4

+ implement `merge` method for `tbl_tree` object (2020-07-03, Fri)
  - <https://github.com/YuLab-SMU/treeio/issues/36>
+ remove mutate_, filter_ and group_by_ according to the change of dplyr (v=1.0.0) (2020-04-09, Thu)

# tidytree 0.3.3

+ remove `data_frame` according to the change of tibble (v=3.0.0)

# tidytree 0.3.2

+ use `tibble` instead of `data_frame` as it was deprecated in tibble (v=3.0.0) (2020-04-02, Thu)
+ compatible with dplyr v=1.0.0 (2020-03-12, Thu)
  - <https://github.com/YuLab-SMU/tidytree/pull/12>
  - <https://github.com/YuLab-SMU/tidytree/issues/13>

# tidytree 0.3.1

+ `groupOTU`: set group from 0 to NA if only root node is in group of 0 (2019-11-25, Mon)

# tidytree 0.3.0

+ `offspring` supports a vector of node ids (2019-11-21, Thu)
+ bug fixed of `nodelab` 
+ `filter` and `select` methods for `ggtree` object (2019-10-31, Thu)

# tidytree 0.2.9

+ `offsprint(tip, self_include=TRUE)` will return the input tip (2019-10-06, Sun)

# tidytree 0.2.8

+ update `offspring` to compatible with missing nodes, e.g. for tree after `ggtree::collapse` (2019-09-16, Mon)

# tidytree 0.2.7

+ allow calling `MRCA` with only one node and will return the node itself (2019-08-30, Fri)

# tidytree 0.2.6

+ `nodeid` and `nodelab` methods for converting from label to node number and vice versa (2019-08-09, Fri)
+ allow determine MRCA of a vector of tips (2019-08-08, Thu)

# tidytree 0.2.5

+ convert elements of roxygen documentation to markdown (2019-05-05, Thu)

# tidytree 0.2.4

+ call `child.tbl_tree` instead of `child` in `offspring`, (2019-02-26, Tue)
  so that it works more robust for `data.frame`.

# tidytree 0.2.3

+ more parameter for `offspring` (2019-01-28, Mon)

# tidytree 0.2.2

+ mv vignette to [treedata-book](https://yulab-smu.top/treedata-book/) (2019-01-10, Thu)

# tidytree 0.2.1

+ `mutate.tbl_tree` method (2018-12-19, Wed)
  - <https://github.com/YuLab-SMU/tidytree/issues/7>
+ bug fixed in `child` 
  - <https://github.com/YuLab-SMU/tidytree/pull/8>

# tidytree 0.2.0

+ compatible with `tibble` v = 2.0.0 (2018-11-29, Thu)
  - change `as_data_frame` method to `as_tibble` since `as_data_frame` was deprecated in `tibble` and not exported as generics
  
# tidytree 0.1.9

+ `as_data_frame.phylo` works with `phylo$root.edge` (2018-06-13, Wed)

# tidytree 0.1.8

+ force `get_tree_data(treedata)$node` to be integer (2018-04-19, Thu)

# tidytree 0.1.7

+ `get.data`, `[` and `[[` methods (2018-02-26, Mon)
