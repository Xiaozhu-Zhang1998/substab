# `substab` project

Literate programming implementation of substab R package. This R package is created using literate programming with the  [litr](https://github.com/jacobbien/litr-project/tree/main/litr) R package.  Please see [substab](substab) for the generated R package itself.

## To install the package
This repository contains the `substab` R package, which can be installed as follows:
```r
remotes::install_github("Xiaozhu-Zhang1998/substab", subdir = "substab")
```
The source code is in the form of a bookdown, available
[here](https://xiaozhu-zhang1998.github.io/substab/create).

## Modification of the `substab` package
To modify the code in this R package, modify the `.Rmd` files in the `create-substab/` directory and then (from an R session in this directory) run the following:

```r
litr::render("create-substab/index.Rmd")
fs::dir_copy("create-substab/_book", "docs/create", overwrite = TRUE)
fs::dir_delete("create-substab/_book")
fs::dir_copy("create-substab/substab", "substab", overwrite = TRUE)
fs::dir_delete("create-substab/substab")
fs::dir_delete("create-substab/_main_files/")
```
