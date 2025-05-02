# substab-project

Literate programming implementation of substab R package. This R package is created using literate programming with the  [litr](https://github.com/jacobbien/litr-project/tree/main/litr) R package.  Please see [substab](substab) for the generated R package itself.

## Code for generating the `substab` package

After cloning this repo...

```r
remotes::install_github("jacobbien/litr-project", subdir = "litr")
litr::render("index.Rmd", output_format = litr::litr_gitbook())
```

This will create the package directory [substab](substab).  It will also create a nice bookdown that explains the source code.  To see this, open `_book/index.html` in the browser. (After downloading/cloning the repo.)

## To install the package

The following command can be used to install the package:
```r
remotes::install_github("Xiaozhu-Zhang1998/substab", subdir = "substab")
```
