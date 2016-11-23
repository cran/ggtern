[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggtern)](https://cran.r-project.org/package=ggtern)
[![Downloads](http://cranlogs.r-pkg.org/badges/ggtern)](https://cran.r-project.org/package=ggtern)

## ggtern

*An extension to ggplot2, for the creation of ternary diagrams.*

`ggtern` is a package that extends the functionality of [ggplot2](http://goo.gl/YDk79h), giving the capability to plot ternary diagrams for (subset of) the `ggplot2` proto geometries. Ternary diagrams are used frequently in a number of disciplines to graph compositional features for mixtures of three different elements or compounds. It is possible to represent a coordinate system having three (3) degrees of freedom, in 2D space, since the third dimention is linear and depends only on the other two. 

`ggtern` is a package that is based on (extends) the very popular `ggplot2`, which is an implementation of Wilkinsons [The Grammar of Graphics](http://goo.gl/G5DEo3),  and, makes provision for a highly methodical construction process for the development  of meaningful (graphical) data representations. Of course, the above book by Wilkinson outlines the theory, whilst Hadley Wickhams `ggplot2` implementation is where much of the magic happens, and, an ideal base-platform for `ggtern`.

### Installation

Install the latest release on CRAN, install just like any other R package:

```
  install.packages('ggtern')
```

To install the development / working version, use the `devtools` package:

```
  devtools::install_git('https://bitbucket.org/nicholasehamilton/ggtern')
```

### Contributing

Please contribute push/pull requests via [BitBucket repository](https://bitbucket.org/nicholasehamilton/ggtern). Financial contributions, where possible, are very much appreciated. Please navigate to the [ggtern website](http://www.ggtern.com) if you are feeling generous...

### Authors

Nicholas Hamilton [aut, cre]

### License

This project is licensed under GPL2 - see the [LICENSE](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) for details.

### Acknowledgments

HUGE thanks to Hadley Wickham and all those that have controbuted to the [ggplot2](http://ggplot2.org/) package, without which, this would not be possible.

