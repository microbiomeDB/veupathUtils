<!-- badges: start -->
  [![R-CMD-check](https://github.com/microbiomeDB/mbioUtils/workflows/R-CMD-check/badge.svg)](https://github.com/microbiomeDB/mbioUtils/actions)
  <!-- badges: end -->

# mbioUtils

mbioUtils is an R package which provides utility functions and data structures for microbiomeDB R packages. It was forked from veupathUtils 2.6.7 and stripped of VEuPathDB-specific functionality.

## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install mbioUtils. From the R command prompt:

```R
remotes::install_github('microbiomeDB/mbioUtils')
```

## Usage
This package is primarily intended for use as a dependency in other R packages. In order to establish that depedency the developer of the 
dependent package must follow these steps:
1. add ```mbioUtils``` to the ```Imports``` section of the dependent package's ```DESCRIPTION``` file.
2. add a ```Remotes``` section to the dependent package's ```DESCRIPTION``` file.
3. add ```microbiomeDB/mbioUtils``` to the ```Remotes``` section of the dependent package's ```DESCRIPTION``` file.
4. add ```#' @import mbioUtils``` to the dependent package's package-level documentation file (usually called ```{mypackage}-package.R```).
5. run ```devtools::document()```.

The developer of the dependent package can either install this package using ```remotes``` as descripted in the "Installation" section above,
or if they mean to also develop mbioUtils simultaneously, can use ```devtools::load_all("{path-to-mbioUtils}")``` to load this package in 
their R session.

## Contributing
Pull requests are welcome and should be made to the **dev** branch. 

For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

As a general policy, we're exporting every function that gets added here. So unless you have very good reason, export!

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)
