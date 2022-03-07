
<!-- README.md is generated from README.Rmd. Please edit that file -->

# remindStart

<!-- badges: start -->
<!-- badges: end -->

With remindStart, you can … start REMIND!

## Installation

You can install the development version of remindStart like so:

``` r
remotes::install_github("johanneskoch94/remindStart")
```

## Start REMIND from the command line

If your working directory is the remind directory:

``` r
# Default cfg
Rscript -e "remindStart::start()" 

# Pass a scenario config file
Rscript -e "remindStart::start()" --args path/to/config_file.csv
Rscript -e "remindStart::start(configFile = 'path/to/config_file.csv')"

# Restart
Rscript -e "remindStart::start()" --args restart
Rscript -e "remindStart::start(restart = TRUE)"

# Test one region
Rscript -e "remindStart::start()" --args testOneRegi
Rscript -e "remindStart::start(testOneRegi = TRUE)"
```

If your working directory is not the remind directory, specify the path
with the `remind` argument:

``` r
Rscript -e "remindStart::start(remind = 'path/to/remind')" 
```

## Start REMIND from within R

``` r
remindStart::start(configFile = "path/to/config_file.csv")
remindStart::start(remind = "path/to/remind", configFile = "path/to/config_file.csv")
remindStart::start(restart = TRUE)
```
