# ess-view

## Summary

A minor-mode to view **R** dataframes within a spreadsheet software.


## Why use ess-view?

*Cleaning Data* is one of the most boring part of the data analysis process; during this process
I find it useful to be able to have a look at the dataframes I'm working with.
Great tools already exist for doing this within R/ess (eg [DT](http://rstudio.github.io/DT/) or [ess-R-data-view](https://github.com/myuhe/ess-R-data-view.el)),
but when working with quite big datasets I personally prefer to use a spreadsheet software.
 The *ess-view* minor mode simply provides functions to create a temporary csv copy of the dataframe of interest and then calls an external spreadsheet software to view it.
 
## Installing

`ess-view` is available through MELPA, so, if you can install it using:

    M-x package-install [RET] ess-view [RET]



## Usage

	
If you want to have a look at a dataframe simply hit (a buffer running a **R** process)

    C-x w

 and you will be asked for the name of the object (dataframe) to view... it's a simple as that!


If you would like to modify the dataframe within the spreadsheet software and then have the modified version
loaded back in the original **R** dataframe, use:

    C-x q

When you've finished modifying the datasets, save the file (depending on the spreadsheet software you use, you may
be asked if you want to save the file as a *csv* file and/or you want to overwrite the original file: the answer to
both question is *yes*).

By default `row.names` of the dataframe are not exported; if you want to see the dataframe with its row names, you
need to use the prefix argument `0` (zero), i.e. use

    C-u 0 C-x w

or

    C-u 0 C-x q

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2016 boccigionata.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
