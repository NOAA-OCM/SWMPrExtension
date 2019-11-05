## Test environments
* local windows 10 install, R 3.6.1
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel, release, and old-release)
* devtools::release()


## R CMD check results
There were no ERRORs, no WARNINGs and 0 NOTES.

Some of the R-hub check_for_cran results gave he below note:
NOTE:  Namespace in Imports field not imported from: ‘rgeos’
     All declared Imports should be used.

     However, `rgeos` is needed for examples in national_sk_map.R.  I'm not sure why it shows up as not being used.

## Downstream dependencies
None.
