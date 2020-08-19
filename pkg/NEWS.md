# rrMisc v0.37 (Release date: 2020-08-19)
* NEWS file in markdown format

# rrMisc v0.36 (Release date: 2020-08-17)
* new function getContStat()

# rrMisc v0.35 (Release date: 2020-08-13)
* new function encodeUTF8()

# rrMisc v0.34 (Release date: 2020-06-30)
* new function testGranularity()
* start using 'data.table'
* drop function descrTable()

# rrMisc v0.33 (Release date: 2018-09-21)
* output from descrTable - bug fixed for cases where no tests are requested

# rrMisc v0.32 (Release date: 2018-09-20)
* output from descrTable refined
  - number of NA's now formatted with big.mark
  - label1 removed in all cases except first one when using only.first.label=TRUE
  - p-values removed in all cases except first one when using only.first.label=TRUE

# rrMisc v0.31 (Release date: 2017-12-30)
* naToZero() -- missing return for factor-input fixed
* createDefMeasures() -- slight change in default statistics, grouped combination of measures
  mean/sd and median/IQR
* Improved documentation, especially for the grep..() functions

# rrMisc v0.30 (Release date: 2017-11-01)
* new function naToZero() -- converting NAs to 0 and '-' respectively
* in createDefMeasures() -- if no variable label is provided then use variable name instead -- fixed

# rrMisc v0.29 (Release date: 2017-09-08)
* fix documentation for compTwoVects()
* in descrTable() introduce attribute 'only.first.label' to give only the first label of the
  resulting table
* descrTable() minor bug fixes

# rrMisc v0.28 (Release date: 2017-08-10)
* correction in example(cols3.theme()) for color scheme qual01.theme()

# rrMisc v0.27  (Release date: 2017-07-19)
* compTwoVects() - documentation now available

# rrMisc v0.26 (Release date: 2017-07-03)
* descrTable()
    - now can calculate the sum of indicated attributes
    - for group sizes (attribute 'i') the variable 'group.size.total' indicates the column which
      contains total size of the investigated group. For group.size.total = 0 there is no column
      containing all individuals but the total is made up of the sum of all given data.frames.

# rrMisc v0.25 (Release date: 2017-03-23)
* countDistinct() named vector of numbers
  - count:  number of different entries (without NAs)
  - NAs:    index of NAs present (0=no, 1=yes)
* new function grepColNegNum()  show columns of dataframe with negative numbers
* new function grepColFactors() show attributes in data.frame wich are factors
* new function compTwoVects()   compare two vectors for equals and NAs

# rrMisc v0.24 (Release date: 2017-02-07)
* Provid this NEWS-file
* countDistinct() handling of NAs

