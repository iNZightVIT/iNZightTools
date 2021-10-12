- fix bug in `smart_read()` where parsing column types (numerc -> categorical) failed if `NA`s in column
- relocate survey specification reading files to new package [`surveyspec`](github.com/tmelliott/surveyspec)

# iNZightTools 1.12.0

- add new support for metadata formats:
  - multiple-response variables (using `@multi`)
  - numeric na codes (using `@numeric x na=99`)
- allow spaces in new factor level names (`renameLevels()`)
- add additional time formats for `extract_part()`
- add `as_survey()` method for `inzsvyspec` objects (#178)
- drops support for R 3.6 due to lack of RCurl binaries
- prevent showing col types with `read_text()`
- pass `lazy = FALSE` to `readr::read_delim()` to prevent lazy loading of data
- handle missing 'reptype' argument in survey specification file (defaults to 'BRR', the `survey::svrepdesign()` default)

## iNZightTools 1.11.4

- fix bug in `aggregateData()` if `summary_vars` is not specified and there are factor columns in the dataset not showing up in `vars`

## iNZightTools 1.11.3

- fix `form_class_intervals` to work with survey designs (count not yet working)
- remove accents/special chars from factor levels when parsing metadata (due to Windows encoding issues)

## iNZightTools 1.11.2

- prevents tests from failing during CRAN checks when resources unavailable

## iNZightTools 1.11.1

- update `survey_IQR` (and some tests) to work with 'survey' 4.1 (new `svyquantile` function)

# iNZightTools 1.11

- use `srvyr` package to handle survey designs using `dplyr`-like syntax
  - `aggregateData()`, `filterNumeric()`, `filterLevels()`, `filterRows()`, ...
- remove redundant code from `filterLevels()` to make it more concise (using `droplevels()`)
- add method to convert survey design-like objects to iNZight's `inzsvyspec` format
- suppress printing of dataset when joining data
- fix bug where special characters in levels during `collapseLevels()` cause error
- add new `form_class_intervals()` function
- update `import_survey()` to handle URL as the data argument
- fix bug in `reorderLevels(..., freq = TRUE)` (#165, @tmelliott)

# iNZightTools 1.10

- refactor `aggregateData` function to improve flexibility
- new `import_survey()` function for importing a survey design (and data) from a specification file (in TOML format)

## iNZightTools 1.9.1

- fix bug in reading delim files where, if first 1000+ rows of a column were `NA`, the column was read as logical instead of character (this is a fixed behaviour of `readr`, so cannot be overridden)

# iNZightTools 1.9.0

Minor version bump to align with release of iNZight 4.0.

- add functions for reading survey design from a file (and applying to dataset)
- add support for reading JSON files
- add new `selectVars` function
- add negative binomial as an option for `family` in `fitModel`
- add `%notin%` operator to more easily/readably check `! x %in% y`

---

## iNZightTools 1.8.6

- ensure non-numeric values are returned as factors by `read_text()`

## iNZightTools 1.8.5

- increase nmax for previews to 100 (from 10)
- use forcats::fct_cross() with optional argument `keep_empty`

## iNZightTools 1.8.4

- fix handling of special characters in column names (spaces replaces with underscores, all others replaced with a period (.))
- create make_names() function (previously from 'iNZight') to create unique
  new variable names for columns in a data.frame

## iNZightTools 1.8.3

- new `add_suffix()` function to smartly add suffixes to object names and append counter if suffix is already present
- edit package description to meet CRAN standards

## iNZightTools 1.8.2

- fix issues from CRAN submission

## iNZightTools 1.8.1

- new `read_text()` function to read from text string/clipboard
- `smart_read()` now supports URLs by downloading to a temporary file, with the same name
- Extract from datetime no longer includes space in `2010M01`, etc. so it works correctly with `iNZightTS`
- Reshape (wide to long) returns a factor to work with the rest of iNZight

## iNZightTools 1.8

**Release date**: 11 November 2019

- ensure column_types argument is respected
- if no col types specified, pass `col_types = cols()` to suppress the col spec messages
- new `load_rda()` function which loads all data frames in an rda file into a list
- new `save_rda())` function allows saving of a data set to a file, optionally with a different name (i.e., the actual object name can be changed)
- `smart_read()` now handles RDS files
- add attribute to preview excel containing names of available sheets

## iNZightTools 1.7.4

**Release date**: 2 September 2019

- fix bug where collapsing a "numeric" factor was giving an error
- fix bug in reordering same factor twice

## iNZightTools 1.7.3

**Release date**: 26 August 2019

- fixes bug where factor order specified by metadata wasn't being respected
- fix bug preventing first row of metadata comments to be read if description was missing

## iNZightTools 1.7.2

**Release date**: 15 July 2019

- various bug fixes

## iNZightTools 1.7.1

**Release date**: 30 April 2019

- add appveyor ci
- add code tidying functionality
- import SAS files (.sas7bdat and .xpt)
- validate datasets
- join data using `*_join()`
- join data by rows
- reshape data from long to wide, and vice versa
- convert characters to factors (this is how iNZight does things)
- respect numerical order in factor conversions

## iNZightTools 1.6.3

**Release date**: 15 November 2018

- [fix] prefix `survey::` namespace to function calls

## iNZightTools 1.6.2

**Release date**: 10 September 2018

- fix a bug in reading csv files with spaces in the header

## iNZightTools 1.6.1

**Release date**: 04 September 2018

- fix bug in reading metadata where non-meta comments would cancel read

# iNZightTools 1.6

**Release date**: 14 August 2018

### Breaking Changes

- data import is now performed by the `smart_read()` function

### Minor Changes and Fixes

- new function to generate R variable names (for code writing)
- various bug fixes

# Version 1.5

**Release date**: 23 January 2018

## Tidyverse and Code Writing

- data manipulation functions rewritten using tidyverse functions/workflow
- functions written such that the code is generated and evaluated, allowing the code history to be obtained

## Patches

### Patch 1.5.1 - 21/02/2018

- fix tiny bug in encoding default that prevented non-UTF-8 files from reading

### Patch 1.5.2 - 22/02/2018

- fix a bug where timezone NA on macs prevented reading data

### Patch 1.5.3 - 12/03/2018

- fix a bug in csv reading on macos

---

# Version 1.4

**Release date**: 25 August 2017

- Code history features: interpolate code
- Re-document package; pass CRAN checks
- start converting data modification functions to tidyverse

## Patches

### Version 1.4-1, 02/10/2017

- fixes to pass CRAN checks

---

# Version 1.3

**Release date**: 23 March 2017

- **NEW** import data function, as well as support for metadata at the top of text files
- Various bug fixes

---

# Version 1.2

**Release date**: 5 September, 2016

- New device function uses the cairo device on Linux

## Patch 1.2.1 - 3 February, 2017

- Only use the Acinonyx package on Mac (crashes R on Linux)

## Patch 1.2.2 - 18 February, 2017

- Stop using `type = "nbcairo"` as macOS fallback in `newdevice()`

---

# Version 1.1

- Directly access `svydesign` function. Temporary fix
  until package gets a more significant revamp.

---

# Version 1.0

Initial release.
