# Version 1.6
__Release date__: 14 Auguest 2018

## Breaking Changes

- data import is now performed by the `smart_read()` function

## Minor Changes and Fixes

- new function to generate R variable names (for code writing)
- various bug fixes

## Patches

### Patch 1.6.1 - 04/09/2018
- fix bug in reading metadata where non-meta comments would cancel read

***

# Version 1.5
__Release date__: 23 January 2018


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

***

# Version 1.4
__Release date__: 25 August 2017

- Code history features: interpolate code
- Redocument package; pass CRAN checks
- start converting data modification functions to tidyverse

## Patches

### Version 1.4-1, 02/10/2017
- fixes to pass CRAN checks


***
# Version 1.3
__Release date__: 23 March 2017

- __NEW__ import data function, as well as support for metadata at the top of text files
- Various bug fixes

***
# Version 1.2
__Release date__: 5 September, 2016

- New device function uses the cairo device on Linux


## Patch 1.2.1 - 3 Febuary, 2017

- Only use the Acinonyx package on Mac (crashes R on Linux)

## Patch 1.2.2 - 18 Febuary, 2017

- Stop using `type = "nbcairo"` as macOS fallback in `newdevice()`

***
# Version 1.1

- Directly access `svydesign` function. Temporary fix
  until package gets a more significant revamp.

***
# Version 1.0

Initial release.
