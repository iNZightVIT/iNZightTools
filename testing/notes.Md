# Notes 8/12/17

- remove "NOTE" comments once fixed
- "~stuff" %>% as.formula %>% replaceVars ??
- spaces in filter args

- Distinction between namespacing internally vs code-writing
  i.e., `stringr::str_c` is bulky and wont appear in the generated
        code, so just use `str_c` instead (+ importFrom when documenting)

- if (x == TRUE) === if (x)


## To do:

- start moving functions into `R` directory (overwrite existing functions, if exist)
- modify argument documentation as necessary
- move tests into `tests/testthat` - and rewrite as necessary

## To do later
- determine `NAMESPACE` requirements (i.e., which packages are we using, besides all of `tidyverse`?)
- add `importFrom(stringr, str_c)` 