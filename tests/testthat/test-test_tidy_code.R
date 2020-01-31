library(magrittr)
context("test if tidycode function can produce prett printing code")

tempf <- function(file) file.path(tempdir(), file)
tidy_all_code(
    "messy_gapminder.txt",
    incl_library = TRUE,
    width = 2,
    indent = 2,
    outfile = tempf("test_gap_w2.txt")
)
tidy_all_code(
    "messy_gapminder.txt",
    incl_library = TRUE,
    width = 50,
    indent = 2,
    outfile = tempf("test_gap_w50.txt")
)
tidy_all_code(
    "messy_gapminder.txt",
    incl_library = TRUE,
    width = 2,
    indent = 4,
    outfile = tempf("test_gap_i4.txt")
)

test_that("test if short parsing code is the same as origin (gapminder_2008)", {
    expect_equal(
        eval(parse(file = "messy_gapminder.txt")),
        eval(parse(file = tempf("test_gap_w2.txt")))
    )
    expect_equal(
        eval(parse(file = "messy_gapminder.txt")),
        eval(parse(file = tempf("test_gap_w50.txt")))
    )
    expect_equal(
        eval(parse(file = "messy_gapminder.txt")),
        eval(parse(file = tempf("test_gap_i4.txt")))
    )
})
unlink(tempf("test_gap_w2.txt"))
unlink(tempf("test_gap_w50.txt"))
unlink(tempf("test_gap_i4.txt"))


tidy_all_code(
    "messy_longer_gap.txt",
    incl_library = TRUE,
    width = 2,
    indent = 2,
    outfile = tempf("test_longer_gap.txt")
)

test_that("test longer parsing code(gapminder_2008)", {
    expect_equal(
        eval(parse(file = "messy_longer_gap.txt")),
        eval(parse(file = tempf("test_longer_gap.txt")))
    )
})
unlink(tempf("test_longer_gap.txt"))

tidy_all_code(
    "messy_census.txt",
    incl_library = TRUE,
    width = 2,
    indent = 2,
    outfile = tempf("test_census.txt")
)

test_that("test longer parsing code(census_at_school_500)", {
    expect_equal(
        eval(parse(file = "messy_census.txt")),
        eval(parse(file = tempf("test_census.txt")))
    )
})
unlink(tempf("test_census.txt"))

messy_code <-
  getText(readLines("messy_code_test.txt"), TRUE)

output <-
  c(
    "gapminder_2008_ex %<>% tibble::add_column(Region.reord = factor(gapminder_2008_ex$Region, levels = c(\"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\")), .after = \"Region\")",
    "gapminder_2008_ex.sorted <- gapminder_2008_ex %>% dplyr::arrange(Year, desc(Country), Imports) %>%  tibble::add_column(log.e.CO2Emissions = log(gapminder_20), .after = \"CO2Emissions\") %>%  tibble::add_column(LifeExpectancy.squared = gapminder_2008_ex.sorted$LifeExpectancy^2, .after = \"LifeExpectancy\") %>%  dplyr::mutate(bmi.diff = BodyMassIndex_M - BodyMassIndex_F) %>%  tibble::add_column(Year.rank = dplyr::min_rank(gapminder_2008_ex.sorted$Year), .after = \"Year\") %>%  dplyr::select(-log.e.CO2Emissions) %>%  tibble::add_column(Year.rank.cat = factor(gapminder_2008_ex.sorted$Year.rank), .after = \"Year.rank\")"
  )

messy_code_without_library <-
  getText(readLines("messy_code_test.txt"), FALSE)

output_without_library <-
  c(
    "gapminder_2008_ex %<>% add_column(Region.reord = factor(gapminder_2008_ex$Region, levels = c(\"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\")), .after = \"Region\")",
    "gapminder_2008_ex.sorted <- gapminder_2008_ex %>% arrange(Year, desc(Country), Imports) %>%  add_column(log.e.CO2Emissions = log(gapminder_20), .after = \"CO2Emissions\") %>%  add_column(LifeExpectancy.squared = gapminder_2008_ex.sorted$LifeExpectancy^2, .after = \"LifeExpectancy\") %>%  mutate(bmi.diff = BodyMassIndex_M - BodyMassIndex_F) %>%  add_column(Year.rank = min_rank(gapminder_2008_ex.sorted$Year), .after = \"Year\") %>%  select(-log.e.CO2Emissions) %>%  add_column(Year.rank.cat = factor(gapminder_2008_ex.sorted$Year.rank), .after = \"Year.rank\")"
  )

test_that("test getText" , {
  expect_equal(messy_code, output)
  expect_equal(messy_code_without_library, output_without_library)
})

code_Vector <- getcode(output[1])
test_that("test getcode" , {
  expect_length(code_Vector, 18)
  expect_equal(code_Vector[1], "gapminder_2008_ex %<>%")
  expect_equal(code_Vector[2], "tibble::add_column(")
})

indents <- c(0, 1, 2, 3, 4, 4, 5, 6, 6, 6, 6, 6, 6, 5, 3, 3, 4, 1)
test_that("test getindnets" , {
  expect_equal(getindents(code_Vector), indents)
})

test_that("test print.txtcodestring" , {
  expect_equal(
    capture.output(
      print.txtcodestring((txtCodeString(
        code_Vector[2], indents[2]
      )))
    ),
    capture.output(
      cat(paste(
        paste(rep(" ", 4), collapse = ""), code_Vector[2], sep = ""
      ))
    )
  )
})

codeList <- list()
for (i in seq_along(1:length(code_Vector))) {
  codeList[[i]] <- txtCodeString(code_Vector[i], indents[i])
}
final <- list()
cl <- makeCodeList(codeList)

test_that("test codelength" , {
  expect_equal(codelength(cl[[1]]), 235)
  expect_equal(codelength(cl[[1]]$subcode[[1]]), 212)
  expect_equal(codelength(cl[[1]]$subcode[[2]]), 1)
})

test_that("test cancollapse" , {
  expect_true(cancollapse(cl[[1]], 235))
  expect_true(cancollapse(cl[[1]], 236))
  expect_false(cancollapse(cl[[1]], 234))
})

width_2 <-
  "gapminder_2008_ex %<>%\n  tibble::add_column(\n    Region.reord =\n      factor(\n        gapminder_2008_ex$Region,\n        levels =\n          c(\n            \"East Asia & Pacific\",\n            \"Europe & Central Asia\",\n            \"Middle East & North Africa\",\n            \"America\",\n            \"Sub-Saharan Africa\",\n            \"South Asia\"\n          )\n      ),\n      .after =\n        \"Region\"\n  )\n"
width_100 <-
  "gapminder_2008_ex %<>%\n  tibble::add_column(\n    Region.reord =\n      factor(\n        gapminder_2008_ex$Region,\n        levels =\n          c(\n            \"East Asia & Pacific\",\n            \"Europe & Central Asia\",\n            \"Middle East & North Africa\",\n            \"America\",\n            \"Sub-Saharan Africa\",\n            \"South Asia\"\n          )\n      ),\n      .after = \"Region\"\n  )\n"
width_200 <-
  "gapminder_2008_ex %<>%\n  tibble::add_column(\n    Region.reord = factor( gapminder_2008_ex$Region, levels = c( \"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\" ) ), .after = \"Region\"\n  )\n"
width_150 <-
  "gapminder_2008_ex %<>%\n  add_column(\n    Region.reord =\n      factor(\n        gapminder_2008_ex$Region,\n        levels = c( \"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\" )\n      ),\n      .after = \"Region\"\n  )\n"
indent_4 <-
  "gapminder_2008_ex %<>%\n    tibble::add_column(\n        Region.reord = factor( gapminder_2008_ex$Region, levels = c( \"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\" ) ), .after = \"Region\"\n    )\n"
test_that("test tidy code" , {
  expect_equal(tidy_code(output[1], width = 2, indent = 2), width_2)
  expect_equal(tidy_code(output[1], width = 100, indent = 2), width_100)
  expect_equal(tidy_code(
    output_without_library[1],
    width = 150,
    indent = 2
  ),
  width_150)
  expect_equal(tidy_code(output[1], width = 200, indent = 2), width_200)
  expect_equal(tidy_code(output[1], width = 200, indent = 4), indent_4)
})

short_code <- "library(iNZightRegression)"
long_code <-
  "model_1 = lm( log( Time ) ~ China..People.s.Republic.of + Japan + United.Kingdom, data = mydata )"
tidy_long_code <-
  "model_1 =\n  lm(\n    log(\n      Time\n    ) ~ China..People.s.Republic.of + Japan + United.Kingdom,\n    data = mydata\n  )\n"
test_that("test short code" , {
  expect_equal(tidy_code(short_code, width = 2, indent = 2), short_code)
  expect_equal(tidy_code(short_code, width = 200, indent = 2), short_code)
  expect_equal(tidy_code(long_code, width = 20, indent = 2), tidy_long_code)
  expect_equal(tidy_code(long_code, width = 100, indent = 2), long_code)
})

pipe_series <-
  "gapminder_2008_ex.sorted <- gapminder_2008_ex %>% arrange(Year, desc(Country), Imports) %>%  add_column(log.e.CO2Emissions = log(gapminder_20), .after = \"CO2Emissions\") %>%  add_column(LifeExpectancy.squared = gapminder_2008_ex.sorted$LifeExpectancy^2, .after = \"LifeExpectancy\") %>%  mutate(bmi.diff = BodyMassIndex_M - BodyMassIndex_F) %>%  add_column(Year.rank = min_rank(gapminder_2008_ex.sorted$Year), .after = \"Year\") %>%  select(-log.e.CO2Emissions) %>%  add_column(Year.rank.cat = factor(gapminder_2008_ex.sorted$Year.rank), .after = \"Year.rank\")"
correct_bracket <-
  "gapminder_2008_ex.sorted <-\n  gapminder_2008_ex %>%\n    arrange( Year, desc( Country ), Imports) %>%\n    add_column( log.e.CO2Emissions = log( gapminder_20 ), .after = \"CO2Emissions\") %>%\n    add_column( LifeExpectancy.squared = gapminder_2008_ex.sorted$LifeExpectancy^2, .after = \"LifeExpectancy\") %>%\n    mutate( bmi.diff = BodyMassIndex_M - BodyMassIndex_F) %>%\n    add_column( Year.rank = min_rank( gapminder_2008_ex.sorted$Year ), .after = \"Year\") %>%\n    select( -log.e.CO2Emissions) %>%\n    add_column( Year.rank.cat = factor( gapminder_2008_ex.sorted$Year.rank ), .after = \"Year.rank\")\n"

test_that("test correct bracket" , {
  expect_equal(tidy_code(pipe_series, width = 150, indent = 2),
               correct_bracket)
})

test_that("Passing in a character vector works too", {
  expect_equal(
    tidy_all_code("messy_census.txt"),
    tidy_all_code(readLines("messy_census.txt"))
  )
  expect_is(tidy_all_code("messy_census.txt"), "character")
})
