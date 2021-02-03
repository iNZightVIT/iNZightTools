library(magrittr)
context("test-test_tidy_code")

messy_code <- getText(readLines("messy_code_test.txt"), TRUE)

test_that("Tidying code works, and is silent", {
  expect_silent(tidied <- tidy_all_code(messy_code))
  expect_is(tidied, "character")
})


skip("No longer using custom code tidying")

output <-
  c(
    "gapminder_2008_ex %<>% tibble::add_column(Region.reord = factor(gapminder_2008_ex$Region, levels = c(\"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\")), .after = \"Region\")",
    "gapminder_2008_ex.sorted <- gapminder_2008_ex %>% dplyr::arrange(Year, desc(Country), Imports) %>%  tibble::add_column(log.e.CO2Emissions = log(gapminder_20), .after = \"CO2Emissions\") %>%  tibble::add_column(LifeExpectancy.squared = gapminder_2008_ex.sorted$LifeExpectancy^2, .after = \"LifeExpectancy\") %>%  dplyr::mutate(bmi.diff = BodyMassIndex_M - BodyMassIndex_F) %>%  tibble::add_column(Year.rank = dplyr::min_rank(gapminder_2008_ex.sorted$Year), .after = \"Year\") %>%  dplyr::select(-log.e.CO2Emissions) %>%  tibble::add_column(Year.rank.cat = factor(gapminder_2008_ex.sorted$Year.rank), .after = \"Year.rank\")"
  )

test_that("test getText" , {
  expect_equal(messy_code, output)
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
indent_4 <-
  "gapminder_2008_ex %<>%\n    tibble::add_column(\n        Region.reord = factor( gapminder_2008_ex$Region, levels = c( \"East Asia & Pacific\", \"Europe & Central Asia\", \"Middle East & North Africa\", \"America\", \"Sub-Saharan Africa\", \"South Asia\" ) ), .after = \"Region\"\n    )\n"
test_that("test tidy code" , {
  expect_equal(tidy_code(output[1], width = 2, indent = 2), width_2)
  expect_equal(tidy_code(output[1], width = 100, indent = 2), width_100)
  expect_equal(tidy_code(output[1], width = 200, indent = 2), width_200)
  expect_equal(tidy_code(output[1], width = 200, indent = 4), indent_4)
})
