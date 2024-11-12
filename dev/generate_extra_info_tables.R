library(rvest)
library(constructive)
library(dplyr)
library(purrr)

base_url <- "https://giscience.github.io/openrouteservice/api-reference/endpoints/directions/extra-info/"

table_names <- c(
  "steepness", "surface", "waycategory", "waytype", "trail-difficulty",
  "road-access-restrictions", "country-list"
)

tables <- map(
  table_names,
  function(x) {
    if (identical(x, "country-list")) {
      base_url <- "https://giscience.github.io/openrouteservice/technical-details/"
    }

    url <- file.path(base_url, x)
    doc <- read_html(url) |>
      html_table() |>
      pluck(1) |>
      mutate(across(everything(), ~case_when(.x %in% c("", "---") ~ NA, .default = .x)))
  }
) |>
  setNames(table_names)

table_names <- c(
  "steepness", "surface", "waycategory", "waytypes", "traildifficulty",
  "roadaccessrestrictions", "countryinfo"
)
names(tables) <- table_names

colnames(tables$steepness) <- c("levels", "labels")
colnames(tables$surface) <- c("levels", "labels", "tags")
colnames(tables$waycategory) <- c("levels", "labels", "tags")
colnames(tables$waytypes) <- c("levels", "labels", "tags")
colnames(tables$traildifficulty) <- c("levels", "foot", "cycling")
colnames(tables$roadaccessrestrictions) <- c("levels", "labels")

# if english country name is missing, merge with orginal name
tables$countryinfo <- tables$countryinfo |>
  mutate(
    `name:en` = ifelse(!nzchar(`name:en`), NA, `name:en`),
    name = coalesce(`name:en`, name)
  ) |>
  select(-`name:en`, levels = country_id, labels = name)

cycle_labels <- rev(setdiff(tables$traildifficulty$cycling, "no tag"))
cycle_levels <- rev(abs(tables$traildifficulty$levels * -1)[seq(2, length(cycle_labels) + 1)])

tables$traildifficulty <- na.omit(data.frame(
  levels = c(cycle_levels, tables$traildifficulty$levels),
  labels = c(cycle_labels, tables$traildifficulty$foot)
))
attr(tables$traildifficulty, "na.action") <- NULL

code <- vapply(table_names, FUN.VALUE = character(1), function(x) {
  code <- construct(tables[[x]])$code
  paste0(x, " = ", paste(code, collapse = "\n"), ",\n")
}) |>
  paste(collapse = "\n")

code <- paste(paste0("    ", strsplit(code, "\n")[[1]]), collapse = "\n")

roc <- readLines("R/info_table.R")
roc <- roc[startsWith(roc, "#'")]

code <- paste0(
  paste(roc, collapse = "\n"), "\n",
  "info_table <- function(type) {\n",
  "  switch(\n",
  "    type,\n",
  code,
  "\n    NULL\n",
  "  )\n",
  "}", "\n"
)

cat(code, file = "R/info_table.R")
