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

colnames(tables$steepness) <- tolower(colnames(tables$steepness))
colnames(tables$surface) <- c("value", "name", "tags")
colnames(tables$waycategory) <- c("value", "name", "tags")
colnames(tables$waytype) <- c("value", "name", "tags")
colnames(tables$`trail-difficulty`) <- c("value", "foot", "cycling")
colnames(tables$`road-access-restrictions`) <- tolower(colnames(tables$`road-access-restrictions`))
tables$`country-list` <- tables$`country-list` |>
  mutate(
    `name:en` = ifelse(!nzchar(`name:en`), NA, `name:en`),
    name = coalesce(`name:en`, name)
  ) |>
  select(-`name:en`)

code <- vapply(table_names, FUN.VALUE = character(1), function(x) {
  code <- construct(tables[[x]])$code
  nm <- gsub("\\-", "_", x)
  paste0(nm, " = ", paste(code, collapse = "\n"), ",\n")
}) |>
  paste(collapse = "\n")

code <- paste(paste0("    ", strsplit(code, "\n")[[1]]), collapse = "\n")

code <- paste0(
  "info_table <- function(type) {\n",
  "  switch(\n",
  "    type,\n",
  code,
  "\n    NULL\n",
  "  )\n",
  "}"
)

cat(code, file = "R/info_tables.R")
