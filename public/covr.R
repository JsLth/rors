report <- xml2::read_html("public/coverage.html")
text <- xml2::xml_find_chr(report, "string(//h2)")
coverage <- strsplit(text, " - ")[[1]][2]
badge_color <- function(coverage) {
  coverage <- as.numeric(gsub("%", "", coverage))
  if (coverage < 30) return("red")
  if (coverage >= 30 && coverage < 70) return("yellow")
  if (coverage >= 70) return("green")
  if (coverage >= 90) return("brightgreen")
}
badge_url <- badger::badge_custom("Coverage", coverage, badge_color(coverage))
badge_url <- gsub("%", "%25", badge_url)