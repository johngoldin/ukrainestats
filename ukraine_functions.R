

# using: https://www.scrapingdog.com/blog/web-scraping-r/
# link <- "https://www.mil.gov.ua/en/news/2023/03/08/the-total-combat-losses-of-the-enemy-from-24-02-2022-to-08-03-2023/"
# page = read_html(link, as_html = FALSE)
# lines <- read_html(link)

library(tidyverse)
library(glue)
library(xml2)

extract_number  <-  function(text, phrase = "(?<=personnel [‒-] about )\\d*") {
  # x <- str_extract(text, "(?<=personnel [‒-] about )\\d*")
  x <- str_extract(text, phrase)
  if (is.na(x)) {
    if (!str_detect(text, phrase)) stop(paste0(phrase, " not found"))
    warning(paste0("number not found:", text()))
    return(NA_real_)
  }
  as.numeric(x)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Notes on scraping and parsing the data on Russian losses from the
# Ukraine Ministry of Defense web site:
#
# April 12, 2022 is earliest Russian casualty count
#
# Some dates that are missing data: "2022-06-16", "2022-06-18" (others in June)
#
# at this point doing two digit date for start of war and end
# https://www.mil.gov.ua/en/news/2023/02/23/the-total-combat-losses-of-the-enemy-from-24-02-22-to-23-02-23/
#
#   back then two digit years
# https://www.mil.gov.ua/en/news/2022/12/16/the-total-combat-losses-of-the-enemy-from-24-02-to-16-12/
#
#   https://www.mil.gov.ua/en/news/2022/12/31/the-total-combat-losses-of-the-enemy-from-24-02-to-31-12/
#
#   3/3/2023 seems to be start of using 4-digit year
#
# wrong:
#   https://www.mil.gov.ua/en/news/2022/09/23/the-total-combat-losses-of-the-enemy-from-24-02-to-23-09/
#
#   On January 1, started doing two digit dates
# https://www.mil.gov.ua/en/news/2023/01/03/the-total-combat-losses-of-the-enemy-from-24-02-22-to-03-01-23/
#
#   works
# https://www.mil.gov.ua/en/news/2023/02/20/the-total-combat-losses-of-the-enemy-from-24-02-22-to-20-02-23/
#
#   does not work:
#   https://www.mil.gov.ua/en/news/23/02/23/the-total-combat-losses-of-the-enemy-from-24-02-22-to-23-02-23/
#
#   does note work:
#   https://www.mil.gov.ua/en/news/23/02/20/the-total-combat-losses-of-the-enemy-from-24-02-22-to-20-02-23/
#
#   works:
#   https://www.mil.gov.ua/en/news/2022/05/09/the-total-combat-losses-of-the-enemy-from-24-02-to-09-05/
#
#   works:
#   https://www.mil.gov.ua/en/news/2022/06/25/the-total-combat-losses-of-the-enemy-from-24-02-to-25-06/
#
#   works:
#   https://www.mil.gov.ua/en/news/2022/04/16/the-total-combat-losses-of-the-enemy-from-24-02-to-16-04/
#
#   first phrase changed between 2022-04-16 and 2022-04-17
# "personnel ‒ about" to
#
#
# wrong:
#   https://www.mil.gov.ua/en/news/2022/07/12/the-total-combat-losses-of-the-enemy-from-24-02-to-12-07/
#
#   wrong:
#   https://www.mil.gov.ua/en/news/2022/06/20/the-total-combat-losses-of-the-enemy-from-24-02-to-20-06/
#
#   More than one line found 2022-06-06 and that goes until 2022-07-26 when hits error
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


create_mod_link <- function(adate) {
  if (!is.Date(adate)) adate <- as_date(adate)
  # must have this form:   (4 digit years)
  # https://www.mil.gov.ua/en/news/2023/03/08/the-total-combat-losses-of-the-enemy-from-24-02-2022-to-08-03-2023/
  if (adate >= as_date("2023-03-03")) link <- glue("https://www.mil.gov.ua/en/news/", format(adate, "%Y/%m/%d"), "/the-total-combat-losses-of-the-enemy-from-24-02-2022-to-", format(adate, "%d-%m-%Y"), "/")
  else if (adate >= as_date("2023-01-01")) link <- glue("https://www.mil.gov.ua/en/news/", format(adate, "%Y/%m/%d"), "/the-total-combat-losses-of-the-enemy-from-24-02-22-to-", format(adate, "%d-%m-%y"), "/")
  else link <- glue("https://www.mil.gov.ua/en/news/", format(adate, "%Y/%m/%d"), "/the-total-combat-losses-of-the-enemy-from-24-02-to-", format(adate, "%d-%m"), "/")
  link
}

fetch_mod_text <- function(adate) {
  if (!is.Date(adate)) adate <- as_date(adate)
  if (adate < ymd("2022-04-12")) return(NA_character_)
  if (adate > today()) return(NA_character_)

  link <- create_mod_link(adate)
  x <- read_lines(link)
  #the_line <- x[x |> str_detect("personnel ‒ about")]
  # the_line <- x[x |> str_detect("The total combat losses of the enemy from")]
  if (adate <= ymd("2022-04-16")) the_line <- x[str_detect(x, "personnel ‒ about")]
  if (adate > ymd("2022-04-16")) the_line <- x[str_detect(x, "persons were liquidated")]
  if (length(the_line) == 0) {
    warning(paste0("Info line not found for ", adate))
    return(NA_character_)
  }
  if (length(the_line) > 1) {
    warning(paste0("More than one line found ", adate, "\n", link))
    return(NA_character_)
  }
  if (adate <= as_date("2022-04-16")) first_line <- which(str_detect(x, "personnel ‒ about"))
  if (adate > as_date("2022-04-16"))  first_line <- which(str_detect(x, "persons were liquidated"))
  last_line <- which(str_detect(x, "special equipment"))
  if (is.na(last_line) || (length(last_line) == 0)) last_line <- which(str_detect(x, "vehicles"))
  if (is.na(last_line) || (length(last_line) == 0)) last_line <- which(str_detect(x, "UAV"))
  if (is.na(last_line) || (length(last_line) == 0)) last_line <- first_line + 10
  # the_line
  str_flatten(c(as.character(adate), x[first_line:last_line]))
}

parse_mod_text <- function(uk_test) {
  uk_test$personnel = map_dbl(str_replace_all(uk_test$report, intToUtf8(160), ""), extract_number, phrase = "(?<=personnel ?[‒-–-] ? ?about )\\d*" )
  uk_test$tanks = map_dbl(uk_test$report, extract_number, phrase = "(?<=tanks [‒-–-] )\\d*" )
  uk_test$apv = map_dbl(uk_test$report, extract_number, phrase = "(?<=APV [‒-–-] )\\d*" )
  uk_test$artillery = map_dbl(uk_test$report, extract_number, phrase = "(?<=artillery systems [‒-–-] )\\d*" )
  uk_test$mlrs = map_dbl(uk_test$report, extract_number, phrase = "(?<=MLRS [‒-–-] )\\d*" )
  uk_test$aa = map_dbl(uk_test$report, extract_number, phrase = "(?<=Anti-aircraft warfare systems [‒-–-] )\\d*" )
  uk_test$aircraft = map_dbl(uk_test$report, extract_number, phrase = "(?<=aircraft [‒-–-] )\\d*" )
  uk_test$helicopters = map_dbl(uk_test$report, extract_number, phrase = "(?<=helicopters [‒-–-] )\\d*" )
  uk_test$uav = map_dbl(uk_test$report, extract_number, phrase = "(?<=UAV operational-tactical level [‒-–-] )\\d*" )
  uk_test$vehicles = map_dbl(uk_test$report, extract_number, phrase = "(?<=vehicles and fuel tanks [‒-–-] )\\d*" )
  # uk_test$special = map_dbl(uk_test$report, extract_number, phrase = "(?<=special equipment [‒-–-] )\\d*" )
  uk_test$warships = map_dbl(uk_test$report, extract_number, phrase = "(?<=warships / boats [‒-–-] )\\d*" )
  uk_test
}
