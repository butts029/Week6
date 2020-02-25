# R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Import
library(stringi)
citations <- readLines("../Data/citations.txt")
citations_txt <- citations[!stri_isempty(citations)]
length(citations) - length(citations_txt)

# Data Cleaning
library(tidyverse)
library(rebus)
library()
sample(citations_txt, 10)
citations_tbl <- enframe(citations_txt, name = "line", value = "cite") %>% 
  mutate(cite = str_remove_all(cite, or("\"", "'"))) %>%
  mutate(year = str_extract(cite, pattern = "(" %R% repeated(DGT, 4, 4) %R% ")")) %>%
  mutate(page_start = as.numeric(str_match(cite, pattern = capture(repeated(DGT, 1, 4)) %R% "-" %R% repeated(DGT, 1, 3))[,2])) %>%
  mutate(perf_ref = str_detect(cite, fixed("performance", ignore_case = TRUE))) %>%
  mutate(title = str_match(cite,  "\\(" %R% repeated(DGT, 4, 4) %R% "\\)" %R% DOT %R% SPC %R% capture(one_or_more(or(ALPHA, SPC, ",", ":","\\?"))) %R% DOT)[,2]) %>%
  mutate(first_author = str_extract(cite, repeated(ALPHA, 1, Inf) %R% optional(or(",", DOT)) %R% optional(SPC) %R% one_or_more(ALPHA) %R% or(DOT, ",") %R% optional(SPC) %R% optional(ALPHA %R% DOT) %R% optional(",")))
