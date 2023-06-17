# formatter.R

library(tidyverse)
#library(kableExtra)
library(htmltools)
library(lubridate)
library(googlesheets4)

gs4_auth('cait.harrigan@mail.utoronto.ca')

#--------------#
# Retrieve data
#--------------#

cv_entries <- read_sheet("1JKlSIuDrfC1wa4V1zf4Si_Tu34upiBEn1DlvFfDeFh0", 
                         sheet = 'new entries', col_types = "lllcDDccccccccc") %>%
  arrange(desc(pmax(year(end), year(begin), na.rm = T)), 
          desc(year(begin)), desc(month(begin))) %>%
  mutate(type = factor(type),
         when = format(begin, "%Om/%y"),
         end = ifelse(end >= now(), "present", format(end, "%Om/%y")),
         when = ifelse(is.na(end), when, str_c(when, " - ", end))
         ) %>%
  filter(!is.na(begin))

#--------------#
# Make buttons
#--------------#

make_buttons <- function(entry){
  buttons <- c()
  if (!is.na(entry['view'])) {
    buttons <- c(buttons, paste0('<a href="', entry['view'],'" class="btn btn-outline-secondary cv-btn"><i class="fa-solid fa-arrow-up-right-from-square"></i> view</a>'))
  }
  if (!is.na(entry['pdf'])) {
    buttons <-  c(buttons, paste0('<a href="', entry['pdf'],'" class="btn btn-outline-secondary cv-btn"><i class="fa-solid fa-file-pdf"></i> pdf</a>'))
  }
  if (!is.na(entry['code'])) {
    buttons <- c(buttons, paste0('<a href="', entry['code'],'" class="btn btn-outline-secondary cv-btn"><i class="fa-solid fa-code"></i> code</a>'))
  }
  return(buttons)
}

print_entry <- function(entry){
  lines <-  c('<div class="grid cv-entry">')
  lines <- c(lines, '<div class="g-col-1">')
  # entry title
  if (!is.na(entry['line1'])){
    lines <- c(lines, '<p class="entry-bold">', entry['line1'], '</p>')
  } 
  # entry may have additional lines
  if (!is.na(entry['line2'])) {
    lines <- c(lines, '<p class="entry-plain">', entry['line2'], '</p>')
  }
  if (!is.na(entry['line3'])) {
    lines <- c(lines, '<p class="entry-plain">', entry['line3'], '</p>')
  }
  if (!is.na(entry['line4'])) {
    lines <- c(lines, '<p class="entry-italic">', entry['line4'], '</p>')
  }
  lines <- c(lines, '</div><div class="g-col-1">')
  if (!is.na(entry['when'])){
    lines <- c(lines, '<span class="entry-when">', entry['when'], '</span>')
  }
  # entry may have buttons 
  if( any(c(!is.na(entry['view']), !is.na(entry['pdf']), !is.na(entry['pdf']))) ){
    lines <- c(lines, make_buttons(entry))
  }
  lines <- c(lines, '</div></div>')
  HTML(lines)
}

print_section <- function(entries){
  paste(apply(entries , 1, print_entry), collapse = "")
}



