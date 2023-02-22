# formatter.R

library(tidyverse)
library(kableExtra)
library(htmltools)
library(googlesheets4)
library(bibtex)
library(RefManageR)

gs4_auth('cait.harrigan@mail.utoronto.ca')

#---------#
# Research
#---------#


# publication formatting:
# make title
make_title <- function(pub){
  title1 <- paste0('<p class="pub-title">', pub['title'], '</p>')
  title2 <- paste0('<p class="pub-authors">', pub['authors'], '</p>')
  title3 <- paste0('<p class="pub-authors">', pub['where'], '</p>')
  return(c(title1, title2, title3))
}

# make icons
make_icons <- function(pub){
  icons <- c('<span class="pub-authors">')
  if(pub['peer_reviewed'] == TRUE){
    icons <- c(icons, '<i class="fa-solid fa-user-check"></i>')
  }
  if(pub['open_access'] == TRUE){
    icons <- c(icons, '<i class="ai ai-open-access ai-lg"></i>')
  }
  icons <- c(icons, '<span>')
  return(icons)
}
# make buttons
make_buttons <- function(pub){
  buttons <- c()
  if (!is.na(pub['doi'])) {
    buttons <- c(buttons, paste0('<a href="', pub['doi'],'" class="btn btn-outline-secondary icon-link"><i class="fa-solid fa-arrow-up-right-from-square"></i> view</a>'))
  }
  if (!is.na(pub['pdf'])) {
    buttons <-  c(buttons, paste0('<a href="', pub['pdf'],'" class="btn btn-outline-secondary icon-link"><i class="fa-solid fa-file-pdf"></i> pdf</a>'))
  }
  if (!is.na(pub['code'])) {
    buttons <- c(buttons, paste0('<a href="', pub['code'],'" class="btn btn-outline-secondary icon-link"><i class="fa-solid fa-code"></i> code</a>'))
  }
  return(buttons)
}

# make image
make_image <- function(pub){
  paste0('<img class="research-thumbnail" src="', pub['thumbnail_path'], '" alt="img"/>')
}

# put it all together
make_pub <- function(pub){
  
  HTML(c(make_title(pub),
         #'<center>',
         make_buttons(pub),
         make_icons(pub)#,
         #'<center>'
  ))
}

#-----#
# CV
#-----#

# bibliography items 
embedURL <- RefManageR:::MakeBibLaTeX()
# Create modifications for Article, and Misc types
with(embedURL,{
  
  embedURL$fmtJTitle <- function (paper) {
    title <- paper$title
    url <- paper$url
    if (!is.null(title) & !is.null(url)) 
      if (grepl("[.?!]$", title, useBytes = TRUE)) 
        paste0("\\dQuote{", paste0("[", collapse(cleanupLatex(title)),
                                   "](", url, ")"), "}")
    else paste0("\\dQuote{", paste0("[", collapse(cleanupLatex(title)),
                                    "](", url, ")"), "}. ")
    else if (!is.null(title)) 
      if (grepl("[.?!]$", title, useBytes = TRUE)) 
        paste0("\\dQuote{", collapse(cleanupLatex(title)), "}")
    else paste0("\\dQuote{", collapse(cleanupLatex(title)), "}. ")
    
    
  }
  
  embedURL$formatArticle <- function (paper) {
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJTitle(paper), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               fmtTranslator(paper), fmtCommentator(paper$commentator), 
               fmtAnnotator(paper$annotator), fmtVersion(paper$version), 
               sentenceP(paste0(c(paste0(c(fmtJournal(paper), fmtSeries(paper$series)), 
                                         collapse = ""), fmtVolume(paper$volume, paper$number), 
                                  fmtJournDate(paper)), collapse = " "), 
                         fmtBTitle(paper$issuetitle, paper$issuesubtitle), 
                         fmtEditor(paper, suffix = NULL, prefix = ". "), 
                         fmtNote(paper$note, prefix = ". ", suffix = NULL), 
                         pgs = fmtPages(paper$pages, paper$pagination), 
                         sep = ""), fmtISSN(paper$issn), fmtDOI(paper$doi), 
               fmtEprint(paper), fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)))
    
  }
  
  embedURL$formatMisc <- function (paper) {
    collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJTitle(paper), 
               fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
               fmtEditor(paper, !length(paper$author)), fmtHowPublished(paper$howpublished), 
               addPeriod(fmtType(paper$type)), fmtVersion(paper$version), 
               fmtNote(paper$note), sentence(fmtPublisher(paper$organization, 
                                                          paper$location, paper$address), 
                                             fmtDate(attr(paper, "dateobj")), sep = ""), 
               fmtDOI(paper$doi), fmtEprint(paper), 
               fmtAddendum(paper$addendum), 
               fmtPubstate(paper$pubstate)))
  }
})
# Register bibliography
tools::bibstyle("embedURL", embedURL)
# Set options
BibOptions(bib.style = "embedURL", match.author = "exact", 
           sorting = 'ydnt', max.names = 99, first.inits = FALSE, 
           style = "text", no.print.fields = c("doi","urldate", "issn", "note"))


boldName <- function(refsList){
  
  # add delim, and split entries
  refsList[which(nchar(refsList) == 0)] <- "<DELIM>"
  
  paste0(refsList, collapse = " ") %>% 
    strsplit(split = "<DELIM>") %>%
    unlist() -> entries
  
  # bold my name
  entries <- gsub("Harrigan, Caitlin F", "**Harrigan, Caitlin F**", fixed = T, entries)
  entries <- gsub("Caitlin F Harrigan", "**Caitlin F Harrigan**", fixed = T, entries)
  entries <- gsub("Harrigan, Caitlin", "**Harrigan, Caitlin**", fixed = T, entries)
  entries <- gsub("Caitlin Harrigan", "**Caitlin Harrigan**", fixed = T, entries)
  
  # remove numeric if necessary
  #entries <- gsub('\\[', '', entries)
  #entries <- gsub('\\]', '.', entries)
  
  return(entries)
  
}

print_publications <- function(refs){
  refs <- refs %>%
    PrintBibliography() %>%
    capture.output() %>%
    boldName() %>% 
    unlist()
  
  paste(paste0(1:length(refs), '. ', refs, '\n'), collapse = '')
  
}


bestOf <- function(l){
  # return the first non-na argument
  #print(head(data.frame(l)))
  r <- apply(data.frame(l), FUN = function(x){x[which(!is.na(x))][[1]]}, MAR = 1)
  return(r)
}


print_entry <- function(line){
  
  if( (nchar(line['what']) + nchar(line['where'])) >65 ){
    line['description'] <- paste0(line['where'], '<p>', line['description'], '</p>')
    line['where'] <- ''
  }
  
  entry <- c(
    '<div class= short-entry>',
    '<p>',
    '<span class="entry-title">',
    line['what'],
    '</span>', 
    line['where'],
    '<span class="side-date">',
    line['when'],
    '</span>',
    '<p>'
  )
  
  if (!is.na(line['description'])){
    entry <- c(entry, line['description'])
  }
  
  entry <- c(entry, '</div>')
  HTML(entry)
}

print_section <- function(cv_entries, sel){
  paste(apply(dplyr::filter(cv_entries, type == sel) , 1, print_entry), collapse = "")
}



