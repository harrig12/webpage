library(bibtex)
library(htmltools)
library(RefManageR)


#-------------------#
# Make bibliography
#-------------------#

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
           style = "text", no.print.fields = c("doi", "urldate", "issn", "note"))


bold_name <- function(refsList){
  
  # add delim, and split entries
  refsList[which(nchar(refsList) == 0)] <- "<DELIM>"
  
  paste0(refsList, collapse = " ") %>% 
    strsplit(split = "<DELIM>") %>%
    unlist() -> entries
  
  # bold my name
  entries <- gsub("Harrigan, Caitlin F.", "<b>Harrigan, Caitlin F.</b>", fixed = T, entries)
  entries <- gsub("Caitlin F. Harrigan", "<b>Caitlin F. Harrigan</b>", fixed = T, entries)
  entries <- gsub("Harrigan, Caitlin", "<b>Harrigan, Caitlin</b>", fixed = T, entries)
  entries <- gsub("Caitlin Harrigan", "<b>Caitlin Harrigan</b>", fixed = T, entries)
  
  # remove numeric if necessary
  #entries <- gsub('\\[', '', entries)
  #entries <- gsub('\\]', '.', entries)
  
  return(entries)
  
}

print_publications <- function(refs){
  
  refs <- refs %>%
    PrintBibliography() %>%
    capture.output() %>%
    bold_name() %>% 
    unlist()
  
  paste(paste0(1:length(refs), '. ', refs, '\n'), collapse = '')
  
}

