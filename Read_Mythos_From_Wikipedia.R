library(RCurl)
library(XML)
library(dplyr)

# function for cleaning up vestiges of HTML origins
cleanText <- function(char.vec) {
  clean.vec <- gsub("\\n", " " , char.vec)
  clean.vec <- gsub("\\[[0-9]+\\]", "", clean.vec)
  clean.vec <- gsub(",[A-T]", " ", clean.vec)
  clean.vec <- gsub("\\^ ", "", clean.vec)
  clean.vec
}

# get mythos content from Wikipedia
GOOUrl <- getURL("https://en.wikipedia.org/wiki/Cthulhu_Mythos_deities#Great_Old_Ones")
OGUrl <- getURL("https://en.wikipedia.org/wiki/Cthulhu_Mythos_deities#List")

# read Great Old One content HTML tree
GOO <- readHTMLTable(GOOUrl, which = 1, header = TRUE, trim = TRUE, stringsAsFactors = FALSE)
# clean up
names(GOO) <- cleanText(names(GOO))
GOO <- sapply(GOO, cleanText)

# read Other God content from HTML tree
OuterG <- readHTMLList(OGUrl, trim = TRUE, which = 3)
ElderG <- readHTMLList(OGUrl, trim = TRUE, which = 4)
OtherG <- data.frame(import =  c(OuterG, ElderG)
                     , category = c(rep("Outer", times = length(OuterG))
                                    , rep("Elder", times = length(ElderG))
                                    )
                    )

# Split the Wiki page indexing from the name
Name <- sub("\\d.\\d{1,2} ", "", OtherG$import)
WikiIndex <- trimws(mapply(FUN = sub, pattern = Name, replacement = "", x = OtherG$import))
names(WikiIndex) <- NULL
OtherG <- mutate(OtherG, name = Name, wikiIndex = WikiIndex) %>%
  select(-import)
rm(list=(c("Name", "WikiIndex")))

# More OG content
OGHTML <- htmlParse(OGUrl)
OG1 <- xpathApply(OGHTML, "//table/following-sibling::p", xmlValue)

# This has all the lists and semi structured information
OGText = unlist(OG1[4:(length(OG1)-1)])

# The following steps ensure the text blocks match up with the OG listing
OuterGParagraph <- paste(OGText[38]
                     , OGText[39]
                     , sep = "\n ")
OuterGCatalog <- c(OGText[1:37]
               , paste(OGText[38]
                       , OGText[39]
                       , OGText[40]
                       , OGText[41]
                       , sep = "\n ")
               , OGText[42])
ElderGParagraph <- paste(OGText[43]
                             , OGText[44]
                             , OGText[45]
                             , sep = "\n ")
ElderGCatalog <- c(OGText[46:55])

OtherG$Description <- c(OuterGCatalog, ElderGCatalog)

# clean up
OtherG <- sapply(OtherG, cleanText)

GOnesText <- OG1[[length(OG1)]]
# There is a Great Ones table that I want to extract.
# Seems a bit tricky

# read misc content from HtML tree
Refs <- readHTMLList(OGUrl, trim = TRUE, which = 11)
Bibl <- readHTMLList(OGUrl, trim = TRUE, which = 12)
Placs <- readHTMLList(OGUrl, trim = TRUE, which = 15)
Characs <- readHTMLList(OGUrl, trim = TRUE, which = 16)
Deits <- readHTMLList(OGUrl, trim = TRUE, which = 17)
Specs <- readHTMLList(OGUrl, trim = TRUE, which = 18)
MiscText <- unlist(OG1[1])
# clean up
Mythos.Misc <- list(References = Refs
                    , Bibliography = Bibl
                    , Places = Placs
                    , Characters = Characs
                    , Deities = Deits
                    , Species = Specs
                    , Miscellaneous = MiscText)
# cleanup
Mythos.Misc <- lapply(Mythos.Misc, cleanText)


# word counting function
findWords <- function(t) {
  text <- tolower(unlist(strsplit(gsub("[,.]+", "", t), " ")))
  wordList <- split(1:length(text), text)
  return(wordList)
}

# sorting function
sortWords <- function(wList, by = "alpha") {
  if (by=="alpha") {
    nms <- names(wList)
    sn <- sort(nms)
    return(wList[sn])
  }
  if (by=="freq") {
    frq <- sapply(wList, length)
    return(wList[order(frq, decreasing = TRUE)])
  }
}

  
  

