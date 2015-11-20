require(RCurl)
require(XML)
require(dplyr)

cleanText <- function(HTMLTbl) {
#   x <- HTMLTbl
     names(HTMLTbl) <- gsub("\\n", " " , names(HTMLTbl))
     for (i in names(HTMLTbl)) {
     HTMLTbl[[i]] <- gsub("\\n", " " , HTMLTbl[[i]])
     HTMLTbl[[i]] <- gsub("\\[[0-9]+\\]", "", HTMLTbl[[i]])
     HTMLTbl[[i]] <- gsub(",[A-T]", " ", HTMLTbl[[i]])
  }
  HTMLTbl
}

GOOUrl <- getURL("https://en.wikipedia.org/wiki/Cthulhu_Mythos_deities#Great_Old_Ones")

GOO <- readHTMLTable(GOOUrl, which = 1, header = TRUE, trim = TRUE)

GOO <- cleanText(GOO)

OGUrl <- getURL("https://en.wikipedia.org/wiki/Cthulhu_Mythos_deities#List")

OuterG <- readHTMLList(OGUrl, trim = TRUE, which = 3)
ElderG <- readHTMLList(OGUrl, trim = TRUE, which = 4)
Refs <- readHTMLList(OGUrl, trim = TRUE, which = 11)
Bibl <- readHTMLList(OGUrl, trim = TRUE, which = 12)
Places <- readHTMLList(OGUrl, trim = TRUE, which = 15)
Characs <- readHTMLList(OGUrl, trim = TRUE, which = 16)
Deities <- readHTMLList(OGUrl, trim = TRUE, which = 17)
Species <- readHTMLList(OGUrl, trim = TRUE, which = 18)

OtherG <- data.frame(import =  c(OuterG, ElderG)
                     , category = c(rep("Outer", times = length(OuterG))
                                    , rep("Elder", times = length(ElderG))
                                    )
                    )

# try using tidyr to replace this sub
Name <- sub("\\d.\\d{1,2} ", "", OtherG$import)
WikiIndex <- trimws(mapply(FUN = sub, pattern = Name, replacement = "", x = OtherG$import))
names(WikiIndex) <- NULL

OtherG <- mutate(OtherG, name = Name, wikiIndex = WikiIndex) %>%
  select(-import)

rm(list=(c("Name", "WikiIndex")))

OGHTML <- htmlParse(OGUrl)

OG1 <- xpathApply(OGHTML, "//table/following-sibling::p", xmlValue)
MiscText <- unlist(OG1[1])

OGText = unlist(OG1[4:(length(OG1)-1)])

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
GOnesText <- OG1[[length(OG1)]]