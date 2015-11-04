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
MiscText <- unlist(OG1[1:3])
GOnesText <- OG1[[length(OG1)]]
OGText = unlist(OG1[4:(length(OG1)-1)])

#OGText needs tidying up to match OtherGods data.fram
paste(OGText[41], OGText[42], OGText[43], sep = "\n ")
