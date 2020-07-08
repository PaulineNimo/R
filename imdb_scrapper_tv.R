library("rvest")
library("XML")
library("xml2")
library("stringr")
library("httr")

# IMDB Top 250 tvs
url = "https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250"

page = read_html(url)

tv.nodes <- html_nodes(page,'.titleColumn a')

tv.link = sapply(html_attrs(tv.nodes),`[[`,'href')
tv.link = paste0("http://www.imdb.com",tv.link)
tv.cast = sapply(html_attrs(tv.nodes),`[[`,'title')
tv.name = html_text(tv.nodes)

sec <- html_nodes(page,'.secondaryInfo')

year = as.numeric(gsub(")","",                          # Removing )
                       gsub("\\(","",                   # Removing (
                            html_text( sec )                 # get text of HTML node  
                       )))

rating.nodes = html_nodes(page,'.imdbRating')
# Check One node
xmlTreeParse(rating.nodes[[20]])

rating.nodes = html_nodes(page,'.imdbRating strong')
votes = as.numeric(gsub(',','',
                        gsub(' user ratings','',
                             gsub('.*?based on ','',
                                  sapply(html_attrs(rating.nodes),`[[`,'title')
                             ))))

rating = as.numeric(html_text(rating.nodes))

top250 <- data.frame(tv.name, tv.cast, tv.link,year,votes,rating)

#read all urls & store the values
url1 <- as.character(top250$tv.link)
tv.tot <- data.frame(matrix(NA,nrow = 250),stringsAsFactors = F)

# Looping starts here 
for(i in 1:length(url1)){
    x <- url1[i]
    
    page <- read_html(x)
    tv.titleYear <- year[i]
    tv.ratingValue <- html_text(html_nodes(page, "strong"))[1]
    tv.ratingCount <- html_text(html_nodes(page, "span.small"))
    tv.runTime <- html_text(html_nodes(page, "time"))[2]
    web.content <- content(GET(x), as="text")
    tv.rating <- str_trim(gsub("\"","",str_match(web.content,"\"contentRating\": (.*?),")[,2]))
    tv.genre <- str_trim(paste(html_text(html_nodes(page, "div.subtext a"))[-length(html_text(html_nodes(page, "div.subtext a")))], collapse = ","))
    tv.releaseDate <- str_trim(gsub("\n","",html_text(html_nodes(page, "div.subtext a"))[length(html_text(html_nodes(page, "div.subtext a")))]))
    tv.story <- html_node(page,'.summary_text')
    xmlTreeParse(tv.story)
    story <- gsub("^$|^[ \t\n\v\r\f]+","NA",
                  gsub("^([ \t\n\r\f]+)|([ \t\n\f\r]+)$","",
                       gsub("\n","",as.character(html_text(tv.story))
                       )))
    tv.details <- html_nodes(page,'div.credit_summary_item')
    tv.director <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",
                              as.character(html_text(tv.details)[1])))
    if(length(tv.director) == 0) tv.director <- ""
    tv.writer <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",
                            as.character(html_text(tv.details)[2])))
    if(length(tv.writer) == 0) tv.writer <- ""
    tv.stars <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",
                                  as.character(html_text(tv.details)[3])))
    if(length(tv.stars) == 0) tv.stars <- ""
    tv.metacriticScore <- gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",html_text(html_nodes(page, "div.metacriticScore")))
    if(length(tv.metacriticScore) == 0) tv.metacriticScore <- ""
    tv.awardsnodes <- html_nodes(page,'span.see-more.inline a')
    award.link <- sapply(html_attrs(tv.awardsnodes),`[[`,'href')
    awards.link <- paste0("http://www.imdb.com",award.link[1])
    awards.noms <- html_text(html_nodes(read_html(awards.link), "div.desc"))
    tv.wins <- str_match(awards.noms,"all (.*?) wins")[2]
    tv.noms <- str_match(awards.noms,"and (.*?) nominations")[2]  
    tv.specs <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",html_text(html_nodes(page, "div.txt-block"))))
    tv.tagline <- tv.specs[startsWith(tv.specs,"Tagline")]
    if(length(tv.tagline) == 0) tv.tagline <- ""
    tv.country <- tv.specs[startsWith(tv.specs,"Country")]
    if(length(tv.country) == 0) tv.country <- ""
    tv.language <- tv.specs[startsWith(tv.specs,"Language")]
    if(length(tv.language) == 0) tv.language <- ""
    tv.locations <- tv.specs[startsWith(tv.specs,"Filming Location")]
    if(length(tv.locations) == 0) tv.locations <- ""
    tv.budget <- tv.specs[startsWith(tv.specs,"Budget")]
    if(length(tv.budget) == 0) tv.budget <- ""
    tv.openingUS <- tv.specs[startsWith(tv.specs,"Opening Weekend US")]
    if(length(tv.openingUS) == 0) tv.openingUS <- ""
    tv.grossUS <- tv.specs[startsWith(tv.specs,"Gross US")]
    if(length(tv.grossUS) == 0) tv.grossUS <- ""
    tv.grossWorld <- tv.specs[startsWith(tv.specs,"Cumulative Worldwide")]
    if(length(tv.grossWorld) == 0) tv.grossWorld <- ""
    tv.prodco <- tv.specs[startsWith(tv.specs,"Production Co")]
    if(length(tv.prodco) == 0) tv.prodco <- ""
    #tv.sound <- tv.specs[startsWith(tv.specs,"Sound Mix")]
    tv.tot$titleYear[i] <- tv.titleYear
    tv.tot$ratingValue[i] <- tv.ratingValue
    tv.tot$ratingCount[i] <- tv.ratingCount
    tv.tot$runTime[i] <- tv.runTime
    tv.tot$rating[i] <- tv.rating
    tv.tot$genre[i] <- tv.genre
    tv.tot$releaseDate[i] <- tv.releaseDate
    tv.tot$story[i] <- story
    tv.tot$director[i] <- tv.director
    tv.tot$writer[i] <- tv.writer
    tv.tot$stars[i] <- tv.stars
    tv.tot$metacriticScore[i] <- tv.metacriticScore
    tv.tot$wins[i] <- tv.wins
    tv.tot$noms[i] <- tv.noms
    tv.tot$tagline[i] <- tv.tagline
    tv.tot$country[i] <- tv.country
    tv.tot$language[i] <- tv.language
    tv.tot$locations[i] <- tv.locations
    tv.tot$budget[i] <- tv.budget
    tv.tot$openingUS[i] <- tv.openingUS
    tv.tot$grossUS[i] <- tv.grossUS
    tv.tot$grossWorld[i] <- tv.grossWorld
    tv.tot$prodco[i] <- tv.prodco
    #tv.tot$sound[i] <- tv.sound
    
    print(paste0("Completed: ",i))
}
tv.tot <- tv.tot[,-1]
tv.tot <- cbind(tv.name,tv.tot)
write.csv(tv.tot,"IMDB Top 250 tvs.csv",row.names = F)
tv.clean <- tv.tot
tv.clean$titleYear <- gsub("\\(|\\)","",tv.clean$titleYear)
tv.clean$director <- gsub("Director:","",tv.clean$director)
tv.clean$writer <- gsub("Writer:|Writers:","",tv.clean$writer)
tv.clean$writer <- str_trim(gsub("\\|.*$", "", tv.clean$writer))
tv.clean$stars <- gsub("Stars:|Star:","",tv.clean$stars)
tv.clean$stars <- str_trim(gsub("\\|.*$", "", tv.clean$writer))
tv.clean$tagline <- gsub("Taglines:|Tagline:","",tv.clean$tagline)
tv.clean$tagline <- str_trim(gsub("See more.*$", "", tv.clean$tagline))
tv.clean$country <- gsub("Countries:|Country:","",tv.clean$country)
tv.clean$country <- str_trim(gsub("\\s+\\|\\s+", ",", tv.clean$country))
tv.clean$language <- gsub("Languages:|Language:","",tv.clean$language)
tv.clean$language <- str_trim(gsub("\\s+\\|\\s+", ",", tv.clean$language))
tv.clean$locations <- gsub("Filming Locations:|Filming Location:","",tv.clean$locations)
tv.clean$locations <- str_trim(gsub("See more.*$", "", tv.clean$locations))
tv.clean$budget <- gsub("Budget:","",tv.clean$budget)
tv.clean$budget <- str_trim(gsub("\\(estimated\\).*$", "", tv.clean$budget))
tv.clean$openingUS <- str_trim(gsub("Opening Weekend USA:","",tv.clean$openingUS))
tv.clean$grossUS <- str_trim(gsub("Gross USA:","",tv.clean$grossUS))
tv.clean$grossWorld <- str_trim(gsub("Cumulative Worldwide Gross:","",tv.clean$grossWorld))
tv.clean$prodco <- gsub("Production Co:","",tv.clean$prodco)
tv.clean$prodco <- str_trim(gsub("See more.*$", "", tv.clean$prodco))
#tv.clean$sound <- gsub("Sound Mix:","",tv.clean$sound)
#tv.clean$sound <- str_trim(gsub("\\|\\s+", ",", tv.clean$sound))

colnames(tv.clean) <- c("title","year","userRating","votes","runtime","mpaaRating","genre","releaseDate","story",
                           "director","writer","stars","metacriticScore","awardWins","awardNominations",
                           "tagline","country","language","filmingLocations","budget","openingWeekendUS",
                           "grossUS","cumulativeWorldwideGross","productionCompany")
write.csv(tv.clean,"IMDB Top 250 TV series data.csv",row.names = F)
