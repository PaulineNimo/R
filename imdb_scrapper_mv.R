library("rvest")
library("XML")
library("xml2")
library("stringr")
library("httr")

# IMDB Top 250 Movies
url = "http://www.imdb.com/chart/top?ref_=nv_wl_img_3"

page = read_html(url)

movie.nodes <- html_nodes(page,'.titleColumn a')

movie.link = sapply(html_attrs(movie.nodes),`[[`,'href')
movie.link = paste0("http://www.imdb.com",movie.link)
movie.cast = sapply(html_attrs(movie.nodes),`[[`,'title')
movie.name = html_text(movie.nodes)

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

top250 <- data.frame(movie.name, movie.cast, movie.link,year,votes,rating)

#read all urls & store the values
url1 <- as.character(top250$movie.link)
movie.tot <- data.frame(matrix(NA,nrow = 250),stringsAsFactors = F)

# Looping starts here 
for(i in 74:length(url1)){
    x <- url1[i]
    
    page <- read_html(x)
    movie.titleYear <- html_text(html_nodes(page, "span#titleYear"))
    movie.ratingValue <- html_text(html_nodes(page, "strong"))[1]
    movie.ratingCount <- html_text(html_nodes(page, "span.small"))
    movie.runTime <- html_text(html_nodes(page, "time"))[2]
    web.content <- content(GET(x), as="text")
    movie.rating <- str_trim(gsub("\"","",str_match(web.content,"\"contentRating\": (.*?),")[,2]))
    movie.genre <- str_trim(paste(html_text(html_nodes(page, "div.subtext a"))[-length(html_text(html_nodes(page, "div.subtext a")))], collapse = ","))
    movie.releaseDate <- str_trim(gsub("\n","",html_text(html_nodes(page, "div.subtext a"))[length(html_text(html_nodes(page, "div.subtext a")))]))
    movie.story <- html_node(page,'.summary_text')
    xmlTreeParse(movie.story)
    story <- gsub("^$|^[ \t\n\v\r\f]+","NA",
                  gsub("^([ \t\n\r\f]+)|([ \t\n\f\r]+)$","",
                       gsub("\n","",as.character(html_text(movie.story))
                       )))
    movie.details <- html_nodes(page,'div.credit_summary_item')
    movie.director <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",
                              as.character(html_text(movie.details)[1])))
    movie.writer <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",
                            as.character(html_text(movie.details)[2])))
    movie.stars <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",
                                  as.character(html_text(movie.details)[3])))
    movie.metacriticScore <- gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",html_text(html_nodes(page, "div.metacriticScore")))
    if(length(movie.metacriticScore) == 0) movie.metacriticScore <- ""
    movie.awardsnodes <- html_nodes(page,'span.see-more.inline a')
    award.link <- sapply(html_attrs(movie.awardsnodes),`[[`,'href')
    awards.link <- paste0("http://www.imdb.com",award.link[1])
    awards.noms <- html_text(html_nodes(read_html(awards.link), "div.desc"))
    movie.wins <- str_match(awards.noms,"all (.*?) wins")[2]
    movie.noms <- str_match(awards.noms,"and (.*?) nominations")[2]  
    movie.specs <- str_trim(gsub("(\t)|(\n)|(\v)|(\r)|(\f)","",html_text(html_nodes(page, "div.txt-block"))))
    movie.tagline <- movie.specs[startsWith(movie.specs,"Tagline")]
    if(length(movie.tagline) == 0) movie.tagline <- ""
    movie.country <- movie.specs[startsWith(movie.specs,"Country")]
    movie.language <- movie.specs[startsWith(movie.specs,"Language")]
    movie.locations <- movie.specs[startsWith(movie.specs,"Filming Location")]
    if(length(movie.locations) == 0) movie.locations <- ""
    movie.budget <- movie.specs[startsWith(movie.specs,"Budget")]
    if(length(movie.budget) == 0) movie.budget <- ""
    movie.openingUS <- movie.specs[startsWith(movie.specs,"Opening Weekend US")]
    if(length(movie.openingUS) == 0) movie.openingUS <- ""
    movie.grossUS <- movie.specs[startsWith(movie.specs,"Gross US")]
    if(length(movie.grossUS) == 0) movie.grossUS <- ""
    movie.grossWorld <- movie.specs[startsWith(movie.specs,"Cumulative Worldwide")]
    if(length(movie.grossWorld) == 0) movie.grossWorld <- ""
    movie.prodco <- movie.specs[startsWith(movie.specs,"Production Co")]
    movie.sound <- movie.specs[startsWith(movie.specs,"Sound Mix")]
    movie.tot$titleYear[i] <- movie.titleYear
    movie.tot$ratingValue[i] <- movie.ratingValue
    movie.tot$ratingCount[i] <- movie.ratingCount
    movie.tot$runTime[i] <- movie.runTime
    movie.tot$rating[i] <- movie.rating
    movie.tot$genre[i] <- movie.genre
    movie.tot$releaseDate[i] <- movie.releaseDate
    movie.tot$story[i] <- story
    movie.tot$director[i] <- movie.director
    movie.tot$writer[i] <- movie.writer
    movie.tot$stars[i] <- movie.stars
    movie.tot$metacriticScore[i] <- movie.metacriticScore
    movie.tot$wins[i] <- movie.wins
    movie.tot$noms[i] <- movie.noms
    movie.tot$tagline[i] <- movie.tagline
    movie.tot$country[i] <- movie.country
    movie.tot$language[i] <- movie.language
    movie.tot$locations[i] <- movie.locations
    movie.tot$budget[i] <- movie.budget
    movie.tot$openingUS[i] <- movie.openingUS
    movie.tot$grossUS[i] <- movie.grossUS
    movie.tot$grossWorld[i] <- movie.grossWorld
    movie.tot$prodco[i] <- movie.prodco
    movie.tot$sound[i] <- movie.sound
    
    print(paste0("Completed: ",i))
}
movie.tot <- movie.tot[,-1]
movie.tot <- cbind(movie.name,movie.tot)
write.csv(movie.tot,"IMDB Top 250 movies.csv",row.names = F)
movie.clean <- movie.tot
movie.clean$titleYear <- gsub("\\(|\\)","",movie.clean$titleYear)
movie.clean$director <- gsub("Director:","",movie.clean$director)
movie.clean$writer <- gsub("Writer:|Writers:","",movie.clean$writer)
movie.clean$writer <- str_trim(gsub("\\|.*$", "", movie.clean$writer))
movie.clean$stars <- gsub("Stars:|Star:","",movie.clean$stars)
movie.clean$stars <- str_trim(gsub("\\|.*$", "", movie.clean$writer))
movie.clean$tagline <- gsub("Taglines:|Tagline:","",movie.clean$tagline)
movie.clean$tagline <- str_trim(gsub("See more.*$", "", movie.clean$tagline))
movie.clean$country <- gsub("Countries:|Country:","",movie.clean$country)
movie.clean$country <- str_trim(gsub("\\s+\\|\\s+", ",", movie.clean$country))
movie.clean$language <- gsub("Languages:|Language:","",movie.clean$language)
movie.clean$language <- str_trim(gsub("\\s+\\|\\s+", ",", movie.clean$language))
movie.clean$locations <- gsub("Filming Locations:|Filming Location:","",movie.clean$locations)
movie.clean$locations <- str_trim(gsub("See more.*$", "", movie.clean$locations))
movie.clean$budget <- gsub("Budget:","",movie.clean$budget)
movie.clean$budget <- str_trim(gsub("\\(estimated\\).*$", "", movie.clean$budget))
movie.clean$openingUS <- str_trim(gsub("Opening Weekend USA:","",movie.clean$openingUS))
movie.clean$grossUS <- str_trim(gsub("Gross USA:","",movie.clean$grossUS))
movie.clean$grossWorld <- str_trim(gsub("Cumulative Worldwide Gross:","",movie.clean$grossWorld))
movie.clean$prodco <- gsub("Production Co:","",movie.clean$prodco)
movie.clean$prodco <- str_trim(gsub("See more.*$", "", movie.clean$prodco))
movie.clean$sound <- gsub("Sound Mix:","",movie.clean$sound)
movie.clean$sound <- str_trim(gsub("\\|\\s+", ",", movie.clean$sound))

colnames(movie.clean) <- c("title","year","userRating","votes","runtime","mpaaRating","genre","releaseDate","story",
                           "director","writer","stars","metacriticScore","awardWins","awardNominations",
                           "tagline","country","language","filmingLocations","budget","openingWeekendUS",
                           "grossUS","cumulativeWorldwideGross","productionCompany","sound")
write.csv(movie.clean,"IMDB Top 250 movies data.csv", row.names = F)
