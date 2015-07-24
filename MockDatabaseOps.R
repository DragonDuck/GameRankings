# Translates a publication's game list into a list of scores and categories
# category should be one of the entries in the games "database" (e.g. developer, publisher, metascore, etc)
# publicationGameList should be the dataframe of reviewed games associated with 
# a publication.
# allGames should be the dataframe with all the information to the games (dev, publisher, score, etc.)
getCategoryScores <- function(category, publicationGameList, allGames) {
  resultDF <- data.frame(name = NULL, category = NULL, score = NULL)
  for(i in 1:nrow(publicationGameList)) {
    gameInfo <- allGames[which(allGames$name == 
                                 paste0(publicationGameList[i,]$name,"_",publicationGameList[i,]$platform)),]
    if(nrow(gameInfo) == 0) next
    gcat <- as.character(gameInfo[[category]])
    gname <- as.character(gameInfo$name)
    gscore <- as.numeric(as.character(publicationGameList[i,]$criticScore))
    resultDF <- rbind(resultDF, data.frame(name = gname, category = gcat, score = gscore))
  }
  return(resultDF)
}

# Like getCategoryScores but not publicationGameLists is a list of games reviewed by a publication
getCategoryScoresForAllPublications <- function(category, publicationGameLists, allGames) {
  resultList <- list()
  pubNames <- names(publicationGameLists)
  for(i in 1:length(publicationGameLists)) {
    cat("Processing",pubNames[i],": ",i,"of",length(publicationGameLists),"\n")
    resultList[[pubNames[i]]] <- getCategoryScores(category, publicationGameLists[[i]], allGames)
  }
  return(resultList)
}