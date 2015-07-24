reviewedGames <- readRDS("Projects/GameRankings/MetacriticReviewedGamesList.rds")
gamesDB <- readRDS("Projects/GameRankings/MetacriticGamesDB.rds")

pubScores.publisher <- getCategoryScoresForAllPublications(category = "publisher", publicationGameList = reviewedGames, allGames = gamesDB)
pubScores.developer <- getCategoryScoresForAllPublications(category = "developer", publicationGameList = reviewedGames, allGames = gamesDB)
pubScores.userscore <- getCategoryScoresForAllPublications(category = "userscore", publicationGameList = reviewedGames, allGames = gamesDB)
pubScores.metascore <- getCategoryScoresForAllPublications(category = "metascore", publicationGameList = reviewedGames, allGames = gamesDB)
  
# This resorts data. Input is a list of data frames, one for each publications, consisting of categories
# (e.g. developer, publisher, etc.) and the respective score given by the publication. Output is a list 
# of data frames, one for each category entry, consisting of the publication and the score given by that
# publication for that list entry.
# pubScores.category should be a list of data frames as returned by getCategoryScoresForAllPublications()
getResortedDataByPublication <- function(pubScores.category) {
  publicationNames <- names(pubScores.category)
  dataSummaryByPublication <- list()
  
  for(i in 1:length(pubScores.category)) {
    cat(i,"of",length(pubScores.category),"\n")
    pubName <- publicationNames[[i]]
    dataDF <- pubScores.category[[i]]
    dataCategoryEntries <- unique(dataDF$category)
    for(dataCategoryEntry in dataCategoryEntries) {
      group <- dataDF[which(dataDF$category == dataCategoryEntry),]
      groupSummary <- summary(group$score)
      # Add to the category entry with the publication's name as a row name
      newRow <- matrix(groupSummary,nrow=1)
      colnames(newRow) <- names(groupSummary)
      rownames(newRow) <- pubName
      dataSummaryByPublication[[dataCategoryEntry]] <- rbind(dataSummaryByPublication[[dataCategoryEntry]], newRow)
    }
  } 
  return(dataSummaryByPublication)
}

# TODO: Find discrepancy in games by platform -> which publication is the worst offender when all games are averaged?
# 1. Loop through all games and find publications which reviewed the game on several platforms.
# 2. Check discrepancy between scores based on platform
# 3. Compare discrepancy between publications and find outliers
# 

# TODO: Find correlations in scores between developers/publishers and game scores
#         In order to do this, see if there are publications which consistently score games from certain devs/pubs higher than other publications or higher than other games
