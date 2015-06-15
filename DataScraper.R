library("rvest")

rootURL <- "http://www.metacritic.com"

# Extract all publications
getPublicationsList <- function(rootURL, save = FALSE) {
  publications <- data.frame(pubName = rep(NA,5000), reviewedGames = rep(NA,5000), url = rep(NA,5000))
  pageNum <- 0
  while(TRUE) {
    cat("Processing page",pageNum,"...")
    scrapeSuccess <- FALSE
    while(!scrapeSuccess) {
      scrapeSuccess <- tryCatch({
        publicationsHTML <- html(sprintf("http://www.metacritic.com/browse/games/publication/score?num_items=100&page=%s",pageNum))
        TRUE
      },
      error = function(x) {
        print(x)
        Sys.sleep(20)
        FALSE
      },
      warning = function(x) {
        print(x)
        Sys.sleep(20)
        FALSE
      })
    }
    cat("Got page",pageNum,"\n")
    publicationsList <- html_nodes(publicationsHTML, ".wrap a")
    publicationsReviewedGames <- html_text(html_nodes(publicationsHTML, "#main .count"))
    publicationsReviewedGames <- gsub(pattern = "\r\n\\s+Games\\s*", replacement = "", x = publicationsReviewedGames)
    publicationsReviewedGames <- gsub(pattern = "\r\n\\s+Game\\s*", replacement = "", x = publicationsReviewedGames)
    publicationsReviewedGames <- as.numeric(gsub(pattern = ",", replacement = "", x = publicationsReviewedGames))
    
    if(length(publicationsList) == 0) break
    
    publications$pubName[(1+(pageNum*100)):(length(publicationsList)+(pageNum*100))] <- html_text(publicationsList)
    publications$url[(1+(pageNum*100)):(length(publicationsList)+(pageNum*100))] <- paste0(
      rootURL,as.character(html_attrs(publicationsList)),"&num_items=100&sort_options=critic_score")
    publications$reviewedGames[(1+(pageNum*100)):(length(publicationsList)+(pageNum*100))] <- publicationsReviewedGames
    
    pageNum <- pageNum + 1
  }
  # Remove unnecessary rows in publications data frame
  publications <- publications[-as.numeric(row.names(publications[which(is.na(publications$pubName) & is.na(publications$url)),])),]
  if(save) saveRDS(object = publications, file = "MetacriticPublications.rds")
  return(publications)
}

# Get the games that each publication has reviewed
getReviewedGameList <- function(rootURL, publications, save = FALSE, saveCheckpoints = FALSE) {
  returnList <- list()
  for(i in 1:nrow(publications)) {
    publication.name <- publications[i,"pubName"]
    publication.games <- publications[i,"reviewedGames"]
    numOfPages <- (publication.games %/% 100)  # Normally the page number needs to be rounded up but pages start at 0 on metacritic, so page 10 is denoted as &page=9
    gamesDF <- data.frame(name = NULL, platform = NULL, url = NULL, criticScore = NULL)
    for(pageNum in 0:numOfPages) {
      cat("Processing publication nr.",i,"of",nrow(publications),": page",pageNum,"of",numOfPages,"\n")
      publication.url <- paste0(publications[i,"url"],sprintf("&page=%s",pageNum))
      publicationHTML <- NULL
      scrapeSuccess <- FALSE
      errorCode <- "NO_ERROR"
      while(!scrapeSuccess) {
        scrapeSuccess <- tryCatch({
          publicationHTML <- html(publication.url)
          TRUE
        },
        error = function(x) {
          print(x)
          if("http_500" %in% class(x)) {
            errorCode <<- "http_500"
            return(TRUE)
          }
          Sys.sleep(20)
          return(FALSE)
        })
      }
      
      if(errorCode == "http_500") {
        next
      }
      
      gamesList <- html_nodes(publicationHTML,".review_product a")
      gamesDF <- rbind(gamesDF,data.frame(name = html_text(gamesList), 
                                          platform = sapply(X = html_attrs(gamesList), function(x) {return(strsplit(x,"/")[[1]][3])}), 
                                          url = paste0(rootURL,html_attrs(gamesList)), 
                                          criticScore = as.numeric(html_text(html_nodes(publicationHTML, ".brief_critscore .indiv"))),
                                          stringsAsFactors = FALSE))
    }
    returnList[[publication.name]] <- gamesDF
    if(saveCheckpoints) {
      unlink("MetacriticReviewedGamesList_Checkpoint.rds")
      saveRDS(object = returnList, file = "MetacriticReviewedGamesList_Checkpoint.rds")
    }
  }
  if(save) saveRDS(object = returnList, file = "MetacriticReviewedGamesList.rds")
  return(returnList)
}

# Generate a "database" of games containing the interesting data (developer, publisher, etc)
# NOTE: Games MUST be separated by platforms as well (create unique ID from name and platform) 
# because ratings may be different and publications may not rate all platform versions of the game
getGameData <- function(gamesList, save = FALSE, saveCheckpoints = FALSE) {
  # Create a comprehensive list of all games from the gameList (which is split by publisher and has duplicates)
  allGames <- data.frame(name = NULL, platform = NULL, url = NULL, stringsAsFactors = FALSE)
  for(pub in gamesList) {
    if(nrow(pub) > 0) allGames <- rbind(allGames, pub[,c("name","platform","url")])
  }
  # Remove duplicate games
  allGames.unique <- unique(x = allGames)
  
  resultDF <- data.frame(name = NULL, url = NULL, platform = NULL, metascore = NULL, userscore = NULL, developer = NULL, publisher = NULL, releaseDate = NULL, genre = NULL)
  # Iterate through games and extract the data
  for(i in 1:nrow(allGames.unique)) {
    cat("Processing game",i,"of",nrow(allGames.unique),"... ")
    game.name <- allGames.unique[i,"name"]
    game.platform <- allGames.unique[i,"platform"]
    game.url <- allGames.unique[i,"url"]
    game.listName <- paste0(game.name,"_",game.platform)
    
    gameHTML <- NULL
    scrapeSuccess <- 0
    while(scrapeSuccess == 0) {
      scrapeSuccess <- tryCatch({
        gameHTML <- html(as.character(game.url))
        1
      },
      error = function(x) {
        print(x)
        # If the page doesn't exist, remove the entry from the games list
        if("http_404" %in% class(x)) return(-1)
        if("http_500" %in% class(x)) return(-2)
        Sys.sleep(20)
        return(0)
      },
      warning = function(x) {
        Sys.sleep(20)
        return(0)
      })
    }
    
    if(scrapeSuccess == -1) {
      cat("failed! (404)\n")
      next
    }
    if(scrapeSuccess == -2) {
      cat("failed! (500)\n")
      next
    }
    
    gameMetascore <- html_text(html_nodes(gameHTML, ".positive span"))
    if(length(gameMetascore) == 0) gameMetascore <- NA
    gameUserscore <- html_text(html_nodes(gameHTML, ".large"))
    if(length(gameUserscore) == 0) { gameUserscore <- NA
    } else gameUserscore <- gameUserscore[[1]]
    
    gamePlatform <- html_text(html_nodes(gameHTML, ".platform span"))
    # Remove whitespace from platform:
    gamePlatform <- gsub(pattern = "\n",replacement = "",x = gamePlatform)
    gamePlatform <- gsub(pattern = "\\s+", replacement = "", gamePlatform)
    if(length(gamePlatform) == 0) gamePlatform <- NA
    
    gameDeveloper <- html_text(html_nodes(gameHTML, ".developer .data"))
    # Remove whitespace from developer:
    gameDeveloper <- gsub(pattern = "\n",replacement = "",x = gameDeveloper)
    gameDeveloper <- gsub(pattern = "\\s+", replacement = "", gameDeveloper)
    if(length(gameDeveloper) == 0) gameDeveloper <- NA
    
    gamePublisher <- html_text(html_nodes(gameHTML,".publisher .data span"))
    # Remove whitespace from publisher:
    gamePublisher <- gsub(pattern = "\n",replacement = "",x = gamePublisher)
    gamePublisher <- gsub(pattern = "\\s+", replacement = "", gamePublisher)
    if(length(gamePublisher) == 0) gamePublisher <- NA
    
    gameReleaseDate <- html_text(html_nodes(gameHTML, ".release_data .data"))
    if(length(gameReleaseDate) == 0) {
      gameReleaseDate <- NA
    } else gameReleaseDate <- as.POSIXlt(gameReleaseDate, format = "%b %d, %Y")
    
    gameGenre <- html_text(html_nodes(gameHTML,".product_genre .data"))
    # Remove whitespace from genre:
    gameGenre <- gsub(pattern = "\n",replacement = "",x = gameGenre)
    gameGenre <- gsub(pattern = "\\s+", replacement = "", gameGenre)
    if(length(gameGenre) == 0) gameGenre <- NA
    
    newDF <- data.frame(name = NA, url = NA, platform = NA, metascore = NA, userscore = NA, developer = NA, publisher = NA, releaseDate = NA, genre = NA)
    newDF$name <- game.listName
    newDF$url <- as.character(game.url)
    newDF$platform <- as.character(gamePlatform)
    newDF$metascore <- as.numeric(gameMetascore)
    newDF$userscore <- as.numeric(gameUserscore)
    newDF$developer <- as.character(gameDeveloper[1])
    newDF$publisher <- as.character(gamePublisher[1])
    newDF$releaseDate <- as.character(gameReleaseDate)
    newDF$genre <- as.character(gameGenre)
    resultDF <- rbind(resultDF,newDF)
    
    if(saveCheckpoints) {
      if(i %% 100 == 0) {
        unlink("MetacriticGamesDB_Checkpoint.rds")
        saveRDS(object = resultDF, file = "MetacriticGamesDB_Checkpoint.rds")
      }
    }
    
    cat("success!\n")
  } # End Games loop
  
  if(save) saveRDS(object = resultDF, file = "MetacriticGamesDB.rds")
  return(resultDF)
}