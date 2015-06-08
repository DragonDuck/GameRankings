library("rvest")

rootURL <- "http://www.metacritic.com"
gamesDB <- readRDS("Projects//MetacriticGames.rds")

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
        Sys.sleep(20)
        FALSE
      },
      warning = function(x) {
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
getReviewedGameList <- function(rootURL, publications, save = FALSE) {
  
}

# Go to individual links and mine the games
# To do this, create a list of games and store the information for these games in sublists:
#   [Game + Platform]
#     [url]
#     [Platform]
#     [Publication Scores]
#       - data.frame
#     [Metascore]
#     [User Score]
#     [Developer]
#     [Publisher]
#     [Release Date]
#     [Genre]
# NOTE: Games MUST be separated by platforms as well (create unique ID from name and platform) 
# because ratings may be different and publications may not rate all platform versions of the game
getGames <- function(rootURL, publications, saveCheckpoints = FALSE) {
  games <- list()
  
  # Go through the list of publications and retrieve all games this publication has 
  for(i in 1:nrow(publications)) {
    publication.name <- publications[i,"pubName"]
    publication.games <- publications[i,"reviewedGames"]
    
    # Get all the games this publication has reviewed
    pageNum <- 0
    endLoop <- FALSE
    currentGame <- 0
    while(TRUE) {
      publication.url <- paste0(publications[i,"url"],sprintf("&page=%s",pageNum))
      
      publicationHTML <- NULL
      scrapeSuccess.pub <- FALSE
      while(!scrapeSuccess.pub) {
        scrapeSuccess.pub <- tryCatch({
          publicationHTML <- html(publication.url)
          TRUE
        },
        error = function(x) {
          cat("...failed!")
          Sys.sleep(60)
          FALSE
        },
        warning = function(x) {
          Sys.sleep(60)
          FALSE
        })
      }
      
      gamesList <- html_nodes(publicationHTML,".review_product a")
      criticScoreList <- html_nodes(publicationHTML, ".brief_critscore .indiv")
      
      if(length(gamesList) < 100) endLoop <- TRUE
      
      # Go through each game, go to the site, and extract the information
      for(j in 1:length(gamesList)) {
        cat("Processing:",publication.name,"(",i,"of",nrow(publications),"on page",pageNum,"| Game",j,"of",length(gamesList),"... ")
        game.name <- html_text(gamesList[[j]])
        game.url <- paste0(rootURL,as.character(html_attrs(gamesList[[j]])))
        gameCriticScore <- data.frame(publication = publication.name, criticScore = html_text(criticScoreList[[j]]))
        
        # If this game does not exist yet, the page must be accessed and the data mined
        if(is.null(games[[game.name]])) {
          gameHTML <- NULL
          scrapeSuccess.game <- 0
          while(scrapeSuccess.game == 0) {
            scrapeSuccess.game <- tryCatch({
              gameHTML <- html(game.url)
              1
            },
            error = function(x) {
              # If the page doesn't exist, remove the entry from the games list
              if("http_404" %in% class(x))
                return(-1)
              Sys.sleep(60)
              return(0)
            },
            warning = function(x) {
              Sys.sleep(60)
              return(0)
            })
          }
          
          if(scrapeSuccess.game == -1) {
            cat("failed! (404)\n")
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
          
          # Add to games list
          newGame <- list()
          newGame[["url"]] <- game.url
          newGame[["platform"]] <- gamePlatform
          newGame[["publicationScores"]] <- gameCriticScore
          newGame[["metascore"]] <- gameMetascore
          newGame[["userscore"]] <- gameUserscore
          newGame[["developer"]] <- gameDeveloper
          newGame[["publisher"]] <- gamePublisher
          newGame[["releaseDate"]] <- gameReleaseDate
          newGame[["genre"]] <- gameGenre
          
          games[[game.name]] <- newGame
        } else {
          # If this game has already been mined, the page itself isn't necessary anymore, 
          # only the critic score must be added to the [Publication Scores] entry
          newCriticScore <- data.frame
          games[[game.name]]$publicationScores <- rbind(games[[game.name]]$publicationScores, gameCriticScore)
        }
        
        cat("success!\n")
        
        currentGame <- currentGame + 1
        
      } # End games loop
      
      if(currentGame > publication.games) endLoop <- TRUE
      
      if(endLoop) break
      pageNum <- pageNum + 1
    }
    
    saveRDS(object = games, file = sprintf("Projects//MetacriticGames_%s.rds",i))
    #publications <- publications[-1, ]
  }
  
  
}