reviewedGames.dataFrame2 <- reviewedGames.dataFrame
appendingFrame <- data.frame(metascore = rep(NA, 352212), userscore = rep(NA, 352212), developer = rep(NA, 352212), publisher = rep(NA, 352212), releaseDate = rep(NA, 352212), genre = rep(NA, 352212))
reviewedGames.dataFrame2 <- cbind(reviewedGames.dataFrame2, appendingFrame)

for(i in 1:nrow(reviewedGames.dataFrame2)) {
  if(i%%1000 == 0) cat(i,"of",nrow(reviewedGames.dataFrame),"\n")
  gamesDBname <- paste(reviewedGames.dataFrame2[i,"name"], reviewedGames.dataFrame2[i,"platform"], sep = "_")
  reviewedGames.dataFrame2[i,6:11] <- gamesDB[which(gamesDB$name == gamesDBname),4:9]
}