Packages <- c("cluster", "ggplot2", "h2o", "fpc")
lapply(Packages, library, character.only = TRUE)

localH2o<-h2o.init()
prostate.hex<- h2o.uploadFile(path = system.file ("extdata", "prostate.csv", package="h2o"), destination_frame = "prostate")
summary(prostate.hex)

dfK<-data.frame(Clusters=numeric(),TotWithinSS=numeric(),BetweenSS=numeric(),TotSS=numeric(),stringsAsFactors = FALSE)
for (i in 2:50) {
  kmeans_imp_hex <- h2o.kmeans(training_frame = prostate.hex, k = i, max_iterations = 1000, standardize = T)
  dfK[i-1,1] <- i
  dfK[i-1,2] <- getTotWithinSS(kmeans_imp_hex) 
  dfK[i-1,3] <- getBetweenSS(kmeans_imp_hex) 
  dfK[i-1,4] <- getTotSS(kmeans_imp_hex)
}
plot(dfK$Clusters, dfK$TotWithinSS, type="l")
prostate.km<- h2o.kmeans(prostate.hex, k = 10, x = c("AGE","RACE","GLEASON","CAPSULE","DCAPS"))
summary(prostate.km)

prd<-h2o.predict(newdata=prostate.hex,prostate.km)
Cluspred<-h2o.cbind(prostate.hex, prd)
Cluspred.data<-as.data.frame(Cluspred)
plot(prostate.data$AGE,Cluspred.data$predict, xlab="Age", ylab="Clusters", main = "Clusters by Age for Prostate Disease")
ggplot(prostate.data,aes(prostate.data$AGE,Cluspred.data$GLEASON))+geom_text(aes(label=prostate.data$ID))+ggtitle("Prostate: Clusters by Age, Gleason Score")+ xlab("Age")+ ylab("Gleason Score")
prostate.data<- as.data.frame(prostate.hex)
prostate.ctrs <- as.data.frame(h2o.centers(prostate.km))

ggplot(mapping=aes(x = prostate.ctrs$race,prostate.ctrs$age))+geom_point(color="blue")+ggtitle("K-means centers for Prostate Disease")+xlab("Race")+ylab("Age")

ggplot(mapping=aes(x = prostate.ctrs$centroid,prostate.ctrs$capsule))+geom_point(color="blue")+ggtitle("K-means centers for Prostate Disease")+xlab("Centroid")+ylab("Capsule")
