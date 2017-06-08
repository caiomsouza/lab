# SocialMediaLab
# https://cran.r-project.org/web/packages/SocialMediaLab/index.html
# https://cran.r-project.org/web/packages/SocialMediaLab/SocialMediaLab.pdf

# install.packages("SocialMediaLab")

library(SocialMediaLab)

# Twitter App Page
# https://apps.twitter.com/



# PythonTwitterCollector

my_api_key <- "A8dkIlhIkk9rfeAZPX2p0yUX4"
my_api_secret <- "fs7c9VZcbvjDVZXAdvjW7lXuDAZ1HgLoL2rrkV1fAKXNPCbglb"
my_access_token <- "36965900-pufEdzIa94GbXeZ2tSTs4XmH9Hv9T83l1nCV0b0G7"
my_access_token_secret <- "AuRIIQLys7GOsJFepjqr4IiKJREuFN3p5b7CncEd3O7H3"
AuthenticateWithTwitterAPI(api_key=my_api_key, api_secret=my_api_secret,
                           access_token=my_access_token, access_token_secret=my_access_token_secret)


myTwitterData <- CollectDataTwitter(searchTerm = "Pentaho", numTweets = 10000, writeToFile = TRUE)

myTwitterData$text

myTwitterData$from_user

myTwitterData$screen_name


set.seed(20)
TwitterTextCluster <- kmeans(myTwitterData$text, 10, nstart = 20)
TwitterTextCluster <- kmeans(myTwitterData, 10, nstart = 20)
TwitterTextCluster

# Inicio



# Fin 



library(tm)
corpus.tmp<-Corpus(VectorSource(myTwitterData$text))

corpus.tmp

corpus.tmp<- tm_map(corpus.tmp,removePunctuation)
corpus.tmp<- tm_map(corpus.tmp, stripWhitespace)
corpus.tmp<- tm_map(corpus.tmp, tolower)
corpus.tmp<- tm_map(corpus.tmp, removeWords, stopwords("english"))
TDM <- TermDocumentMatrix(corpus.tmp)
inspect(TDM)

tdm_tfxidf<-weightTfIdf(TDM)

m<- as.matrix(tdm_tfxidf)
rownames(m)<- 1:nrow(m)

norm_eucl<- function(m)
  m/apply(m,1,function(x) sum(x^2)^.5)

m_norm<-norm_eucl(m)

results<-kmeans(m_norm,5,5)









# https://gist.github.com/josecarlosgonz/6417633

library(RCurl)
library(RJSONIO)

# install.packages("RJSONIO")

library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}


address <- geoCode("The White House, Washington, DC")

address


address <- c("The White House, Washington, DC","The Capitol, Washington, DC")
locations  <- ldply(address, function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)


address <- c("Parque del Retiro, Madrid","Aeropuerto Barajas, Madrid")
locations  <- ldply(address, function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)


address <- c("Wellington, New Zeland","Madrid, Spain")
locations  <- ldply(address, function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)





