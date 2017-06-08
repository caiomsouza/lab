# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html


#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

#cname <- file.path("~", "Desktop", "texts")   
#cname 
#dir(cname)

#setwd("C:/Users/csouza/Desktop")

cname <- file.path("C:/Users/csouza/Desktop/tmp", "texts")   
cname   
dir(cname)



library(tm)   
docs <- Corpus(DirSource(cname))   

summary(docs)

inspect(docs[2])

docs <- tm_map(docs, removePunctuation)   

for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, tolower)  

docs <- tm_map(docs, removeWords, stopwords("english")) 

docs <- tm_map(docs, removeWords, c("department", "email"))

for (j in seq(docs))
{
  docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
  docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
  docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
}

library(SnowballC)   
docs <- tm_map(docs, stemDocument)  


docs <- tm_map(docs, stripWhitespace)  

docs <- tm_map(docs, PlainTextDocument)  

dtm <- DocumentTermMatrix(docs)   
dtm   


tdm <- TermDocumentMatrix(docs)   
tdm  

freq <- colSums(as.matrix(dtm))   
length(freq)   

ord <- order(freq) 

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms) 

freq[head(ord)]  


freq[tail(ord)] 

head(table(freq), 20)   

tail(table(freq), 20) 

freq <- colSums(as.matrix(dtms))   
freq  

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

library(ggplot2)   
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98  

findAssocs(dtms, "contrast", corlimit=0.90) # specifying a correlation limit of 0.95 

library(wordcloud)  

set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)   

set.seed(142)   
wordcloud(names(freq), freq, max.words=100)   

set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  

dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)   

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit   


plot(fit, hang=-1) 

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters 

library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   


