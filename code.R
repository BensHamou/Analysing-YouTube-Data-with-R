library(plyr)
library(vosonSML)
library(magrittr)
library(stringr)
library(sna)
library(igraph)
library(Matrix)

my_apiKeyYoutube<-"AIzaSyBodp9cxX55MhRUNWCNPQ_fi95L8IvRuGQ"
apiKeyYoutube<-Authenticate("youtube", apiKey=my_apiKeyYoutube)

df <- read.csv(file='C:/Users/bensh/Desktop/YouTube.csv',header = TRUE, sep = '\t')
head(df)

cat <- unique(df$category)
print(cat)

df2<-subset(df, views>200000 & rate>4)
len <- dim(df2)[1]
print(len)

mat <- matrix(0, 17, len)
data.frame(as.list(cat))

for(i in 1:17)
  for(j in 1:len)
    if(df2$category[j] == cat[i])
      mat[i,j] <- 1

print(rowSums(mat[]))
print(cat[which.max(rowSums(mat[]))])

net <- network(mat)
plot(net)

videoIDs=c("6366dxFf-Os")
myYoutubeData<-Collect(apiKeyYoutube, videoIDs, writeToFile=TRUE, verbose=TRUE, maxComments=300)

summary(myYoutubeData)
easy_dataset <- data.frame(myYoutubeData$AuthorDisplayName, myYoutubeData$Comment, myYoutubeData$ReplyCount, myYoutubeData$LikeCount) 
colnames(easy_dataset) <- c("author", "sentence", "replies", "likes")
summary(easy_dataset)
easy_dataset$likes <- as.numeric(as.character(easy_dataset$likes))
easy_dataset$replies <- as.numeric(as.character(easy_dataset$replies)) 
easy_dataset$sentence <- as.character(easy_dataset$sentence) 
easy_dataset$author <- as.character(easy_dataset$author)
summary(easy_dataset)

comments <- count(easy_dataset$author) 
comments <- comments$freq

temp <- strsplit(easy_dataset$sentence, split=" ") 
easy_dataset$words <- sapply(temp, length)

easy_dataset <- ddply(easy_dataset, "easy_dataset$author", numcolwise(sum))

easy_dataset$comments <- comments 
colnames(easy_dataset) <- c("author", "replies", "likes", "words", "comments")

head(easy_dataset[2:5])

identification <- subset(easy_dataset, replies>="1" & likes>="1" & words>="10" & comments>="1") 
head(identification)

most_replies <- easy_dataset[order(-easy_dataset$replies),] 
most_likes <- easy_dataset[order(-easy_dataset$likes),] 
most_words <- easy_dataset[order(-easy_dataset$words),] 
most_comments <- easy_dataset[order(-easy_dataset$comments),]

head(most_likes)

sentences <- subset(myYoutubeData[c(1:2)], AuthorDisplayName=="Mardy Bum") 
sentences[c(1), 1]

summary(easy_dataset[c(2:5)])

easy_dataset$relevant <- ifelse(easy_dataset$replies>=0.05 & easy_dataset$likes>=1.5 & easy_dataset$words>=11.75 & easy_dataset$comments>=1.15, 1,0)
summary(easy_dataset[c(6)])

easy_dataset$replies <- ifelse(easy_dataset$replies>=0.05,1,0) 
easy_dataset$likes <- ifelse(easy_dataset$likes>=1.5,1,0) 
easy_dataset$words <- ifelse(easy_dataset$words>=11.75,1,0) 
easy_dataset$comments <- ifelse(easy_dataset$comments>1.15,1,0)
summary(easy_dataset[c(2:5)])

glm.fit <- glm(data=easy_dataset, relevant~replies+likes+words+comments, family=binomial()) 
glm.fit

probabilities <- glm.fit %>% predict(easy_dataset, type = "response") 
predicted.classes <- ifelse(probabilities > 0.5, "1", "0") 
which(predicted.classes=="1") 
mean(predicted.classes == easy_dataset$relevant)

activityNetwork <- myYoutubeData %>% Create("activity") %>% AddText(myYoutubeData)
activityGraph <- activityNetwork %>% Graph(writeToFile = TRUE)

which(activityNetwork$nodes[6]=="Mardy Bum")

(V(activityGraph)$author_id[62])
activityNetwork$nodes[62, 6]

V(activityGraph)$color <- "orange"
V(activityGraph)$color[which(V(activityGraph)$node_type=="video")] <- "red"
V(activityGraph)$color[which(V(activityGraph)$node_type=="comment")] <- "purple"
V(activityGraph)$color[which(V(activityGraph)$author_id=="UCu-tfD-kMo0XLxhWdAjUZNg")] <- "blue"
marker <- grep("listening", tolower(V(activityGraph)$vosonTxt_comment))
V(activityGraph)$color[marker] <- "green"
plot(activityGraph, vertex.label="", vertex.size=4, edge.arrow.size=0.6, main="Activity Network (N=300)")


#music
videoIDs1 <- c("VafTMsrnSTU")
myYoutubeData1 <- Collect(apiKeyYoutube, videoIDs1, writeToFile=TRUE, verbose=TRUE, maxComments=500)

#sport
videoIDs2 <- c("x3dS_AoxvF4")
myYoutubeData2 <- Collect(apiKeyYoutube, videoIDs2, writeToFile=TRUE, verbose=TRUE, maxComments=500)

#gaming
videoIDs3 <- c("ooL9nVQA6qU")
myYoutubeData3 <- Collect(apiKeyYoutube, videoIDs3, writeToFile=TRUE, verbose=TRUE, maxComments=500)

#politics
videoIDs4 <- c("zKAFCArVDH0")
myYoutubeData4 <- Collect(apiKeyYoutube, videoIDs4, writeToFile=TRUE, verbose=TRUE, maxComments=500)

activityNetwork1 <- myYoutubeData1 %>% Create("activity") %>% AddText(myYoutubeData1)
activityGraph1 <- activityNetwork1 %>% Graph(writeToFile = TRUE)

activityNetwork2 <- myYoutubeData2 %>% Create("activity") %>% AddText(myYoutubeData2)
activityGraph2 <- activityNetwork2 %>% Graph(writeToFile = TRUE)

activityNetwork3 <- myYoutubeData3 %>% Create("activity") %>% AddText(myYoutubeData3)
activityGraph3 <- activityNetwork3 %>% Graph(writeToFile = TRUE)

activityNetwork4 <- myYoutubeData4 %>% Create("activity") %>% AddText(myYoutubeData4)
activityGraph4 <- activityNetwork4 %>% Graph(writeToFile = TRUE)


V(activityGraph1)$color <- "orange"
V(activityGraph2)$color <- "orange"
V(activityGraph3)$color <- "orange"
V(activityGraph4)$color <- "orange"

V(activityGraph1)$color[which(V(activityGraph1)$node_type=="video")] <- "red"
V(activityGraph2)$color[which(V(activityGraph2)$node_type=="video")] <- "red"
V(activityGraph3)$color[which(V(activityGraph3)$node_type=="video")] <- "red"
V(activityGraph4)$color[which(V(activityGraph4)$node_type=="video")] <- "red"

V(activityGraph1)$color[which(V(activityGraph1)$node_type=="comment")] <- "purple"
V(activityGraph2)$color[which(V(activityGraph2)$node_type=="comment")] <- "purple"
V(activityGraph3)$color[which(V(activityGraph3)$node_type=="comment")] <- "purple"
V(activityGraph4)$color[which(V(activityGraph4)$node_type=="comment")] <- "purple"

marker1 <- grep("listening", tolower(V(activityGraph1)$vosonTxt_comment))
marker2 <- grep("play", tolower(V(activityGraph2)$vosonTxt_comment))
marker3 <- grep("block", tolower(V(activityGraph3)$vosonTxt_comment))
marker4 <- grep("people", tolower(V(activityGraph4)$vosonTxt_comment))

V(activityGraph1)$color[marker1] <- "green"
V(activityGraph2)$color[marker2] <- "green"
V(activityGraph3)$color[marker3] <- "green"
V(activityGraph4)$color[marker4] <- "green"

plot(activityGraph1, vertex.label="", vertex.size=4, edge.arrow.size=0.6, main="Music")
plot(activityGraph2, vertex.label="", vertex.size=4, edge.arrow.size=0.6, main="Sport")
plot(activityGraph3, vertex.label="", vertex.size=4, edge.arrow.size=0.6, main="Gaming")
plot(activityGraph4, vertex.label="", vertex.size=4, edge.arrow.size=0.6, main="Politics")
