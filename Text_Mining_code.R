setwd("C:/Users/Amol/Desktop/Fall 18/MIS 620/Assignments/Assignment 3")

library(tm) #for corpus and term document matrix creation/processing
library(SnowballC) #for stemming
library(wordcloud)
library(cluster)
library(rpart)
library(data.table)

#Read the Subset of Book Pride and Prejudice
Pride_A <- read.csv("Pride and Prejudice.csv")

#Find the number of Missing Rows  
table(is.na(Pride_A$Text))

#Delete the Missing rows
na.omit(Pride_A$Text)    

head(Pride_A)
View(Pride_A)
 
str(Pride_A)

#convert tweet text to character, read.csv defaults to Factor
Pride_A <- as.character(Pride_A$Text)

Pride_A$Text <- iconv(Pride_A$Text, to="utf-8",sub="")

#Regular Expression Removing - 
Pride_A <- gsub("-","", Pride_A)

#Removing Letters Repeated more than Twice
Pride_A  <- gsub('([[:alpha:]])\\1+', '\\1\\1', Pride_A)

#additional cleaning removing leaving only letters numbers or spaces 
Pride_A <- gsub("[^a-zA-Z0-9 ]","",Pride_A)

#create corpus and clean up text before creating DTM
Pride_A_Corpus <- Corpus(VectorSource(Pride_A))

Pride_A_Corpus <- tm_map(Pride_A_Corpus, stemDocument)
Pride_A_Corpus <- tm_map(Pride_A_Corpus, removePunctuation)
Pride_A_Corpus <- tm_map(Pride_A_Corpus, removeNumbers)
Pride_A_Corpus <- tm_map(Pride_A_Corpus, removeWords, stopwords("english"))
Pride_A_Corpus <- tm_map(Pride_A_Corpus, stripWhitespace)  

#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(Pride_A_Corpus)

#count row (i.e, terms)
#must convert to matrix to work with as dtm is stored as a memory efficient sparse matrix doesn't store
#empty fields
tdm$nrow 

#inspect the term document matrix, make sure to subset it is very large 
inspect(tdm[1:30, 1:30])

#there are over 30,000 terms and very high sparsity lets trim down and remove terms
#remove words that are over 98% sparse (i.e., do not appear in 98% of documents)
tdm <- removeSparseTerms(tdm, 0.98)
tdm$nrow #now 23 terms
tdm$ncol # 11,017 Documents 
inspect(tdm[1:22, 1:3])

inspect(tdm)


# define tdm as matrix
m = as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d #lets see frequency of words

# plot wordcloud
set.seed(1)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#lets make a bar chart of frequent words
barplot(d[2:10,]$freq, las = 2, names.arg = d[2:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#lets cluster the documents, but first find optimal k
wss <- numeric(15) 
for (k in 1:10) wss[k] <- sum(kmeans(tdm, centers=k)$withinss)
plot(wss, type="b") #seems like 2 or 3 will cover it

Pride.kmeans <- kmeans(tdm,4)
Pride.kmeans$cluster #lets looks at cluster membership

tdm$cluster <- Pride.kmeans$cluster
length(tdm[tdm$cluster==1,]$words)
tdm[tdm$cluster==2,]$words
