mostFrequentTerms <- function (x.dtm, n=6) {
    freq <- colSums(as.matrix(x.dtm))
    ord <- order(freq)
    freq[tail(ord, n)]
} 

findTerms <- function (x.dtm, pattern) {
    freq <- colSums(as.matrix(x.dtm))
    terms <- names(freq)
    freq[grep(pattern, terms)]
}


library(tm)
library(stringr)

LANG <- "en_US"
#ROOT_DIR <- "C:/NO_BACKUP/final"
ROOT_DIR <- "C:/NO_BACKUP/small"

ptime0 <- proc.time()
fdir <- sprintf("%s/%s", ROOT_DIR, LANG)
corpus <- Corpus(DirSource(fdir, encoding="UTF-8"), readerControl = list(language=LANG))
ptime1 <- proc.time()
ptime1-ptime0

corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^[:print:]]", " ", x))) # remove unprintable characters
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z]{2,}", " ", x))) # remove repeated non_letters
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[()]", " ", x))) # remove parens
corpus <- tm_map(corpus, content_transformer(function(x) gsub("^[^a-z]", " ", x))) # remove parens
corpus <- tm_map(corpus, content_transformer(stripWhitespace))

dtm <- DocumentTermMatrix(corpus)
dtm
nterms <- dtm$ncol
inspect(dtm[, c(1:3, seq(nterms-4,nterms))])  

nterms #raw term count

freq <- colSums(as.matrix(dtm))
hist(freq)
freq[which.max(freq)]

mostFrequentTerms(dtm,10)
freq.top100 <- mostFrequentTerms(dtm,100)
hist(freq.top100)

# corpus.nonstop <- corpus  #remove stopwords in a copy
# corpus.nonstop <- tm_map(corpus.nonstop, removeWords, stopwords("english"))
# dtm.nonstop <- DocumentTermMatrix(corpus.nonstop)
# nterms.nonstop <- dtm.nonstop$ncol
# freq.nonstop <- colSums(as.matrix(dtm.nonstop))
# hist(mostFrequentTerms(dtm.nonstop,10))
# hist(mostFrequentTerms(dtm.nonstop,100))
# barplot(mostFrequentTerms(dtm.nonstop,10))  
# barplot(mostFrequentTerms(dtm,10))

sum.top10         <- sum(mostFrequentTerms(dtm,10))
# sum.top10.nonstop <- sum(mostFrequentTerms(dtm.nonstop,10))

sum.top10/sum(freq) #perctance of words represented by top 10
# sum.top10.nonstop/sum(freq.nonstop)  #this probably isn't important

#create a Fibonacci series for xvalues
xFib <- numeric()
xFib[1] <- xFib[2] <- 1
while (max(xFib) < nterms) { xFib <- c(xFib, sum(tail(xFib,2)))}
xFib <- xFib[c(-1,(-length(xFib)))]; #first 2 values are identical, last is out of bounds

ydata <- numeric()
for (i in seq_along(xFib))  { 
    ydata[i] <- sum(mostFrequentTerms(dtm,xFib[i]))/sum(freq)
}
plot(xFib,ydata, type="l", xlab="Terms (decreasing freq)", ylab="Coverage")
abline(h=.5, col="red")
abline(h=.9, col="red")

#trial and error (and looking at the graph)
sum(mostFrequentTerms(dtm,350))/sum(freq) #  ~ 50%
sum(mostFrequentTerms(dtm,13000))/sum(freq) # ~ 90%

#abline(v=100, col="red")

#table(freq)
#plot(table(freq))
plot(table(round(log(freq),2)))
# plot(table(round(log(freq.nonstop),2)))

#count word lengths
hist(str_length(names(freq)), xlim=c(0,20), breaks=100)

#top 10 7 letter words
freq7 <- mostFrequentTerms(dtm,1000)  #assuming there are at least 10 in here
freq7 <- freq7[str_length(names(freq7))==7]
freq7.top10 <- tail(freq7,10)
par(las=2)
barplot(tail(freq7,15))
par(las=0)

profane <- c (findTerms(dtm, "fuck")
             ,findTerms(dtm, "cock.*suck")
             ,findTerms(dtm, "cunt")
             ,findTerms(dtm, "nigge*r|nigga")
             ,findTerms(dtm, "^twat") )

corpus <- tm_map(corpus, removeWords, profane)
dtm <- DocumentTermMatrix(corpus)

# --------------- BIGRAMS -----------------------
library("RWeka")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

inspect(tdm[c(1:5,3400:3450),1:3])
