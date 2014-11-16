mostFrequentTerms <- function (x.tdm, n=6) {
    freq <- rowSums(as.matrix(x.tdm))
    ord <- order(freq)
    freq[tail(ord, n)]
} 

findTerms <- function (x.tdm, pattern) {
    freq <- rowSums(as.matrix(x.tdm))
    terms <- names(freq)
    freq[grep(pattern, terms)]
}


library(tm)
library(stringr)
library(RWeka)

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
corpus <- tm_map(corpus, content_transformer(function(x) gsub("^[^a-z]", " ", x))) # remove starting punctuation
corpus <- tm_map(corpus, content_transformer(function(x) gsub("n't", " not", x))) # expand "not"
corpus <- tm_map(corpus, content_transformer(function(x) gsub("'s", " is", x))) # expand "is"
corpus <- tm_map(corpus, content_transformer(function(x) gsub("i'm", "i am", x))) # expand "I am"
corpus <- tm_map(corpus, content_transformer(function(x) gsub("'re", " are", x))) # expand "are"
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[\\/_]|-", " ", x))) # remove some punctuation
corpus <- tm_map(corpus, content_transformer(stripWhitespace))

tdm <- TermDocumentMatrix(corpus)

profane <- c (findTerms(tdm, "fuck")
              ,findTerms(tdm, "cock.*suck")
              ,findTerms(tdm, "cunt")
              ,findTerms(tdm, "nigge*r|nigga")
              ,findTerms(tdm, "^twat") )

corpus <- tm_map(corpus, removeWords, profane)
tdm <- TermDocumentMatrix(corpus)
tdm
nterms <- tdm$nrow
inspect(tdm[ c(1:3,seq(nterms-4,nterms)),1:3 ])  

nterms #raw term count

freq <- rowSums(as.matrix(tdm))
hist(freq)
freq[which.max(freq)]

mostFrequentTerms(tdm,10)
freq.top100 <- mostFrequentTerms(tdm,100)
hist(freq.top100)

# corpus.nonstop <- corpus  #remove stopwords in a copy
# corpus.nonstop <- tm_map(corpus.nonstop, removeWords, stopwords("english"))
# tdm.nonstop <- DocumentTermMatrix(corpus.nonstop)
# nterms.nonstop <- tdm.nonstop$ncol
# freq.nonstop <- colSums(as.matrix(tdm.nonstop))
# hist(mostFrequentTerms(tdm.nonstop,10))
# hist(mostFrequentTerms(tdm.nonstop,100))
# barplot(mostFrequentTerms(tdm.nonstop,10))  
# barplot(mostFrequentTerms(tdm,10))

sum.top10         <- sum(mostFrequentTerms(tdm,10))
# sum.top10.nonstop <- sum(mostFrequentTerms(tdm.nonstop,10))

sum.top10/sum(freq) #perctance of words represented by top 10
# sum.top10.nonstop/sum(freq.nonstop)  #this probably isn't important

#create a Fibonacci series for xvalues
xFib <- numeric()
xFib[1] <- xFib[2] <- 1
while (max(xFib) < nterms) { xFib <- c(xFib, sum(tail(xFib,2)))}
xFib <- xFib[c(-1,(-length(xFib)))]; #first 2 values are identical, last is out of bounds

ydata <- numeric()
for (i in seq_along(xFib))  { 
    ydata[i] <- sum(mostFrequentTerms(tdm,xFib[i]))/sum(freq)
}
plot(xFib,ydata, type="l", xlab="Terms (decreasing freq)", ylab="Coverage")
abline(h=.5, col="red")
abline(h=.9, col="red")

#trial and error (and looking at the graph)
sum(mostFrequentTerms(tdm,330))/sum(freq) #  ~ 50%
sum(mostFrequentTerms(tdm,11200))/sum(freq) # ~ 90%
abline(v=330, col="red", lty=3)
abline(v=11200, col="red", lty=3)

#table(freq)
#plot(table(freq))
plot(table(round(log(freq),2)))
# plot(table(round(log(freq.nonstop),2)))

#count word lengths
hist(str_length(names(freq)), xlim=c(0,20), breaks=100)

#top 10 7 letter words
freq7 <- mostFrequentTerms(tdm,1000)  #assuming there are at least 10 in here
freq7 <- freq7[str_length(names(freq7))==7]
freq7.top10 <- tail(freq7,10)
par(las=2)
barplot(tail(freq7,15))
par(las=0)

tdm.1 <- tdm

# 6% accuracy if we guess "the" every time
sum(mostFrequentTerms(tdm,1))/sum(freq) #


# --------------- BIGRAMS -----------------------
ptime20 <- proc.time()

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
nterms <- tdm$nrow
inspect(tdm[c(1:5,3400:3450),1:3])

freq <- rowSums(as.matrix(tdm))
hist(freq)
freq[which.max(freq)]

mostFrequentTerms(tdm,10)
freq.top100 <- mostFrequentTerms(tdm,100)
hist(freq.top100)

sum.top10         <- sum(mostFrequentTerms(tdm,10))
sum.top10/sum(freq) #perctance of words represented by top 10

#create a Fibonacci series for xvalues
xFib <- numeric()
xFib[1] <- xFib[2] <- 1
while (max(xFib) < nterms) { xFib <- c(xFib, sum(tail(xFib,2)))}
xFib <- xFib[c(-1,(-length(xFib)))]; #first 2 values are identical, last is out of bounds

ydata <- numeric()
for (i in seq_along(xFib))  { 
    ydata[i] <- sum(mostFrequentTerms(tdm,xFib[i]))/sum(freq)
}

plot( c(xFib,nterms), c(ydata,1), type="l", xlab="Terms (decreasing freq)", ylab="Coverage")
abline(h=.5, col="red")
abline(h=.9, col="red")

#trial and error (and looking at the graph)
sum(mostFrequentTerms(tdm,28500))/sum(freq) #  ~ 50%
sum(mostFrequentTerms(tdm,342000))/sum(freq) # ~ 90%
abline(v=28500, col="red", lty=3)
abline(v=342000, col="red", lty=3)

freqN <- mostFrequentTerms(tdm,1000)  #assuming there are at least 10 in here
freqN <- freqN[str_length(names(freqN))==10]
freqN.top10 <- tail(freqN,10)
par(las=2)
barplot(tail(freqN,15))
par(las=0)

ptime21 <- proc.time()
ptime21-ptime20

tdm.2 <- tdm

# 0.4% accuracy if we guess "of the" every time
mostFrequentTerms(tdm,1)
sum(mostFrequentTerms(tdm,1))/sum(freq) #
# --------------- TRIGRAMS -----------------------
ptime30 <- proc.time()

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
nterms <- tdm$nrow
inspect(tdm[c(1:5,3400:3450),1:3])

freq <- rowSums(as.matrix(tdm))
hist(freq)
freq[which.max(freq)]

mostFrequentTerms(tdm,10)
freq.top100 <- mostFrequentTerms(tdm,100)
hist(freq.top100)

sum.top10         <- sum(mostFrequentTerms(tdm,10))
sum.top10/sum(freq) #perctance of words represented by top 10

#create a Fibonacci series for xvalues
xFib <- numeric()
xFib[1] <- xFib[2] <- 1
while (max(xFib) < nterms) { xFib <- c(xFib, sum(tail(xFib,2)))}
xFib <- xFib[c(-1,(-length(xFib)))]; #first 2 values are identical, last is out of bounds

ydata <- numeric()
for (i in seq_along(xFib))  { 
    ydata[i] <- sum(mostFrequentTerms(tdm,xFib[i]))/sum(freq)
}

plot( c(xFib,nterms), c(ydata,1), type="l", xlab="Terms (decreasing freq)", ylab="Coverage")
abline(h=.5, col="red")
abline(h=.9, col="red")

#trial and error (and looking at the graph)
sum(mostFrequentTerms(tdm,310000))/sum(freq) #  ~ 50%
sum(mostFrequentTerms(tdm,700000))/sum(freq) # ~ 90%
abline(v=310000, col="red", lty=3)
abline(v=700000, col="red", lty=3)

plot(table(round(log(freq),2)))
# plot(table(round(log(freq.nonstop),2)))

#count word lengths
hist(str_length(names(freq)), xlim=c(0,20), breaks=100)

freqN <- mostFrequentTerms(tdm,100000)  #assuming there are at least 10 in here
freqN <- freqN[str_length(names(freqN))==14]
freqN.top10 <- tail(freqN,10)
par.mar <- par()$mar
par(las=2,mar=c(7,4,4,2))
barplot(tail(freqN,15))
par(las=0, mar=par.mar)

ptime31 <- proc.time()
ptime31-ptime30

tdm.3 <- tdm

# 0.04% accuracy if we guess "i do not" every time
mostFrequentTerms(tdm,1)
sum(mostFrequentTerms(tdm,1))/sum(freq) #

# ------------- PLOT ngram effectiveness
# ( # of unique n-grams) / (# of n-grams) versus n-gram key length

tdm <- tdm.2
freq <- rowSums(as.matrix(tdm))

ptime40 <- ptime()
t1Counts <- numeric()  #number of ngrams for the first token in each ngram
it1 <- 0; #counter variable for the first tokens 
t1.last <- "?"
for(i in 1:length(freq)) {
    tokens <- unlist(str_split(names(freq[i]), " "))
    t1 <- tokens[1]; tokens <- tokens [-1]  ;  
    if (i %% 100000 == 0) cat("...", i, " of ", length(freq), " (", t1, ")\n")
    if (t1 != t1.last) { it1 <- it1 + 1; t1Counts[it1] = 0}
    t1Counts[it1] = t1Counts[it1] + 1
    t1.last <- t1
}
eff <- sum(t1Counts==1) / length(freq) 

ptime41 <- ptime()
ptime41-ptimer40
