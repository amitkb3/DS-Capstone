library(tm)
library(stringr)
library(RWeka)

prepCorpus <- function( corpus ) {
    corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^[:print:]]", " ", x))) # remove unprintable characters
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, content_transformer(removeNumbers))
    
    corpus <- tm_map(corpus, content_transformer(function(x) gsub("(.*)", " \\1 ", x))) #remove tokens with no vowels
    corpus <- tm_map(corpus, content_transformer(function(x) gsub(" [^aeiouy]* ", " ", x))) #remove tokens with no vowels
    corpus <- tm_map(corpus, content_transformer(function(x) gsub(" [[:punct:]]+", " ", x))) # remove token leading punctuation
    corpus <- tm_map(corpus, content_transformer(function(x) gsub("([a-z])'([a-z])", "\\1<CONTRACT>\\2", x))) # protect contractions
    corpus <- tm_map(corpus, content_transformer(function(x) gsub("'", " ", x))) # remove single quotes
    corpus <- tm_map(corpus, content_transformer(function(x) gsub("<CONTRACT>", "'", x))) # restore apostrophe
    
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("[{}\\(\\)<>\\[]|\\]+", " ", x))) # remove parens
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("\r|\n", " ", x))) # remove returns
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("(\\w)\\s*[?!.]+", " <SB> ", x))) # Add Sentence breaks
    # 
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("n't", " not", x))) # expand "not"
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("it's", "it is", x))) # expand "it's"  -- Note: Expanding 's leads to possessive inflection screwups
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("i'm", "i am", x))) # expand "I am"
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("'re", " are", x))) # expand "are"
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("'ll", " will", x))) # expand "will"
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("\"", " ", x))) # remove quotes
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("[#@]\\w+", " ", x))) # remove hashtags
    # corpus <- tm_map(corpus, content_transformer(function(x) gsub("[&:;$*,.!'-_@#~]+", " ", x))) # 
    #
    
    corpus <- tm_map(corpus, content_transformer(stripWhitespace))
    corpus
}

predictNext <- function( str) {
    grams <- unlist(str_split(str, "\\s+"))
    n <- length(grams)
    
    guess = NULL
    while (length(guess) < 1) { 
        ss <- sprintf("^%s\\s", paste(grams, collapse="\\s"))
        hits <- findTerms(ngram[[n+1]], ss)
        if ((length(hits) >0)) { 
            best <- names(hits)[which.max(hits)] 
            guess <- gsub(".*\\s(\\S+)?$", "\\1", best)  #strip off last word for guess
        } else {
            grams <- grams[-1]
            n <- length(grams)
            if (length(grams) < 1) { guess <- "the" }
        }
    }
    guess
}

mostFrequentTerms <- function (x.tdm, n=6) {
    freq <- rowSums(as.matrix(x.tdm))
    ord <- order(freq)
    freq[rev(tail(ord, n))]
} 

findTerms <- function (x.tdm, pattern) {
    freq <- rowSums(as.matrix(x.tdm))
    terms <- names(freq)
    freq[grep(pattern, terms)]
}

findTermRank <- function (x.tdm, pattern, exact=FALSE) {
    if (exact) pattern <- gsub("(.*)", "^\\1$", pattern)   #exact matches must match full string
    freq <- rowSums(as.matrix(x.tdm))
    ranks <- names(freq[rev(order(freq))])
    grep(pattern, ranks )
}

getRankedWord <- function (x.tdm, ranks) {
    freq <- rowSums(as.matrix(x.tdm))
    ranked.list <- names(freq[rev(order(freq))])
    ranked.list[ranks]
}

predictBigram <- function (p, w, n=1) {
    # p is a square probability matrix with identically named rows and columns that must contain word (index or name)
    rownames(p)[head(rev(order(p[w,])), n)]    
}

predictNgram <- function (p, w, n=1 ) {
    p.ix <- head( rev(order(p[w,]) ),  n)
    colnames(p)[p.ix]
}

predictNgram.p <- function (p, w, n=1 ) {
    p.ix <- head( rev(order(p[w,]) ),  n)
    p[w, p.ix]
}

predictNgram.backoff <- function (ptms, w, n=1, ptm.squashed=FALSE ) {
    #predicts an ngram using full or squashed PTMs.   w should be a tokenized vector (use tokenize(txt))
    NMAX <- 3  #maximum length of predictor string 
    words <- tail(w, NMAX)
    
    p.ord <- length(words)
    
    str <- paste(words, collapse=" ")
    r.ix <- which(str == rownames(ptms[[p.ord]]))
    if (p.ord == 1 & length(r.ix) <= 0 )  r.ix <- 8  #"guess anything"for" -- will result in "the"
    
    while( length(r.ix) <= 0 ) {
        p.ord <- p.ord - 1
        words <- words[-1]
        
        str <- paste(words, collapse=" ")
        r.ix <- which(str == rownames(ptms[[p.ord]]))       
        if (p.ord == 1 & length(r.ix) <= 0 )  r.ix <- 8  #"guess anything"for" -- will result in "the"
    } 
    
    
    if (ptm.squashed) {
        p.row <- unlist(ptms[[p.ord]][r.ix, ] )
        output <- p.row[1:n]
    } else {
        p.row <- ptms[[p.ord]][r.ix,]    
        p.ix <- head( rev(order(p.row)),  n)
        output <- p.row[p.ix]
    }
    output
}

tokenize <- function ( txt ) {
    txt <- gsub("[.,;:()?![\"]+", " ", txt)
    txt.corpus <- Corpus(VectorSource(txt))
    txt.corpus <- prepCorpus(txt.corpus)
    #txt.tdm <- TermDocumentMatrix(txt.corpus)
    
    words.all <- character()
    for(i in 1:length(txt.corpus)) {
        words <- unlist(str_split(as.character(txt.corpus[[i]]), " "))
        words <- words[-c( 1, length(words))]  #first and last are null
        words.all <- c(words.all, words)
    }
    words.all
}

plotColorText <- function(x,y, x.start={x}, v.txt, col={rep("black", length(v.txt))}, font={rep(1,length(v.txt))} ) {
    x.pos <- x
    y.pos <- y
    x.space <- strwidth(" ")*1.5
    
    for( i in 1:length(v.txt) ) {
        x.dist <- strwidth(v.txt[i]) + x.space
        if ( (x.pos + x.dist) > 1 ) { y.pos <- y.pos - strheight(v.txt[i])*1.5 ; x.pos <- x.start }  #wrap
        text( x.pos, y.pos, v.txt[i], col=col[i], font=font[i], pos=4)
        x.pos <- x.pos + x.dist
    }
    c(x.pos, y.pos)
}

findCoverageX <- function( tdm, pct ) {
    freq <- rowSums(as.matrix(tdm))
    x0 <- 0
    xi <- as.integer(.2 * length(freq))  # initialize 20% steps
    c <- sum(mostFrequentTerms(tdm, x0)) / sum(freq)
    pos <- TRUE
    while  (c < pct-.01 || c > pct+.01 ) {
        if (c < pct ) { x0 <- x0+xi ; pos <- TRUE
        } else        { 
            if (pos == TRUE) { x0 <- x0 - xi}
            xi <- as.integer(xi/2) 
            pos <- FALSE
        } 
        c <- sum(mostFrequentTerms(tdm, x0+xi)) / sum(freq)
    }
    x0+xi
}

eda.by.ngram <- function (tdm) {
    
    nterms <- tdm$nrow
    mid <- as.integer(nterms/2)
    beg.mid.end <- c(1:5,  seq(mid-4,mid), seq(nterms-4,nterms))
    inspect(tdm[ beg.mid.end,1:3 ])  
    
    cat("Number of terms: ", nterms, "\n") #raw term count
    
    freq <- rowSums(as.matrix(tdm))
    #hist(freq)            # the first term dominates
    freq[which.max(freq)]  #list the most frequent term
    
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
    plot( c(xFib, length(freq)), c(ydata,1), type="l", xlab="Terms (decreasing freq)", ylab="Coverage")
    abline(h=.5, col="red")
    abline(h=.9, col="red")
    
    #trial and error (and looking at the graph)
    cover.50 <- findCoverageX (tdm, .5)
    cover.90 <- findCoverageX (tdm, .9)

    abline(v=cover.50, col="red", lty=3)
    abline(v=cover.90, col="red", lty=3)
    
    cover.50
    cover.90
    
    #table(freq)
    #plot(table(freq))
    plot(table(round(log(freq),2)))
    
    # plot(table(round(log(freq.nonstop),2)))
    
    #count word lengths
    hist(str_length(names(freq)), xlim=c(0,20), breaks=100)
    mode <- names(sort(-table(str_length(names(freq)))))[1] #calculates the mode
    
    #top 10 of highest mode letter words
    freqN <- mostFrequentTerms(tdm,10000)  #assuming there are at least 10 in here
    freqN <- freqN[str_length(names(freqN)) == mode]
    
    freqN.top10 <- tail(freqN,10)
    par.mar <- par()$mar
    par(las=2,mar=c(9,4,4,2))
    barplot(tail(freqN,15))
    par(las=0,mar=par.mar)
    
    wc <- sum(freq)
    n.unique <- sum(freq[freq == 1])
    p.unique <- n.unique/wc          ; cat("percent of terms appearing 1 time : ", p.unique,"\n")
    n.twice <- sum(freq[freq <= 2])
    p.twice <- n.twice/wc            ; cat("percent of terms appearing 2 times: ", p.twice,"\n")
    n.five <- sum(freq[freq <= 5]) 
    p.five <- n.five/wc              ; cat("percent of terms appearing 5 times: ", p.five,"\n")
    cat( sprintf("50th & 90th percentile words (%d,%d):\n", cover.50, cover.90))
    cover.50.freq <- head(mostFrequentTerms(tdm,cover.50),1)  ; print(cover.50.freq)
    cover.90.freq <- head(mostFrequentTerms(tdm,cover.90),1)  ; print(cover.90.freq) 
    
    
    p.most <- sum(mostFrequentTerms(tdm,1))/wc #
    
}


