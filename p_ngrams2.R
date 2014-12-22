library(pryr)
library(slam)

log.df <- data.frame(log=character(), time.inc=numeric(), time.cum=numeric() , time.stamp=character() )
ptime0 <- ptime1 <- proc.time()
wlog <- function(x) {
    ptime2 <- proc.time()
    inc <- (ptime2-ptime1)[3]
    cum <- (ptime2-ptime0)[3]
    ptime1 <<- ptime2
    line <- data.frame( x, inc, cum, date() )
    log.df <<- rbind( log.df, line)
    print(line)
}
wlog("Restarting log")

probTermMatrix <- function ( ng, tdm.ngram , tdm.nm1gram, tdm.1gram, Tmax=1000) {
        
###    nm1.freq <- rowSums(as.matrix(tdm.nm1gram))
###    nm1.ix <- rev(tail(order(nm1.freq), Tmax))
        
    #counts <- simple_triplet_zero_matrix( nrow=Tmax, ncol=Tmax)  #was class matrix
    counts <- matrix( 0, nrow=Tmax, ncol=Tmax)  #was class matrix
    Rterms <- names(mostFrequentTerms(tdm.nm1gram,Tmax))
    Cterms <- names(mostFrequentTerms(tdm.1gram,Tmax))
    #counts[1:10,1:10]
    
    n.ngrams <- nTerms(tdm.ngram)
    ngrams <- mostFrequentTerms(tdm.ngram, n.ngrams)   # named list of ngrams in ranked order
    
    dropped <- character()
    drop <- 0
    for (i in 1:n.ngrams) {
        if (i %% 10000 == 0) cat ("... counting ", ng, "-grams ", i, "\n")
        #    w <- numeric()
        i.ngram <- names(ngrams[i])
        word <- unlist( str_split(i.ngram, " ") )
        i.nm1gram <- paste(word[1:ng-1], collapse=" ")
        r.ix <- which(Rterms == i.nm1gram)
        c.ix <- which(Cterms == word[ng])
        
        if (length(r.ix) == 0)  r.ix <- Tmax + 1    #dumb workaround
        if (length(c.ix) == 0)  c.ix <- Tmax + 1   #dumb workaround
        if ( r.ix <= Tmax & c.ix <= Tmax) {
            counts[r.ix,c.ix] <-  ngrams[i]      #if all values are found
        } else {
            drop <- drop + 1; dropped <- c(dropped, i.ngram)# cat("dropping:", names(ngrams[i]), ":", drop, "\n")
        }
    }
    wlog(sprintf("... %d ngrams were dropped because their terms were out of range(%f%%)", drop, 100*drop/n.ngrams))
    
    #raw probabilities
    # p.word <- counts/nm1.freq[nm1.ix]
    # rownames(p.word) <- Rterms
    # colnames(p.word) <- Cterms
    
    #recalculate probabilities with add-1 smoothing
    nm1.freq <- mostFrequentTerms(tdm.nm1gram,Tmax)
    #nm1.freq.mx <- matrix( rep(nm1.freq, ncol(counts)), ncol=ncol(counts), byrow=FALSE)
    #p.word2 <- (matrix(counts, nrow=length(nm1.freq)) + 1)/(nm1.freq +Tmax)  #if we use library (slam)
    p.word2 <- (counts + 1)/(nm1.freq +Tmax)
    rownames(p.word2) <- Rterms
    colnames(p.word2) <- Cterms
    
    # adjusted counts
    #counts2 <- p.word2 * freq[nm1.ix]
    
    rm(counts)
    p.word2
}
probTermMatrix.s <- function ( ng, tdm.ngram , tdm.nm1gram, tdm.1gram, Tmax=1000, n=1) {
    # n is the number of top level probabilities to return per term
    
    ###    nm1.freq <- rowSums(as.matrix(tdm.nm1gram))
    ###    nm1.ix <- rev(tail(order(nm1.freq), Tmax))
    
    #counts <- simple_triplet_zero_matrix( nrow=Tmax, ncol=Tmax)  #was class matrix
    counts <- matrix( 0, nrow=Tmax, ncol=Tmax)  #was class matrix
    Rterms <- names(mostFrequentTerms(tdm.nm1gram,Tmax))
    Cterms <- names(mostFrequentTerms(tdm.1gram,Tmax))
    #counts[1:10,1:10]
    
    n.ngrams <- nTerms(tdm.ngram)
    ngrams <- mostFrequentTerms(tdm.ngram, n.ngrams)   # named list of ngrams in ranked order
    
    dropped <- character()
    drop <- 0
    for (i in 1:n.ngrams) {
        if (i %% 10000 == 0) cat ("... counting ", ng, "-grams ", i, "\n")
        #    w <- numeric()
        i.ngram <- names(ngrams[i])
        word <- unlist( str_split(i.ngram, " ") )
        i.nm1gram <- paste(word[1:ng-1], collapse=" ")
        r.ix <- which(Rterms == i.nm1gram)
        c.ix <- which(Cterms == word[ng])
        
        if (length(r.ix) == 0)  r.ix <- Tmax + 1    #dumb workaround
        if (length(c.ix) == 0)  c.ix <- Tmax + 1   #dumb workaround
        if ( r.ix <= Tmax & c.ix <= Tmax) {
            counts[r.ix,c.ix] <-  ngrams[i]      #if all values are found
        } else {
            drop <- drop + 1; dropped <- c(dropped, i.ngram)# cat("dropping:", names(ngrams[i]), ":", drop, "\n")
        }
    }
    wlog(sprintf("... %d ngrams were dropped because their terms were out of range(%f%%)", drop, 100*drop/n.ngrams))
    
    #raw probabilities
    # p.word <- counts/nm1.freq[nm1.ix]
    # rownames(p.word) <- Rterms
    # colnames(p.word) <- Cterms
    
    #recalculate probabilities with add-1 smoothing
    nm1.freq <- mostFrequentTerms(tdm.nm1gram,Tmax)
    
    p.df <- data.frame()
    for (i in 1:nrow(counts)) {
        c.row <- counts[i, ]
        p.row <- (c.row+1)/(nm1.freq[i]+Tmax)
        best <- head(rev(order(p.row)), n)
        best.p <- p.row[best]
        names(best.p) <- Cterms[best] 
        p.df[i,1] <- list(list(best.p))
    }

    rownames(p.df) <- Rterms
    
    rm(counts)
    p.df
}


squashPTM <- function (p, n=1) {
    #squash down a Probabilty Term Matrix into just the most probable terms
    #squash <- list(list())
    squash <- data.frame()
    
    for( i in 1:nrow(p)) {
        best <- head(rev(order(p[i, ])), n)
        best.p <- p[i, best]
        if (n == 1) { names(best.p) <- colnames(p)[best] }  #only need the because R acts differently for n=1
        squash[i,1] <- list(list(best.p))
        #squash[i] <- list(list(best.p, rownames(p)[i] ))
    }
    rownames(squash) <- rownames(p)
    squash
}

scoreText <- function (ptm, txt, ptm.squashed={ object_size(ptm) < 100e6 }) {
    #handle string input (convert to corpus)
    if( class(txt)[1] == "character" ) {
        txt.corp <- Corpus(VectorSource(txt), readerControl = list(language=LANG))
        txt <- txt.corp[[1]]
    }
    
    too.short <- numeric()   # list of indexes of phrases that are too short (one word)
    scores <- list()
    for(j in 1:length(txt$content)) {
        
        words <- tokenize(txt$content[j])
        
        n.words <- length(words)
        if (n.words <= 1)  too.short <- c(too.short, j)
        check <- logical(n.words -1);  
        for( i in 1:(n.words-1)) {
            next.word <- names(predictNgram.backoff(ptm, words[1:i], n=1, ptm.squashed=ptm.squashed))
            check[i] <- next.word == words[i+1]
            judge <- ifelse( check[i], "Correct", "False  ")
            score <- sum(check)/(i)
            
        }
        names(check) <- words[-1]    # didn't check the first word
        scores[j] <- list(check)        
    }
    if (length(too.short > 0))  scores <- scores[-too.short]
    scores
}

plotPhraseScore <- function (ptm, phrase, ptm.squashed={ object_size(ptm) < 100e6 }) {
    par.orig <- par(mfrow=c(2,1), mar=c(2,2,2,2), las=1)
    
    score <- scoreText(ptm, phrase, ptm.squashed=ptm.squashed) [[1]]
    score.txt <- sprintf("Score: %d/%d => %5.1f%%", sum(score), length(score), 100*sum(score)/length(score))
    
    stxt.col <- rep("red", length(score))
    stxt.col[score] <- "dark green"
    stxt.font <- rep(1, length(score))
    stxt.font[score] <- 2
    first.word <- tokenize(phrase)[1]
    
    plot.new()
    xypos <- plotColorText(0,.9, 0, first.word, col="grey")
    xypos <- plotColorText(xypos[1], xypos[2], 0, names(score), col=stxt.col, font=stxt.font)
    xypos <- plotColorText(0, xypos[2]-.1, 0, score.txt)
    
    n.guess <- 5
    guess <- predictNgram.backoff(pmat.s, tokenize(phrase), n.guess, ptm.squashed=TRUE)
    par(mar=c(4,7,2,4), las=1)
    barplot( rev(guess), horiz=TRUE, main="Probability that the next word is ___" )
    
    par(mfrow=par.orig$mfrow, mar=par.orig$mar, las=par.orig$las)
}

    
