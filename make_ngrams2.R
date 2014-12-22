# split from make_ngrams.R  when increasing order of N-grams
MAX_NGRAMS <- 100000
MAX_TERMS  <- 15000

REGEN_CORPUS <- FALSE
REGEN_TDM <- FALSE
REGEN_INDEX <- FALSE
REGEN_PROB <-FALSE
RUN_TEST <- FALSE
RUN_SCORE <- TRUE

NGRAM_ORDER <- 6
     
library(tm)
library(stringr)
library(RWeka)
library(ggplot2)
source("text_utils.R")
source("p_ngrams2.R")  #cleanup required ... make this ngram_utils

LANG <- "en_US"
#ROOT_DIR <- "C:/NO_BACKUP/final"
ROOT_DIR <- "C:/NO_BACKUP/small10"
TEST_DIR <- "C:/NO_BACKUP/small01"

wlog(sprintf("MAX_NGRAMS=%d, MAX_TERMS=%d, NGRAM_ORDER=%d, ROOT_DIR=%s", MAX_NGRAMS, MAX_TERMS, NGRAM_ORDER, ROOT_DIR))

corpusFile <- paste(ROOT_DIR, "corpus_clean.RData", sep="/")

if (REGEN_CORPUS == TRUE) {

fdir <- sprintf("%s/%s", ROOT_DIR, LANG)
corpus <- Corpus(DirSource(fdir, encoding="UTF-8"), readerControl = list(language=LANG))

corpus <- prepCorpus(corpus)

profane <- character()

# corpus <- tm_map(corpus, removeWords, profane)

wlog("...read and cleaned corpus")


save(file=corpusFile, corpus)   #load(file=corpusFile)
} else {
    load(file=corpusFile)
}

process_1grams <- function (x.tdm) {
    
    freq <- rowSums(as.matrix(tdm))
#     rare.words <- names(freq[freq==1])
#     writeLines(rare.words, paste(ROOT_DIR, "rare_words.txt", sep="/")) 
#     wlog(sprintf("%d unique words found (written to rare_words.txt)", length(rare.words)))
    
    #GLOBAL
    profane <<- c (findTerms(tdm, "fuck")
                  ,findTerms(tdm, "cock.*suck")
                  ,findTerms(tdm, "cunt")
                  ,findTerms(tdm, "nigge*r|nigga")
                  ,findTerms(tdm, "^twat") )
    wlog(sprintf("%d  profane words found", length(profane)))
}

tdms <- list()
if(REGEN_TDM == TRUE) {
    cat("attempting ",NGRAM_ORDER,"N-grams... ")
    
    for (i in 1:NGRAM_ORDER) { 
        
        nTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = i, max = i, delimiters=" \\r\\n\\t.,;:\"()\\[\\]?!"))  #default except for apostrophe
        bounds <- list( local = c( 2, Inf))
        tdm.control <- list(tokenize = nTokenizer, wordLengths=c(1,Inf), bounds=bounds )
        tdm <- TermDocumentMatrix(corpus, control = tdm.control)
        wlog(sprintf("create %d-gram TDM... ", i))
                
        if (i == 1 ) process_1grams(tdm)
        
        ngFile <- sprintf("%s/%d-grams.RData", ROOT_DIR, i)
        save(file=ngFile, tdm)
        rm(tdm)
        gc()
        
    }
} 

    for (i in 1:NGRAM_ORDER) {
        tdm.file <- sprintf("%s/%d-grams.RData", ROOT_DIR, i)
        load(file=tdm.file)
        tdms[i] <- list(tdm)
        rm(tdm)
    }


# 
# for (i in 2:4) {
#     rm.terms <- numeric()   #keep a list of terms to remove (substituted)
#     unk.terms <- character()  #vector of substited ngrams
#     ix <- 0
#     for (i.gram in Terms(tdms[[i]])) {
#         ix <- ix+1
#         i.gram.c <- as.matrix(tdms[[i]])[i, ]  #TDM counts for this n-gram 
#         sub <- FALSE
#         words <- unlist(str_split(i.gram, " "))
#         for (i.word in words) {
#             if (! i.word %in% known) {
#                 sub = TRUE
#                 i.gram <- gsub(i.word, "<UNK>", igram)
#             }
#         }
#         if (sub == TRUE) {
#             if (i.gram %in% unk.terms) {
#                 
#             }
#             unk.terms <- c(unk.terms, i.gram)
#         }
#     }
#     add
# }
# rm(known)

# terms.alpha <- Terms(tdms[[1]])
# toIndex <- function (x) {   #x named integer
#     words <- unlist(str_split(names(x), " "))
#     ind <- numeric()
#     for (i in 1:length(words)) {
#         ix <- which( terms.alpha == words[i]) # %in% won't work if there are repeated words
#         ind <- c(ind, ix)
#     }
#     #ind <- which (names(terms) %in% words)
#     out <- c(ind, x)
#     out
# }
# 
# fromIndex <- function (x) terms.alpha[x]  #returns the word
# 
# if (REGEN_INDEX == TRUE) {
# 
# terms.ix <- rev(order(rowSums(as.matrix(tdms[[1]]))))
# plot(mostFrequentTerms(tdms[[1]], 10000), type="l")
# save(file=paste(ROOT_DIR, "ix1.RData", sep="/"), terms.ix)
# 
# bigrams <- mostFrequentTerms(tdms[[2]], MAX_NGRAMS)
# 
# bigrams.ix <- matrix(ncol=3, nrow=length(bigrams))
# for (i in 1:length(bigrams) ) {
#     bigrams.ix[i, ] <- toIndex( bigrams[i] )
#     if (i %% 1000 == 0) cat ("... indexing bigrams ", i, "\n")
# }
# save(file=paste(ROOT_DIR, "ix2.RData", sep="/"), bigrams.ix)
# 
# trigrams <- mostFrequentTerms(tdms[[3]], MAX_NGRAMS)
# trigrams.ix <- matrix(ncol=4, nrow=length(trigrams))
# for (i in 1:length(trigrams) ) {
#     trigrams.ix[i, ] <- toIndex( trigrams[i] )
#     if (i %% 1000 == 0) cat ("... indexing trigrams ", i, "\n")
# }
# save(file=paste(ROOT_DIR, "ix3.RData", sep="/"), trigrams.ix)
# 
# Ngrams.4 <- mostFrequentTerms(tdms[[4]], MAX_NGRAMS)
# Ngrams.4.ix <- matrix(ncol=5, nrow=length(Ngrams.4))
# for (i in 1:length(Ngrams.4) ) {
#     Ngrams.4.ix[i, ] <- toIndex( Ngrams.4[i] )
#     if (i %% 1000 == 0) cat ("... indexing Ngrams.4 ", i, "\n")
# }
# save(file=paste(ROOT_DIR, "ix4.RData", sep="/"), Ngrams.4.ix)
# 

pmat.s <- list()
if (REGEN_PROB == TRUE) {
    
    ptop <- 10
    highest.ngram <- length(tdms)  #might not = NGRAM_ORDER if TDM process choked
    for (i in 2:highest.ngram) {
        ptm <- probTermMatrix.s( i, tdms[[i]], tdms[[i-1]], tdms[[1]], Tmax=MAX_TERMS, ptop) 
        pmat.s[i-1] <- list(ptm)   
    }
    
    rm (ptm)
    save(file=paste(ROOT_DIR, "ptmSquash.RData", sep="/"), pmat.s)
    
} else {
    #load(file=paste(ROOT_DIR, "pmat.RData",      sep="/"))
    load(file=paste(ROOT_DIR, "ptmSquash.RData", sep="/"))
}

date_str <- format(Sys.time(), "%y-%m-%d-%H%M%S")
log_file <- sprintf("%s/log_%s.RData", ROOT_DIR, date_str)
save(file=log_file, log.df)

if (RUN_TEST) {
    

predictNgram(pmat[[1]], "him")
predictNgram(pmat[[1]], "him",10)
round( predictNgram.p(pmat[[1]], "him",10), 4)
round( predictNgram.p(pmat[[1]], "heard",10), 4)
round( predictNgram.p(pmat[[1]], "maple",10), 4)
round( predictNgram.p(pmat[[2]], "at least",10), 4)
round( predictNgram.p(pmat[[3]], "at least the",10), 4)

predictNgram.backoff(pmat.s, tokenize("get the maple"), 10, ptm.squashed=TRUE)
predictNgram.backoff(pmat, tokenize("get the maple"), 10, ptm.squashed=FALSE)
predictNgram.backoff(pmat.s, tokenize("go get the maple"), 10, ptm.squashed=FALSE)
predictNgram.backoff(pmat.s, tokenize("you beat me to the"), 10, ptm.squashed=FALSE)
predictNgram.backoff(pmat.s, tokenize("beat me to the"), 10, ptm.squashed=FALSE)

#Quiz2
round( predictNgram.backoff(pmat, tokenize("a case of"),10), 4)
findTermRank(tdms[[1]], "^cheese$|^beer$|^soda$|^pretzels$")
findTerms(tdms[[1]], "^cheese$|^beer$|^soda$|^pretzels$")

q5 <- round( predictNgram.backoff(pmat,  tokenize("date at the"), 4000), 4)
q5[grep( "^grocery$|^mall$|^beach$|^movies$", names(q5))]
findTermRank(tdms[[1]], "^grocery$|^mall$|^beach$|^movies$")


quizGuess <- function (ptm, phrase, choices, squashed.PTM=FALSE) {
    words <- tokenize(phrase)
    ng <- length(words)
    pattern <- paste(sprintf("^%s$", unlist(str_split(choices, ","))), collapse="|")
    p <- predictNgram.backoff(ptm, words, MAX_TERMS, squashed.PTM)
    hits <- p[grep (pattern, names(p))]
    if (length(hits) <= 0) { 
        hits <- "N/A" 
    } else { 
        wins <- which(hits == max(hits))
        if ( length(wins) > 1  & (ng > 1) ) {
            choices.tie <- paste(names(hits)[wins], collapse=",")
            phrase.m1 <- paste(words[2: ng], collapse=" ")
            #p <- predictNgram.backoff(phrase.m1, MAX_TERMS)
            hits <- quizGuess( ptm, phrase.m1, choices.tie)  #recursive   (probability will be too high, but tie is broken)
        }
    }
    hits
}


#quiz 2
ptm <- pmat  ; squashed <- FALSE
#ptm <- pmat.s; squashed <- TRUE
quizGuess( ptm, "a case of", "cheese,beer,soda,pretzels", squashed.PTM=squashed )   #cheese   
quizGuess( ptm, "would mean the", "best,universe,most,world", squashed.PTM=squashed  )   #world
quizGuess( ptm, "make me the" , "smelliest,bluest,saddest,happiest", squashed.PTM=squashed  )  #happiest #no answer with small10
quizGuess( ptm, "struggling but the","defense,crowd,referees,players", squashed.PTM=squashed  ) #defense 
quizGuess( ptm, "date at the", "grocery,mall,beach,movies", squashed.PTM=squashed  )   #beach
quizGuess( ptm, "be on my", "phone,way,motorcycle,horse", squashed.PTM=squashed  )  #way
quizGuess( ptm, "in quite some", "time,week,thing,year", squashed.PTM=squashed  )   #time
quizGuess( ptm, "with his little", "ears,toes,fingers,eyes", squashed.PTM=squashed  )  #fingers
quizGuess( ptm, "faith during the", "bad,hard,sad,worse", squashed.PTM=squashed  )         #bad    
quizGuess( ptm, "you must be", "asleep,insane,insensitive,callous", squashed.PTM=squashed  )  #insane 

#quiz 2 small10: beer, world, N/A, defense, beach, way, time, fingers, bad , insane  ==== 9/10
#quiz 2 small1 : beer, world, happiest, crowd, beach, way, time, toes, bad , insane  ==== 8/10

#quiz 3
# When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd  #die
quizGuess( ptm, "live and i'd", "die,sleep,give,eat", squashed.PTM=squashed )   
# Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his #marital
quizGuess( ptm, "me about his", "marital,horticulture,spiritual,financial", squashed.PTM=squashed )
# I'd give anything to see arctic monkeys this        #weekend
quizGuess( ptm, "arctic monkeys this" , "decade,weekend,month,morning", squashed.PTM=squashed )   
# Talking to your mom has the same effect as a hug and helps reduce your   #stress
quizGuess( ptm, "helps reduce your","hunger,happiness,stress,sleepiness", squashed.PTM=squashed )   
# When you were in Holland you were like 1 inch away from me but you hadn't time to take a  #picture
quizGuess( ptm, "to take a", "minute,picture,walk,look", squashed.PTM=squashed )
# I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the  #matter
quizGuess( ptm, "to settle the", "account,matter,case,incident", squashed.PTM=squashed )
# I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each  #hand
quizGuess( ptm, "groceries in each", "arm,finger,hand,toe", squashed.PTM=squashed )
# Every inch of you is perfect from the bottom to the          #top
quizGuess( ptm, "Every inch of you is perfect from the bottom to the", "middle,top,side,center", squashed.PTM=squashed )
# I’m thankful my childhood was filled with imagination and bruises from playing    #outside
quizGuess( ptm, "bruises from playing", "daily,inside,weekly,outside", squashed.PTM=squashed )             
# I like how the same people are in almost all of Adam Sandler's           #movies
quizGuess( ptm, "of Adam Sandler's", "pictures,stories,movies,novels", squashed.PTM=squashed )  

#quiz 3 small10: give, spiritual, morning, happiness, look, case, hand, top, outside , stories  ==== 3/10
#quiz 3 small 1: give, spiritual, morning, hunger, picture, case, toe, top, weekly , pictures  ==== 2/10


calcScore <- function (x) {
    n.phrase <- length(x)
    score.phrase <- matrix( rep(NA, 2 * n.phrase), ncol=2, byrow=TRUE )
    total.right <- sum( sapply(x, sum))
    total.tries <- sum( sapply(x, length))
    c(n.phrase, total.right, total.tries, 100*total.right/total.tries)
}
calcScore.r <- function (x) { round(calcScore(x))}

stxt <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd die"
ctxt <- "what time is the party tomorrow because i love you"
ctxt2 <- "clash of clans is a good game"
plotPhraseScore( pmat.s, stxt)
plotPhraseScore( pmat.s, ctxt2)
score <- scoreText( pmat.s, ctxt2)
calcScore( score )
calcScore.r( score )

calcScore.r( scoreText( pmat.s, stxt) )

tdir <- sprintf("%s/%s", TEST_DIR, LANG)
tfile <- paste( c(tdir, "en_US.blogs.txt"), collapse="/")
ttxt <- readLines (tfile)
score <- scoreText( pmat.s, ttxt)
head(score)

library(qdap)
sentences <- sent_detect(ttxt)

tcorpus <- Corpus(DirSource(tdir, encoding="UTF-8"), readerControl = list(language=LANG))

score.1 <- scoreText(pmat.s, tcorpus[[1]])
calcScore.r( score.1 )
tcorpus[[1]]$content[92]
plotPhraseScore( pmat.s, tcorpus[[1]]$content[92])  #example phrase

score.2 <- scoreText(pmat.s, tcorpus[[2]])
calcScore.r( score.2 )
tcorpus[[2]]$content[101]
plotPhraseScore( pmat.s, tcorpus[[2]]$content[101])  #example phrase

score.3 <- scoreText(pmat.s, tcorpus[[3]])
calcScore.r( score.3 )
tcorpus[[3]]$content[229]
plotPhraseScore( pmat.s, tcorpus[[3]]$content[229])  #example phrase
}

calcScore <- function (x) {
    n.phrase <- length(x)
    score.phrase <- matrix( rep(NA, 2 * n.phrase), ncol=2, byrow=TRUE )
    total.right <- sum( sapply(x, sum))
    total.tries <- sum( sapply(x, length))
    c(n.phrase, total.right, total.tries, total.right/total.tries)
}
calcScore.r <- function (x) { round(calcScore(x))}

if (RUN_SCORE) {
    
    
    tdir <- sprintf("%s/%s", TEST_DIR, LANG)
    tcorpus <- Corpus(DirSource(tdir, encoding="UTF-8"), readerControl = list(language=LANG))
    
    pmat.tmp <- pmat.s
    for( i in length(pmat.tmp):1) {
        score.df <- data.frame(phrases=integer(), correct=integer(), chances=integer(), score=numeric() )
        for (j in 1:3) score.df[j, ] <- calcScore( scoreText(pmat.tmp, tcorpus[[j]]) )
        
        wlog(sprintf("%d-gram: %d correct /  %d chances = %f\n", i, sum(score.df$correct), sum(score.df$chances), sum(score.df$correct) / sum(score.df$chances)))
        pmat.tmp <- pmat.tmp[-i]
    }
    
}
print(log_file)
