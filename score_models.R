MAX_NGRAMS <- 100000
MAX_TERMS  <- 10000

REGEN_CORPUS <- FALSE
REGEN_TDM <- FALSE
REGEN_INDEX <- FALSE
REGEN_PROB <-FALSE

library(tm)
library(stringr)
library(RWeka)
library(ggplot2)
source("text_utils.R")
source("p_ngrams2.R")  #cleanup required ... make this ngram_utils

LANG <- "en_US"
#ROOT_DIR <- "C:/NO_BACKUP/final"
ROOT_DIR <- "C:/NO_BACKUP/small1"
TEST_DIR <- "C:/NO_BACKUP/small01"

ptm.squashed.file <- paste(ROOT_DIR, "ptmSquash.RData", sep="/")
load(file=ptm.squashed.file)

calcScore <- function (x) {
    n.phrase <- length(x)
    score.phrase <- matrix( rep(NA, 2 * n.phrase), ncol=2, byrow=TRUE )
    total.right <- sum( sapply(x, sum))
    total.tries <- sum( sapply(x, length))
    c(n.phrase, total.right, total.tries, total.right/total.tries)
}
calcScore.r <- function (x) { round(calcScore(x))}


tdir <- sprintf("%s/%s", TEST_DIR, LANG)
tcorpus <- Corpus(DirSource(tdir, encoding="UTF-8"), readerControl = list(language=LANG))

for( i in length(pmat.s):1) {
    score.df <- data.frame(phrases=integer(), correct=integer(), chances=integer(), score=numeric() )
    for (j in 1:3) score.df[j, ] <- calcScore( scoreText(pmat.s, tcorpus[[j]]) )
    
    print(sprintf("%d-gram: %d correct /  %d chances = %f\n", i, sum(score.df$correct), sum(score.df$chances), sum(score.df$correct) / sum(score.df$chances)))
    pmat.s <- pmat.s[-i]
}
