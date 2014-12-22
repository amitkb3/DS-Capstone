library(tm)

LANG <- "en_US"
ROOT_DIR <- "C:/NO_BACKUP/final"
OUT_DIR <- "C:/NO_BACKUP/small01"
fname <- sprintf("%s/%s/%s.blogs.txt", ROOT_DIR, LANG, LANG)
info <- file.info(fname)
info$size/(1024)^2

SAMPLE_SIZE <- .00010  # as a proportion of overall text size

ptime0 <- proc.time()
SRC <- c("blogs", "news", "twitter")
files <- list()
stream <- character()
info <- data.frame(name=NULL, size=NULL, lines=NULL)
for (i in 1:3) {
    fname <- sprintf("%s/%s/%s.%s.txt", ROOT_DIR, LANG, LANG, SRC[i])
    sname <- sprintf("%s/%s/%s.%s.txt",  OUT_DIR, LANG, LANG, SRC[i])
    cat("parsing ",fname,"...\n")
    con <- file(fname, "rb")
    f <- readLines(con, n=-1)
    close(con)
  
    set.seed(525 + 10000*SAMPLE_SIZE)
    
    f.small <- f[(1:length(f)) * rbinom(length(f), 1, SAMPLE_SIZE) > 0]  #random sample of 1%
    writeLines( f.small, con=sname)
    
    stream <- c(stream, f)
    file.df <- data.frame(name=fname, size=file.info(fname)$size, lines=length(f))
    info <- rbind(info, file.df)
    files[i] <- list(f)

}
rm(stream)
ptime1 <- proc.time()
ptime1

# based on warning add one of the complaint lines into twitter (line 167155)
#f.small <- c(f[167154:167156], f.small)
#writeLines( f.small, con=sname)

con <- file(sname, "wb")
writeLines( f.small, con=con)
close(con)


rm(f)