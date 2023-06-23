library(readr)
library(readxl)
library(stringr)

fileNames <- list.files(path="texts", pattern="*.tsv")

# number of tokens, words and sentences

df <- data.frame()
DF <- data.frame()

for (i in 1:length(fileNames))
{
  cat(fileNames[i], "\n")

  corpus <- read_delim(paste0("texts/", fileNames[i]), show_col_types = FALSE)

  df0 <- data.frame(source        = fileNames[i],
                       nTokens    = nrow(corpus),
                    percTokens    = NA,
                       nWords     = length(str_count(corpus$token[corpus$upos!="PUNCT"])),
                    percWords     = NA,
                       nSentences = length(unique(corpus$sentence_id)),
                    percSentences = NA)

  df <- rbind(df, df0)
  corpus$source <- rep(fileNames[i],nrow(corpus)) ###
  DF <- rbind(DF, corpus)
}

cat("\n")

round2 <- function(x, n=0)
{
  scale<-10^n
  return(trunc(x*scale+sign(x)*0.5)/scale)
}

df$percTokens    <- round2((df$nToken   /sum(df$nToken   )) * 100)
df$percWords     <- round2((df$nWord    /sum(df$nWord    )) * 100)
df$percSentences <- round2((df$nSentence/sum(df$nSentence)) * 100)

print(df)

cat("\n")
cat("Number of tokens   : ", sum(df$nTokens   ), "\n")
cat("Number of words    : ", sum(df$nWords    ), "\n")
cat("Number of sentences: ", sum(df$nSentences), "\n")

# get frequencies of POS tags in corpus

print(table(DF$upos))
