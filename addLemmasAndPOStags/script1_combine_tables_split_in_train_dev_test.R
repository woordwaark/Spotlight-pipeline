library(readr)
library(udpipe)

fileNames <- list.files(path="texts", pattern="*.tsv")

# Combine the tables into one

l <- 0

Table <- data.frame()

for (i in 1:length(fileNames))
{
  cat(fileNames[i], "\n")

  subTable <- read_tsv(paste0("texts/", fileNames[i]), show_col_types = FALSE)

  lines <- unique(subTable$sentence_id)

  df <- data.frame()

  for (j in 1:length(lines))
  {
    dfSub <- subset(subTable, sentence_id==lines[j])

    l <- l + 1
    dfSub$sentence_id <- l

    df <- rbind(df, dfSub)
  }

  df$doc_id <- substr(fileNames[i], 1, nchar(fileNames[i]) - 4)

  Table <- rbind(Table, df)
}

Table$paragraph_id <- NA

# Randomiseer

lines <- unique(Table$sentence_id)
lines <- sample(x = lines, replace = FALSE)

x <- data.frame()

for (i in lines)
{
  x <- rbind(x, subset(Table, sentence_id==i))
}

# Split in train, dev and test

groupSize <- ceiling(length(lines)/10)

#

i <- 1

first <- ((i-1) * groupSize) + 1
last <-    i    * groupSize

con <- file("conllu/nds-gronings-ud-dev.conllu", encoding = "UTF-8")
cat(as_conllu(subset(x,  is.element(sentence_id, lines[first:last]))), file = con)
on.exit(close(con))

#

i <- 2

first <- ((i-1) * groupSize) + 1
last <-    i    * groupSize

con <- file("conllu/nds-gronings-ud-test.conllu", encoding = "UTF-8")
cat(as_conllu(subset(x,  is.element(sentence_id, lines[first:last]))), file = con)
on.exit(close(con))

#

i <- 3

first <- ((i-1) * groupSize) + 1
last <-  length(lines)

con <- file("conllu/nds-gronings-ud-train.conllu", encoding = "UTF-8")
cat(as_conllu(subset(x,  is.element(sentence_id, lines[first:last]))), file = con)
on.exit(close(con))

