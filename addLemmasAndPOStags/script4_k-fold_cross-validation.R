library(udpipe)
library(word2vec)
library(stringr)
library(crayon)

setwd("./conllu/")

# read tables

x <- rbind(udpipe_read_conllu("nds-gronings-ud-train.conllu"),
           udpipe_read_conllu("nds-gronings-ud-dev.conllu"  ),
           udpipe_read_conllu("nds-gronings-ud-test.conllu" ))

# randomize order of sentences

lines <- unique(x$sentence_id)
lines <- sample(x = lines, replace = FALSE)

x0 <- data.frame()

for (i in lines)
{
  x0 <- rbind(x0, subset(x, sentence_id==i))
}

# create k groups

K <- 10

groupSize <- ceiling(length(lines)/K)

sel <- list()

for (i in 1:K)
{
  first <- ((i-1) * groupSize) + 1
  last <-    i    * groupSize

  sel[[i]] <- as.numeric(lines[first:last])
}

# k-fold cross-validation

# The parameters that are used here were also used for Lassysmall UD 2.5 and can be found at:
# https://github.com/bnosac/udpipe/tree/master/inst/models-ud-2.5.

df1 <- data.frame()
df2 <- data.frame()

for (i in 1:K)
{
  cat(bold(cyan("\n ********** ", i, " **********\n\n")))

  if (i==K) j <- 1 else j <- i+1

  # create sets

  trainSet <- subset(x0, !is.element(sentence_id, c(sel[[i]],sel[[j]])))
  cat(as_conllu(trainSet), file = file("trainSet.conllu", encoding = "UTF-8"))

  devSet   <- subset(x0,  is.element(sentence_id, sel[[j]]))
  cat(as_conllu(  devSet), file = file(  "devSet.conllu", encoding = "UTF-8"))

  testSet <- subset(x0,  is.element(sentence_id, sel[[i]]))
  cat(as_conllu( testSet), file = file( "testSet.conllu", encoding = "UTF-8"))

  # train model

  udpipe_train(
    file                  =    "model.udpipe",
    files_conllu_training = "trainSet.conllu",
    files_conllu_holdout  =   "devSet.conllu",

    annotation_tokenizer  = list(dimension = 24,
                                 epochs = 100,
                                 segment_size = 200,
                                 initialization_range = 0.1,
                                 batch_size = 50,
                                 learning_rate = 0.002,
                                 dropout = 0.1,
                                 early_stopping = 1,
                                 learning_rate_final = 0),

    annotation_tagger     = list(models = 2,
                                 templates_1 = "tagger",
                                 iterations_1 = 20,
                                 guesser_suffix_rules_1 = 8,
                                 guesser_enrich_dictionary_1 = 6,
                                 guesser_prefixes_max_1 = 0,
                                 use_lemma_1 = 0,
                                 use_xpostag_1 = 1,
                                 use_feats_1 = 1,
                                 provide_lemma_1 = 0,
                                 provide_xpostag_1 = 1,
                                 provide_feats_1 = 1,
                                 prune_features_1 = 0,
                                 templates_2 = "lemmatizer",
                                 iterations_2 = 20,
                                 guesser_suffix_rules_2 = 4,
                                 guesser_enrich_dictionary_2 = 5,
                                 guesser_prefixes_max_2 = 4,
                                 use_lemma_2 = 1,
                                 use_xpostag_2 = 0,
                                 use_feats_2 = 0,
                                 provide_lemma_2 = 1,
                                 provide_xpostag_2 = 0,
                                 provide_feats_2 = 0,
                                 prune_features_2 = 0),

    annotation_parser     = "none"
  )

  # evaluate

  m <- udpipe_load_model("model.udpipe")

  metrics1 <- udpipe_accuracy(m, "testSet.conllu", tokenizer = "default", tagger = "default", parser = "none")
  metrics2 <- udpipe_accuracy(m, "testSet.conllu", tokenizer = "none"   , tagger = "default", parser = "none")

  # save results in tables

  df1 <- rbind(df1, data.frame(
    f1Words = as.numeric(str_extract(metrics1$accuracy[2], "(?<=(f1: ))([0-9]+\\.[0-9]+)((?=(%)))")),
    f1Sents = as.numeric(str_extract(metrics1$accuracy[3], "(?<=(f1: ))([0-9]+\\.[0-9]+)((?=(%)))")),
    UPOS    = as.numeric(str_extract(metrics1$accuracy[4], "(?<=(upostag: ))([0-9]+\\.[0-9]+)((?=(%)))")),
    Lemma   = as.numeric(str_extract(metrics1$accuracy[4], "(?<=(lemmas: ))([0-9]+\\.[0-9]+)((?=(%)))"))
  ))

  df2 <- rbind(df2, data.frame(
    UPOS    = as.numeric(str_extract(metrics2$accuracy[1], "(?<=(upostag: ))([0-9]+\\.[0-9]+)((?=(%)))")),
    Lemma   = as.numeric(str_extract(metrics2$accuracy[1], "(?<=(lemmas: ))([0-9]+\\.[0-9]+)((?=(%)))"))
  ))

  # remove files

  system("rm   devSet.conllu")
  system("rm trainSet.conllu")
  system("rm  testSet.conllu")

  system("rm    model.udpipe")
}

# save results

setwd("../evaluation/")

write.table(df1, "results1.tsv", quote = FALSE, sep = "\t", row.names = F, col.names = T)
write.table(df2, "results2.tsv", quote = FALSE, sep = "\t", row.names = F, col.names = T)

round2 <- function(x, digits=0)
{
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  z * posneg
}

options(width = 160)

sink("results.txt")
cat("Raw text\n\n")
print(round2(apply(df1, 2, mean), 1))
cat("\n")
print(round2(apply(df1, 2, sd  ), 2))
cat("\n\n")
cat("Gold tok\n\n")
print(round2(apply(df2, 2, mean), 1))
cat("\n")
print(round2(apply(df2, 2, sd  ), 2))
sink()
