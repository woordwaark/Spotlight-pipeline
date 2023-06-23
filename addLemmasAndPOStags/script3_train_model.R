# https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-train.html

library(utils)
library(udpipe)
library(word2vec)

# Train model
# The parameters that are used here were also used for Lassysmall UD 2.5 and can be found at:
# https://github.com/bnosac/udpipe/tree/master/inst/models-ud-2.5.

setwd("./conllu/")

m <- udpipe_train(
  file                  = "../nds_gronings-ud-GitHub-demo.udpipe",
  files_conllu_training =    "nds-gronings-ud-train.conllu",
  files_conllu_holdout  =    "nds-gronings-ud-dev.conllu",

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

m <- udpipe_load_model("../nds_gronings-ud-GitHub-demo.udpipe")

goodness_of_fit <- udpipe_accuracy(m, "nds-gronings-ud-test.conllu", tokenizer = "default", tagger = "default", parser = "none")
cat(goodness_of_fit$accuracy, sep = "\n")

goodness_of_fit <- udpipe_accuracy(m, "nds-gronings-ud-test.conllu", tokenizer = "none"   , tagger = "default", parser = "none")
cat(goodness_of_fit$accuracy, sep = "\n")
