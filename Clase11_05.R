library(tm)

data("AssociatedPress", package = "topicmodels")

data("data_corpus_inaugural", package = "quanteda")


library(topicmodels)
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
