# first, load general libraries

library(topicanalysis)
library(polmineR)
library(topicmodels)

# load UNGA 

library(UNGA)
use("UNGA")


# get LDA topic model

lda_filename <- list.files(system.file(package = "UNGA", "extdata", "topicmodels"), full.names = TRUE)
lda <- readRDS(lda_filename)
words <- topicmodels::terms(x = lda, k = 100)
ts <- topicmodels::topics(x = lda, k = 100)
View(ts) # Inspect table


TA <- Topicanalysis$new(lda)
topic_to_get <- 105
words[,105]

# get the documents classified for a topic model

mig_docs <- TA$docs(x = topic_to_get, n = 2L)

sps <- as.speeches("UNGA", s_attribute_name = "speaker")
sps_mig <- sps[[mig_docs]]

# iterate through topic models

for (i in seq_along(sps)){
  html(sps_mig[[i]]) %>% highlight(yellow = words[,topic_to_get]) %>% show()
  y <- readline("wait ...")
  if (y == "q") break
}
