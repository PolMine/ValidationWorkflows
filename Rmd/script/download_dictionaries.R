# libraries

library(dplyr)
library(magrittr)
library(data.table)

get_formatted_SentWordNet = function() {
  
  # One of the dictionaries we could use to perform a sentiment analysis is the
  # language resource SentiWordNet 3.0 (see Baccianella et al. 2010,
  # http://nmis.isti.cnr.it/sebastiani/Publications/LREC10.pdf). SentiWordNet 3.0
  # provides a list of over 56.000 terms which are provided with three numeric
  # values for positivity, negativity and objectivity (i.e. neutral). The values
  # range from 0 to 1, with their sum being 1 for each term (see ibid. 2200).
  # SentiWordNet 3.0 is available under the CC BY-SA 4.0 License. It can be easily
  # downloaded from the author's GitHub page as a data.table. Due to its size,
  # this might take a little bit.
  
  message("... download SentWordNet")
  
  SentiWordNet_lexicon <- read.csv("https://raw.githubusercontent.com/aesuli/SentiWordNet/master/data/SentiWordNet_3.0.0.txt", 
                                   skip = 25, sep = "\t", stringsAsFactors = FALSE)
  
  
  # There is still a bit of cleaning to do to use it with polmineR.
  #
  # First, we want to use an harmonized weight between -1 and 1 to express the
  # positivity or negativity of a term. In SentiWordNet these are seperated
  # values. We combine them here, with weight being PosScore - NegScore.
  
  message("... calculate weight and clean up columns")
  
  # calculate 
  SentiWordNet_lexicon["weight"] <- SentiWordNet_lexicon["PosScore"] - SentiWordNet_lexicon["NegScore"]
  
  # remove columns we do not want to use anymore
  SentiWordNet_lexicon[,c("PosScore", "NegScore")] <- NULL
  
  # In addition, there are columns we do not need anyway. In "Gloss" a gloss, or a
  # short description of the meaning of the term is provided. This is interesting,
  # but not necessary for our analysis. So we omit the column along with the ID
  # column we do not need either.
  
  SentiWordNet_lexicon[,c("ID", "Gloss")] <- NULL
  
  # Being derived from WordNet, the lexicon does not provide terms per se but
  # synsets, which represent "sets of cognitive synonyms"
  # (https://wordnet.princeton.edu), which explains why some rows do contain more
  # than one term, while others are numbered. If we want to work with the data, we
  # will have to split rows which do contain multiple words and remove the
  # numbering. We will use the `dplyr` package for this.
  
  message("... split multi-meaning synsets")
  
  
  SentiWordNet_lexicon <- SentiWordNet_lexicon %>% 
    mutate(SynsetTerms = strsplit(as.character(SynsetTerms), " ")) %>% 
    tidyr::unnest(SynsetTerms) %>%
    mutate(SynsetTerms = gsub("#\\d+", "", SynsetTerms))
  
  
  # The UNGA corpus we are working with is linguistically annotated. For the "Part
  # of Speech" annotation, the Penn Treebank system is used
  # (https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Penn-Treebank-Tagset.pdf).
  # SentiWordNet 3.0, based on WordNet, uses a rather simple annotation scheme:
  
  # * n for noun
  # * v for verb
  # * a for adjectives
  # * r for adverbs
  
  # We translate the WordNet scheme. Before, we rename the remaining columns and
  # transform the data.frame to a data.table object to enhance the analysis later
  # on.
  
  message("... replace POS-Tag scheme")
  
  library(data.table)
  names(SentiWordNet_lexicon) <- c("pos", "weight", "word")
  SentiWordNet_lexicon <- as.data.table(SentiWordNet_lexicon)
  
  tagSubs <- c("NN", "JJ", "RB", "VB")
  names(tagSubs) <- c("n", "v", "a", "r")
  
  SentiWordNet_lexicon[, pos := stringr::str_replace_all(SentiWordNet_lexicon$pos, tagSubs)]
  
  # Two more cleaning steps are necessary. First, we want to remove all terms
  # which consist of two or more parts as we do not want to apply the dictionary
  # to bigrams.
  
  message("... remove compound words")
  
  
  SentiWordNet_lexicon <- SentiWordNet_lexicon %>%
    filter(!grepl("_", word))
  
  # Finally, there are words which occur multiple times in different meanings
  # (which is why we had to remove the numbers earlier). Here, if a word occurs
  # multiple times with the same POS tag, we calculate the mean of the weight.
  
  message("... harmonize words which occur more than once per POS")
  
  SentiWordNet_lexicon <- SentiWordNet_lexicon %>%
    group_by(pos, word) %>%
    summarise(weight = mean(weight)) %>%
    ungroup() %>%
    filter(!is.na(weight)) %>%
    as.data.table()
  
  return(SentiWordNet_lexicon)
}


get_formatted_afinn <- function(){
  
  afinn_tmp_dir <- file.path(tempdir(), "afinn")
  if (!file.exists(afinn_tmp_dir)) dir.create(afinn_tmp_dir)
  afinn_zipfile <- file.path(afinn_tmp_dir, "imm6010.zip")
  afinn_url <- "https://www2.imm.dtu.dk/pubdb/views/edoc_download.php/6010/zip/imm6010.zip"
  download.file(url = afinn_url, destfile = afinn_zipfile)
  unzip(zipfile = afinn_zipfile, exdir = afinn_tmp_dir)
  
  afinn_path <- paste0(file.path(paste0(afinn_tmp_dir, "/AFINN")), "/AFINN-111.txt")
  afinn_df <- read.csv(afinn_path, sep = "\t", stringsAsFactors = FALSE, header = FALSE)
  names(afinn_df) <- c("word", "weight")
  
  # normalize to -1 to +1
  afinn_df$weight <- afinn_df$weight / 5
  return(as.data.table(afinn_df))
}
