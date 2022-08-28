
library(tidytext)
library(dplyr)
library(ggplot2)
library(markovchain)
library(reshape)
library(tidyr)
library(r2r)
library(stringr)
library(data.table)



data.blogs <- readLines("final/en_US/en_US.blogs.txt")
data.news <- readLines("final/en_US/en_US.news.txt")
data.twitter <- readLines("final/en_US/en_US.twitter.txt")


all.data <- c(data.blogs, data.news, data.twitter)
#all.data <- sample(all.data, 30)
all.data <- split(all.data, 1:20)
rm(data.blogs)
rm(data.news)
rm(data.twitter)


ngram.count <- function(x) {
  
  freq.table <- table(x)
  freq.ngrams <- data.frame(ngram = names(freq.table), freq = as.numeric(freq.table))
  #rownames(freq.ngrams) <- freq.ngrams$ngram
  
  return(freq.ngrams)
}


next.word.instant <- function(input) {
  
  if (input %in% all.bigrams.count$ngram) {
  input.count <- all.bigrams.count[input,]$freq
  }
  
  possible.word <- paste(input, all.unigrams.count$ngram)
  con <- possible.word %in% all.trigrams.count$ngram
  possible.word <- possible.word[con]
  possible.count <- all.trigrams.count[possible.word,]
  possible.count$prob <- possible.count$freq / (input.count)
  possible.count %>% arrange(desc(prob))
}


dir.create("cach")


train.ngram <- function(x, index){
  df.all <- data.frame(text = x)
  
  
  df.all.prep <- str_replace_all(df.all$text, "[[:punct:]]", "")
  df.all.prep <- paste("_s_>", df.all.prep, "<_e_>")
  df.all.prep <- data.frame(text = df.all.prep)
  
  all.unigrams <- df.all.prep %>%
                        unnest_tokens(ngram, text, token = "ngrams", n = 1)
  
  all.bigrams <- df.all.prep %>%
                        unnest_tokens(ngram, text, token = "ngrams", n = 4)
  
  all.trigrams <- df.all.prep %>%
                        unnest_tokens(ngram, text, token = "ngrams", n = 5)
  
  all.unigrams.count <- ngram.count(all.unigrams)
  fwrite(all.unigrams.count, paste0("cach/", index, "unigrams.csv"))

  all.bigrams.count <- ngram.count(all.bigrams)
  fwrite(all.bigrams.count, paste0("cach/",index, "bigrams.csv"))

  all.trigrams.count <- ngram.count(all.trigrams)
  fwrite(all.trigrams.count, paste0("cach/",index, "trigrams.csv"))

}


for (i in 1:20) {
  train.ngram(all.data[[i]], i)
}

rm(all.data)


final.data.unigrams <- data.frame() 
final.data.bigrams <- data.frame() 
final.data.trigrams <- data.frame()


  final.data.unigrams <- read.csv(paste0("cach/", 1, "unigrams.csv"))

  final.data.bigrams <- read.csv(paste0("cach/", 1, "bigrams.csv"))
  
  final.data.trigrams <- read.csv(paste0("cach/", 1, "trigrams.csv"))

for (i in 2:20) {
  temp <- read.csv(paste0("cach/", i, "unigrams.csv"))
  final.data.unigrams <- full_join(final.data.unigrams, temp, by = "ngram") %>%
                         mutate(freq = rowSums(cbind(freq.x, freq.y), na.rm = TRUE)) %>%
                         select(ngram, freq)

  temp <- read.csv(paste0("cach/", i, "bigrams.csv"))
  print(temp)
  final.data.bigrams <- full_join(final.data.bigrams, temp, by = "ngram") %>%
                        mutate(freq = rowSums(cbind(freq.x, freq.y), na.rm = TRUE)) %>%
                        select(ngram, freq)
  
  temp <- read.csv(paste0("cach/", i, "trigrams.csv"))
  final.data.trigrams <- full_join(final.data.trigrams, temp, by = "ngram") %>%
                         mutate(freq = rowSums(cbind(freq.x, freq.y), na.rm = TRUE)) %>%
                         select(ngram, freq)
}


# (final.data.unigrams)


# next.word.instant("telling me about his")






















