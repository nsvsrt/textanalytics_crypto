A cryptocurrency is a digital-assets that are designed based on blockchain technology where it is used as a
medium of exchange where the coins can be owned by any individuals which are stored in a secured
ledger using cryptography. Bitcoin(BTC) being the heart of all the cryptocurrency coins trading at around
$45,000 and the base coin Ethereum(ETH) trading at $1700 and Litecoin (LTC) which is a replica of
bitcoin are the three most famous and high market cap coins to be noted in the cryptocurrency market.
Three articles were taken for this analysis to convert unstructured data to structured data, one article for
each crypto coin. The reason for this analysis to check the different and common factors that could drive
each crypto coin. To find patten of words that could positively and negatively effect the coins. The
frameworks used for this analysis are tokenization, finding out the positive-negative effecting words,
word-cloud analysis, sentiment analysis, frequency and correlograms of the three articles.


#code using r

#############################
#####Calling Libraries#######
#############################
library(textdata)
library(tidytext)
library(tidyverse)
library(dplyr)
library(janeaustenr)
library(wordcloud)
library(textdata)
library(gutenbergr)
library(reshape2)
library(textreadr)
library(scales)
library(plotly)
library(igraph)
library(ggraph)
library(tm)
library(RColorBrewer)
############################
#####Calling Dataframes#####
############################
# Reading bitcoin
file_1 <- read_document(file="C:/Users/Teja/Documents/NLP/bitcoin/bitcoin.txt")
bitcoin <- c(file_1)
bitcoin <- data_frame(line=1, text=bitcoin)
# Reading ethereum
file_2 <- read_document(file="C:/Users/Teja/Documents/NLP/ethereum/ethereum.txt")
ethereum <- c(file_2)
ethereum <- data_frame(line=1, text=ethereum)
# Reading litecoin
file_3 <- read_document(file="C:/Users/Teja/Documents/NLP/business_insights/litecoin.txt")
litecoin <- c(file_3)
litecoin <- data_frame(line=1, text=litecoin)
#############################################
#############Tokenization####################
#############################################
# Tokenizing bitcoin
bitcoin_token <- bitcoin %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 count(word, sort = TRUE)
bitcoin_token
# Tokenizing ethereum
ethereum_token <- ethereum %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 count(word, sort = TRUE)
ethereum_token
# Tokenizing litecoin
litecoin_token <- litecoin %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 count(word, sort = TRUE)
litecoin_token
##########################################
#######Combining dataframes###############
##########################################
# Combining into single data frame
crypto <- bind_rows(mutate(bitcoin_token, author="bitcoin"),
 mutate(ethereum_token, author= "ethereum"),
 mutate(litecoin_token, author="litecoin"))%>%#closing bind_rows
 mutate(word=str_extract(word, "[a-z']+")) %>%
 count(author, word) %>%
 group_by(author)
crypto
########################################
#####Sentiment Analysis#################
########################################
# Bing Sentiment Analysis using Plotly
# bitcoin
bitcoin_senti <- bitcoin %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 inner_join(get_sentiments("bing")) %>%
 count(word, sentiment, sort=T) %>%
 ungroup()
bitcoin_bing <- bitcoin_senti %>%
 group_by(sentiment) %>%
 top_n(5) %>%
 ungroup() %>%
 mutate(word=reorder(word, n)) %>%
 ggplot(aes(word, n, fill=sentiment)) +
 geom_col(show.legend = FALSE) +
 facet_wrap(~sentiment, scales = "free_y")+
 labs(y="Contribution to sentiment Bitcoin", x=NULL)+
 coord_flip()
bitcoin_bing <- ggplotly(bitcoin_bing)
bitcoin_bing
# Ethereum
ethereum_senti <- ethereum %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 inner_join(get_sentiments("bing")) %>%
 count(word, sentiment, sort=T) %>%
 ungroup()
ethereum_bing <- ethereum_senti %>%
 group_by(sentiment) %>%
 top_n(5) %>%
 ungroup() %>%
 mutate(word=reorder(word, n)) %>%
 ggplot(aes(word, n, fill=sentiment)) +
 geom_col(show.legend = FALSE) +
 facet_wrap(~sentiment, scales = "free_y")+
 labs(y="Contribution to sentiment Ethereum", x=NULL)+
 coord_flip()
ethereum_bing <- ggplotly(ethereum_bing)
ethereum_bing
# litecoin
litecoin_senti <- litecoin %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 inner_join(get_sentiments("bing")) %>%
 count(word, sentiment, sort=T) %>%
 ungroup()
litecoin_bing <- litecoin_senti %>%
 group_by(sentiment) %>%
 top_n(5) %>%
 ungroup() %>%
 mutate(word=reorder(word, n)) %>%
 ggplot(aes(word, n, fill=sentiment)) +
 geom_col(show.legend = FALSE) +
 facet_wrap(~sentiment, scales = "free_y")+
 labs(y="Contribution to sentiment Litecoin", x=NULL)+
 coord_flip()
litecoin_bing <- ggplotly(litecoin_bing)
litecoin_bing
# NRC Sentiment Analysis - Word Cloud
# bitcoin
bitcoin_senti_nrc <- bitcoin %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 inner_join(get_sentiments("nrc")) %>%
 count(word, sentiment, sort=T) %>%
 ungroup()
bitcoin_senti_nrc %>%
 inner_join(get_sentiments("nrc")) %>%
 count(word, sentiment, sort=TRUE) %>%
 acast(word ~sentiment, value.var="n", fill=0) %>%
 comparison.cloud(colors = c("grey20", "gray80"),
 title.colors=c("red","green"),
 max.words=100, fixed.asp=TRUE,
 scale=c(0.6,0.6), title.size=1, rot.per=0.25)
# ethereum
ethereum_senti_nrc <- ethereum %>%
 unnest_tokens(word, text) %>%
anti_join(stop_words) %>%
 inner_join(get_sentiments("nrc")) %>%
 count(word, sentiment, sort=T) %>%
 ungroup()
ethereum_senti_nrc %>%
 inner_join(get_sentiments("nrc")) %>%
 count(word, sentiment, sort=TRUE) %>%
 acast(word ~sentiment, value.var="n", fill=0) %>%
 comparison.cloud(colors = c("grey20", "gray80"),
 title.colors=c("red","green"),
 max.words=100, fixed.asp=TRUE,
 scale=c(0.6,0.6), title.size=1, rot.per=0.25)
# litecoin
litecoin_senti_nrc <- litecoin %>%
 unnest_tokens(word, text) %>%
 anti_join(stop_words) %>%
 inner_join(get_sentiments("nrc")) %>%
 count(word, sentiment, sort=T) %>%
 ungroup()
litecoin_senti_nrc %>%
 inner_join(get_sentiments("nrc")) %>%
 count(word, sentiment, sort=TRUE) %>%
 acast(word ~sentiment, value.var="n", fill=0) %>%
 comparison.cloud(colors = c("grey20", "gray80"),
 title.colors=c("red","green"),
 max.words=100, fixed.asp=TRUE,
 scale=c(0.6,0.6), title.size=1, rot.per=0.25)
# Creating Bigrams and plotting the networks
# bitcoin
bitcoin_bigrams <- bitcoin %>%
 unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
 separate(bigram, c("word1", "word2"), sep = " ") %>%
 filter(!word1 %in% stop_words$word) %>%
 filter(!word2 %in% stop_words$word) %>%
 count(word1, word2, sort = TRUE)
bitcoin_bigrams
bitcoin_bigram_graph <- bitcoin_bigrams %>%
 filter(n>2) %>%
 graph_from_data_frame()
ggraph(bitcoin_bigram_graph, layout = "fr") +
 geom_edge_link()+
 geom_node_point()+
 geom_node_text(aes(label=name), vjust =1, hjust=1)
# ethereum
ethereum_bigrams <- ethereum %>%
 unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
 separate(bigram, c("word1", "word2"), sep = " ") %>%
 filter(!word1 %in% stop_words$word) %>%
 filter(!word2 %in% stop_words$word) %>%
 count(word1, word2, sort = TRUE)
ethereum_bigram_graph <- ethereum_bigrams %>%
 filter(n>2) %>%
 graph_from_data_frame()
ggraph(ethereum_bigram_graph, layout = "fr") +
 geom_edge_link()+
 geom_node_point()+
 geom_node_text(aes(label=name), vjust =1, hjust=1)
# Litecoin
litecoin_bigrams <- litecoin %>%
 unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
 separate(bigram, c("word1", "word2"), sep = " ") %>%
 filter(!word1 %in% stop_words$word) %>%
 filter(!word2 %in% stop_words$word) %>%
 count(word1, word2, sort = TRUE)
litecoin_bigrams
litecoin_bigram_graph <- litecoin_bigrams %>%
 filter(n>2) %>%
 graph_from_data_frame()
ggraph(litecoin_bigram_graph, layout = "fr") +
 geom_edge_link()+
 geom_node_point()+
 geom_node_text(aes(label=name), vjust =1, hjust=1, scale=c(0.6,0.6))
# Creating single data frame with all tokens along with frequency proportions
frequency <- bind_rows(mutate(bitcoin_token, author="bitcoin"),
 mutate(ethereum_token, author= "ethereum"),
 mutate(litecoin_token, author="litecoin")
)%>%#closing bind_rows
 mutate(word=str_extract(word, "[a-z']+")) %>%
 count(author, word) %>%
 group_by(author) %>%
 mutate(proportion = n/sum(n))%>%
 #select(-n) %>%
 spread(author, proportion) %>%
 gather(author, proportion, `ethereum`, `litecoin`)
frequency 
# Plotting a correlogram using Plotly
plotting_graph <- ggplot(frequency, aes(x=proportion, y=bitcoin,
 color = abs(bitcoin- proportion)))+
 geom_abline(color="red", lty=2)+
 geom_jitter(aes(text=paste("word: ", word)), alpha=.1, size=2.5, width=0.3, height=0.3)+
 #geom_text(aes(label=word), colour="gray20", alpha=1) +
 #scale_x_log10(labels = percent_format())+
 #scale_y_log10(labels= percent_format())+
 scale_color_gradient(limits = c(0,0.001), low = "blue", high = "blue")+
 facet_wrap(~author, ncol=2)+
 theme(legend.position = "none")+
 labs(y= "bitcoin", x=NULL)
plotting_graph <- ggplotly(plotting_graph)
plotting_graph
# Creating a Document Term Matrix (DTM)
crypto_dtm <- crypto %>%
 group_by(author) %>%
 cast_dtm(author, word, n)
crypto_dtm
# Sentiment Analysis using AFINN
ethereum_afinn <- ethereum_token %>%
 inner_join(get_sentiments("afinn"))%>%
 summarise(mean(value))
ethereum_afinn
litecoin_afinn <- litecoin_token %>%
 inner_join(get_sentiments("afinn"))%>%
 summarise(mean(value))
litecoin_afinn
# bitcoin
bitcoin_afinn <- bitcoin_token %>%
 inner_join(get_sentiments("afinn"))%>%
 summarise(mean(value))
bitcoin_afinn

