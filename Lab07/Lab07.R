# Instalacja pakietów
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("ggplot2")

# Importowanie potrzebnych bibliotek
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Odczyt tekstu z pliku
text <- readLines("E:/Users/denn/Desktop/ai.txt")

# Tworzenie obiektu Corpus
TextDoc <- Corpus(VectorSource(text))

# Funkcje czyszcz¹ce tekst
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "^a“")
TextDoc <- tm_map(TextDoc, toSpace, ":")
TextDoc <- tm_map(TextDoc, toSpace, ";")
TextDoc <- tm_map(TextDoc, toSpace, ",")
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team"))
TextDoc <- tm_map(TextDoc, removePunctuation)
TextDoc <- tm_map(TextDoc, stripWhitespace)
TextDoc <- tm_map(TextDoc, stemDocument)
TextDoc <- tm_map(TextDoc, content_transformer(function(x) gsub(x, pattern = "mathemat", replacement = "math")))
TextDoc <- tm_map(TextDoc, content_transformer(function(x) gsub(x, pattern = " r ", replacement = " Rlanguage ")))

# Tworzenie TermDocumentMatrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
head(dtm_d, 5)

# Tworzenie wykresu s³upkowego
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col = "lightgreen",
        main = "Top 20 most frequent words",
        ylab = "Frequency")

# Generowanie chmury s³ów
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, scale = c(5, 0.5),
          min.freq = 1,
          max.words = 100, random.order = FALSE,
          rot.per = 0.40,
          colors = brewer.pal(8, "Dark2"))

# Wyszukiwanie powi¹zañ
findAssocs(TextDoc_dtm, terms = c("program", "algorithm", "layer", "logic"),
           corlimit = 0.5)

findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 30),
           corlimit = 0.5)

# Analiza sentymentu za pomoc¹ metody syuzhet
syuzhet_vector <- get_sentiment(text, method = "syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)

# Analiza sentymentu za pomoc¹ metody bing
bing_vector <- get_sentiment(text, method = "bing")
head(bing_vector)
summary(bing_vector)

# Analiza sentymentu za pomoc¹ metody afinn
afinn_vector <- get_sentiment(text, method = "afinn")
head(afinn_vector)
summary(afinn_vector)

# Tworzenie tabeli z wynikami sentymentu
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# Analiza sentymentu NRC
d <- get_nrc_sentiment(as.vector(dtm_d$word))
head(d, 10)

# Tworzenie wykresu s³upkowego dla sentymentu
td <- data.frame(t(d))
td_new <- data.frame(rowSums(td[1:56]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2 <- td_new[1:8,]
quickplot(sentiment, data = td_new2, weight = count, geom = "bar", fill = sentiment,
          ylab = "count") + ggtitle("Survey sentiments")

# Wykres s³upkowy dla emocji
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text",
  xlab = "Percentage"
)

# Instalacja dodatkowych pakietów
install.packages("tidytext")
install.packages("igraph")
install.packages("ggraph")

# Importowanie dodatkowych bibliotek
library("tidytext")
library("igraph")
library("ggraph")

# Instalacja pakietu dplyr
install.packages("dplyr")
library("dplyr")

# Odczyt tekstu z pliku
fileName <- "ai.txt"
text <- readChar(fileName, file.info(fileName)$size)
text_df <- data_frame(line = 1, text = text)
text_df

# Tokenizacja tekstu
tidy_text <- text_df %>%
  unnest_tokens(word, text)
data(stop_words)

# Dodawanie niestandardowych s³ów kluczowych
de <- data.frame("thy", "OLD_WORDS")
names(de) <- c("word", "lexicon")
stop_words <- rbind(stop_words, de)

# Usuwanie stop words
tidy_text <- tidy_text %>%
  anti_join(stop_words)

# Liczenie czêstotliwoœci s³ów
tidy_text %>%
  count(word, sort = TRUE)

# Tworzenie bigramów
text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
text_bigrams
text_bigrams %>%
  count(bigram, sort = TRUE)

# Instalacja pakietu tidyr
install.packages("tidyr")
library("tidyr")

# Przetwarzanie bigramów
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Liczenie czêstotliwoœci bigramów
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts

# £¹czenie s³ów w bigramy
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# Tworzenie grafu bigramów
bigram_graph <- bigram_counts %>%
  filter(word1 == "ai" | word2 == "ai") %>%
  graph_from_data_frame()

bigram_graph2 <- bigram_counts %>%
  filter(word1 == "layer" | word2 == "layer") %>%
  graph_from_data_frame()

bigram_graph3 <- bigram_counts %>%
  filter(word1 == "neuron" | word2 == "neuron") %>%
  graph_from_data_frame()

# Wykres dla wybranego bigramu
dev.new()
ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = NULL, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), position = "identity") + 
  theme_void()

# Wykres dla wybranego bigramu
dev.new(width = 550, height = 330, unit = "px")
ggraph(bigram_graph3, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = NULL, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), position = "identity") + 
  theme_void()

# Tworzenie grafu dla wybranych bigramów
bigram_graph1 <- bigram_counts %>%
  filter(word1 %in% c("ai", "network") | word2 %in% c("ai", "network"))


bigram_graph2 <- bigram_counts %>%
  filter(word1 %in% bigram_graph1$word1 | word1 %in% bigram_graph1$word2 | 
           word2 %in% bigram_graph1$word1 | word2 %in% bigram_graph1$word2)


bigram_graph3 <- bigram_counts %>%
  filter(word1 %in% bigram_graph2$word1 | word1 %in% bigram_graph2$word2 | 
           word2 %in% bigram_graph2$word1 | word2 %in% bigram_graph2$word2)

bigram_graph

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Wykres sieciowy dla wybranych bigramów
ggraph(bigram_graph1 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(bigram_graph2 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(bigram_graph3 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()