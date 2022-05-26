#####################ESAME TEXT MINING by LUCA CEFALONI#########################
#importare dataset da pulire 
Amazon_Review___Complete_Data__UK <- read.csv("~/Desktop/Lavori text mining/Esame Text Mining/Amazon_Review___Complete_Data__UK.csv")
View(Amazon_Review___Complete_Data__UK)
################analisi Rating Stelle###########################################
#pulizia rating stelle 
Stellepulite <- gsub("out of 5 stars", "", Amazon_Review___Complete_Data__UK$Rating)#pulizia dataset
piestelle <- as.numeric(Stellepulite)#tramuto in numeri
tavolastelle <- (table(piestelle))#tavola stelle
tavolastelle
#colori etichette/ valori legenda 
cols <- c("darkblue","lightyellow","orange","lightblue","pink")
labs <- c("70","31","67","122","268")
labs1 <- c("1","2","3","4","5")
#grafico stelle
pie(tavolastelle, main = "Stelle Kindle", labels = labs1, col = cols)#grafico a torta 
legend(0.9, 1.0, cex = 1.0, legend = labs, fill = cols) #legenda
barplot(tavolastelle,main = "Stelle Kindle",col = c("darkblue","lightyellow","orange","lightblue","pink"))# grafico a barre
###############################Frequenza delle parole###########################
#------------------------------------------------------------------------------#
#PULIZIA DELLE RECENZIONI
install.packages("tm")
library(tm)
KC <- Amazon_Review___Complete_Data__UK$Review.Content#recenzioni non pulite 
KC <- gsub(pattern = " ☹️☹️", replacement = " ", KC)#rimozione emoticon
KC <- gsub(pattern = " ☹️", replacement = " ", KC)#rimozione emoticon
KC <- VCorpus(VectorSource(KC))#KindleCorpus diventa corpus 
KC <- tm_map(KC, removeWords, stopwords("en"))#Rimozione stop word 
KC <- tm_map(KC, content_transformer(tolower))# tutto minuscolo
KC <- tm_map(KC, stripWhitespace)#rimuovere spazi bianchi 
KC <- tm_map(KC, removePunctuation)#rimuovere punteggiatura
KC <- tm_map(KC, removeNumbers)#rimuovere numeri
KC<- tm_map(KC, stemDocument)#rimuove parti parole NON UTILIZZARE
#matrice documento 
Matrice_Kindle <- DocumentTermMatrix(KC)#matrice
Matrice_Kindle
#frequenza parole 
freq <- colSums(as.matrix(Matrice_Kindle))   
freq
#------------------------------------------------------------------------------#
#grafico con frequenza min 250
library(ggplot2)
#creazione dataset per graficarlo
dataset_1 <- data.frame(word=names(freq), freq=freq)#
dataset_1

p <- ggplot(subset(dataset_1, freq>200), main = "Frequenze Parole", aes(x = freq, y = reorder(word, -freq))) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90, hjust=1, size = 10))
p   
#--------------------------Associazioni Parole----------------------------#
# Calcola le correlazioni e memorizza nel frame di dati...
kindle <- findAssocs(Matrice_Kindle, terms = "kindle", corlimit = 0.23)[[1]]
kindle <- cbind(read.table(text = names(kindle), stringsAsFactors = FALSE), kindle)
books <- findAssocs(Matrice_Kindle, terms = "books", corlimit = 0.23)[[1]]
books <- cbind(read.table(text = names(books), stringsAsFactors = FALSE), books)
#li unisco insieme
library(dplyr)
two_terms_corrs <- full_join(kindle, books)
# raccolgo per la trama plot
library(tidyr)
two_terms_corrs_gathered <- gather(two_terms_corrs, term, correlation,kindle:books)
#Creoo il grafico 
require(ggplot2)
ggplot(two_terms_corrs_gathered, aes(x = V1, y = correlation, colour =  term ) ) +
  geom_point(size = 2) +
  ylab(paste0("Correlation with the terms ", "\"", "kindle",  "\"", " and ",  "\"", "books", "\"")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#associazioni con parole positive e negative 
# Calcola le correlazioni e memorizza nel frame di dati...
positive <- findAssocs(Matrice_Kindle, terms = c("good","adept","beneficial","effective","dear"), corlimit = 0.3)[[1]]
positive <- cbind(read.table(text = names(positive), stringsAsFactors = FALSE), positive)
negative <- findAssocs(Matrice_Kindle, terms = c("bad","badly","badness","forged","dear"), corlimit = 0.3)[[1]]
negative  <- cbind(read.table(text = names(negative ), stringsAsFactors = FALSE),negative )

#li unisco insieme
library(dplyr)
two_terms_pos <- full_join(positive, negative )
# raccolgo per la trama plot
library(tidyr)
two_terms_pos_gathered <- gather(two_terms_pos, term, correlation,positive:negative)
#Creoo il grafico 
require(ggplot2)
ggplot(two_terms_pos_gathered, aes(x = V1, y = correlation, colour =  term ) ) +
  geom_point(size = 2) +
  ylab(paste0("Correlation with the terms ", "\"", "positive",  "\"", " and ",  "\"", "negative", "\"")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

############SentimentAnalysis#############
library(sentimentr) #IMPORTO LIBRERIA SENTIMENTR 
library(tidyverse)
#sentiment recenzioni 
debates <- Amazon_Review___Complete_Data__UK$Review.Content #DATASET 
db <- as.data.frame(debates) #DIVENTA DATAFRAME
#------------------------------------------------------------------------------#
# Calcolo sentiment e polarita frasi 
debates_with_pol <- db %>%  
  get_sentences() %>% 
  sentiment() %>% 
  mutate(polarity_level = ifelse(sentiment < 0.2, "Negative",
                                 ifelse(sentiment > 0.2, "Positive","Neutral")))
View(debates_with_pol)
#------------------------------------------------------------------------------#
#PLOT sentiment frasi !!!!!mandarli insieme!!!!! 
h <- hist(debates_with_pol$sentiment, main = "Sentiment Frasi")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
#------------------------------------------------------------------------------#
tabella_polarita <- table(debates_with_pol$polarity_level)# tabella polarità
#BOX PLOT POLARITA
debates_with_pol %>% 
ggplot() + geom_boxplot(aes(y = element_id, x = polarity_level)) #Box Plot Polarità
#GRAFICO A TORTA POLARITA
pie(tabella_polarita, col = c("blue", "red", "green"))
cols <- c("blue", "red", "green")
labs <- c("2309", "6", "1404")
legend(0.9, 1.0, cex = 1.0, legend = labs, fill = cols) #legenda

###########CREAZIONE WORDCLOUD PER COPERTINA###########################
install.packages("wordcloud")
library(wordcloud)
set.seed(1234)
wordcloud(words = dataset_1$word, freq = freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(11, "RdYlBu"))
###########Term Frequency#######
install.packages("doSNOW")
library(doSNOW)
#calcolo relativo al term frequency (TF)
term.frequency <- function(row) {
  row/sum(row)
}
#calcolo relativo alla frequenza del documento inverso 
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length((which(col > 0)))
  
  log10(corpus.size / doc.count)
}
#funzione per calolare TF-IDF
tf.idf <- function(x, idf) {
  x * idf
}
# Step uno, normalizzare tutti i documenti via TF
kindle.tokens.df <- apply(Matrice_Kindle, 1, term.frequency)
dim(kindle.tokens.df)
View(kindle.tokens.df)
#Step due, calculate IDF vector
kindle.tokens.idf <- apply(Matrice_Kindle, 2, inverse.doc.freq)
str(kindle.tokens.idf)
#step tre calcolo TF-IDF for our training corpus
kindle.tokens.tfidf <- apply(kindle.tokens.df, 2, tf.idf, idf = kindle.tokens.idf)
dim(kindle.tokens.tfidf)
View(kindle.tokens.tfidf)
#transpose the matrix 
kindle.tokens.tfidf <- t(kindle.tokens.tfidf)
dim(kindle.tokens.tfidf)
View(kindle.tokens.tfidf)
#controllo dei casi incompleti 
library(ggplot2)

ggplot(kindle.tokens.tfidf, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
library(dplyr)
library(tidytext)

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- dataset_1 %>%
  unnest_tokens(dataset_1$word, dataset_1$freq) %>%
  count(dataset_1$freq, dataset_1$word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)


total_words <- dataset_1 %>% 
  group_by(dataset_1$word) %>% 
  summarize(total = sum(n))

book_words <- left_join(dataset_1$freq, dataset_1$word)

book_words

library(ggplot2)

ggplot(book_words, aes(n/total, fill = dataset_1$word)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

##############Frequenza Date Recenzioni###########
date <- Amazon_Review___Complete_Data__UK$Review.Date
datepulite <- gsub("Reviewed in the United Kingdom on ", "", date)
datepulite
datepulite2 <- gsub("1 ", " ", datepulite )
datepulite2  <- gsub("2 ", " ", datepulite2 )
datepulite2 <- gsub("3 ", " ", datepulite2 )
datepulite2 <- gsub("4 ", " ", datepulite2 )
datepulite2  <- gsub("5 ", " ", datepulite2 )
datepulite2  <- gsub("6 ", " ", datepulite2 )
datepulite2  <- gsub("7 ", " ", datepulite2 )
datepulite2  <- gsub("8 ", " ", datepulite2 )
datepulite2  <- gsub("9 ", " ", datepulite2 )
datepulite2  <- gsub("10 ", " ", datepulite2 )###
datepulite2  <- gsub("11 ", " ", datepulite2 )
datepulite2  <- gsub("12 ", " ", datepulite2 )
datepulite2  <- gsub("13 ", " ", datepulite2 )
datepulite2  <- gsub("14 ", " ", datepulite2 )
datepulite2  <- gsub("15 ", " ", datepulite2 )
datepulite2  <- gsub("16 ", " ", datepulite2 )
datepulite2  <- gsub("17 ", " ", datepulite2 )
datepulite2  <- gsub("18 ", " ", datepulite2 )
datepulite2  <- gsub("19 ", " ", datepulite2 )
datepulite2  <- gsub("20 ", " ", datepulite2 )
datepulite2  <- gsub("21 ", " ", datepulite2 )
datepulite2  <- gsub("22 ", " ", datepulite2 )
datepulite2  <- gsub("23 ", " ", datepulite2 )
datepulite2  <- gsub("24 ", " ", datepulite2 )
datepulite2  <- gsub("25 ", " ", datepulite2 )
datepulite2  <- gsub("26 ", " ", datepulite2 )
datepulite2  <- gsub("27 ", " ", datepulite2 )
datepulite2  <- gsub("28 ", " ", datepulite2 )
datepulite2  <- gsub("29 ", " ", datepulite2 )
datepulite2  <- gsub("30 ", " ", datepulite2 )
datepulite2  <- gsub("31 ", " ", datepulite2 )
datefinali  <- gsub("1 ", " ", datepulite2 )
datefinali <- gsub("2 ", " ",datefinali)
datefinali
tavoladate <- table(datefinali)
barplot(tavoladate, horiz = T, main="Recenzioni per date ",ylab="Freqency",las=2, cex.axis=0.8, cex.names=0.5)

