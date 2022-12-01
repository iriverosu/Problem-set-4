
# Cargamos los paquetes
install.packages(("rvest"))
library(rvest)
library(pacman)
install.packages(("tm"))
library(tm)
p_load(tidyverse, rstudioapi)
p_load("stopwords", "stringi", "tm", "rvest")

# Definimos el directorio
setwd(dirname(getActiveDocumentContext()$path))


# Visualizamos las primeras filas
head(train)

# Estudiamos la estructura de la data
glimpse(train)
table(train$name)

# Creamos una lista con todos los stopwords en español
stopwords_español <- stopwords::stopwords("es", source = "snowball")
train[1,3]
# Eliminamos los acentos de los stopwords
stopwords_español <- stri_trans_general(str = stopwords_español, id = "Latin-ASCII")
train[9,3]

# Normalizamos nuestros textos
# Eliminamos los acentos
train$text <- stri_trans_general(str = train$text, id = "Latin-ASCII")
# Ponemos el texto en minúscula
train$text <- tolower(train$text)
# Reemplazamos todos los caracteres no alfanumericos con un espacio
train$text <- str_replace_all(train$text, "[^[:alnum:]]", " ")
# Eliminamos los números
train$text <- gsub('[[:digit:]]+', '', train$text)
# Quitamos stopwords
install.packages("tm")
library(tm)
train$text <- removeWords(train$text, stopwords_español)
# Eliminamos todos los espacios extras
train$text <- gsub("\\s+", " ", str_trim(train$text))




# Vamos a crear una función para lematizar (que es lenta como un hpta)
lematiza = function(frase){
  # Se reemplazan los espacios con +
  query <- gsub(" ", "+", frase)
  url_base <- "https://www.lenguaje.com/cgi-bin/lema.exe?edition_field="
  url_final <- paste0(url_base, query,"&B1=Lematizar")
  lemma <- read_html(url_final, encoding = "latin1") %>%
    html_nodes('div') %>% 
    tail(1) %>% 
    html_nodes("li") %>% 
    html_text2() %>% 
    tail(1)
  # lemma <- read_html(url_final, encoding = "latin1") %>% 
  #   html_node(css = "div div div div div li") %>% 
  #   html_text() 
  # lemma <- gsub("La palabra:", "", lemma)
  # lemma <- gsub("tiene los siguientes lemas:", "", lemma)
  # error <- "\r\n     Palabra no encontrada\r\n     Palabra no encontrada"
  # lemma <- ifelse(lemma == error, frase, lemma)
  if (length(lemma) == 0) {
    return(frase)
  } else {
    lemma <- str_split(lemma, "\\n")[[1]][1]
    return(lemma)
  }
}

lematiza("comieron")
lematiza("comimos")
lematiza('comeremos')
lematiza("Bad Bunny")

# Para evitar doble computación vamos a crear un diccionario de palabras con su respectiva lematización

# Tokenizaremos nuestros textos
p_load(tidytext)
train$id <- 1:nrow(train)
tidy_ensayos <- train %>%
  unnest_tokens(output = token, input = text)

# Tenemos 13 mil palabras únicas. 
# Esto no lo vamos a correr en la complementaria porque se demora mucho. Vamos a importar el diccionario ya melo
diccionario_lemmatizador <- data.frame(corpus = unique(tidy_ensayos$token))

diccionario_lemmatizador <- diccionario_lemmatizador %>% 
  # Si se hacen demasiadas querys nos van a bloquear entonces toca
  # darle con calma
  mutate(lemma = sapply(corpus, lematiza))



# Usando el comando automático de R con una aproximación más eficiente
p_load(udpipe)
library(udpipe)

# Creamos el id de cada documento
train$id <- paste0("doc", 1:nrow(train))

# Descargamos el modelo pre entrenado
 udmodel <- udpipe_download_model(language = "spanish")
 modelo <- udpipe_load_model(file = udmodel$file_model)
#file_model <- "C:/Users/User/Dropbox/BDML_Fall_2022_shared/04_Complementarias/14_LDA/spanish-gsd-ud-2.5-191206.udpipe"
#modelo <- udpipe_load_model(file = file_model)
x <- udpipe_annotate(modelo, x = train$text)
tidy_train <- as.data.frame(x)


word_count <- tidy_train %>%
  group_by(doc_id) %>%
  count(lemma) %>%
  ungroup()
table(word_count$n)
word_count$lemma<- ifelse(word_count$lemma == "t", NA, word_count$lemma)
word_count$lemma<- ifelse(word_count$lemma == "co", NA, word_count$lemma)
word_count$lemma<- ifelse(word_count$lemma == "https", NA, word_count$lemma)
word_count = subset(x = word_count, subset = is.na(lemma)==FALSE)

# Ahora vamos a eliminar las palabras extrañas (aquellas que aparezcan menos de 20 veces en los documentos) o demasiado comunes (que aparezcan en más del 50% de los documentos)
word_count2 <- word_count %>%
  left_join(
     word_count %>%
      count(lemma) %>%
      mutate(filtro = !((n < 2))) %>%
      select(-n)
  ) %>%
  filter(filtro) %>%
  select(-filtro)

word_count2$lemma<- ifelse(word_count2$lemma == "t", NA, word_count2$lemma)
word_count2$lemma<- ifelse(word_count2$lemma == "co", NA, word_count2$lemma)
word_count2$lemma<- ifelse(word_count2$lemma == "https", NA, word_count2$lemma)
word_count2 = subset(x = word_count2, subset = is.na(lemma)==FALSE)

table(word_count2$n)

# Se nos colaron stopwords entonces otra vez chao
# Normalizamos nuestros textos
# Eliminamos los acentos
word_count$lemma <- stri_trans_general(str = word_count$lemma, id = "Latin-ASCII")
# Quitamos stopwords
filtro <- !(word_count$lemma %in% stopwords_español)
word_count <- word_count[filtro, ]

p_load(tm, tidytext)
train_dtm <- cast_dtm(data = word_count, 
                        document = doc_id, 
                        term = lemma, 
                        value = n)
inspect(train_dtm)

# Visualicemos las palabras más relevantes
p_load(wordcloud)
freq <- sort(colSums(as.matrix(train_dtm)), 
             decreasing = T)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(names(freq), freq, max.words = 50,
          random.order = FALSE,
          colors = brewer.pal(8, "Accent"),
          scale = c(4, 0.5), rot.per = 0)



train_dtm2 <- cast_dtm(data = word_count, 
                         document = doc_id, 
                         term = lemma, 
                         value = n,
                         weighting = tm::weightTfIdf)
inspect(train_dtm2)



# Visualicemos las palabras más relevantes
freq2 <- sort(colSums(as.matrix(train_dtm2)), 
              decreasing = T)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(names(freq2), freq2, max.words = 50,
          random.order = FALSE, min.freq = 0,
          colors = brewer.pal(8, "Accent"),
          scale = c(4, 0.5), rot.per = 0)


X <- as.matrix(train_dtm2)
X_std <- (X - min(X)) / (max(X) - min(X))
X_scaled <- X_std * (1000 - 0) + 0
X_scaled <- round(X_scaled, 0)

colnames(X_scaled)
removeWords(word_count$lemma,"bihepbwjx")
removeWords(word_count$lemma,"fgbynkfep")
removeWords(word_count$lemma,"ufyjpwb")
removeWords(word_count$lemma,"kfsysiebtp")
removeWords(word_count$lemma,"dihneenhu")
removeWords(word_count$lemma,pattern= "^[^aáeéiíoóuúü]{3}+")
word_count<-stringr::str_remove_all(word_count$lemma, pattern= "^[^aáeéiíoóuúü]{3}+")
word_count <- as.data.frame(word_count)


# Por toda esta gestión toca volver a hacer el proceso desde cero
train_dtm3 <- X_scaled %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  cast_dtm(document = rowname,
           term = name, 
           value = value)
inspect(train_dtm3)

# Ahora estamos listos para usar LDA
p_load(topicmodels)
train_lda <- LDA(train_dtm3, k = 3, 
                   control = list(seed = 666))

train_lda


train_topics <- tidy(train_lda, matrix = "beta")
train_topics



train_top_terms <- train_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

train_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



