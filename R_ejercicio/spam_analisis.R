# Analisis SPAM

# Librerias

library(ggplot2)
library(tm)

# Funciones

LeerCorpus <- function(folder){
  texto <- scan(folder, character(), sep='\n', quiet=TRUE,
               blank.lines.skip = FALSE, encoding='LATIN1')
  # El mensaje empieza cuando hay una linea vacia
  msg <- texto[seq(which(texto == "")[1] + 1, length(texto), 1)]
  return(paste(msg, collapse='\n'))
}

Calcula.TDM <- function(vec.doc){
  # Construye el corpus de los textos
  doc.corpus <- Corpus(VectorSource(vec.doc, encoding = "UTF-8"))
  # Limpiar lectura
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  doc.corpus <- tm_map(doc.corpus, removeNumbers)
  doc.corpus <- tm_map(doc.corpus, tolower)
  doc.corpus <- tm_map(doc.corpus, removeWords, stopwords())
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  # TDM
  doc.tdm <- TermDocumentMatrix(doc.corpus, 
                                control=list(minDocFreq = 2))
  return(doc.tdm)
}

Clasifica.email <- function(folder, training.df, prior = 0.5, c = 1e-6)
{
  # Leer los correos para ponerlos en formato trabajable
  msg <- LeerCorpus(folder)
  msg.tdm <- Calcula.TDM(msg)
  msg.frec <- rowSums(as.matrix(msg.tdm))
  # Encontrar la inteseccion de terminos
  msg.match <- intersect(names(msg.frec), training.df$termino)
  # Calculo de Bayes
  if(length(msg.match) < 1)
  {
    return(log(prior) + (length(msg.frec))*log(c))
  }
  else
  {
    match.probs <- training.df$ocurrencia[match(msg.match, training.df$termino)]
    return(log(prior) + sum(log(match.probs)) + 
      (length(msg.frec) - length(msg.match))*log(c))
  }
}

# Directorios data
spam.folder <- 'data/spam/'
ham.folder <- 'data/easy_ham/'
hardham.folder <- 'data/hard_ham/'

# Corpus SPAM
spam.docs <- dir(spam.folder)
spam.docs <- spam.docs[which(spam.docs!='cmds')]
todo.spam <- sapply(spam.docs, 
                   function(f) LeerCorpus(paste(spam.folder, f, sep='')))
# Remove mail mal codificado (MacOS)
todo.spam <- todo.spam[-243]

# TDM para SPAM
spam.tdm <- Calcula.TDM(todo.spam)
spam.matriz <- as.matrix(spam.tdm)
spam.contar <- rowSums(spam.matriz)
spam.df <- data.frame(termino=names(spam.contar), 
                      frecuencia=as.numeric(spam.contar), 
                      stringsAsFactors=FALSE)
# No. de documentos donde aparece cada termino 
spam.ocurrencia <- sapply(1:nrow(spam.matriz), function (i) {length(which(spam.matriz[i, ]>0))/ncol(spam.matriz)}) 
# Densidad de la palabra en todos los documentos
spam.densidad <- spam.df$frecuencia/sum(spam.df$frecuencia)
spam.df <- transform(spam.df, densidad=spam.densidad, 
                     ocurrencia=spam.ocurrencia)

# Corpus HAM
ham.docs <- dir(ham.folder)
ham.docs <- ham.docs[which(ham.docs!='cmds')]
todo.ham <- sapply(ham.docs, 
                   function(f) LeerCorpus(paste(ham.folder, f, sep='')))
# TDM para HAM
ham.tdm <- Calcula.TDM(todo.ham)
ham.matriz <- as.matrix(ham.tdm)
ham.contar <- rowSums(ham.matriz)
ham.df <- data.frame(termino=names(ham.contar), 
                     frecuencia=as.numeric(ham.contar), 
                     stringsAsFactors=FALSE)
# No. de documentos donde aparece cada termino 
ham.ocurrencia <- sapply(1:nrow(ham.matriz), function (i) {length(which(ham.matriz[i, ]>0))/ncol(ham.matriz)}) 
# Densidad de la palabra en todos los documentos
ham.densidad <- ham.df$frecuencia/sum(ham.df$frecuencia)
ham.df <- transform(ham.df, densidad=ham.densidad, 
                    ocurrencia=ham.ocurrencia)

# Clasificacion de hardham
hardham.docs <- dir(hardham.folder)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

hardham.spamtest <- sapply(hardham.docs,
                           function(p) Clasifica.email(paste(hardham.folder, p, sep=''), training.df = spam.df))

hardham.hamtest <- sapply(hardham.docs,
                          function(p) Clasifica.email(paste(hardham.folder, p, sep=''), training.df = ham.df))

hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)