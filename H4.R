setwd('/users/mingjie/desktop/UPM/intelligent systems/Unit 4/R')
# Needed for OutOfMemoryError: Java heap space 
library(rJava)
.jinit(parameters="-Xmx4g")
# If there are more memory problems, invoke gc() after the POS tagging

library(NLP)
library(openNLP) 
library(openNLPmodels.en)
library(tm)
library(stringr)
library(SPARQL)
library(parallel)


# Returns annotations for the text document: word, sentence, POS
# As an alternative, the koRpus package uses TreeTagger for POS tagging
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  return(y2)  
} 


getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
} 

getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
} 

detectPatternOnDocument <- function(doc, pattern) {
  x=as.String(doc)
  res=str_match(x,pattern)
  
  if (length(res)==1){
    return (res)
  } else {
    if (all(is.na(res[,2:length(res)])))
      return (NA)
    else {
      ret=list()
      for (i in 2:length(res)){
        ret = paste(ret,res[i])
      }
      return(ret)
    }
  }
}


detectPatternsInCorpus = function(corpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(mclapply(corpus, detectPatternOnDocument, pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

countMatchesPerColumn = function (df) {
  entityCountPerPattern <- data.frame(matrix(NA, ncol = 2, nrow = length(names(df))-1))
  names(entityCountPerPattern) <- c("Entity","Count")
  
  for (i in 2:length(names(df))) {
    entityCountPerPattern$Entity[i-1] = names(df)[i]
    entityCountPerPattern$Count[i-1] = nrow(subset(df, !is.na(df[i])))
  }
  return (entityCountPerPattern)
}


countMatchesPerRow = function (df) {
  entityCountPerFile <- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(entityCountPerFile) <- c("File","Count")
  
  for (i in 1:nrow(df)) {
    entityCountPerFile$File[i] = df$File[i]
    entityCountPerFile$Count[i] = length(Filter(Negate(is.na),df[i,2:length(df[i,])]))
  }
  return (entityCountPerFile[entityCountPerFile[2]!=0,])
}


mergeAllMatchesInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  
  for (i in 1:nrow(df)) {    
    matches=as.list(unname(unlist(Filter(Negate(is.na),df[i,2:length(df[i,])]))))
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}


mergeGoldStandardInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  
  for (i in 1:nrow(df)) {    
    matches=as.list(unlist(Filter(Negate(is.na),df[i,2:length(df)])))
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}


calculateMetrics = function (matches, matches.gs) {
  
  metrics<- data.frame(matrix(NA, ncol = 3, nrow = 1))
  names(metrics) <- c("Precision","Recall","Fmeasure")
  
  numCorrect = 0
  allAnswers = 0
  possibleAnswers = 0
  
  for (i in 1:nrow(matches)) {    
    if (length(matches.gs$Matches[[i]])!=0) {
      l = str_trim(unlist(matches[i,2]))
      l.gs = unname(unlist(matches.gs[i,2]))
      
      intersection = intersect(l, l.gs)
      
      numCorrect = numCorrect + length(intersect(l, l.gs))
      allAnswers = allAnswers + length (l)
      possibleAnswers = possibleAnswers + length(l.gs)    
    }
  }
  
  metrics$Precision = numCorrect / allAnswers
  metrics$Recall = numCorrect / possibleAnswers
  
  beta = 1
  metrics$Fmeasure= ((sqrt(beta)+1) * metrics$Precision * metrics$Recall) / ((sqrt(beta)*metrics$Precision) + metrics$Recall)
  
  return(metrics)
}


# load corpus
source.pos = DirSource("./review_polarity/txt_sentoken/pos", encoding = "UTF-8")
corpus = Corpus(source.pos)[0:10]
inspect(corpus[[1]])

# Annotate corpus
annotations = lapply(corpus, getAnnotationsFromDocument)
corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)

# Get actor names from DBpedia
prefixT <- c("skos","http://www.w3.org/2004/02/skos/core#")

sparql_prefixT <- "
PREFIX owl: <http://www.w3.org/2002/07/owl#>
"

qT <- paste(sparql_prefixT,"
            SELECT DISTINCT ?label where {
            ?actor a <http://dbpedia.org/class/yago/Actor109765278> .
            ?actor rdfs:label ?label .
            } 
            LIMIT 10000
            OFFSET 0
            ")

# evaluate the query against the SPARQL endpoint.
endpointT <- "http://dbpedia.org/sparql"
optionsT=""

actors <- SPARQL(endpointT,qT,ns=prefixT,extra=optionsT)$results

length(actors)
actors[1:30]

# Clean the query result
actors.2 <- mclapply(actors, function(x) strsplit(x,'"')[[1]][2])
actors.3 <- mclapply(actors.2, function(x) strsplit(x,' \\(')[[1]][1])
actor.names <- unique(actors.3)
actor.names <- mclapply(actor.names, gsub, pattern="\\.", replacement=" ")
actor.names <- mclapply(actor.names, tolower)
length(actor.names)

head(actor.names,10)

# Write gazetteer to a file
write.table(unlist(actor.names), file = "gazetteer.txt", row.names = F, col.names = F, na="", sep=";")

# Detect patterns
pattern.an <- mclapply(actor.names, function(x) return(paste(" ",x," ",sep = "")))
pattern.an=unlist(pattern.an)

# There is some actor named "you" that is spoiling our results; we remove it
pattern.an = pattern.an[grep("^ you $", pattern.an, invert = TRUE)]
matches.an = detectPatternsInCorpus(corpus, pattern.an)

countMatchesPerRow(matches.an) 

countColum = countMatchesPerColumn(matches.an) 
countColum[countColum$Count != 0,]

write.table(matches.an, file = "allEntitiesGazetteer.csv", row.names = F, na="", sep=";")


# Evaluate using gold standard
allMatches = mergeAllMatchesInLists(matches.an)
head(allMatches,10)

# load the gold standard and put all gold standard matches in a list for comparison.
goldStandard = read.table(file = "goldStandard.csv", quote = "", na.strings=c(""), colClasses="character", sep=";")
allMatchesGold = mergeGoldStandardInLists(goldStandard)
head(allMatchesGold,10)

metrics = calculateMetrics(allMatches, allMatchesGold)
metrics
