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

getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  #  parse_annotator <- Parse_Annotator()
  #  y3 <- annotate(x, parse_annotator, y2)
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

detectPatternOnDocumentWithContext <- function(doc, pattern) {
  txt=as.String(doc)
  number=50
  coord=str_locate(txt,pattern)
  res3=substr(txt,coord[1]-number,coord[2]+number)
  return (res3)
}

detectPatternsInCorpus = function(corpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(corpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

detectPatternsInTaggedCorpus = function(corpus, taggedCorpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, 
                                    nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(taggedCorpus, detectPatternOnDocument, 
                                     pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

countMatchesPerColumn = function (df) {
  entityCountPerPattern <- data.frame(matrix(NA, ncol = 2, 
                                             nrow = length(names(df))-1))
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

printMatchesPerPattern = function (patterns, matches) {
  for (i in 1:length(patterns)){
    print(paste("PATTERN: ",patterns[i]))
    strings = matches[,i+1][!is.na(unlist(matches[,i+1]))]
    print(strings)
    print(" ") 
  }
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
  metrics$Fmeasure= ((sqrt(beta)+1) * metrics$Precision * metrics$Recall) / 
    ((sqrt(beta)*metrics$Precision) + metrics$Recall)
  
  return(metrics)
}


# Load

source.pos = DirSource("./review_polarity/txt_sentoken/pos", encoding = "UTF-8")
corpus = Corpus(source.pos)
inspect(corpus[[1]])

annotations = lapply(corpus, getAnnotationsFromDocument)
head(annotations[[1]])
tail(annotations[[1]])
corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
inspect(corpus.tagged[[1]])
corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)
corpus.taggedText[[1]] 


# find simple patterns
pattern0=c("created by")
pattern0=c(pattern0,"screenwriter[s]?")
pattern0=c(pattern0,"cinematographer")
pattern0=c(pattern0,"oscar winner")

matches0 = detectPatternsInCorpus(corpus, pattern0)
matches0[!is.na(matches0[3]),c(1,3)]

# check how many patterns we have found in each file.
countMatchesPerRow(matches0) 

# check how many times each pattern has been found.
countMatchesPerColumn(matches0) 

for (i in 1:length(pattern0)){
  print(paste("PATTERN: ",pattern0[i]))
  strings = lapply(corpus, detectPatternOnDocumentWithContext, pattern=pattern0[i])
  print(unlist(strings[!is.na(unlist(strings))]))
  print(" ")
}

# Find entities
pattern1=c("created by ([A-z]* [A-z]*)")
pattern1=c(pattern1,"created by [A-z]* [A-z]* \\( and ([A-z]* [A-z]*)")
pattern1=c(pattern1,"screenwriter[s]? ([A-z]* [A-z]*)")
pattern1=c(pattern1,"cinematographer(?: ,)? ([A-z]* [A-z]*)")
pattern1=c(pattern1,"oscar winner ([A-z]* [A-z]*)")

matches1 = detectPatternsInCorpus(corpus, pattern1)
matches1[!is.na(matches1[4]),c(1,4)]

printMatchesPerPattern(pattern1, matches1)

countMatchesPerRow(matches1) 
countMatchesPerColumn(matches1) 


# Find entities using part-of-speech (POS) tags
pattern2=c("created/VBN by/IN ([A-z]*)/NN ([A-z]*)/NN")
pattern2=c(pattern2,"created/VBN by/IN [A-z]*/NN [A-z]*/NN \\(/-LRB- and/CC ([A-z]*)/JJ ([A-z]*)/NN")
pattern2=c(pattern2,"screenwriter[s]?/NN[S]? ([A-z]*)/(?:NN[S]?|JJ) ([A-z]*)/(?:NN|JJ)")
pattern2=c(pattern2,"cinematographer/NN(?: ,/,)? ([A-z]*)/NN ([A-z]*)/NN")
pattern2=c(pattern2,"cinematographer/NN(?: ,/,)? ([A-z]*)/NN ([A-z]*)/IN ([A-z]*)/NN")
pattern2=c(pattern2,"oscar/NN winner/NN ([A-z]*)/VBG ([A-z]*)/NNS")

allEntities = detectPatternsInTaggedCorpus(corpus, corpus.taggedText, pattern2)
allEntities[!is.na(allEntities[4]),c(1,4)]

# view the entities for a certain pattern.
Filter(Negate(is.na),allEntities[[4]])

printMatchesPerPattern(pattern2, allEntities)

entityCountPerPattern = countMatchesPerColumn(allEntities)
entityCountPerPattern

hist(entityCountPerPattern$Count)

entityCountPerFile=countMatchesPerRow(allEntities)
entityCountPerFile

hist(entityCountPerFile$Count)

# Write results to a file
write.table(allEntities, file = "allEntities.csv", row.names = F, na="", sep=";")


# Compare with a gold standard
allMatches = mergeAllMatchesInLists(allEntities)
head(allMatches)

goldStandard = read.table(file = "goldStandard.csv", quote = "", na.strings=c(""),
                          colClasses="character", sep=";")
allMatchesGold = mergeGoldStandardInLists(goldStandard)
head(allMatchesGold)

metrics2 = calculateMetrics(allMatches, allMatchesGold)
metrics2

## Metrics of pattern1
allMatches1 = mergeAllMatchesInLists(matches1)
metrics1 = calculateMetrics(allMatches1, allMatchesGold)
metrics1


