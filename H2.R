setwd('/users/mingjie/desktop/UPM/intelligent systems/Unit 4/R')
# Needed for OutOfMemoryError: Java heap space 
library(rJava)
.jinit(parameters="-Xmx4g")
# If there are more memory problems, invoke gc() after the POS tagging

# The openNLPmodels.en library is not in CRAN; it has to be installed from another repository
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")

library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)


# Auxiliary functions

# getAnnotationsFromDocument returns annotations for the text document: word, sentence, part-of-speech, and Penn Treebank parse annotations.
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  parse_annotator <- Parse_Annotator()
  y3 <- annotate(x, parse_annotator, y2)
  return(y3)  
} 


# getAnnotatedMergedDocument returns the text document merged with the annotations.
getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
} 

# getAnnotatedPlainTextDocument returns the text document along with its annotations in an AnnotatedPlainTextDocument.
getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
} 



source.pos = DirSource("./review_polarity/txt_sentoken/pos", encoding = "UTF-8")
corpus_all = Corpus(source.pos)
# Let'see the first 10 corpus.
corpus=corpus_all[0:10]

inspect(corpus[[1]])
annotations = lapply(corpus, getAnnotationsFromDocument)
# The first annotations are sentence annotations
head(annotations[[1]])

# Word annotations also are defined. They indicate where the word starts, where it ends, and the part-of-speech tag.
tail(annotations[[1]])

# create AnnotatedPlainTextDocuments that attach the annotations to the document and store the annotated corpus in another variable 
corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
corpus.tagged[[1]] 
head(corpus.tagged[[1]])
# store all the annotations inline with the text and store the annotated corpus in another variable (since we destroy the corpus metadata).
corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)
corpus.taggedText[[1]] 

doc = corpus.tagged[[1]] 
doc
# For accessing the text representation of the document.
as.character(doc)
# For accessing its words.
head(words(doc))
# For accessing its sentences.
head(sents(doc),3)
# For accessing its tagged words.
head(tagged_words(doc))
# For accessing its tagged sentences.
head(tagged_sents(doc),3)
# For accessing the parse trees of its sentences.
head(parsed_sents(doc),3)





# Hand-on 2 part

# Let's see evaluate the POS annotation manually in the first two sentences in the first corpus.
inspect(corpus_all[1])

all_doc=corpus.tagged
word_length=0
for (doc_i in all_doc){
  word_length=word_length+length(words(doc_i))
}
word_length

