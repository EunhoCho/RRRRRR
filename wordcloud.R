# Packages
library(reshape)
library(tm)
library(wordcloud)



df <- read.csv('dataframes/wordcloud_df.csv')

my_dataset_s <- split(df$text, df$location)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# -- STEP 1 : GET THE DATA
# A dataset with 5485 lines, each line has several words.
dataset=read.delim("https://raw.githubusercontent.com/TATABOX42/text-mining-in-r/master/dataset.txt", header=FALSE)

# The labels of each line of the dataset file
dataset_labels <- read.delim("https://raw.githubusercontent.com/TATABOX42/text-mining-in-r/master/labels.txt",header=FALSE)
dataset_labels <- dataset_labels[,1]
dataset_labels_p <- paste("class",dataset_labels,sep="_")
unique_labels <- unique(dataset_labels_p)

# merge documents that match certain class into a list object
dataset_s <- sapply(unique_labels,function(label) list( dataset[dataset_labels_p %in% label,1] ) )

dataset_s

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# -- STEP2 : COMPUTE DOCUMENT CORPUS TO MAKE TEXT MINING
# convert each list content into a corpus
dataset_corpus <- lapply(my_dataset_s, function(x) VCorpus(VectorSource( toString(x) ))) 

labels <- labels(my_dataset_s)
labels

# merge all documents into one single corpus
dataset_corpus_all <- dataset_corpus[[1]]
for (i in 2:8) { 
  print(dataset_corpus[[i]])
  dataset_corpus_all <- c(dataset_corpus_all, dataset_corpus[[i]]) 
}

dataset_corpus_all

# remove punctuation, numbers and stopwords
dataset_corpus_all <- tm_map(dataset_corpus_all, removePunctuation)
dataset_corpus_all <- tm_map(dataset_corpus_all, removeNumbers)
dataset_corpus_all <- tm_map(dataset_corpus_all, function(x) removeWords(x,stopwords("english")))

# compute term matrix & convert to matrix class --> you get a table summarizing the occurence of each word in each class.
document_tm <- TermDocumentMatrix(dataset_corpus_all)
document_tm_mat <- as.matrix(document_tm)
colnames(document_tm_mat) <- labels
# document_tm_clean <- removeSparseTerms(document_tm, 0.8)
document_tm_clean_mat <- as.matrix(document_tm_mat)
colnames(document_tm_mat) <- labels
document_tm_mat
# remove words in term matrix with length < 4


# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# -- STEP 3 : make the graphics !

par(family="a펜글씨B")

# Graph 1 : first top 500 discriminant words

comparison.cloud(document_tm_mat, max.words=300, random.order=FALSE,c(4,0.4), title.size=1.4, width=500, height = 500, family="서울남산체 B")


# Graph 2 : first top 2000 discriminant words
png("#102_1_comparison_cloud_top_2000_words.png", width = 480, height = 480)
comparison.cloud(document_tm_mat,max.words=2000,random.order=FALSE,c(4,0.4), title.size=1.4)
dev.off()

# Graph 3: commonality word cloud : first top 2000 common words across classes
png("#103_commonality_wordcloud.png", width = 480, height = 480)
commonality.cloud(document_tm_mat, max.words=2000, random.order=FALSE)
dev.off()
