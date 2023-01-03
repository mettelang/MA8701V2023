library(wordcloud2)
library(tm)
all = scan("https://www.math.ntnu.no/emner/TMA4315/2018h/2MLR.Rmd", what = "s")

corpus = Corpus(VectorSource(all))
corpus[[1]][1]
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("---", "bf", "boldsymbol", "will", 
                                       "include", "use", "can", "follow", "provide", "using"))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
# corpus=tm_map(corpus,stemDocument)

tdm = TermDocumentMatrix(corpus)
m = as.matrix(tdm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
dim(d)
d[1:10, ]
wordcloud2(d, shape = "cardioid", maxRotation = pi/10, minRotation = -pi/10)