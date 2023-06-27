## in this R script we do the mandatory assignment 2

# Name: Ibeh Amaka Mary
# Program: Agricultural Economics
# Student ID: 940508
# 
# Name: Haruto Honda
# Program: Agricultural Economics
# Student ID: 932059


rm(list = ls())
#create a path variable (global)



path.appstore <- "/Users/harutohonda/Documents/Hohenheim/master_3rd_semester/applied data science"

#load the data
load(paste0(path.appstore, "/final submission/1/allreviews.Rdata"))#packages
library(stringr)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(syuzhet)
library(tidyverse)
library(tidytext)
library(dplyr)
library(data.table)
library(sentimentr)


##clean my title
clean.text = function(sometext){
  #set apostrophe
  clean.tx = gsub("????T", "'", sometext)
  
  #set all letters to lowercase letters
  clean.tx  = tolower(  clean.tx)
  
  # replace n't with not
  clean.tx  = gsub("can't", "can not",   clean.tx)
  clean.tx  = gsub("wont't", "will not",   clean.tx)
  clean.tx  = gsub("n't", "not",  clean.tx)
  
  ##know what the following command means
  #removes menzions
  clean.tx  <- gsub("@\\w+", "",   clean.tx)
  #removes urls
  clean.tx  <- gsub("https?://.+", "",   clean.tx)
  #removes numbers
  clean.tx<- gsub("\\d+\\w*\\d*", "",   clean.tx)
  #removes harshtags
  clean.tx  <- gsub("#\\w+", "",   clean.tx)
  # removes all non ASCII values and emojies
  clean.tx  <- gsub("[^\x01-\x7F]", "",   clean.tx)
  #it removes punctuations
  clean.tx <- gsub("[[:punct:]]", " ",  clean.tx)
  #removes multiple lines 
  clean.tx <- gsub("\n", " ",   clean.tx)
  #removes empty space in the beginning
  clean.tx  <- gsub("^\\s+", "",   clean.tx)
  #removes empty spaces in the end
  clean.tx <- gsub("\\s+$", "",   clean.tx)
  #removes empty spaces between two words
  clean.tx  <- gsub("[ |\t]+", " ",   clean.tx)
  #replace "" with NA
  clean.tx <- replace(clean.tx, clean.tx== "", NA) 
  return (clean.tx)
}
###########################################
all.reviews$clean.title = clean.text(all.reviews$title)
all.reviews$clean.rev = clean.text(all.reviews$review)
View(all.reviews)
########################
############################## manauel work

#create data frame of negative words
negative.words <- read.csv("~/Documents/Hohenheim/master_3rd_semester/applied data science/negative_positive_words/opinion_lexicon/negative-words.txt", sep=";")
negative.words <- negative.words[-c(1:32), ]
negative.words = subset(negative.words, select = c(X))
negative.words <- negative.words %>% 
  rename("title" = "X")
negative.words$negative.words <- -1

#create data frame of positive words
positive.words <- read.csv("~/Documents/Hohenheim/master_3rd_semester/applied data science/negative_positive_words/opinion_lexicon/positive-words.txt", sep=";")
positive.words <- positive.words[-c(1:32), ]
positive.words = subset(positive.words, select = c(X))
positive.words <- positive.words %>% 
  rename("title" = "X")
positive.words$positive.words <- 1

manual_count <- merge(all.reviews,negative.words, by = 'title', all.x = TRUE ) 
manual_count <- merge(manual_count,positive.words, by = 'title',all.x = TRUE ) 
table(manual_count$negative.words)
table(manual_count$positive.words )

manual_count$count <- rowSums(manual_count[c("negative.words", "positive.words")], na.rm =TRUE)
table(manual_count$count)
# -1    0    1 
# 12 1479    9 

#####################
###using other means to check
### using syuzhet
######################
#syuzhet
vector_score = all.reviews$clean.rev
syuzhet_score = get_sentiment(vector_score, method = "syuzhet")
table(syuzhet_score)

#bing
vector_score2 = all.reviews$clean.rev
bing_score = get_sentiment(vector_score, method = "bing")
table(bing_score)

#afinn
vector_score3 = all.reviews$clean.rev
afinn_score = get_sentiment(vector_score3, method = "afinn")
table(afinn_score)

##################question 1
### Is the rating score and the review tone consistent?
##get ratings
cov_rat= as.numeric(all.reviews$rating)

cor(syuzhet_score, cov_rat)
###  0.3861637  ###weak corrolation

cor(bing_score, cov_rat) 
#####  ###moderate corrolation   0.5100876

cor(afinn_score, cov_rat)
##### 0.5274049  ####moderate corrolation

####result will be baised useing manual becuse positive and negative has low numbers. decide to use
###bing, now i have to add it to the data.
####################################


########################
##task 2
##
########################
#Does a review with an extreme rating of 1 (5) list more negative (positive) words than
#a review with a more moderate rating of 2 (4)?

nrc = get_sentiment(all.reviews$clean.rev, method = "bing") # making the sentiment score
new_nrc = cbind(all.reviews, nrc)

table(new_nrc$nrc)

new_nrc = cbind(all.reviews, nrc)

#convert all positive to 1 and negative to -1
new_nrc$nrc.review_original <- new_nrc$nrc
new_nrc["nrc"][new_nrc["nrc"] > 0] <-1
new_nrc["nrc"][new_nrc["nrc"] < 0] <- -1
table(new_nrc$nrc)

#renaming
new_nrc <- new_nrc %>% 
  rename("score_review" = "nrc")

#create data frame for graph
new_nrc$N <- 1
q2 <- aggregate(N ~ rating + score_review, data = new_nrc, FUN = sum, na.rm = TRUE)

q2 <- q2 %>% 
  rename("Review" = "score_review")

jpeg("Rplot1.jpeg")
# 2. Create the plot
ggplot(q2, aes(x =Review , y = N, fill = rating))+
  geom_col(position = "dodge")
# 3. Close the file
dev.off()

q2 <- q2 %>% 
  rename("score_review" = "Review")

##########################################
## Is the sentiment the same in the title and the actual text of a review?

new_nrc$nrc2 = get_sentiment(all.reviews$clean.title, method = "bing")

#replacing all positive to 1 and negative to -1 for title 
new_nrc$nrc.title_original <- new_nrc$nrc2
new_nrc <- new_nrc %>% 
  rename("score_title" = "nrc2")
new_nrc["score_title"][new_nrc["score_title"] > 0] <-1
new_nrc["score_title"][new_nrc["score_title"] < 0] <- -1

table(new_nrc$score_title, new_nrc$score_review)


##### Is there a difference in rating tone among countries?

jpeg("Rplot2.jpeg")
# 2. Create the plot
boxplot(new_nrc$nrc.review_original~new_nrc$country,
        xlab="Country",
        ylab="Rating tone",
        col="orange",
        border="brown"
)
# 3. Close the file
dev.off()


######Does the sentiment vary over time?

new_nrc <- aggregate(new_nrc$nrc.review_original,list(new_nrc$date),mean)
new_nrc
jpeg("Rplot3.jpeg")
plot(new_nrc$Group.1,                          
     new_nrc$x,
     type = "l",
     col = 2,
     ylim = c(-1, 3),
     xlab = "Date",
     ylab = "Sentiment score")
dev.off()
#########################################
#additional analysis 
nrc_sentiment = get_nrc_sentiment(all.reviews$clean.rev)
sentisum = colSums(nrc_sentiment)
sentisum
jpeg("Rplot4.jpeg")
barplot(sentisum[1:10],col=rainbow(10), cex.names = 0.8, cex.axis = 0.8,
        las = 2, ylab = "N")
dev.off()
###################################
######trying to check the sentiments for the features in sanpchat


nrc = get_sentiment(all.reviews$clean.rev, method = "bing") # making the sentiment score
new_nrc = cbind(all.reviews, nrc)

table(new_nrc$nrc)

new_nrc = cbind(all.reviews, nrc)

#convert all positive to 1 and negative to -1
new_nrc$nrc.review_original <- new_nrc$nrc
new_nrc["nrc"][new_nrc["nrc"] > 0] <-1
new_nrc["nrc"][new_nrc["nrc"] < 0] <- -1
table(new_nrc$nrc)

kk = new_nrc %>%
  mutate(clean.rev = stringr::str_extract_all(new_nrc$clean.rev,
                                              "(bitmoji)|(filter)|(ads)|(stickers)|(snap map)|(avatar)|(memories)|(video)|(audio)")) %>%
  tidyr::unnest(cols = c(clean.rev))


kk2 = kk %>%
  select(clean.rev,nrc)

word.freq.wof = data.frame(table(kk2))

setnames(setDT(word.freq.wof, keep.rownames = FALSE), c(1:3),c("review_text_features","seniment_score","frequency"))

###plot a graph
word.freq.wof$frequency <- word.freq.wof$freq

q3 <- aggregate(frequency ~ review_text_features + seniment_score, data = word.freq.wof, FUN = sum, na.rm = TRUE)
q3

ggplot(q3, aes(x =seniment_score , y = frequency, fill = review_text_features))+
  geom_col(position = "dodge")

jpeg("Rplot5.jpeg")
# 2. Create the plot
ggplot(q3, aes(x =seniment_score , y = frequency, fill = review_text_features))+
  geom_col(position = "dodge")
# 3. Close the file
dev.off()
