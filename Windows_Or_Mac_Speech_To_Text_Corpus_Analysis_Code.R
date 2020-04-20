#####################
# Loading libraries #
#####################
library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)

###########################################
# Loading lexicons for sentiment analysis #
###########################################
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

####################
# Loading the data #
####################
survey <- read_document(file="Survey Answers Group 6.txt")
class_combo <- c(survey)

a <- 27
b <- 6
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- class_combo[i*b+z-b]
  }#closing z loop
}#closing i loop

###################################################
# Taking one question at a time and tokenizing it #
###################################################

##############
# QUESTION 1 #
##############
############################################################################
# Would you buy a Mac or a Windows laptop? (Please choose one preference.) #
############################################################################

###############
# Step 1: taking my 6 questions and saving as my_txt
my_txt1 <- my_df$V1

###############
# Step 2: I have the variable with one answer, I want to add location information to it - representing the index of the person who responded and that adding my 6 answers in my_txt
mydf1 <- data_frame(line=1:a, text=my_txt1)
print(mydf1)

###############
# Step 3: tokenizing + anti_join
data(stop_words)
frequencies_tokens_nostop_q1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort=T)
print(frequencies_tokens_nostop_q1)

# removing words that do not include brand
my_junk1 <- data_frame(word = c("laptop", "money", "buy"),
                       lexicon = rep("junk", each=3))

###############
# Step 4: token frequency histograms
frequencies_tokens_nostop1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(my_junk1)%>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostop1)

# This graph in PP  
Q1_graph <- frequencies_tokens_nostop1%>%
  mutate(word= reorder(word, n)) %>%
  top_n(10)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
Q1_graph

###############
# Step 5: sentiment analysis
# Q1 sentiment makes no sense

#Q2
# Please explain, why would you buy Mac/Windows laptop 
# instead of Windows/Mac laptop. 
########################################################
#############################################################

my_txt2 <- my_df$V2

mydf2 <- data_frame(line=1:a, text=my_txt2)
print(mydf2)

# remove words that do not explain purpose
my_junk2 <- data_frame(
  word = c("windows", "mac","laptop", "apple", "buy", "laptops"),
  lexicon = rep("junk", each=6)
)
# FREQUENCY
frequencies_tokens_nostop2 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk2) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostop2)

# useless cloud
#frequencies_tokens_nostop2%>%
#  with(wordcloud(word, n, max.words = 10))

# NRC is not relevant, bing is better
Q2_nrc <- frequencies_tokens_nostop2  %>%
  inner_join(nrc)
my_cloud_Q2 <- frequencies_tokens_nostop2%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# This bing graph shows positive and negative sentiment in PP
Q2_bing_count <- frequencies_tokens_nostop2 %>%
  inner_join(bing)# %>%
  #count(word, sentiment, sort=T) %>%
  #ungroup()#%>%
  #count(sentiment)

Q2_bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#N-grams and tokenizing
# BIGRAM question 2 
bigram_q2 <- mydf2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
bigram_q2

bigram_q2 %>%
  count(bigram, sort = TRUE)

bigram_q2_separated <- bigram_q2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_q2_filtered <- bigram_q2_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_q2_counts <- bigram_q2_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_q2_counts
View(bigram_q2_counts)

# visualizing the bigram network
bigram_q2_graph <- bigram_q2_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()
bigram_q2_graph

ggraph(bigram_q2_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) 

# QUADROGRAM question 2 
quadrogram_q2 <- mydf2 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 
quadrogram_q2

# visualising negated words in sentiment analysis 
negation_tokens_q2 <- c("no", "never", "without", "not") 

negated_words_q2 <- bigram_q2_separated %>%
  filter(word1 %in% negation_tokens_q2) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
negated_words_q2

#Q3
# What do you use your laptop for? 
########################################################
#############################################################
my_txt3 <- my_df$V3

mydf3 <- data_frame(line=1:a, text=my_txt3)
print(mydf3)

my_junk3 <- data_frame(word = c("laptop"),
  lexicon = rep("junk", each=1))
# FREQUENCY
frequencies_tokens_nostop3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk3) %>%
  count(word, sort=TRUE)

print(frequencies_tokens_nostop3)

freq_hist_q3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word,n)) %>% # IMPORTANT FUNCTION, without this, we would have the order random, this way it is sorted by the most frequent to less frequent
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_q3)

# exceptionaly in this case is not probably useless and
# therefore it is in PP
frequencies_tokens_nostop3 %>%
  with(wordcloud(word, n, max.words = 30))

# NRC for me makes no sence it is not about feelings
# but the way they use it
Q3_nrc <- frequencies_tokens_nostop3 %>%
  inner_join(nrc)

my_cloud_Q3 <- frequencies_tokens_nostop3 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, fixed.asp=TRUE, scale=c(0.6,0.6), 
                   title.size=1, rot.per=0.25)

# also make no sense not big vocabulary enough

Q3_bing <- frequencies_tokens_nostop3 %>%
  inner_join(afinn)# %>%
#count(word, sentiment, sort=T) %>%
#ungroup()#%>%
#count(sentiment)
Q3_bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# N-grams and tokenizing

# BIGRAM question 3
bigram_q3 <- mydf3 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
bigram_q3

bigram_q3 %>%
  count(bigram, sort = TRUE)

bigram_q3_separated <- bigram_q3 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_q3_filtered <- bigram_q3_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_q3_counts <- bigram_q3_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_q3_counts
View(bigram_q3_counts)

# visualizing the bigram network
bigram_q3_graph <- bigram_q3_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()
bigram_q3_graph

ggraph(bigram_q3_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) 

# QUADROGRAM question 3
quadrogram_q3 <- mydf3 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 
quadrogram_q3

# visualising negated words in sentiment analysis 
negation_tokens_q3 <- c("no", "never", "without", "not") 

negated_words_q3 <- bigram_q3_separated %>%
  filter(word1 %in% negation_tokens_q3) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
negated_words_q3

#Q4
# What brand is your phone and why did you choose the brand 
# you are currently using?
########################################################
#############################################################
my_txt4 <- my_df$V4

mydf4 <- data_frame(line=1:a, text=my_txt4)
print(mydf4)

my_junk4 <- data_frame(word = c("phone"),
                       lexicon = rep("junk", each=1))
# FREQUENCY
frequencies_tokens_nostop4 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk4)%>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostop4)

# This graph in PP  
Q4_graph <- frequencies_tokens_nostop4%>%
  mutate(word= reorder(word, n)) %>%
  top_n(10)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(Q4_graph)

# AFINN is interesting here can express reason why people 
# have their phones, in PP
Q4_afinn <- frequencies_tokens_nostop4%>%
  inner_join(afinn)
Q4_afinn %>%
  group_by(value) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# not very impactful if everything is in the middle
my_cloud_Q4 <- frequencies_tokens_nostop4 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, fixed.asp=TRUE, scale=c(0.6,0.6), 
                   title.size=1, rot.per=0.25)

#N-grams and tokenizing
# BIGRAM question 4 
bigram_q4 <- mydf4 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
bigram_q4

bigram_q4 %>%
  count(bigram, sort = TRUE)

bigram_q4_separated <- bigram_q4 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_q4_filtered <- bigram_q4_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_q4_counts <- bigram_q4_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_q4_counts
View(bigram_q4_counts)

# visualizing the bigram network
bigram_q4_graph <- bigram_q4_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()
bigram_q4_graph

ggraph(bigram_q4_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) 

# QUADROGRAM question 4
quadrogram_q4 <- mydf4 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 
quadrogram_q4

# visualising negated words in sentiment analysis 
negation_tokens_q4 <- c("no", "never", "without", "not") 

negated_words_q4 <- bigram_q4_separated %>%
  filter(word1 %in% negation_tokens_q4) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
negated_words_q4

#Q5
# How/What for do you use your phone most frequently?
########################################################
#############################################################
my_txt5 <- my_df$V5

mydf5 <- data_frame(line=1:a, text=my_txt5)
print(mydf5)

my_junk5 <- data_frame(word = c("phone"),
                       lexicon = rep("junk", each=1))
# FREQUENCY
frequencies_tokens_nostop5 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk5) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostop5)

freq_hist_q5 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word,n)) %>% # IMPORTANT FUNCTION, without this, we would have the order random, this way it is sorted by the most frequent to less frequent
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_q5)

# exceptionaly in this case is not probably useless and
# therefore it is in PP
frequencies_tokens_nostop5 %>%
  with(wordcloud(word, n, max.words = 30))

# only inpact is a fun, not relevant
Q5_afinn <- frequencies_tokens_nostop5 %>%
  inner_join(afinn)
Q5_afinn %>%
  group_by(value) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

# not insightful
my_cloud_Q5 <- frequencies_tokens_nostop5 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, fixed.asp=TRUE, scale=c(0.6,0.6), 
                   title.size=1, rot.per=0.25)

#N-grams and tokenizing
# BIGRAM question 5
bigram_q5 <- mydf5 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
bigram_q5

bigram_q5 %>%
  count(bigram, sort = TRUE)

bigram_q5_separated <- bigram_q5 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_q5_filtered <- bigram_q5_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_q5_counts <- bigram_q5_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_q5_counts
View(bigram_q5_counts)

# visualizing the bigram network
bigram_q5_graph <- bigram_q5_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()
bigram_q5_graph

ggraph(bigram_q5_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# QUADROGRAM question 5
quadrogram_q5 <- mydf_5 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 
quadrogram_q5

# visualising negated words in sentiment analysis 
negation_tokens_q5 <- c("no", "never", "without", "not") 

negated_words_q5 <- bigram_q5_separated %>%
  filter(word1 %in% negation_tokens_q5) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
negated_words_q5

#Q6
# Please tell us about your professional background
#############################################################
my_txt6 <- my_df$V6

mydf6 <- data_frame(line=1:a, text=my_txt6)
print(mydf6)

my_junk6 <- data_frame(word = c("background", "international"),
                       lexicon = rep("junk", each=2))
# FREQUENCY
frequencies_tokens_nostop6 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk6)%>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostop6)

# what prefessions are frequent in PP
Q6_graph <- frequencies_tokens_nostop6%>%
  mutate(word= reorder(word, n)) %>%
  top_n(10)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(Q6_graph)

Q6_nrc <- frequencies_tokens_nostop6 %>%
  inner_join(nrc)

my_cloud_Q6 <-frequencies_tokens_nostop6 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# N-grams and tokenizing
# BIGRAM question 6
bigram_q6 <- mydf6 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
bigram_q6

bigram_q6 %>%
  count(bigram, sort = TRUE)

bigram_q6_separated <- bigram_q6 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_q6_filtered <- bigram_q6_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_q6_counts <- bigram_q6_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_q6_counts
View(bigram_q6_counts)

# visualizing the bigram network
bigram_q6_graph <- bigram_q6_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()
bigram_q6_graph

ggraph(bigram_q6_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) 

# QUADROGRAM question 6
quadrogram_q6 <- mydf_6 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 
quadrogram_q6

# visualising negated words in sentiment analysis 
negation_tokens_q6 <- c("no", "never", "without", "not") 

negated_words_q6 <- bigram_q6_separated %>%
  filter(word1 %in% negation_tokens_q6) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
negated_words_q6

#########################################################
# Combining the six questions and plotting correlograms # 
#########################################################
# We want to see if using a specific phone influence the choice of a laptop (Q2 & Q4)
# We want to see if people are using phones and laptops for similar or different purposes (Q3 & Q5)
# tidy q1
tidy_q1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_q1

#tidy q2
tidy_q2 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_q2

#tidy q3
tidy_q3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_q3

#tidy q4
tidy_q4 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_q4

#tidy q5
tidy_q5 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_q5

#tidy q6
tidy_q6 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_q6

frequency <- bind_rows(mutate(tidy_q1, df= "Question 1"),
                       mutate(tidy_q2, df= "Question 2"),
                       mutate(tidy_q3, df= "Question 3"),
                       mutate(tidy_q4, df= "Question 4"),
                       mutate(tidy_q5, df= "Question 5"),
                       mutate(tidy_q6, df= "Question 6")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(df, word) %>%
  group_by(df) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(df, proportion) %>%    
  gather(df, proportion, `Question 3`)

View(frequency)

#############################
# Plotting the correlograms #
#############################

ggplot(frequency, aes(x=proportion, y=`Question 5`, 
                      color = abs(`Question 5`- proportion)))+
  geom_abline(color="gray40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), 
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~df, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Question 5", x=NULL)

# Doing the cor.test()
cor.test(data=frequency[frequency$df == "Question 3",],
         ~proportion + `Question 5`)

### Questions 2 + 4
frequency <- bind_rows(mutate(tidy_q1, df= "Question 1"),
                       mutate(tidy_q2, df= "Question 2"),
                       mutate(tidy_q3, df= "Question 3"),
                       mutate(tidy_q4, df= "Question 4"),
                       mutate(tidy_q5, df= "Question 5"),
                       mutate(tidy_q6, df= "Question 6")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(df, word) %>%
  group_by(df) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(df, proportion) %>%    
  gather(df, proportion, `Question 4`)

View(frequency)

#############################
# Plotting the correlograms #
#############################

ggplot(frequency, aes(x=proportion, y=`Question 2`, 
                      color = abs(`Question 2`- proportion)))+
  geom_abline(color="gray40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), 
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~df, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Question 2", x=NULL)

# Doing the cor.test()
cor.test(data=frequency[frequency$df == "Question 4",],
         ~proportion + `Question 2`)

################################################################
# All dataset, tokenization, frequency, word cloud, tf_idf
################################################################

# puting all questions together

all_Q <- bind_rows(mutate(mydf1, Question = "Q1"),
                   mutate(mydf2, Question = "Q2"),
                   mutate(mydf3, Question = "Q3"),
                   mutate(mydf4, Question = "Q4"),
                   mutate(mydf5, Question = "Q5"),
                   mutate(mydf6, Question = "Q6"))

all_junk <- data_frame(word = c("laptop", "phone", "buy",
                                "background", "international"),
                       lexicon = rep("junk", each=5))
# tokenizing and frequency
all_Q_token_nostop_frequency <- all_Q %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(all_junk)%>%
  count(word, sort = T)

  
# showing histogram with top 10 word frequencies
all_Q_frequency_graph <- all_Q_token_nostop_frequency %>%
  #filter(n > 4) %>% 
  mutate(word= reorder(word, n)) %>%
  top_n(10)%>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(all_Q_frequency_graph)

# showing of frequency cloud useless
all_Q_token_nostop_frequency %>%
  with(wordcloud(word, n, max.words = 100))

##############################################################
# ALL SENTIMENT

# in general positive survey in PP
all_nrc <- all_Q_token_nostop_frequency %>%
  inner_join(nrc)

all_Q_sentiment_cloud <- all_Q_token_nostop_frequency%>%
  inner_join(nrc) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, fixed.asp=TRUE, scale=c(0.6,0.6), 
                   title.size=1, rot.per=0.25)


###################################################
################# TF_IDF ##########################
###################################################
together_Q <- bind_rows(mutate(frequencies_tokens_nostop1, Question = "Q1"),
                   mutate(frequencies_tokens_nostop2, Question = "Q2"),
                   mutate(frequencies_tokens_nostop3, Question = "Q3"),
                   mutate(frequencies_tokens_nostop4, Question = "Q4"),
                   mutate(frequencies_tokens_nostop5, Question = "Q5"),
                   mutate(frequencies_tokens_nostop6, Question = "Q6"))


total_words <- together_Q %>%
  group_by(Question) %>%
  summarize(total=sum(n))

Question_words <- left_join(together_Q, total_words)

print(Question_words)

# proportion and contribution into data, more about data set 
# no insight
ggplot(Question_words, aes(n/total, fill = Question))+
  geom_histogram(show.legend=FALSE)+
  facet_wrap(~Question, ncol=2, scales="free_y")

tf_words <- Question_words %>%
  bind_tf_idf(word, Question, n)
tf_words 

tf_words %>%
  arrange(desc(tf_idf))

#TF IDF graph for all dataset
tf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  coord_flip()

#############
# tf_idf for each question separately in PP
tf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(Question) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~Question, ncol=3, scales="free")+
  coord_flip()


# Class 5 LDA analysis
####################################################################
library(topicmodels)

# preparation DTM format
all_Q_DTM <- all_Q %>%
  group_by(line)%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  #anti_join(all_junk)%>%
  count(word, sort=T)%>%
  cast_dtm(line, word, n)

Q_LDA <- LDA(all_Q_DTM, k=2, control=list(seed=123))

# very small beta probabilities therefore not very insightful
Q_topics <- tidy(Q_LDA, matrix="beta")
Q_topics

# top terms look very similar not very insightful
top_terms <- Q_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic not insightful
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# in PP 2 different topics

# Topic 1
beta_spread_topic1 <- Q_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1))%>%
  arrange(log_rate)
beta_spread_topic1
View(beta_spread_topic1)

# Topic 2
beta_spread_topic2 <- Q_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1))%>%
  arrange(desc(log_rate))
beta_spread_topic2

# 27 documents which represents individual students
# can we do a plot or histogram? per topic?
my_gamma <- tidy(Q_LDA, matrix= "gamma")

my_gamma <- my_gamma %>%
  separate(document, c("respondent"),sep = "_", convert=TRUE)
my_gamma

# two topics boxplot not insightful
my_gamma %>%
  mutate(document=reorder(respondent, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot()+
  facet_wrap(~topic)

#my_gamma %>%
  #filter(gamma>0.5)%>%
  #count(topic)%>%
  #ggplot(aes(factor(topic), gamma)) +
  #geom_boxplot()+
  #facet_wrap(~topic)

my_classifications <- my_gamma %>%
  group_by(respondent) %>%
  top_n(1, gamma) %>%
  ungroup()

my_classifications
View(my_classifications)


# Unsupervised model please insert into PP
############################################################################
supervisedtxt <- unite(my_df, text, sep = " ", remove = TRUE, na.rm = FALSE)

supervisedtxt <- supervisedtxt %>%
  mutate(id = row_number())

supervised_dfm <- supervisedtxt %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  cast_dfm(id, word, n)

supervised_dfm.train <- supervised_dfm[1:20,]
supervised_dfm.test <- supervised_dfm[20:27,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(supervised_dfm.train, c(1,0,0,0,1,1,0,0,1,1,0,0,0,0,1,0,1,1,0,0))
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, supervised_dfm.test)
pred
