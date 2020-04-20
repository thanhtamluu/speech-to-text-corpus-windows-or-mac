#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Loading libraries
#######################################################################
library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)
library(igraph)
library(ggraph)
library(scales)
library(topicmodels)
library(quanteda)
library(RColorBrewer)





# Loading lexicons for sentiment analysis
#######################################################################
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

# Loading data from txt
#######################################################################
survey <- read_document(file="./Survey Answers Group 6.txt")
class_combo <- c(survey)

a <- 27
b <- 6
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
    for(i in 1:a){
        my_df[i,z]<- class_combo[i*b+z-b]
    }#closing z loop
}#closing i loop

############################################################
##### ANALYSIS OF DATA ################################
############################################################



#Q1
# Would you buy a Mac or a Windows laptop?
########################################################
#############################################################

my_txt1 <- my_df$V1

mydf1 <- data_frame(line=1:a, text=my_txt1)
# print(mydf1)

# remove words that do not include brand
my_junk1 <- data_frame(word = c("laptop", "money", "buy"),
                       lexicon = rep("junk", each=3))

# FREQUENCY
frequencies_tokens_nostop1 <- mydf1 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% #here's where we remove tokens
    anti_join(my_junk1)%>%
    count(word, sort=TRUE)
# print(frequencies_tokens_nostop1)


my_txt4 <- my_df$V4

mydf4 <- data_frame(line=1:a, text=my_txt4)
# print(mydf4)

my_junk4 <- data_frame(word = c("phone"),
                       lexicon = rep("junk", each=1))
# FREQUENCY
frequencies_tokens_nostop4 <- mydf4 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(my_junk4)%>%
    count(word, sort=TRUE)
# print(frequencies_tokens_nostop4)



my_txt2 <- my_df$V2

mydf2 <- data_frame(line=1:a, text=my_txt2)
# print(mydf2)

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
# print(frequencies_tokens_nostop2)



# This bing graph positive negative in PP
Q2_bing_count <- frequencies_tokens_nostop2 %>%
    inner_join(bing)# %>%
#count(word, sentiment, sort=T) %>%
#ungroup()#%>%
#count(sentiment)



my_txt3 <- my_df$V3

mydf3 <- data_frame(line=1:a, text=my_txt3)
# print(mydf3)

my_junk3 <- data_frame(word = c("laptop"),
                       lexicon = rep("junk", each=1))
# FREQUENCY
frequencies_tokens_nostop3 <- mydf3 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(my_junk3) %>%
    count(word, sort=TRUE)

Q4_afinn <- frequencies_tokens_nostop4%>%
    inner_join(afinn)


my_txt5 <- my_df$V5

mydf5 <- data_frame(line=1:a, text=my_txt5)
# print(mydf5)

my_junk5 <- data_frame(word = c("phone"),
                       lexicon = rep("junk", each=1))
# FREQUENCY
frequencies_tokens_nostop5 <- mydf5 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(my_junk5) %>%
    count(word, sort=TRUE)
# print(frequencies_tokens_nostop5)



################################################
my_txt6 <- my_df$V6

mydf6 <- data_frame(line=1:a, text=my_txt6)
# print(mydf6)

my_junk6 <- data_frame(word = c("background", "international"),
                       lexicon = rep("junk", each=2))
# FREQUENCY
frequencies_tokens_nostop6 <- mydf6 %>%
    unnest_tokens(word, text) %>%
    anti_join(my_junk6)%>%
    anti_join(stop_words) %>%
    count(word, sort=TRUE)
# print(frequencies_tokens_nostop6)







all_Q <- bind_rows(mutate(mydf1, Question = "Q1"),
                   mutate(mydf2, Question = "Q2"),
                   mutate(mydf3, Question = "Q3"),
                   mutate(mydf4, Question = "Q4"),
                   mutate(mydf5, Question = "Q5"),
                   mutate(mydf6, Question = "Q6"))

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
# Q_topics





my_gamma <- tidy(Q_LDA, matrix= "gamma")




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


tf_words <- Question_words %>%
    bind_tf_idf(word, Question, n)




bigram_q2 <- mydf2 %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
# bigram_q2

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
# bigram_q2_counts


negation_tokens_q2 <- c("no", "never", "without", "not") 

negated_words_q2 <- bigram_q2_separated %>%
    filter(word1 %in% negation_tokens_q2) %>%
    inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
    count(word1, word2, value, sort=TRUE) %>%
    ungroup()
# negated_words_q2

bigram_q3 <- mydf3 %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
# bigram_q3

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
# bigram_q3_counts




quadrogram_q3 <- mydf3 %>%
    unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
    separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    filter(!word4 %in% stop_words$word) 
# quadrogram_q3






# tidy q1
tidy_q1 <- mydf1 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
# tidy_q1

#tidy q2
tidy_q2 <- mydf2 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
# tidy_q2

#tidy q3
tidy_q3 <- mydf3 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
# tidy_q3

#tidy q4
tidy_q4 <- mydf4 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
# tidy_q4

#tidy q5
tidy_q5 <- mydf5 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
# tidy_q5

#tidy q6
tidy_q6 <- mydf6 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
# tidy_q6


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


bigram_q4 <- mydf4 %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
# bigram_q4

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


quadrogram_q4 <- mydf4 %>%
    unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
    separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    filter(!word4 %in% stop_words$word) 






bigram_q5 <- mydf5 %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
# bigram_q5

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
# bigram_q5_counts




bigram_q6 <- mydf6 %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
# bigram_q6

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
# bigram_q6_counts


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
# NB_classifier
# summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, supervised_dfm.test)
# pred

all_Q <- bind_rows(mutate(mydf1, Question = "Q1"),
                   mutate(mydf2, Question = "Q2"),
                   mutate(mydf3, Question = "Q3"),
                   mutate(mydf4, Question = "Q4"),
                   mutate(mydf5, Question = "Q5"),
                   mutate(mydf6, Question = "Q6"))



my_gamma <- tidy(Q_LDA, matrix= "gamma")


my_classifications <- my_gamma %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup()

my_classifications
# View(my_classifications)

my_gamma <- tidy(Q_LDA, matrix= "gamma")

my_gamma <- my_gamma %>%
    separate(document, c("respondent"),sep = "_", convert=TRUE)
# my_gamma












# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$questions <- renderPrint({
        
        if(input$questions == "Q1"){
            print("Would you buy a Mac or a Windows laptop?")
        } 
        
        if(input$questions == "Q2"){
            print("Please explain, why would you buy Mac/Windows laptop instead of Wind)Mac laptop.")
        }
        
        if(input$questions == "Q3"){
            print("What do you use your laptop for?")
        }
        
        if(input$questions == "Q4"){
            print("What brand is your phone and why did you choose the brand you are cu)tly using? ")
        }
        
        
        if(input$questions == "Q5"){
            print("How/What for do you use your phone most frequently?")
        }
        
        if(input$questions == "Q6"){
            print("Please tell us about your professional background.")
        }
        
        
        output$questionss <- renderPlot({
            print(input$questions)
            if( input$questions == "Q1" ){
                
                
                # This graph in PP
               aa <- frequencies_tokens_nostop1%>%
                    mutate(word= reorder(word, n)) %>%
                    top_n(10)%>%
                    ggplot(aes(word, n))+
                    geom_col()+
                    xlab(NULL)+
                    coord_flip()
               print(aa)
                
            }
            if(input$questions == "Q2"){
                
               
            q2 <- Q2_bing_count %>%
                    group_by(sentiment) %>%
                    top_n(10) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
            print(q2)
                
            }
           
            if(input$questions == "Q3"){
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
            }
            
            
            
            
            
            if(input$questions == "Q5"){
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
            }
            
            if(input$questions == "Q6"){
            
            # what prefessions are frequent in PP
            Q6_graph <- frequencies_tokens_nostop6%>%
                mutate(word= reorder(word, n)) %>%
                top_n(10)%>%
                ggplot(aes(word, n))+
                geom_col()+
                xlab(NULL)+
                coord_flip()
            print(Q6_graph)
            
            }
        })
        
        output$model <- renderPlot({
            my_gamma %>%
                filter(gamma > 0.5) %>%
                count(topic) %>%
                ggplot(aes(x = topic,y = n))+
                geom_bar(stat = "identity")+
                theme_economist()
            
        })
        
    })
    
    
    
    # Generate a summary of the dataset
    output$all_qestions <- renderPrint({
    
        print("Q1")
        print("Would you buy a Mac or a Windows laptop?")
        
        print("Q2")
        print("Please explain, why would you buy Mac/Windows laptop instead of Wind)Mac laptop.")
        
        print("Q3")
        print("What do you use your laptop for?")
        
        print("Q4")
        print("What brand is your phone and why did you choose the brand you are cu)tly using? ")
        
        print("Q5")
        print("How/What for do you use your phone most frequently?")
        
        print("Q6")
        print("Please tell us about your professional background.")
        
           
        
        
    })
    
    
    output$tf_idf <- renderPlot({
        
        # tf_idf for each question separately in PP
        tf_words %>%
            arrange(desc(tf_idf)) %>%
            mutate(word=factor(word, levels=rev(unique(word)))) %>%
            group_by(Question) %>%
            top_n(input$sliderinput_td_idf) %>%
            ungroup %>%
            ggplot(aes(word, tf_idf, fill=Question))+
            geom_col(show.legend=FALSE)+
            labs(x=NULL, y="tf-idf")+
            facet_wrap(~Question, ncol=3, scales="free")+
            coord_flip()
        
    })
    output$q1 <- renderPlot({

        frequencies_tokens_nostop1%>%
            mutate(word= reorder(word, n)) %>%
            top_n(10)%>%
            ggplot(aes(word, n))+
            ggtitle("More people would buy a windows laptop")+
            geom_col()+
            xlab(NULL)+
            coord_flip()
    })
    output$q4 <- renderPlot({
        # This graph in PP  
        Q4_graph <- frequencies_tokens_nostop4%>%
            mutate(word= reorder(word, n)) %>%
            top_n(10)%>%
            ggplot(aes(word, n))+
            ggtitle("Most students use an iPhone")+
            geom_col()+
            xlab(NULL)+
            coord_flip()
        print(Q4_graph)
    })

    
    
    output$q2 <- renderPlot({
        
        
        
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
    })
    
    output$q3 <- renderPlot({
        ###################
        
        
        
        # exceptionaly in this case is not probably useless and
        # therefore it is in PP
        frequencies_tokens_nostop3 %>%
            with(wordcloud(word, n, max.words = 30))
    
    })
    
    
    
    
    output$q4_afinn <- renderPlot({
        aasdsdswde <-Q4_afinn %>%
            group_by(value) %>%
            top_n(10) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=value)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~value, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        print(aasdsdswde)
    
    })
    
    
    
    
    
    
    output$q5 <- renderPlot({
    
    # exceptionaly in this case is not probably useless and
    # therefore it is in PP
    frequencies_tokens_nostop5 %>%
        with(wordcloud(word, n, max.words = 30))
        
        
        
    })
    
    
    
    output$q6 <- renderPlot({
            
        # what prefessions are frequent in PP
        Q6_graph <- frequencies_tokens_nostop6%>%
            mutate(word= reorder(word, n)) %>%
            top_n(10)%>%
            ggplot(aes(word, n))+
            geom_col()+
            xlab(NULL)+
            coord_flip()
        print(Q6_graph)
    })
    
    
    # Generate a summary of the dataset
    output$beta_spread_topic1 <- renderTable({
        # Topic 1
        beta_spread_topic1 <- Q_topics %>%
            mutate(topic=paste0("topic", topic)) %>%
            spread(topic, beta) %>%
            filter(topic1>.001 | topic2 >0.001) %>%
            mutate(log_rate = log2(topic2/topic1))%>%
            arrange(log_rate)
            # anti_join(term = c("technical"))
        head(beta_spread_topic1,10)
        
        # summary(beta_spread_topic1)
    })
    
    # Generate a summary of the dataset
    output$beta_spread_topic2 <- renderTable({
        
        # Topic 2
        beta_spread_topic2 <- Q_topics %>%
            mutate(topic=paste0("topic", topic)) %>%
            spread(topic, beta) %>%
            filter(topic1>.001 | topic2 >0.001) %>%
            mutate(log_rate = log2(topic2/topic1))%>%
            arrange(desc(log_rate))
        head(beta_spread_topic2,10)
        
        # summary(beta_spread_topic2)
    })
    
    
    output$qgamma21312312 <- renderPlot({
    
        my_gamma %>%
            filter(gamma > 0.5) %>%
            count(topic) %>%
            ggplot(aes(x = topic ,y = n))+
            geom_bar(stat = "identity")
    
    })
    
    # # A plot of fixed size
    # output$image_001 <- renderImage({
    #     # Return a list
    #     list(src = './Windows-Vs-Mac-1024x525.jpg',
    #          width = '100%',
    #          height = '100%',
    #          alt = "This is alternate text")
    # }, deleteFile = FALSE)
    
    
    output$hearr <- renderText({
        
        if( input$questions == "Q1" ){
            print("56% of respondents would buy a Windows laptop")
        }
        if( input$questions == "Q2" ){
            print("Ease of usage, performance, and price play a big role when choosing a laptop")
        }
        if( input$questions == "Q3" ){
            print("The way laptops are used does not require specific brand connected featuress")        
        }
        if( input$questions == "Q4" ){
            print("A majority of the respondents own iPhones")        
        }
        if( input$questions == "Q5" ){
            print("The way phones are used does not require specific brand connected features")        
        }
        if( input$questions == "Q6" ){
            print("A majority of respondents come from a technical background")        
        }
    })
    
    output$bigram_q2_counts <- renderTable({
        
        if( input$questions == "Q2" ){
            bigram_q2_counts
        }
        if( input$questions == "Q3" ){
            bigram_q3_counts
        }
        
        if( input$questions == "Q4" ){
            bigram_q4_counts
        }
        if( input$questions == "Q5" ){
            bigram_q5_counts
        }
        if( input$questions == "Q6" ){
            bigram_q6_counts
        }
    })
    
    output$negated_words_q2 <- renderTable({
        
        if( input$questions == "Q2" ){
            negated_words_q2
        }
        if( input$questions == "Q3" ){
            quadrogram_q3
        }
        
        
        if( input$questions == "Q4" ){
            quadrogram_q4
        }
        
        
        
        
    })
    
    output$q4_left <- renderPlot({
        
        if( input$questions == "Q4" ){
            
            # This graph in PP  
            Q4_graph <- frequencies_tokens_nostop4%>%
                mutate(word= reorder(word, n)) %>%
                top_n(10)%>%
                ggplot(aes(word, n))+
                ggtitle("Most students use an iPhone")+
                geom_col()+
                xlab(NULL)+
                coord_flip()
            print(Q4_graph)
        
           
        }
    })
    
    
    
    output$q4_right <- renderPlot({
        if( input$questions == "Q4" ){
            aasdsdswde <-Q4_afinn %>%
                group_by(value) %>%
                top_n(10) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=value)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~value, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()
            print(aasdsdswde)
            
            
        }

    })
    
    output$q4_left_bot <- renderPlot({
        if(input$questions == "Q4"){
            gg123 <-  ggplot(frequency, aes(x=proportion, y=`Question 2`, 
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
            
            print(gg123)
            
        }
    })
    
    output$q4_right_bot <- renderText({
        
        if(input$questions == "Q4"){
        # Doing the cor.test()
        ssss <- as.character( 
            cor.test(data=frequency[frequency$df == "Question 4",],
                 ~proportion + `Question 2`)
        )
        print(ssss)
        }
    })
    
    # ####################################################################### 
    # Q1 
    # #######################################################################
    output$q1_frequencies_tokens_nostop1 <- renderPlot({
            
            asdsdsd <- frequencies_tokens_nostop1%>%
                mutate(word= reorder(word, n)) %>%
                top_n(input$sliderinput_q1_frequencies_tokens_nostop1)%>%
                ggplot(aes(word, n))+
                geom_col()+
                theme_grey(base_size = 22) +
                xlab('count')+
                coord_flip()
            print(asdsdsd)
            
        })
    
    # ####################################################################### 
    # Q2
    # #######################################################################
    output$q2_Q2_bing_count <- renderPlot({
    
        print(input$sliderinput_q2_Q2_bing_count)
        Q2_bing_count %>%
            group_by(sentiment) %>%
            ungroup() %>%
            top_n(input$sliderinput_q2_Q2_bing_count) %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            theme_grey(base_size = 22) +
            coord_flip()
    })
    
    output$q2_bigram_counts <- renderTable({
        bigram_q2_counts
    })
    
    output$q2_negated_words <- renderTable({
            negated_words_q2
    })
    
    
    # ####################################################################### 
    # Q3
    # #######################################################################
    
    output$q3_Q3_freq_hist_q3 <- renderPlot({
        freq_hist_q3 <- mydf3 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort=TRUE) %>%
            # anti_join(word == input$checkGroup) %>%
            top_n(input$sliderinput_q3_Q3_freq_hist_q3) %>%
            mutate(word=reorder(word,n)) %>% # IMPORTANT FUNCTION, without this, we would have the order random, this way it is sorted by the most frequent to less frequent
            ggplot(aes(word, n))+
            geom_col()+
            xlab(NULL)+
            theme_grey(base_size = 22) +
            coord_flip()
        print(freq_hist_q3)
    })
    
    output$q3_bigram_counts <- renderTable({
        bigram_q3_counts
    })
    
    output$q3_quadrogram <- renderTable({
        quadrogram_q3
    })
    
    # ####################################################################### 
    # Q4
    # #######################################################################
    
    output$q4_left_graph <- renderPlot({
        # This graph in PP  
        Q4_graph_asdsad <- frequencies_tokens_nostop4%>%
            mutate(word= reorder(word, n)) %>%
            top_n(input$sliderinput_q4_left_graph)%>%
            ggplot(aes(word, n))+
            # ggtitle("Most students use an iPhone")+
            geom_col()+
            xlab(NULL)+
            theme_grey(base_size = 22) +
            coord_flip()
        print(Q4_graph_asdsad)
    })
    
    
    
    output$q4_right_graph <- renderPlot({
        Q4_graph_aasdsdswde <-Q4_afinn %>%
            group_by(value) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            top_n(input$sliderinput_q4_right_graph) %>%
            ggplot(aes(word, n, fill=value)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~value, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            theme_grey(base_size = 22) +
            coord_flip()
        print(Q4_graph_aasdsdswde)
    })
    
    
    
    output$q4_left_bot_graph <- renderPlot({
        gg123sadsad <-  ggplot(frequency, aes(x=proportion, y=`Question 2`, 
                                        color = abs(`Question 2`- proportion)))+
            geom_abline(color="gray40", lty=2)+
            geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
            theme_grey(base_size = 22) +
            geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
            scale_x_log10(labels = percent_format())+
            scale_y_log10(labels= percent_format())+
            scale_color_gradient(limits = c(0,0.001), 
                                 low = "darkslategray4", high = "gray75")+
            facet_wrap(~df, ncol=2)+
            theme(legend.position = "none")+
            labs(y= "Question 2", x=NULL)
        
        print(gg123sadsad)
    })
    
    output$q4_right_bot_graph <- renderText({
        
        # Doing the cor.test()
        sdsdsadafeg <- as.character( 
            cor.test(data=frequency[frequency$df == "Question 4",],
                     ~proportion + `Question 2`)
        )
        print(sdsdsadafeg)
        
    })
    

    output$q4_bigram_counts <- renderTable({
        bigram_q4_counts
    })
    
    output$q4_quadrogram <- renderTable({
        quadrogram_q4
    })
    
    
    # ####################################################################### 
    # Q5
    # #######################################################################
    
    output$q5_freq_hist_q5 <- renderPlot({
        
        freq_hist_q5 <- mydf5 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort=TRUE) %>%
            top_n(input$sliderinput_q5_freq_hist_q5) %>%
            mutate(word=reorder(word,n)) %>% # IMPORTANT FUNCTION, without this, we would have the order random, this way it is sorted by the most frequent to less frequent
            ggplot(aes(word, n))+
            geom_col()+
            theme_grey(base_size = 22) +
            xlab(NULL)+
            coord_flip()
        print(freq_hist_q5)
        
    })
    
    
    output$q5_bigram_counts <- renderTable({
            bigram_q5_counts
    })
    
    # whatspp 
    output$q5_frequency_dsd <- renderPlot({
        
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
        #############################
        # Plotting the correlograms #
        #############################
        
       q5_dsadasdas <-  ggplot(frequency, aes(x=proportion, y=`Question 5`, 
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
       print(q5_dsadasdas)
    })
    
    
    output$q5_frequency_cos <- renderPrint({
        
        
        
        
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
        
        
        
        
        
        cor.test(data=frequency[frequency$df == "Question 3",],
                 ~proportion + `Question 5`)
    })
    # Doing the cor.test()
    
    
    
    
    
    
    # ####################################################################### 
    # Q6
    # #######################################################################
    
    output$q6_graph <- renderPlot({
        # what prefessions are frequent in PP
        Q6_graphsdsd <- frequencies_tokens_nostop6%>%
            mutate(word= reorder(word, n)) %>%
            top_n(input$sliderinput_q6_graph)%>%
            ggplot(aes(word, n))+
            geom_col()+
            theme_grey(base_size = 22) +
            xlab(NULL)+
            coord_flip()
        print(Q6_graphsdsd)
    })
    
    output$q6_bigram_counts <- renderTable({
        bigram_q6_counts
    })
    
    # ####################################################################### 
    # model
    # #######################################################################
  
    
    output$mdoel_NB_classifier <- renderPrint({
        summary(NB_classifier)
    })
    
    # ####################################################################### 
    # gama # two topics boxplot not insightful
    # #######################################################################
    output$gamma_my_gamma111 <- renderPlot({
        sadasd <- my_gamma %>%
            mutate(document=reorder(document, gamma*topic)) %>%
            ggplot(aes(factor(topic), gamma)) +
            geom_boxplot()+
            theme_grey(base_size = 22) +
            facet_wrap(~topic)
        print(sadasd)
    })
    
    
    output$gamma_my_gamma_select <- renderPlot({
        select_gama <- my_gamma %>%
            filter(gamma > 0.5) %>%
            count(topic) %>%
            ggplot(aes(x = topic,y = n))+
            geom_bar(stat = "identity")+
            theme_grey(base_size = 22) +
            theme_economist()
        
        print(select_gama)
    })
    
    output$gamma_my_classifications <- renderPrint({
        print(my_classifications)
    })

    output$gamma_my_gamma_text <- renderPrint({
        print(my_gamma)
    })
    
    
})
