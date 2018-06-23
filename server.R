library(data.table)
library(dplyr)
library(sqldf)
library(htmltools)
library(shiny)
library(DT)
library(tm)
library(tidytext)
library(gridExtra)
library(shinydashboard)
library(scales)
library(rvest)    
library(stringr)   
library(knitr)
library(dplyr)
library(purrr)
library(caret)


shinyServer(function(input,output) {
  
  #ActionButton
  observeEvent(input$go,{
    
  #Passing Base URL  
  url <- 'https://www.cars.com/research/toyota-camry-%d/consumer-reviews/'
  
  
  #Identifying  no of pages dynamically in each year
  get_last_page <- function(html){
    
    No_of_pages <- html %>%
      html_node('.page-list') %>%
      html_nodes('a') %>%
      html_text()
    No_of_pages[(length(No_of_pages))] %>%
      unname() %>%
      as.numeric()
  }
  
  
  #Extarct Review
  get_reviews <- function(html){
    html %>% 
      html_nodes('.mmy-reviews__blurb') %>%
      html_text() %>% 
      str_trim() %>%                       
      unlist()                             
  }
  
  #Extarct starrating
  get_starrating <- function(html)
  {
    
    ratings <-html %>%
      html_nodes(".cr-star-rating") %>%
      html_attr("rating") %>%
      unlist() %>%
    as.integer()
    ratings[1:length(ratings)] 
  }
  
  #Extarct Review Year
  get_year <- function(html)
  {
    date <- html %>%
      html_nodes(".mmy-reviews__date") %>%
      html_text() %>% 
      str_trim() %>%  
      unlist() 
    
    str_sub(date,-4,-1)
    
  }
  
  
  get_data_table <- function(html){
    
    reviews <- get_reviews(html)
    
    ratings <- get_starrating(html)
    
    year_review <- get_year(html)
    
    combined_data <- data.frame(
      review_year = year_review,
      rating = ratings,
      review = reviews) 
    
    combined_data %>% 
      
      select(review_year,rating,review)
  }
  
  
  #Combine data for Each year
  get_data_from_url <- function(url){
    html <- read_html(url)
    get_data_table(html)
  }
  
  
  #Generating Page URL's for each year
  scrape_reviews <- function(url){

    first_page <- read_html(url)
    
    last_page <- get_last_page(first_page)
    
    All_pages <- str_c(url, '?pg=', 1:last_page)
    
    All_pages %>% 
      
      map(get_data_from_url) %>%  
      
      bind_rows()                       
  }
  
  #Test data (2017)
  test_data <- function () {
    
    test_data <- scrape_reviews(sprintf(url,2017))
    
    return (test_data)}
  
  #Training data (2012-2016)
  
  training_data <- function () {training_data <- rbind(
    (scrape_reviews(sprintf(url,2012))),(scrape_reviews(sprintf(url,2013))),(scrape_reviews(sprintf(url,2014))),(scrape_reviews(sprintf(url,2015))),(scrape_reviews(sprintf(url,2016))))
  
  return(training_data)}
  
  
  #Output Test Data
  output$test <- DT::renderDataTable({
  test_data <- DT :: datatable(test_data())
    return(test_data)
  })
  
  #Output Training Data
  output$training <- DT::renderDataTable({
    training_data <- DT :: datatable(training_data())
    return(training_data)
  })
  
  #Normalized Test data
  test_data_normalized <- function() {
    test_data_normalized <- as.data.frame(test_data())
    #based on training data work change here also
    test_data_normalized$normalized_review <- tolower(test_data_normalized$review)
    test_data_normalized$normalized_review <-  gsub("[[:punct:]]", "",test_data_normalized$normalized_review)
    
    test_data_normalized <- test_data_normalized
    return(test_data_normalized)
    
  }
  
  #Normalized Training data
  training_data_normalized <- function() { 
    training_data_normalized <- as.data.frame(training_data())
    
    norm_review = Corpus(VectorSource(training_data_normalized$review))
    norm_review = tm_map(norm_review, content_transformer(tolower))
    norm_review = tm_map(norm_review, removePunctuation)
    data=rep(NULL,dim(training_data_normalized)[1])

    for(i in seq(1,dim(training_data_normalized)[1],1)){
      data[i]=norm_review[[i]]$content
      
    }
    
    training_data_normalized$normalized_review =data
   
    return(training_data_normalized)
    
  }
  
  #Output Normalized Test data
  output$test_normalized <- DT::renderDataTable({
    
    DT:: datatable(test_data_normalized())
  })
  
  #Output Normalized Training data
  output$training_normalized <- DT::renderDataTable({
    DT ::datatable(training_data_normalized())
  })
  
  
  #Tag Normalized Test Data
  test_data_normalized_tag  <- function() {
    
    test_data_normalized <- as.data.frame(test_data_normalized())
    
    test_data_normalized$service <- ifelse(grepl("service",test_data_normalized$normalized_review),"service","")
    test_data_normalized$price <- ifelse(grepl("price",test_data_normalized$normalized_review),"price","")
    test_data_normalized$handling <- ifelse(grepl("handling",test_data_normalized$normalized_review),"handling","")
    test_data_normalized$interior <- ifelse(grepl("interior",test_data_normalized$normalized_review),"interior","")
    test_data_normalized$tags <- paste(test_data_normalized$service,test_data_normalized$price,test_data_normalized$handling,test_data_normalized$interior,sep = " ")
    
    test_data_normalized_tag <- test_data_normalized[,c(1,2,3,4,9)]
    
    test_data_normalized_tag <- test_data_normalized_tag
    
    return(test_data_normalized_tag)
    
    }
  
  #Tag Normalized training Data
  training_data_normalized_tag <- function() {
  
    training_data_normalized <- as.data.frame(training_data_normalized())
    training_data_normalized$service <- ifelse(grepl("service",training_data_normalized$normalized_review),"service","")
    training_data_normalized$price <- ifelse(grepl("price",training_data_normalized$normalized_review),"price","")
    training_data_normalized$handling <- ifelse(grepl("handling",training_data_normalized$normalized_review),"handling","")
    training_data_normalized$interior <- ifelse(grepl("interior",training_data_normalized$normalized_review),"interior","")
    training_data_normalized$tags <- paste(training_data_normalized$service,training_data_normalized$price,training_data_normalized$handling,training_data_normalized$interior,sep = " ")

    training_data_normalized_tag <- training_data_normalized[,c(1,2,3,4,9)]
    
    training_data_normalized_tag <- training_data_normalized_tag
    
    return(training_data_normalized_tag)
    }
  
  #output_Tag Normalized Test Data
  output$test_normalized_tag <- DT::renderDataTable({
    
    DT:: datatable(test_data_normalized_tag())
  })

  # output_Tag Normalized training Data
  output$training_normalized_tag <- DT::renderDataTable({
    DT ::datatable(training_data_normalized_tag())
  })
  
  
  #Sentiment Analysis Training Data
  training_data_sentiment_analysis <- function() {
    training_data_sentiment <- as.data.frame(training_data_normalized_tag())
    training_data_sentiment$ID <- seq.int(nrow(training_data_sentiment))
    
    training_data_sentiment$stopwords <- gsub("[[:digit:]]","",training_data_sentiment$normalized_review )
    
     AFINN <- sentiments %>%
      filter(lexicon == "AFINN") %>%
      select(word, afinn_score = score)
    
     
    review_words <- training_data_sentiment %>%
      unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
      inner_join(AFINN, by = "word")

    review_sentiment_training <- review_words %>%
     group_by(ID,review_year,rating,review,normalized_review,tags) %>%
    summarize(total_score = sum(afinn_score, na.rm = TRUE))
    
    review_sentiment_training$ID <- NULL
    
    review_sentiment_training[review_sentiment_training$total_score==0,'Sentiment']='Neutral'
    review_sentiment_training[review_sentiment_training$total_score>0,'Sentiment']='Positive'
    review_sentiment_training[review_sentiment_training$total_score<0,'Sentiment']='Negative'
    
    review_sentiment_training <- review_sentiment_training
    
    return(review_sentiment_training)
    
  }
  
  
  #Sentiment Analysis Test Data
  test_data_sentiment_analysis <- function() {
    test_data_sentiment <- as.data.frame(test_data_normalized_tag())
    test_data_sentiment$ID <- seq.int(nrow(test_data_sentiment))
    
    test_data_sentiment$stopwords <- gsub("[[:digit:]]","",test_data_sentiment$normalized_review )
    
     AFINN <- sentiments %>%
      filter(lexicon == "AFINN") %>%
      select(word, afinn_score = score)

    review_words <- test_data_sentiment %>%
      unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
      inner_join(AFINN, by = "word") 

    review_sentiment_test <- review_words %>%
      
     group_by(ID,review_year,rating,review,normalized_review,tags) %>%
   
    summarize(total_score = sum(afinn_score, na.rm = TRUE))
    review_sentiment_test$ID <- NULL
    
    review_sentiment_test[review_sentiment_test$total_score==0,'Sentiment']='Neutral'
    review_sentiment_test[review_sentiment_test$total_score>0,'Sentiment']='Positive'
    review_sentiment_test[review_sentiment_test$total_score<0,'Sentiment']='Negative'
    
    
    review_sentiment_test  <- review_sentiment_test
    return(review_sentiment_test)
  }
  
  
  #Output_Sentiment Analysis Test Data
  output$test_sentiment_analysis <- DT::renderDataTable({
    
    DT ::datatable(test_data_sentiment_analysis())
  })
  

  #Output_Sentiment Analysis Training Data
  output$training_sentiment_analysis <- DT::renderDataTable({
    
    DT ::datatable(training_data_sentiment_analysis())
  })
  

#Comparing User and sentiment Rating
compare_rating_training <- function() {
sentimentrating <- training_data_sentiment_analysis()
sentiment_avg   <- as.numeric(sentimentrating$total_score)
user_rating_avg   <- as.numeric(sentimentrating$rating)

avg_sentiment <- mean(sentiment_avg)
avg_user_rating <- mean(user_rating_avg)

compare_rating_training <- data.frame(avg_sentiment,avg_user_rating)

return(compare_rating_training)
}

#Output_Comparing User and sentiment Rating
output$compare_rating <- renderDataTable({
  
  compare_rating_training <- compare_rating_training()
  
  colnames(compare_rating_training) <- c("Average Sentiment rating","Average User star rating")
  
  compare_rating_training[1] <- round(compare_rating_training[1],3)
  compare_rating_training[2] <- round(compare_rating_training[2],3)
  
  compare_rating_training <- compare_rating_training
  
  datatable(compare_rating_training)
  
})

#Sentiment for Tag Service
training_data_tag_service <- function() {
  
  training_data_normalized_ind_tag <- training_data_normalized_tag()
  training_data_normalized_ind_tag$ID <- seq.int(nrow(training_data_normalized_ind_tag))
  training_data_normalized_ind_tag$stopwords <- gsub("[[:digit:]]","",training_data_normalized_ind_tag$normalized_review )
  training_data_normalized_ind_tag$service <- ifelse(grepl("service",training_data_normalized_ind_tag$tags),"service","")
  training_data_normalized_ind_tag_service <- training_data_normalized_ind_tag %>% filter(service!='') 
  
  AFINN <- sentiments %>%
    filter(lexicon == "AFINN") %>%
    select(word, afinn_score = score)
  
  review_words <- training_data_normalized_ind_tag_service %>%
    unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
    inner_join(AFINN, by = "word")
  
  review_sentiment_training_service <- review_words %>%
    group_by(ID,review_year,rating,normalized_review,tags,service) %>%
    summarize(total_score = sum(afinn_score, na.rm = TRUE))
  review_sentiment_training_service <- review_sentiment_training_service
  
  sentiment_rating_service   <- as.numeric(review_sentiment_training_service$total_score)
  user_rating_service <- as.numeric(review_sentiment_training_service$rating)
  
  avg_sentiment_service <- mean(sentiment_rating_service)
  avg_star_rating_service <- mean(user_rating_service)
  
  
  compare_service <- data.frame(avg_sentiment_service,avg_star_rating_service)
  colnames(compare_service) <- c("average sentiment rating","average star rating")
  
  return(compare_service)
  
}


#Sentiment for Tag Price
training_data_tag_price <- function() {
  
  training_data_normalized_ind_tag <- training_data_normalized_tag()
  training_data_normalized_ind_tag$ID <- seq.int(nrow(training_data_normalized_ind_tag))
  training_data_normalized_ind_tag$stopwords <- gsub("[[:digit:]]","",training_data_normalized_ind_tag$normalized_review )
  training_data_normalized_ind_tag$price <- ifelse(grepl("price",training_data_normalized_ind_tag$tags),"price","")
  training_data_normalized_ind_tag_price <- training_data_normalized_ind_tag %>% filter(price!='') 
  
  
  AFINN <- sentiments %>%
    filter(lexicon == "AFINN") %>%
    select(word, afinn_score = score)
  
  review_words <- training_data_normalized_ind_tag_price %>%
    unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
    inner_join(AFINN, by = "word")
  
  review_sentiment_training_price <- review_words %>%
    group_by(ID,review_year,rating,normalized_review,tags,price) %>%
    summarize(total_score = sum(afinn_score, na.rm = TRUE))
  
  sentiment_rating_price   <- as.numeric(review_sentiment_training_price$total_score)
  user_rating_price <- as.numeric(review_sentiment_training_price$rating)
  
  avg_sentiment_price <- mean(sentiment_rating_price)
  avg_star_rating_price <- mean(user_rating_price)
  
  
  compare_price <- data.frame(avg_sentiment_price,avg_star_rating_price)
  
  colnames(compare_price) <- c("average sentiment rating","average star rating")
  
  return(compare_price)
  
}


#Sentiment for Tag Handling
training_data_tag_handling <- function() {
  
  training_data_normalized_ind_tag <- training_data_normalized_tag()
  training_data_normalized_ind_tag$ID <- seq.int(nrow(training_data_normalized_ind_tag))
  training_data_normalized_ind_tag$stopwords <- gsub("[[:digit:]]","",training_data_normalized_ind_tag$normalized_review )
  training_data_normalized_ind_tag$handling <- ifelse(grepl("handling",training_data_normalized_ind_tag$tags),"handling","")
  training_data_normalized_ind_tag_handling <- training_data_normalized_ind_tag %>% filter(handling!='') 
  
  AFINN <- sentiments %>%
    filter(lexicon == "AFINN") %>%
    select(word, afinn_score = score)
  
  review_words <- training_data_normalized_ind_tag_handling %>%
    unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
    inner_join(AFINN, by = "word")
  
  review_sentiment_training_handling <- review_words %>%
    group_by(ID,review_year,rating,normalized_review,tags,handling) %>%
    summarize(total_score = sum(afinn_score, na.rm = TRUE))
  
  
  sentiment_rating_handling   <- as.numeric(review_sentiment_training_handling$total_score)
  user_rating_handling <- as.numeric(review_sentiment_training_handling$rating)
  
  avg_sentiment_handling <- mean(sentiment_rating_handling)
  avg_star_rating_handling <- mean(user_rating_handling)
  
  compare_handling <- data.frame(avg_sentiment_handling,avg_star_rating_handling)
  
  colnames(compare_handling) <- c("average sentiment rating","average star rating")
  
  return(compare_handling)
  
}


#Sentiment for Tag Interior
training_data_tag_interior <- function() {
  
  training_data_normalized_ind_tag <- training_data_normalized_tag()
  training_data_normalized_ind_tag$ID <- seq.int(nrow(training_data_normalized_ind_tag))
  training_data_normalized_ind_tag$stopwords <- gsub("[[:digit:]]","",training_data_normalized_ind_tag$normalized_review )
  training_data_normalized_ind_tag$interior <- ifelse(grepl("interior",training_data_normalized_ind_tag$tags),"interior","")
  training_data_normalized_ind_tag_interior <- training_data_normalized_ind_tag %>% filter(interior!='') 
  AFINN <- sentiments %>%
    filter(lexicon == "AFINN") %>%
    select(word, afinn_score = score)
  
  review_words <- training_data_normalized_ind_tag_interior %>%
    unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
    inner_join(AFINN, by = "word")
  
  review_sentiment_training_interior <- review_words %>%
    group_by(ID,review_year,rating,normalized_review,tags,interior) %>%
    summarize(total_score = sum(afinn_score, na.rm = TRUE))
  
  sentiment_rating_interior   <- as.numeric(review_sentiment_training_interior$total_score)
  user_rating_interior <- as.numeric(review_sentiment_training_interior$rating)
  
  avg_sentiment_interior <- mean(sentiment_rating_interior)
  avg_star_rating_interior <- mean(user_rating_interior)
  
  compare_interior <- data.frame(avg_sentiment_interior,avg_star_rating_interior)
  
  colnames(compare_interior) <- c("average sentiment rating","average star rating")
  
  return(compare_interior)
  
}

#Output_Compare rating for each tag
output$comapare_rating_tag <- renderDataTable ({
  
  ind_tag <- c("service","price","handling","interior")
  
  compare_each_tag_rating<- data.frame(rbind(training_data_tag_service(),training_data_tag_price(),training_data_tag_handling(),training_data_tag_interior()))
  
  compare_each_tag_rating$tag <- ind_tag
  
  compare_each_tag_rating <- compare_each_tag_rating %>% select(3,1,2)
  
  colnames(compare_each_tag_rating) <- c("Tag","Average Sentiment rating","Average User star rating")
  
  compare_each_tag_rating[2] <- round(compare_each_tag_rating[2],2)
  compare_each_tag_rating[3] <- round(compare_each_tag_rating[3],2)
  
  compare_each_tag_rating <- compare_each_tag_rating
  
  datatable(compare_each_tag_rating)
})
  

#Training Data for Linear Regression Model 
  training_data_Model <- function() {
    training_data_sentiment <- as.data.frame(training_data_normalized_tag())
    training_data_sentiment$ID <- seq.int(nrow(training_data_sentiment))
    
    training_data_sentiment$stopwords <- gsub("[[:digit:]]","",training_data_sentiment$normalized_review )
  
    AFINN <- sentiments %>%
      filter(lexicon == "AFINN") %>%
      select(word, afinn_score = score)
    
    review_words <- training_data_sentiment %>%
      unnest_tokens(word, stopwords) %>% anti_join(stop_words) %>%
      inner_join(AFINN, by = "word")
    
    review_sentiment_training_model <- review_words %>%
      group_by(ID,review_year,rating,normalized_review,tags) %>%
      summarize(total_score = mean(afinn_score, na.rm = TRUE))
  
    review_sentiment_training_model <- review_sentiment_training_model
    
    return(review_sentiment_training_model)
  
  } 
  
  #Model Output
  output$Model_summary <- renderPrint({
    
    review_sentiment_training_model <- training_data_Model()
    
    linear_regressionmodel <- lm(formula = rating ~ total_score, data = review_sentiment_training_model)
    
    summary(linear_regressionmodel)
    
    
    
  })
  
  
  #Plot_Model Output
  output$Training_model_plot <- renderPlot({
    
    review_sentiment_training_model <- training_data_Model()
    
    plot(y = review_sentiment_training_model$rating, x = review_sentiment_training_model$total_score, main = "prediction of star rating given sentiment score (Training Data)")
    abline(lm(rating ~ total_score, data = review_sentiment_training_model), col = "red", lwd = 2)
    
    
  })
  
  #Star Rating Prediction Function
  act_pred <- function() {
    
    review_sentiment_training_model <- training_data_Model()
    
    test_data_sentiment_analysis <- test_data_sentiment_analysis()
    
    linear_regressionmodel <- lm(formula = rating ~ total_score, data = review_sentiment_training_model)
    
    test_model <- predict(linear_regressionmodel,test_data_sentiment_analysis) 
    
    test_model <-  scales :: rescale(test_model, to=c(1,6))
    
    act_pred <- data.frame(cbind(actual_values=test_data_sentiment_analysis$rating, predicted_values=test_model)) 
    
    act_pred$actual_values <- round(act_pred$actual_values,2)
    act_pred$predicted_values <- round(act_pred$predicted_values,2)
    act_pred <- act_pred
    return(act_pred)
    
  }
  
  
  #Output _ Actual vs Predicted values
  output$act_pred <- DT::renderDataTable({
    
    act_pred <- act_pred()
    
    DT ::datatable(act_pred)
  })
  
  #Model Accuracy
  output$accuracy <- renderPrint({
    
    act_pred <- act_pred()
    
    # correlation_accuracy <- cor(act_pred)
    
    min_max_accuracy <- mean(apply(act_pred, 1, min) / apply(act_pred, 1, max))
    
    print(min_max_accuracy)
    
    # confusionMatrix(actuals=test_data_sentiment_analysis$rating, predictedScores=test_model
    
  })
  
  #Model Mape
  output$mape <- renderPrint({
    
    act_pred <- act_pred()
    
    mape <- mean(abs((act_pred$predicted_values - act_pred$actual_values))/act_pred$actual_values)
    
   print(mape)
    
  })
  
  #Tag splitting for TF-IDF
  training_data_normalized_ind_tags_all <- function()  {
    training_data_normalized_ind_tag <- training_data_normalized_tag()
    training_data_normalized_ind_tag$service <- ifelse(grepl("service",training_data_normalized_ind_tag$tags),"service","")
    training_data_normalized_ind_tag$price <- ifelse(grepl("price",training_data_normalized_ind_tag$tags),"price","")
    training_data_normalized_ind_tag$handling <- ifelse(grepl("handling",training_data_normalized_ind_tag$tags),"handling","")
    training_data_normalized_ind_tag$interior <- ifelse(grepl("interior",training_data_normalized_ind_tag$tags),"interior","")
    training_data_normalized_ind_tag_service <- training_data_normalized_ind_tag %>% filter(service!='') %>% select(c(1,2,3,4,5,6))
    colnames(training_data_normalized_ind_tag_service)[6] <- "ind_tag"
    training_data_normalized_ind_tag_price <- training_data_normalized_ind_tag %>% filter(price!='') %>% select(c(1,2,3,4,5,7))
    colnames(training_data_normalized_ind_tag_price)[6] <- "ind_tag"
    training_data_normalized_ind_tag_handling <- training_data_normalized_ind_tag %>% filter(handling!='') %>% select(c(1,2,3,4,5,8))
    colnames(training_data_normalized_ind_tag_handling)[6] <- "ind_tag"
    training_data_normalized_ind_tag_interior <- training_data_normalized_ind_tag %>% filter(interior!='') %>% select(c(1,2,3,4,5,9))
    colnames(training_data_normalized_ind_tag_interior)[6] <- "ind_tag"
    training_data_normalized_ind_tags_all <- rbind(training_data_normalized_ind_tag_service,training_data_normalized_ind_tag_price,training_data_normalized_ind_tag_handling,training_data_normalized_ind_tag_interior)
    
    return(training_data_normalized_ind_tags_all)
    
  }
  
  #TF-IDF
  training_data_tf_idf <- function() {
    
    training_data_normalized_ind_tags_all <- training_data_normalized_ind_tags_all()
  
    training_data_normalized_ind_tags_all$normalized_review <- gsub("[[:digit:]]","",training_data_normalized_ind_tags_all$normalized_review )
    
    training_data_tf_idf <- training_data_normalized_ind_tags_all  %>%
      
      unnest_tokens(word, normalized_review) %>%
      count(ind_tag, word, sort = TRUE) %>%
      ungroup()
    # Calculate tf-idf
    training_data_tf_idf <- training_data_tf_idf %>%
      bind_tf_idf(word, ind_tag, n)%>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))
    
    return(training_data_tf_idf)
  }
  
  #Output_TF-IDF
  output$TF_IDF <- DT::renderDataTable({
     
    DT:: datatable(training_data_tf_idf())
  })
  
  training_data_tf_idf_sw <- function() {
    training_data_tf_idf <- training_data_tf_idf()
    
    training_data_tf_idf_sw <- training_data_tf_idf %>%
      anti_join(stop_words, by= c("word" = "word"))
    
    return(training_data_tf_idf_sw)
    }
  
  #Output_TF-IDF_without stopwords
   output$TF_IDF_stop_words <- DT::renderDataTable({
    
    DT:: datatable(training_data_tf_idf_sw())
    
  })
  
   
  #output_TF_IDF_viz
  output$TF_IDF_viz <- renderPlot({
    training_data_tf_idf_sw_viz <- training_data_tf_idf_sw()
    
    # service
    service <- ggplot(training_data_tf_idf_sw_viz %>% 
                        filter(ind_tag == "service") %>% 
                        top_n(10),
                      aes(x = word, y = tf_idf)) +
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#7B3F00") +
      coord_flip() +
      #scale_y_continuous(limits = c(0, 0.002)) +
      labs(x = NULL, y = "tf-idf", title = "service") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    
    # price
    price <- ggplot(training_data_tf_idf_sw_viz %>% 
                      filter(ind_tag == "price") %>% 
                      top_n(10),
                    aes(x = word, y = tf_idf)) +    
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#008B45") +
      coord_flip() +
      labs(x = NULL, y = "tf-idf", title = "price") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    
    #handling
    handling <- ggplot(training_data_tf_idf_sw_viz %>% 
                      filter(ind_tag == "handling") %>% 
                      top_n(10),
                    aes(x = word, y = tf_idf)) +    
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#E91D0E") +
      coord_flip() +
      labs(x = NULL, y = "tf-idf", title = "handling") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    #interior
    interior <- ggplot(training_data_tf_idf_sw_viz %>% 
                      filter(ind_tag == "interior") %>% 
                      top_n(10),
                    aes(x = word, y = tf_idf)) +    
      geom_bar(aes(alpha = tf_idf), 
               stat="identity", 
               fill = "#325C74") +
      coord_flip() +
      labs(x = NULL, y = "tf-idf", title = "interior") +
      scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
    

    grid.arrange(service, price, handling,interior,ncol = 2) }) })
  
  
})
  
  






