library(htmltools)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(networkD3)
library(sqldf)
library(ggplot2)
library(scales)


shinyUI(fluidPage(
                  dashboardPage(
                    #Title
                  dashboardHeader(title = "Ramesh_Nunna_NLP"),
                  dashboardSidebar(
                    sidebarMenu(
                      #sidebar panel
                      actionButton("go", "Get Data", icon("paper-plane"),
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      menuItem("Toyota Reviews (2012-2016)", tabName = "Reviews_Training", icon = icon("th")),
                      menuItem("Toyota Reviews (2017)", tabName = "Reviews_Test", icon = icon("th")),
                      menuItem("Normalized Reviews", tabName = "Normalized_Reviews", icon = icon("th")),
                      menuItem("Normalized Reviews_Tags", tabName = "Normalized_Reviews_tag", icon = icon("th")),
                      menuItem("Setiment Analysis", tabName = "Setiment_Analysis", icon = icon("th")),
                      menuItem("Rating Comparision", tabName = "Rating_Comparision", icon = icon("dashboard")),
                      menuItem("Linear Regression Model", tabName = "Regression_Model_Training", icon = icon("dashboard")),
                      menuItem("Star Rating Prediction", tabName = "Predictions_Test_Data", icon = icon("dashboard")),
                      menuItem("TF-IDF", tabName = "TF_IDF", icon = icon("th")),
                      menuItem("TF-IDF Visualization", tabName = "TF-IDF_Visualization", icon = icon("dashboard"))
                      
                      
                    )),
                    
                  #Dashboardbody for outputs
                  
                    dashboardBody(
                      
                      tabItems(

                        tabItem(tabName = "Reviews_Training",
                                fluidRow(box(title = "Training Data (2012-2016)",width=12, dataTableOutput("training")))),
                        
                        tabItem(tabName = "Reviews_Test",
                                fluidRow(box(title = "Test Data (2017)",width=12, dataTableOutput("test")))),
                        
                        tabItem(tabName = "Normalized_Reviews",
                                fluidRow(
                                  tabsetPanel(type="tabs",
                          tabPanel("Training Data Normalized Reviews", width=12,dataTableOutput("training_normalized")),
                          tabPanel("Test Data Normalized Reviews", width=12,dataTableOutput("test_normalized")
                        )))),
                        
                        tabItem(tabName = "Normalized_Reviews_tag",
                                fluidRow(
                                  tabsetPanel(type="tabs",
                          tabPanel("Training Data Normalized Reviews_Tags", width=12,dataTableOutput("training_normalized_tag")),
                          tabPanel("Test Data Normalized Reviews_Tags", width=12,dataTableOutput("test_normalized_tag")
                          )))),
                       
                        tabItem(tabName = "Setiment_Analysis",
                                fluidRow(
                                  tabsetPanel(type="tabs",
                                    tabPanel("Training Data Sentiment Analysis",width=12,dataTableOutput("training_sentiment_analysis")),
                                    tabPanel("Test Data Sentiment Analysis", width=12,dataTableOutput("test_sentiment_analysis")
                                    )))),
                        
                        tabItem(tabName = "Rating_Comparision",
                                fluidRow(
                                  tabsetPanel(type="tabs",
                                                        tabPanel("Rating comparison", width=12,

                                                    fluidRow(box(dataTableOutput("compare_rating"))),

                                                    fluidRow( box(background = "light-blue",h5("Sentiment analysis process we followed here is simple & straight forward and it always does not work.

                                                          With sentiment analysis the tone of the writer cannot be properly analyzed whether the user review is in normal or in sarcastic tone.

                                                          The analysis above is capturing sentiment based on individual words in the review and misses out on the overall context of the tweet."))),
                                                    fluidRow(box( background = "light-blue",h5("By considering Bi-grams instead of uni-grams and using complex sentiment scoring algorithms , the process can be improved.

                                                          And also the dictionary scores should be assigned based on the connotative meaning of the word.")))),
                                              tabPanel("Rating comparison by each tag", width=12,

                                                       fluidRow(box(dataTableOutput("comapare_rating_tag"))),
                                                       # fluidRow(box(dataTableOutput("compare_rating"))),
                                                       fluidRow(box(background = "light-blue",h5 ("Process Followed :punctuation removal, convert uppercase to lowercase, digits removal, tokenization ,stop words removal "))),
                                                       fluidRow(box(background = "light-blue",h5 ("The user ratings and the scores in the dictionary are scaled differently.In the analysis scaling is not taken into consideration. Process can be improved by scaling dictionary scores to the scale of (1,5)")))


                                              )))),
                        
                        
                        tabItem(tabName = "Regression_Model_Training",
                                fluidRow(tabsetPanel(type="tabs",
                                                     
                                                     
                                                     tabPanel("Model Summary",
                                                              
                                                              fluidRow(box(verbatimTextOutput("Model_summary"))),
                                                              fluidRow( box(background = "light-blue",
h5("A simple linear regression model has been modelled using training data. P value is significant . Rsquare value is around 27% which can be improved by improving regression model and implementing complex sentiment scoring algorithms.")))),
                                                              
                                                     tabPanel("Scatter Plot",width="100%",height = "600px", plotOutput("Training_model_plot"))))),
                        
                        
                        tabItem(tabName = "Predictions_Test_Data",
                                fluidRow(tabsetPanel(type="tabs",
                                                     tabPanel("Predicted Ratings",dataTableOutput("act_pred")),
                                                     tabPanel("Model Accuracy",
                                                              h5("Min_Max Accuracy :"),
                                                              verbatimTextOutput("accuracy"),
                                                              h5("Mean absolute percentage error :"),
                                                              verbatimTextOutput("mape"),
                                                              h5("By testing model built on test data, the model accuracy is around 50% and mapr is 51.1%
The predicted values in test data are more than rating 5 which is due to different scaling of dictionary scores and user ratings. We can scale the dictionary scores in our analysis to get the predicted values within the user ratings scale."))))),
                        tabItem(tabName = "TF_IDF",
                                fluidRow(
                                  tabsetPanel(type="tabs",
                                              tabPanel("TF-IDF", width=12,dataTableOutput("TF_IDF")),
                                              tabPanel("TF-IDF (Without Stopwords)", width=12,dataTableOutput("TF_IDF_stop_words")
                                              )))),
                        
                        tabItem(tabName = "TF-IDF_Visualization",
                                fluidRow(box(title = "TF-IDF Scores for Top 10 Words",width="100%",height = "100%", plotOutput("TF_IDF_viz"))))
                                
                        
                  )))))            
                  
                  
