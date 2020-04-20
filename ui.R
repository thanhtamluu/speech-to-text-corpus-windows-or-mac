#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(title = "Team 6"),
        ## Sidebar content
        dashboardSidebar(
            
            collapsed = FALSE, 
            
            sidebarMenu(
                menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
                
                menuItem(
                    "Analysis",
                    tabName = "questions_list",
                    icon = icon("tv"),
                    menuItem("Question1",  tabName = "question_01", icon = icon("fab fa-acquisitions-incorporated")),
                    menuItem("Question2", tabName = "question_02", icon = icon("fab fa-acquisitions-incorporated")),
                    menuItem("Question3", tabName = "question_03", icon = icon("fab fa-acquisitions-incorporated")),
                    menuItem("Question4", tabName = "question_04", icon = icon("fab fa-acquisitions-incorporated")),
                    menuItem("Question5", tabName = "question_05", icon = icon("fab fa-acquisitions-incorporated")),
                    menuItem("Question6", tabName = "question_06", icon = icon("fab fa-acquisitions-incorporated"))
                ),
                # menuItem("Analysis", tabName = "widgets", icon = icon("th")),
                menuItem("TD_IDF", tabName = "td_idf", icon = icon("th")),
                menuItem("LDA Analysis", tabName = "gamma", icon = icon("th")),
                menuItem("Naive Bayes Model", tabName = "model_nb_classification", icon = icon("th")),
                menuItem("Conclusion", tabName = "conclusion", icon = icon("th"))
            )
        ),
        ## Body content
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "question_01",
                    fluidRow(
                        box(
                            title = "Question 1: Would you buy a Mac or a Windows laptop?", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            h1("56% of respondents prefer Windows laptop"),
                            br(),
                            sliderInput("sliderinput_q1_frequencies_tokens_nostop1", "Slider input:", 1, 8, 2),
                            br(),
                            plotOutput("q1_frequencies_tokens_nostop1")
                        ),
                    )
                ),
                tabItem(
                    tabName = "question_02",
                    fluidRow(
                        box(
                            title = "Question 2: Please explain, why would you buy Mac/Windows laptop instead of Windows/Mac laptop.", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            h1("Ease of usage, performance, and price play a big role"),
                            br(),
                            sliderInput("sliderinput_q2_Q2_bing_count", "Slider input:", 10, 30, 20),
                            br(),
                            plotOutput("q2_Q2_bing_count"),
                            fluidRow(
                                box(
                                    
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    # collapsed = TRUE,
                                    width = 12,
                                    
                                box(
                                    status = "warning", 
                                    tableOutput("q2_bigram_counts")
                                ),
                                box(
                                    tableOutput("q2_negated_words")
                                ),
                                )
                            ),
                            
                        ),
                    )
                ),
                tabItem(
                    tabName = "question_03",
                    fluidRow(
                        box(
                            title = "Question 3: What do you use your laptop for?", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            h1("The way laptops are used does not require specific brand feature"),
                            br(),
                            sliderInput("sliderinput_q3_Q3_freq_hist_q3", "Slider input:", 1, 20, 10),
                            # checkboxGroupInput("checkGroup", 
                            #                    label = h3("Drop unnecenssary items"), 
                            #                    choices = list("Laptop" = "laptop"),
                            #                    selected = 1),
                            br(),
                            plotOutput("q3_Q3_freq_hist_q3"),
                            fluidRow(
                                box(
                                    
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    # collapsed = TRUE,
                                    width = 12,
                                box(
                                    status = "warning", 
                                    tableOutput("q3_bigram_counts")
                                ),
                                box(
                                    title = "", 
                                    tableOutput("q3_quadrogram")
                                ),
                                
                                )
                                
                            ),
                        ),
                    )
                ),
                tabItem(
                    tabName = "question_04",
                    fluidRow(
                        box(
                            title = "Question 4: What brand is your phone and why did you choose the brand you are currently using?", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            h1("A majority of the respondents own iPhones"),
                            br(),
                            # sliderInput("sliderinput_q3_", "Slider input:", 1, 20, 5),
                            fluidRow(
                                box(
                                    status = "warning", 
                                    title = "Most students use an iPhone",
                                    sliderInput("sliderinput_q4_left_graph", "Slider input:", 1, 20, 5),
                                    plotOutput("q4_left_graph")
                                ),
                                box(
                                    title = "",  
                                    sliderInput("sliderinput_q4_right_graph", "Slider input:", 1, 20, 5),
                                    plotOutput("q4_right_graph")
                                )
                            ),
                            br(),
                            fluidRow(
                                box(
                                    
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    # collapsed = TRUE,
                                    width = 12,
                                box(
                                    status = "warning", 
                                    title = " ",
                                    # sliderInput("sliderinput_q4_left_graph", "Slider input:", 1, 20, 5),
                                    plotOutput("q4_left_bot_graph")
                                ),
                                box(
                                    title = "",  
                                    # sliderInput("sliderinput_q4_right_graph", "Slider input:", 1, 20, 5),
                                    verbatimTextOutput("q4_right_bot_graph")
                                )
                                )
                            ),
                            br(),
                            fluidRow(
                                box(
                                    
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    # collapsed = TRUE,
                                    width = 12,
                                box(
                                    status = "warning", 
                                    title = " ",
                                    # sliderInput("sliderinput_q4_left_graph", "Slider input:", 1, 20, 5),
                                    tableOutput("q4_bigram_counts")
                                ),
                                box(
                                    title = "",  
                                    # sliderInput("sliderinput_q4_right_graph", "Slider input:", 1, 20, 5),
                                    tableOutput("q4_quadrogram")
                                )
                                )
                            ),
                        ),
                    )
                ),
                tabItem(
                    tabName = "question_05",
                    fluidRow(
                        box(
                            title = "Question 5: How/What for do you use your phone most frequently?", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            h1("The way phones are used does not require specific brand connected features"),
                            br(),
                            sliderInput("sliderinput_q5_freq_hist_q5", "Slider input:", 1, 20, 10),
                            br(),
                            plotOutput("q5_freq_hist_q5"),
                            br(),
                            
                            box(
                                
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                # collapsed = TRUE,
                                width = 12,
                                box(
                                    status = "warning", 
                                    title = " ",
                                    
                                    collapsible = TRUE,
                                    tableOutput("q5_bigram_counts")
                                ),
                                fluidRow(
                                    box(
                                        status = "warning", 
                                        title = " ",
                                        # sliderInput("sliderinput_q4_left_graph", "Slider input:", 1, 20, 5),
                                        plotOutput("q5_frequency_dsd")
                                    ),
                                    box(
                                        title = "",  
                                        # sliderInput("sliderinput_q4_right_graph", "Slider input:", 1, 20, 5),
                                        verbatimTextOutput("q5_frequency_cos")
                                    )
                                ),
                            ),
                        ),
                    )
                ),
                tabItem(
                    tabName = "question_06",
                    fluidRow(
                        box(
                            title = "Question 6: Please tell us about your professional background.", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            h1("A majority of respondents come from a technical background"),
                            br(),
                            sliderInput("sliderinput_q6_graph", "Slider input:", 1, 20, 10),
                            br(),
                            plotOutput("q6_graph"),
                            br(),
                            box(
                                    collapsible = TRUE,
                                    # collapsed = TRUE,
                                    width = 12,
                                status = "warning", 
                                title = " ",
                                tableOutput("q6_bigram_counts")
                            )
                        ),
                    )
                ),
                tabItem(
                    tabName = "model_nb_classification",
                    fluidRow(
                        box(
                            title = "Model:", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            br(),
                            h1("Contribution to business success of Mac"),
                            br(),
                            fluidRow(
                            box( width = 8,
                                status = "warning", 
                                title = " ",
                                verbatimTextOutput("mdoel_NB_classifier")
                            ),
                            box( 
                                width = 4,
                                fluidRow(
                                box(
                                    width = 12,
                                    title = "Business Success:",
                                    p("assignment"),
                                    p("dramas"),
                                    p("efficient"),
                                    
                                    p("movies"),
                                    p("tax officer"),
                                    
                                ),
                                
                                
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        title = "Business Failure:",
                                        p("iphone"),
                                        p("laptop"),
                                        p("comfortable"),
                                        
                                        p("control"),
                                        p("games"),
                                        p("pharmaceutical"),
                                        
                                    ),
                                    
                                    
                                )
                                 
                                 
                                 
                            ),
                            
                            )
                        ),
                    )
                ),
                tabItem(
                    tabName = "td_idf",
                    fluidRow(
                        box(
                            title = "TD_IDF:", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            br(),
                            
                            
                            sliderInput("sliderinput_td_idf", "Slider input:", 1, 30, 10),
                            
                            br(),
                            box(
                                width = 12,
                                status = "warning", 
                                title = " ",
                                plotOutput("tf_idf")
                            ),
                            
                        ),
                    )
                ),
                
                
                # verbatimTextOutput("beta_spread_topic1"),
                # verbatimTextOutput("beta_spread_topic2"),
                # plotOutput("qgamma")
                
                tabItem(
                    tabName = "gamma",
                    fluidRow(
                        box(
                            title = "Model:", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            br(),
                            fluidRow(
                                box(
                                    status = "warning", 
                                    title = "Relaxed Minds",
                                    tableOutput("beta_spread_topic1")
                                ),
                                box(
                                    status = "warning", 
                                    title = "Productive Minds",
                                    tableOutput("beta_spread_topic2")
                                )
                               
                            ),

                            br(),
                            fluidRow(
                                status = "primary", 
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                box(
                                    status = "warning", 
                                    title = "63% of respondents fall into Relaxed Minds",
                                    width = 12,
                                    # sliderInput("sliderinput_q4_left_graph", "Slider input:", 1, 20, 5),
                                    plotOutput("qgamma21312312")
                                )
                            ),
                            br(),
                        ),
                    ),
                    br(),
                    fluidRow(
                    box(
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        
                        # collapsed = TRUE,
                        
                        box(
                            
                            title = " ",
                            # sliderInput("sliderinput_q4_left_graph", "Slider input:", 1, 20, 5),
                            verbatimTextOutput("gamma_my_classifications")
                        ),
                        box(
                            
                            title = " ",  
                            # sliderInput("sliderinput_q4_right_graph", "Slider input:", 1, 20, 5),
                            verbatimTextOutput("gamma_my_gamma_text")
                        )
                    ),
                    )
                ),
                tabItem(
                    tabName = "conclusion",
                    fluidRow(
                        box(
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Conclusion",
                            width = 12,
                            
                            
                            
                            h1("Review of Key Points"),
                            br(),
                            fluidRow(
                                infoBox("", 
                                        "Slightly more respondents prefer Windows laptop to Macbook most probably because of their technical professional background, better performance, and cheaper price. ", 
                                        color = "teal",
                                        width = 12 ,icon = icon("credit-card"))
                            ),
                            fluidRow(
                                infoBox("", 
                                        "Macbook is characterized by ease of usage, efficiency, and Apple ecosystem. However, Macbook is considered to be not worth paying. ", 
                                        color = "teal",
                                        width = 12 ,icon = icon("credit-card"))
                            ),
                            fluidRow(
                                infoBox("", 
                                        "A phone brand does not influence the choice of a laptop. Laptops and phones are used for different purposes. ", 
                                        color = "teal",
                                        width = 12 ,icon = icon("credit-card"))
                            ),
                            fluidRow(
                                infoBox("", 
                                        "A lot of students are not clearly aware of all the functionalities and do not realize all the benefits that Apple and Macbook offer. ", 
                                        color = "teal",
                                        width = 12 ,icon = icon("credit-card"))
                            ),
                            
                            
                            
                            # h3("1. Slightly more respondents prefer Windows laptop to Macbook most probably because of their technical professional background, better performance, and cheaper price. "),
                            # h3("2. Macbook is characterized by ease of usage, efficiency, and Apple ecosystem. However, Macbook is considered to be not worth paying. "),
                            # h3("3. A phone brand does not influence the choice of a laptop. Laptops and phones are used for different purposes."),
                            # h3("4. A lot of students are not clearly aware of all the functionalities and do not realize all the benefits that Apple and Macbook offer."),
                            br(),
                            h1("Recommendation"),
                            br(),
                            fluidRow(
                                infoBox("", 
                                        "Expand the community of users at Hult and potentially other universities by hiring and training brand ambassadors that will share their experiences with the students and show them how macOS can be relevant for their studies and future employment. ", 
                                        color = "olive",
                                        width = 12 ,icon = icon("credit-card"))
                            ),
                            
                            # h3("Expand the community of users at Hult and potentially other universities by hiring and training brand ambassadors that will share their experiences with the students and show them how macOS can be relevant for their studies and future employment."),
                        
                            
                        )
                    ),
                    
                ),
                
                
                # First tab content
                tabItem(tabName = "introduction",
                        fluidRow(
                            
                            # A static valueBox
                            valueBox('27', "Number of Respondents",color = "green", width = 4,icon = icon("credit-card")),
                            valueBox('21-49', 'Age',color = "blue",width = 4, icon = icon("credit-card")),
                            valueBox('Students', 
                                     "Occupation", width = 4,color = "yellow",
                                     icon = icon("credit-card"))       
                            ),
                        # fluidRow(
                        #     valueBox('Students & Working Professionals', "Occupation", width = 12,color = "yellow",icon = icon("credit-card"))
                        # ),
                        fluidRow(
                            infoBox("Business Problem", 
                                    "Windows or Mac? Apple wants to understand the aspects of consumer buying behavior in regards to the decision making behind this question. The goal is to analyze behavior patterns among our respondents and what their preferences are.", 
                                    color = "red",
                                    width = 12 ,icon = icon("credit-card"))
                        ),
                        fluidRow(
                            infoBox("Survey Question 1:",
                                    "Would you buy a Mac or a Windows laptop?",
                                    width = 12 ,icon = icon("fas fa-question"))
                        ),fluidRow(
                            infoBox("Survey Questions 2:",
                                    "Please explain, why would you buy Mac/Windows laptop instead of Windows/Mac laptop.",
                                    
                                    width = 12 ,icon = icon("fas fa-question"))
                        ),
                        fluidRow(
                            infoBox("Survey Question 3:",
                                    "What do you use your laptop for?",
                                    width = 12 ,icon = icon("fas fa-question"))
                        ),
                        fluidRow(
                            infoBox("Survey Question 4:",
                                    "What brand is your phone and why did you choose the brand you are currently using?",
                                    width = 12 ,icon = icon("fas fa-question"))
                        ),
                        fluidRow(
                            infoBox("Survey Questions 5:",
                                    "How/What for do you use your phone most frequently?",
                                    width = 12 ,icon = icon("fas fa-question"))
                        ),
                        fluidRow(
                            infoBox("Survey Question 6:",
                                    "Please tell us about your professional background.",
                                    width = 12 ,icon = icon("fas fa-question"))
                        ),
                        
                ),
                
                # Second tab content
                tabItem(tabName = "widgets",
                        
                        h2("Survey Questions"),
                        
                        
                        fluidRow(
                            box(
                                title = "Questions", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                selectInput("questions", "Choose a question:", 
                                            choices = c("General","Q1", "Q2", "Q3","Q4","Q5","Q6")),
                                verbatimTextOutput("questions")
                            )
                        ),
                        fluidRow(
                            
                            box(
                                width = 12,
                                title = "Plots", solidHeader = TRUE,
                                fluidRow(
                                    box(
                                        width = 12,
                                        textOutput("hearr"),
                                        plotOutput("questionss"),
                                        fluidRow(
                                            box( width = 6, 
                                                tableOutput("bigram_q2_counts")
                                            ),
                                            box(width = 6,
                                                tableOutput("negated_words_q2")
                                            )  
                                        ),
                                        fluidRow(
                                            box( width = 6, 
                                                 plotOutput("q4_left")
                                            ),
                                            box(width = 6,
                                                plotOutput("q4_right")
                                            )  
                                        ),
                                        fluidRow(
                                            box( width = 6, 
                                                 plotOutput("q4_left_bot")
                                            ),
                                            box(width = 6,
                                                textOutput("q4_right_bot")
                                            )  
                                        ),
                                    
                                       
                                    )
                                )
                            )
                            
                        )
                ),
                tabItem(tabName = "model",
                        
                        plotOutput("model")
                )
                # tabItem(tabName = "allq",
                #         tabBox(
                #             title = "First tabBox",
                #             # The id lets us use input$tabset1 on the server to find the current tab
                #             id = "tabset1", height = "250px",
                #             width = 12,
                #             
                #             tabPanel("BETA", 
                #                      tableOutput("beta_spread_topic2")
                #                      ),
                #             tabPanel("GAMMA", 
                #                      plotOutput("qgamma")
                #             )
                #         )
                #         
                # )
            )
        )
    )
    


)
