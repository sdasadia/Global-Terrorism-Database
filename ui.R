library(shiny)
library(shinyGlobe)
library(ggplot2)
library(dplyr)
library(ggmap)
library(maptools)
library(maps)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(DT)
library(leaflet)
library(shinyjs)
library(V8)
library(reshape)

dashboardPage(skin = "yellow",
              dashboardHeader(title = "Global Terrorism"),
              dashboardSidebar(
                sidebarMenu(id = "sbm",
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Data Overview", tabName = "globalattack1", icon = icon("area-chart"),
                              menuSubItem("Global Attacks - I", icon = icon("check-circle"),tabName = "globalattack1"),
                              menuSubItem("Global Attacks - II", icon = icon("check-circle"), tabName = "globalattack2"),
                              menuSubItem("Globe Visulization", icon = icon("check-circle"), tabName = "globegl")),
                            menuItem("Terrorism by Country", tabName = "bycountry", icon = icon("gears"),badgeLabel = "new", badgeColor = "green"),
                            conditionalPanel(
                              condition = "input.sbm == 'valueAnalysis' || input.sbm == 'trainModels' || input.sbm == 'compareModels' || input.sbm == 'forecast'",
                              uiOutput("stateUi"),
                              uiOutput("countyUi"),
                              uiOutput("cityUi"),
                              uiOutput("zipUi")
                            ),
                            menuItem("Predicting Terrorism", tabName = "predict", icon = icon("dashboard"))
                            #menuItem("Help", tabName = "help", icon = icon("question-circle"))
                            )),
              dashboardBody(
                useShinyjs(), 
                extendShinyjs(text = "shinyjs.activateTab = function(name){
                              setTimeout(function(){
                              $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').closest('li').addClass('active')
                              }, 200);
                              }"
                              ),
                tabItems(
                  tabItem(tabName = "predict",
                          fluidPage(
                            title = "Studying Bomb / Explotion Attacks",
                            column(
                              width = 12,
                          
                              box(
                                title = "Studying Bomb / Explotion Attacks",
                                status = "primary",
                                width = 12,
                                #height = 680,
                                collapsible = TRUE,
                                solidHeader = TRUE,
                                box(
                                  width = 6,
                                  title = "Logistic Regression Model",
                                  status = "primary",
                                  style = "font-size: 120%;",
                                  style = "color: #444",
                                  tags$ul(
                                    tags$li("Data: The data set consists of 167 features describing over 150,000 different terrorist attacks
                                            between 1970 - 2015. In this analysis, I study patterns in bomb or explosion related attacks. 
                                            The features used to develop the logistic regression model are Success (1/0), Multiple (1/0),
                                            Suicide (1/0), Target Type, Fatalities, and Property damage."),
                                    p(""),
                                    tags$li("The final model was developed by including features that are more likely to influence the outcome and by 
                                            performing various statistical tests to check its significance."),
                                    p(""),
                                    tags$li("A comparison of models is shown here:"),
                                    p(""),
                                    img(src = "model.png",height = 200),
                                    p(""),
                                    tags$li("For a Chi-Square statistics, the P-value is almost 0 suggesting that there is almost no evidence to
                                            support null hypothesis. In short, the model seems to better support the hypothesis."),
                                    p(""),
                                    tags$li("The 95th percentile of the Chi-Squared distribution with 89444 degrees of freedom is 90120.75 and the residual
                                            deviance is 113194 which is >> 90120.75. Thus, the null hypothesis is rejected.")
                                    
                                    )
                                  
                                  
                                  
                                ),
                                box(
                                  width = 6,
                                  status = "warning",
                                  title = "Model:",
                                  img(src = "2.png",height = 560, width = 550)
                                  
                                )
                              ),
                            
                            box(
                              width = 12,
                              title = "Predicting Terrorist Groups",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              box(
                                width = 6,
                                style = "font-size: 120%;",
                                style = "color: #444",
                                status = "primary",
                                withMathJax(),
                                
                                tags$ul(
                                  p(""),
                                tags$li("Goal: Terrorist attacks are biggest and leading issue in the world. To predict the terrorist
                                  group responsible for attacks is essential in order to prevent further attacks."),
                                p(""),
                                tags$li("In this section, I develop an accurate classification technique to predict 40 most deadly and frequent terrorist 
                                  groups in the world. The following features were used to build the classification model: attack location, date, attack type,
                                        weapon type, suicide attack (Yes / No), Multiple Attacks (Yes / No)."),
                                p(""),
                                tags$li("Random Forest technique was used. The developed model is ~95% accurate on the test data. The plot showing the
                                        feature importance other than the date and location is shown. The importance was measured by the overall reduction in 
                                        the Gini impurity index."),
                                p(""),
                                tags$li("Gini impurity index is defined as:
                                       
                                        $$ G = \\sum_{i=1}^n p_i (1 - p_i),$$
                                        where \\(\\ n\\) is the number of classes in the target feature and \\(\\ p_i\\) is the
                                        ratio of \\(\\ i^{ th}\\)class.")
                            
                                )
                              ),
                              box(
                                width = 6,
                                status = "warning",
                                solidHeader = F,
                                p(""),
                                
                                plotlyOutput("plot12",height = 407)
                                
                              )
                            )
                            )
                            
                          )
                    
                  ),
                  tabItem(tabName = "bycountry",
                          fluidPage(
                            title = "Terrorism by Country",
                            column(width = 2,
                                   box(
                                     title = "Query Builder",
                                     status = "primary",
                                     width = 12,
                                     solidHeader = TRUE,
                                     background = "navy",
                                     box(
                                       width = 12,
                                       status = "primary",
                                       solidHeader = FALSE,
                                       background = "navy",
                                       uiOutput("levelQueryUi")
                                        ), # query builder box closing
                                     conditionalPanel(
                                       condition = "input.analysisLevel == 1",
                                       box(
                                         status = "primary",
                                         solidHeader = FALSE,
                                         width = 12,
                                         background = "navy",
                                         uiOutput("regionlist")
                                       )# end of box
                                     ),# end of conditional panel 
                                     conditionalPanel(
                                       condition = "input.analysisLevel == 2",
                                       box(
                                         status = "primary",
                                         solidHeader = FALSE,
                                         width = 12,
                                         background = "navy",
                                         uiOutput("countrylist")
                                       )# end of box
                                     ), # enf of conditional box
                                     box(
                                       status = "primary",
                                       solidHeader = FALSE,
                                       width = 12,
                                       background = "navy",
                                       sliderInput("hviQuery", label = "Year Range", min = 1970, max = 2015, value = c(1970,2015))
                                       #checkboxInput("maxValue", label = "Excluded uncertain attacks", value = FALSE)
                                     ),
                                     box(
                                       width = 12,
                                       status = "primary",
                                       solidHeader = FALSE,
                                       background = "navy",
                                       uiOutput("timeplot")
                                     ),
                                     actionButton("query", label = "Go")
                                   )
                            ),
                                     conditionalPanel(
                                      condition = "input.query",
                                       column(width = 10,
                                              box(
                                                title = textOutput("Design"), 
                                                status = "primary",
                                                width = 12,
                                                height = 1500,
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                fluidRow(
                                                  box(
                                                    status = "primary",
                                                    width = 12,
                                                    solidHeader = FALSE,
                                                    collapsible = TRUE,
                                                    valueBoxOutput("totcity_country", width = 3),
                                                    valueBoxOutput("totAttacks_country", width = 3),
                                                    valueBoxOutput("totlife_country", width = 3),
                                                    valueBoxOutput("totloss_country", width = 3)
                                                  )# end of box
                                                ), #end of fluid row
                                                fluidRow(
                                                  column(width = 12,
                                                         box(
                                                           title = "Attacks",
                                                           status = "primary",
                                                           width = 4,
                                                           solidHeader = FALSE,
                                                           collapsible = TRUE,
                                                           plotlyOutput("plot7",height = 250)
                                                         ),
                                                         box(
                                                           title = "Targets",
                                                           status = "primary",
                                                           width = 4,
                                                           solidHeader = FALSE,
                                                           collapsible = TRUE,
                                                           plotlyOutput("plot8",height = 250)
                                                         ),
                                                         box(
                                                           title = "Weapons",
                                                           status = "primary",
                                                           width = 4,
                                                           solidHeader = FALSE,
                                                           collapsible = TRUE,
                                                           plotlyOutput("plot9",height = 250)
                                                         )
                                                    
                                                  )
                                                ), # end of fluid row
                                                fluidRow(
                                                  column(width = 12,
                                                         
                                                         box(
                                                           title = "Major Attacks (Click on points for more information)",
                                                           status = "primary",
                                                           width = 6,
                                                           height = 475,
                                                           solidHeader = FALSE,
                                                           collapsible = TRUE,
                                                           style = "color: #444",
                                                           leafletOutput("country_map")
                                                         ),
                                                         box(
                                                           title = "Major Terrorist Groups",
                                                           status = "primary",
                                                           width = 6,
                                                           solidHeader = FALSE,
                                                           collapsible = TRUE,
                                                           style = "color: #444",
                                                           DT::dataTableOutput("groupnameTbl")
                                                         )
                                                  )
                                                  
                                                ),
                                                fluidRow(
                                                  column(width = 12,
                                                         box(
                                                           title = textOutput("tseries"),
                                                           status = "primary",
                                                           width = 12,
                                                           solidHeader = FALSE,
                                                           collapsible = TRUE,
                                                           plotlyOutput("plot10",height = 350)
                                                           
                                                         )
                                                    
                                                  )
                                                )
                                              )
                                       )
                                     )
                                     
                          ) # Closing fluidpage
                          ), # Closing tabName = bycountry

                  tabItem(tabName = "globegl",
                           fluidPage(
                             title = "Globe",
                             fluidRow(
                               column(width = 12,
                                      box(
                                        width = 12,
                                        height = 1030,
                                        status = "primary",
                                        solidHeader = TRUE,
                                        style = "font-size: 120%;",
                                        style = "color: #444",
                                        
                                        h1("Global Terrorist Attacks",style = "text-align: center"),
                                       
                                        h4("Global terrorist attacks with more than 10 fatalities are represented as bars rising from a 3D globe.
                                           There are more than 6400 such incidents. The length of the bar represents the 
                                          number of fatalities in the attack. ", style = "text-align: center"),
                                        globeOutput("globe")
                              
                                       
                                     )

                              )
                            )
                          )
                  ),
                  tabItem(tabName = "dashboard",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                              column(width = 4,
                                     box(
                                       style = "font-size: 110%;",
                                       width = 17,
                                       height = 490,
                                       background = "light-blue",
                                       solidHeader = FALSE,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       h2("Global-Terrorism Database"),
                                       p("The Global Terrorism Database (GTD) is an open-source database including information on 
                                         terrorist attacks around the world from 1970 through 2015 (with annual updates planned for 
                                         the future). The GTD includes systematic data on domestic as well as international terrorist
                                         incidents that have occurred during this time period, with more than 150,000 
                                         cases. The database is maintained by researchers at the National Consortium for the Study 
                                         of Terrorism and Responses to Terrorism (START), headquartered at the University of 
                                         Maryland."),
                                       tags$ul(
                                         tags$li("Time period: 1970-2015, except 1993"),
                                         p(""),
                                         tags$li("Unit of analysis: Attack"),
                                         p(""),
                                         tags$li("Variables: >100 variables on location, tactics, perpetrators, targets, and outcomes"),
                                         p(""),
                                         tags$li("Sources: Unclassified media articles (Note: Please interpret changes over time with caution. 
                                                  Global patterns are driven by diverse trends in particular regions, and data collection is influenced by 
                                                  fluctuations in access to media coverage over both time and place.)")
                                        
                                       
                                       )
                                     ),
                                     box(
                                       h2("Data Source"),
                                       style = "font-size: 120%; background-color : #e77e7e;",
                                       #background = "red",
                                       
                                       width = 17,
                                       solidHeader = TRUE,
                                       p("GTD can be obtained from",a(strong("Kaggle"), href = "https://www.kaggle.com/START-UMD/gtd",target="_blank", style = "color :white; font-weight : bold;"), "or",
                                         a(href = "https://www.start.umd.edu/gtd/","University of Maryland GTD",target="_blank",style = "color :white; font-weight : bold;"),
                                         "The clean data set used in this application is also available to download."),
                                       p(""),
                                       p("Cleaned dataset:-"),
                                       tags$ul(
                                         tags$li("The dataset consists of 42 features and 156,771 records "),
                                         p(""),
                                           tags$li("Feature names are modified to friendlier ones and Na / NaN / -9999 are replaced by NA"),
                                         p(""),
                                         tags$li("Year, Month, and Days are combined to form a single feature \"date\""),
                                         p("")
                                       ),
                                         
                                       downloadButton('downloadData', 'Download Clean Data'),
                                       
                                       downloadButton('downloadData2', 'Download CookBook')
                                     )
                                     ),
                                     column(width = 8,
                                            height = 530,
                                            fluidRow(
                                              box(
                                                title = "Key Conclusions",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                width = 12,
                                                #height = 490,
                                                fluidRow(
                                                  box(
                                                    style = "font-size: 120%;",
                                                    title = "1. Analyzing Bomb / Explosion Attacks",
                                                    status = "primary",
                                                    width = 7,
                                                    style = "color: #444",
                                                    tags$ul(
                                                    tags$li("Goal: To recognize patterns behind the Bomb / Explosion
                                                               type terrorist attacks. This analysis includes attack pattern such as a suicide or 
                                                               non-suicidal, whether the attack is a part of multiple attacks and if the attack was a successful
                                                               incident. Along with these features, what are the most likely targets for such horrific
                                                               incidents."),
                                                    
                                                   
                                                    p(""),
                                                    tags$li("Conclusions: A logistic regression model was developed to classify bomb/explosion 
                                                            type of attacks. Following are key conclusions."),
                                                   p(""),
                                                      tags$ol(
                                                        tags$li(" The odds of bomb/explosion type attacks increase by ~16 when there is a suicide attack"),
                                                        p(""),
                                                        tags$li("Bomb attacks are more likely to be successful and are often part of
                                                                series of attacks"),
                                                        p(""),
                                                        tags$li("Most probable targets for bomb attacks are utilities, public transportation,
                                                                and businesses while tourists and media are less likely to be the targets for 
                                                                bomb attacks")
                                                      )
                                                    )        
                                                
                                              ),
                                              box(
                                                title = "Model Paramters",
                                                style = "font-size: 120%;",
                                                status = "warning",
                                                width = 5,
                                                height = 435,
                                                solidHeader = FALSE,
                                                collapsible = FALSE,
                                                DT::dataTableOutput("logisticTbl")
                                            
                                              )
                                              
                                              
                                            ),
                                            fluidRow(
                                              box(
                                                style = "font-size: 120%;",
                                                
                                                status = "primary",
                                                width = 12,
                                                style = "color: #444",
                                                box(
                                                  width = 6,
                                                  title = "2. Predicting Terrorist Groups",
                                                  solidHeader=TRUE,
                                                tags$ul(
                                                  tags$li("Problem: Predicting the terrorist group which is responsible
                                                          for attacks is a crucial but challenging
                                                          task due to uncertainties in the terrorist database. "),

                                                          #Terrorist attacks are the biggest challenging problem for the mankind across the world, which need the
                                                          #wholly attention of the researchers, practitioners to cope up deliberately."),
                                                  p(""),
                                                  tags$li("Conclusion: In this analysis, around 40 different groups were selected from all around the world based on 
                                                          the attack frequency and fatalities. Out of four major classifier techniques: K-Nearest 
                                                          Neighbors (KNN), Decision Tree (DT), Naive Bayes (NB) and Random Forest (RF), the RF showed maximum prediction accuracy of 
                                                          ~94 % on the test dataset.")
                                                
                                                  
                                                  
                                                  
                                                 
                                                )
                                                ),
                                                box(
                                                  width = 6,
                                                  #status = "warning",
                                                  solidHeader=T,
                                                  p(""),
                                                  plotlyOutput("plot11",height = 270)
                                                  
                                                )
                                                )
                                              
                                            )
                                            
                                            )
                              )
                                     )
                            )
                            
                          )
                            ),
                  tabItem(tabName = "globalattack1",
                          fluidPage(
                            title = "Data Overview",
                            fluidRow(
                              column(width = 12,
                                     valueBoxOutput("totCountry", width = 3),
                                     valueBoxOutput("totAttacks", width = 3),
                                     valueBoxOutput("totDeaths", width = 3),
                                     valueBoxOutput("totLoss", width = 3))),
                            fluidRow(
                              plotlyOutput("plot1",height='auto', width = 'auto'))
                          )
                  ),
                  tabItem(tabName = "globalattack2",
                          fluidPage(
                            title = "Market Explorer",
                            fluidPage(
                          
                              column(width = 12,
                                     height = 300,
                                     box(
                                       title = "Top 10 Countries By",
                                       status = "primary",
                                       width = 12,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       fluidRow(
                                         box(
                                           title = "Fatalities",
                                           status = "primary",
                                           width = 4,
                                           solidHeader = FALSE,
                                           collapsible = FALSE,
                                           plotlyOutput("plot3",height = 250)
                                         ),
                                         box(
                                           title = "Damages",
                                           status = "primary",
                                           width = 4,
                                           solidHeader = FALSE,
                                           collapsible = FALSE,
                                           plotlyOutput("plot4",height = 250)
                                         ),
                                         box(
                                           title = "Attacks",
                                           status = "primary",
                                           width = 4,
                                           solidHeader = FALSE,
                                           collapsible = FALSE,
                                           plotlyOutput("plot2",height = 250)
                                         )
                                       )
                                     )
                              ),
                              column(width = 12,
                                     box(
                                       title = "Time-series of Attacks",
                                       status = "primary",
                                       width = 6,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot5")
                                     ),
                                     box(
                                       title = "Time-series of Fatalities",
                                       status = "primary",
                                       width = 6,
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       plotlyOutput("plot6")
                                     )
                                       )
                                     )
                              )
                            )
                          )
              
)
)
