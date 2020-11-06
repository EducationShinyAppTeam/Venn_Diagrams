library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(V8)
library(boastUtils)

#Note: variables ending in 1 are for the slider diagram with 1 event
#         Variables ending in 11 are for the input diagram with 1 event
#         variables ending in 2 are for the slider diagram with 2 events
#         Varialbes ending in 22 are for the input diagram with 2 event
#               and so on with 3 eve

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Venn Diagram App"
APP_DESCP  <<- paste(
  "This app is meant to show the probability of different combinations of events 
  illistrated by a Venn Diagram.",
  "The user can either type in values or use sliders to make them larger 
  or smaller."
)
## End App Meta Data------------------------------------------------------------

#fresh the whole page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

# Define UI for application that draws a histogram
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Venn Diagrams",
                    titleWidth = 250,
                    tags$li(class = "dropdown",
                            actionLink("info",
                                       icon("info", class = "myClass"))),
                    
                    tags$li(class = "dropdown",
                            tags$a(href = 'https://shinyapps.science.psu.edu/',
                                   icon("home")))
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "Overview",
                 icon = icon("dashboard")),
        menuItem("Venn Diagrams", tabName = "circle",
                 icon = icon("cogs")),
        menuItem("References", tabName = "references",
                 icon = icon("leanpub"))
        
      ),
      tags$div(class = "sidebar-logo",
               boastUtils::psu_eberly_logo("reversed"))
    ),
    dashboardBody(
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
          href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
      ),
      tabItems(
        #Homepage
        ############# The Overview Tab ################### 
        tabItem(tabName = "Overview",
                
                h1("Venn Diagrams"),
                p("The goals of this game are to introduce 
                probability, to help you think about the relationships 
               between events, and how to learn to model real 
                  problems using Venn diagrams."),
                br(),
                h2("Instructions"),
                
                tags$ul(
                  tags$li("If you want to adjust the circles in the 
                          Venn Diagram:"),
                  tags$ol(
                    tags$li("Move or change the size of the circles 
                            using sliders (accessed by the button)."),
                    tags$li("Press the 'Submit' button to see if 
                            your answer is correct")
                  ),
                  tags$li("If you want to enter the probabilities 
                          directly:"),
                  tags$ol(
                    tags$li("Enter your probability in the textbox."),
                    tags$li("Press the 'Submit' button to see if 
                            your answer is correct")
                  ), 
                  tags$li("The 'Reset' button lets you try again, 
                          the 'Next' button provides a new question."),
                  tags$li("After attempting the problem, you can 
                          check the basic relationship of the events 
                          using the 'Sample Answer' button.")
                ),
                
                div(style = "text-align: center",
                    bsButton("go", "GO!", icon("bolt"))), br(),
                h2("Acknowledgements"),
                p("This app was developed and coded by Qichao Chen 
                with input from Yuxin Zhang, Sitong Liu and Yingjie Wang 
                in 2017. It was then modified by Yubaihe Zhou to 
                allow direct numeric input in 2018. This was further 
                modified by Jingjun Wang in 2019. 
                It was then modified by Ethan Wright to improve the 
                  visual presentation, feedback, and formatting in 2020."),
                
                div(class = "updated", "Last Update: 07/05/2020 by EJW."    
                )),
        
        ################ Main Venn Diagram Tab #######################
        tabItem(tabName = "circle",
                
                # fresh the whole page
                useShinyjs(),
                extendShinyjs(text = jsResetCode, functions = c("hidden", "disable", "enable", "toggle")), #If there are any functions that
                      #use javascript simply put thme inside of the c().
                sidebarLayout(
                  sidebarPanel( 
                    #hides value of slider bar
                    tags$head(
                    tags$style(HTML('.irs-from, .irs-to,
                    .irs-min, .irs-max, .irs-single {
                                              visibility: hidden !important;
                                              }'))),
                    #CREATES 3 INPUTS FOR LIST, each creates one of the 3 outputs
                    selectInput("modes", "Modes",   
                                list( 
                                  "One Event" = "level1",
                                  "Two Events" = "level2",
                                  "Three Events" = "level3"
                                )
                    ),
                    bsButton("random", "Random Question"),
                    bsPopover("random", " ", "Randomly get questions",
                              placement = "bottom", trigger = "hover", 
                              options = NULL),
                    
            ########## 1 Event Sidebar #############################
                    conditionalPanel(
                      condition = "input.modes == 'level1'",
                      br(),
                      #Radio Buttons
                      div(style = "display: inline-block;vertical-align:top;",
                          radioButtons("check1", label = "", 
                                       choices = c("Numeric Input" = "Numeric",
                                                   "Slider Input" = "Slider"),
                                       selected = "Numeric", inline = F,
                                       width = NULL)
                      ),
                      ##### Numeric Sidebar Level 1 ###########
                      conditionalPanel(
                        condition = "input.check1 == 'Numeric'",
                        fluidRow(
                          column(4, uiOutput("labeldol1")),
                          column(6, offset = 0, uiOutput("PA11")), 
                          column(6, numericInput("PA", label = "",
                                                 value = NULL, min = 0, 
                                                 max = 1, step = 0.01 )))
                      ),
                      
                      ####### Slider Sidebar Level 1 #################
                      conditionalPanel(
                        condition = "input.check1 == 'Slider'",
                        fluidRow(
                          column(5, 
                           dropdownButton( #The 3 pop up sliders
                             tags$h3("Blue Circle"),
                             div(style = "position: absolute; left: 0.5em;
                                 top: 5em", p("Small")),
                             div(style = "position: absolute; right: 0.5em;
                                 top: 5em", p("Large")),
                             sliderInput("radiusl1", label = "", min = 0,
                                         max = 1.2, step = 0.005,
                                         value = 0.05, ticks = F),
                             div(style = "position: absolute; left: 0.5em;
                                 top: 11em", p("Left")),
                             div(style = "position: absolute; right: 0.5em;
                                 top: 11em", p("Right")),
                             sliderInput("movel1", label = "", min = 0, max = 1, 
                                         step = 0.01, ticks = FALSE, value = 0.49),
                             div(style = "position: absolute; left: 0.5em;
                                 top: 17em", p("Down")),
                             div(style = "position: absolute; right: 0.5em;
                                 top: 17em", p("Up")),
                             sliderInput("move1l1", label = "", min = 0, max = 1,
                                         step = 0.01, ticks = FALSE, value = 0.5),
                             circle = T, status = "info", 
                             icon = icon("sliders"), width = "300px", up = T,
                             tooltip = tooltipOptions(title = "Click to 
                                                      adjust blue circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PA1")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("PAl1"))
                        )
                      ),
                      tags$head(
                        #Probability of blue circle
                        tags$style(HTML("
                                        #dol1 {
                                        background-color:#B2B2FF;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #B2B2FF;
                                        color:#000000;
                                        }
                                        "))
                      ),
                    ),
                    ############# 2 Events Sidebar #############################
                    conditionalPanel(
                      condition = "input.modes == 'level2'",
                      br(),
                      #Radio Buttons
                      div(style = "display: inline-block;vertical-align:top;",
                          radioButtons("check2",
                                       label = "", choices = c("Numeric Input" = "Numeric2",
                                                               "Slider Input" = "Slider2"),
                                       selected = "Numeric2", inline = F, width = NULL),
                      ),
                      ################ 2 Events Slider ####################
                      conditionalPanel(
                        condition = "input.check2 == 'Slider2'",
                        #Blue Circle
                        fluidRow(
                          column(5, 
                                 dropdownButton(
                                   tags$h3("Blue Circle"),
                                   div(style = "position: absolute; 
                                       left: 0.5em; top: 5em", p("Small")),
                                   div(style = "position: absolute; 
                                       right: 0.5em; top: 5em", p("Large")),
                                   sliderInput("radiusl2", label = "", min = 0,
                                               max = 1.2, step = 0.005, 
                                               value = 0.05, ticks = F),
                                   div(style = "position: absolute; 
                                       left: 0.5em; top: 11em", p("Left")),
                                   div(style = "position: absolute; right: 0.5em;
                                       top: 11em", p("Right")),
                                   sliderInput("movel12", label = "", min = 0,
                                               max= 1, step = 0.01, ticks = FALSE,
                                               value = 0.45),
                                   div(style = "position: absolute;
                                       left: 0.5em; top: 17em", p("Down")),
                                   div(style = "position: absolute; 
                                       right: 0.5em; top: 17em", p("Up")),
                                   sliderInput("move1l2", label = "", min = 0,
                                               max = 1, step = 0.01, ticks = FALSE,
                                               value = 0.5),
                                   circle = T, status = "info",
                                   icon = icon("sliders"), width = "300px", up = T,
                                   tooltip = tooltipOptions(
                                     title = "Click to adjust blue circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PA2")),
                          column(4, offset = 0, style = 'padding:0px;', 
                                 verbatimTextOutput("PAl2"))
                        ),
                        #Green Circle
                        fluidRow(
                          column(5, 
                           dropdownButton(
                             tags$h3("Green Circle"),
                             div(style = "position: absolute; left: 0.5em;
                                 top: 5em", p("Small")),
                             div(style = "position: absolute; right: 0.5em; 
                                 top: 5em", p("Large")),
                             sliderInput("radius2l2", label = "", min = 0, 
                                         max = 1.2, step = 0.005, 
                                         value = 0.05, ticks = F),
                             div(style = "position: absolute; left: 0.5em; 
                                 top: 11em", p("Left")),
                             div(style = "position: absolute; right: 0.5em; 
                                 top: 11em", p("Right")),
                             sliderInput("movel2", label = "", min = 0, max = 1,
                                         step = 0.01, ticks = FALSE, value = 0.55),
                             div(style = "position: absolute; left: 0.5em; 
                                 top: 17em", p("Down")),
                             div(style = "position: absolute; right: 0.5em; 
                                 top: 17em", p("Up")),
                             sliderInput("move2l2", label = "", min = 0, max = 1, 
                                         step = 0.01, ticks = FALSE, value = 0.5),
                             circle = T, status = "success",
                             icon = icon("sliders"), width = "300px", up = T,
                             tooltip = tooltipOptions(title = "Click to adjust green circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PB2")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("PBl2"))
                        ),
                        fluidRow(column(6, offset = 0, uiOutput("inter2"))),
                        br(),
                        fluidRow(
                          column(5, uiOutput("labeldoBGl2")),
                          column(6, offset = 0, uiOutput("AB2")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("ABl2"))
                        )
                      ),
                      
                      ################## 2 Event Numeric ###########################
                      conditionalPanel(
                        condition = "input.check2 == 'Numeric2'",
                        #Blue
                        fluidRow(
                          column(4, uiOutput("labeldoBl2")),
                          column(6, offset = 0, uiOutput("PA22")),
                          column(6, numericInput("P2A", label = "",
                                                 value = NULL, min = 0.01, 
                                                 max = 1, step = 0.01 ))
                        ),
                        #Green
                        fluidRow(
                          column(4, uiOutput("labeldoGl2")),
                          column(6, offset= 0, uiOutput("PB22")),
                          column(6, numericInput("P2B", label = "",
                                                 value = NULL, min = 0.01,
                                                 max = 1, step = 0.01 ))
                        ),
                        #Intersection
                        fluidRow(
                          column(4, uiOutput("labeldoBGl22")),
                          column(6, offset = 0, uiOutput("AB22")),
                          column(6, numericInput("A2B", label = "",
                                                 value = NULL, min = 0.01,
                                                 max = 1, step = 0.01 ))
                        ),
                      ),
                      
                      ############# Probability of circles ###############
                      tags$head(
                        #Probability of blue circle
                        tags$style(HTML("
                                        #dol2 {
                                        background-color:#B2B2FF;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #B2B2FF;
                                        color:#000000;
                                        }
                                        ")),
                        
                        #Probability of green circle
                        tags$style(HTML("
                                        #do2l2 {
                                        background-color:#B2FFB2;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #B2FFB2;
                                        color:#000000;
                                        }
                                        ")),
                        #Probability of cyan
                        tags$style(HTML("
                                        #do4l2 {
                                        background-color:#7CC9B2;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #7CC9B2;
                                        color:#000000;
                                        }
                                        ")),
                      ),
                    ),
            
   ######################## 3 Events #########################################
                    conditionalPanel(
                      condition = "input.modes == 'level3'",
                      br(),
                      #Radio Buttons
                      div(style = "display: inline-block;vertical-align:top;",
                          radioButtons("check3", label = "", 
                                       choices = c("Numeric Input" = "Numeric3",
                                                   "Slider Input" = "Slider3"),
                                       selected = "Numeric3",
                                       inline = F, width = NULL)
                      ),
                      ########## 3 Events Slider Input
                      conditionalPanel(
                        condition = "input.check3 == 'Slider3'",
                        #Blue Circle
                        fluidRow(
                          column(5, 
                             dropdownButton(
                               tags$h3("Blue Circle"),
                               div(style = "position: absolute; left: 0.5em; top: 5em", 
                                   p("Small")),
                               div(style = "position: absolute; right: 0.5em; top: 5em", 
                                   p("Large")),
                               sliderInput("radiusl3", label = "", min = 0, max = 1.2, 
                                           step = 0.005, value = 0.05, ticks = F),
                               div(style = "position: absolute; left: 0.5em; top: 11em",
                                   p("Left")),
                               div(style = "position: absolute; right: 0.5em; top: 11em",
                                   p("Right")),
                               sliderInput("movel13", label = "", min = 0, max = 1, 
                                           step = 0.01, ticks = FALSE, value = 0.45),
                               div(style = "position: absolute; left: 0.5em; top: 17em",
                                   p("Down")),
                               div(style = "position: absolute; right: 0.5em; top: 17em",
                                   p("Up")),
                               sliderInput("move1l3", label = "", min = 0, max = 1,
                                           step = 0.01, ticks = FALSE, value = 0.5),
                               circle = T, status = "info", icon = icon("sliders"),
                               width = "300px", up = T, #status = myClass1
                               tooltip = tooltipOptions(title = "Click to adjust blue circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PA3")),
                          column(4, offset = 0, style = 'padding:0px;', 
                                 verbatimTextOutput("PAl3"))
                        ),
                        #Green Circle
                        fluidRow(
                          column(5, 
                           dropdownButton(
                             tags$h3("Green Circle"),
                             div(style = "position: absolute;
                             left: 0.5em; top: 5em", p("Small")),
                             div(style = "position: absolute; right:
                             0.5em; top: 5em", p("Large")),
                             sliderInput("radius2l3", label = "", min = 0, max = 1.2,
                                         step = 0.005, value = 0.05, ticks = F),
                             div(style = "position: absolute; left: 0.5em;
                                 top: 11em", p("Left")),
                             div(style = "position: absolute; right: 0.5em;
                                 top: 11em", p("Right")),
                             sliderInput("movel23", label = "", min = 0, max = 1,
                                         step = 0.01, ticks = FALSE, value = 0.55),
                             div(style = "position: absolute; left: 0.5em;
                                 top: 17em", p("Down")),
                             div(style = "position: absolute; right: 0.5em;
                                 top: 17em", p("Up")),
                             sliderInput("move2l3", label = "", min = 0, max = 1,
                                         step = 0.01, ticks = FALSE, value = 0.5),
                             circle = T, status = "success", icon = icon("sliders"),
                             width = "300px", up = T, 
                             tooltip = tooltipOptions(title = "Click to adjust green circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PB3")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("PBl3"))
                        ),
                        #Red Circle
                        fluidRow(
                          column(5, 
                             dropdownButton(
                               tags$h3("Red Circle"),
                               div(style = "position: absolute; left: 0.5em;
                                   top: 5em", p("Small")),
                               div(style = "position: absolute; right: 0.5em;
                                   top: 5em", p("Large")),
                               sliderInput("radius3l3", label = "", min = 0, max = 1.2,
                                           step = 0.005, value = 0.05, ticks = F),
                               div(style = "position: absolute; left: 0.5em;
                                   top: 11em", p("Left")),
                               div(style = "position: absolute; right: 0.5em;
                                   top: 11em", p("Right")),
                               sliderInput("movel33", label = "", min = 0, max = 1,
                                           step = 0.01, ticks = FALSE, value = 0.5),
                               div(style = "position: absolute; left: 0.5em;
                                   top: 17em", p("Down")),
                               div(style = "position: absolute; right: 0.5em;
                                   top: 17em", p("Up")),
                               sliderInput("move3l3", label = "", min = 0, max = 1,
                                           step= 0.01, ticks = FALSE, value = 0.45),
                               circle = T, status = "danger", icon = icon("sliders"),
                               width = "300px", up = T, #status = MyClass3 
                               tooltip = tooltipOptions(title = "Click to adjust red circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PC3")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("PCl3"))
                        ),
                        fluidRow(column(6, offset = 0, uiOutput("inter3"))),
                        br(),
                        #Cyan
                        fluidRow(
                          column(5, uiOutput("labeldoBGl3")),
                          column(6, offset = 0, uiOutput("AB3")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("ABl3"))
                        ),
                        # Purple
                        fluidRow(
                          column(5, uiOutput("labeldoBRl3")),
                          column(6, offset = 0, uiOutput("AC3")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("ACl3"))
                        ),
                        #Darkolivegreen
                        fluidRow(
                          column(5, uiOutput("labeldoGRl3")),
                          column(6, offset = 0, uiOutput("BC3")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("BCl3"))
                        ),
                        #Central part
                        fluidRow(
                          column(5, uiOutput("labeldoBGRl3")),
                          column(6, offset = 0, uiOutput("ABC3")),
                          column(4, offset = 0, style = 'padding:0px;',
                                 verbatimTextOutput("ABCl3"))
                        )
                      ),
                      
          ########################## 3 Event Numerics #########################
                      conditionalPanel(
                        condition = "input.check3 == 'Numeric3'",
                        #Blue
                        fluidRow(
                          column(4, uiOutput("labeldoBl3")),
                          column(6, offset = 0, uiOutput("PA33")),
                          column(6, numericInput("P3A", label = "", value = NULL,
                                                 min = 0, max = 1, step = 0.01 ))
                        ),
                        #Green
                        fluidRow(
                          column(4, uiOutput("labeldoGl3")),
                          column(6, offset = 0, uiOutput("PB33")),
                          column(6, numericInput("P3B", label = "", value = NULL,
                                                 min = 0, max = 1, step = 0.01 ))
                        ),
                        #Red
                        fluidRow(
                          column(4, uiOutput("labeldoRl3")),
                          column(6, offset= 0, uiOutput("PC33")),
                          column(6, numericInput("P3C", label = "", value = NULL, 
                                                 min = 0, max = 1, step = 0.01 ))
                        ),
                        #Intersection
                        #Cyan
                        fluidRow(
                          column(4, uiOutput("labeldoBGl33")),
                          column(6, offset = 0, uiOutput("AB33")),
                          column(6, numericInput("A3B", label = "", value = NULL, 
                                                 min = 0, max = 1, step = 0.01 ))
                        ),
                        #Purple
                        fluidRow(
                          column(4, uiOutput("labeldoBRl33")),
                          column(6, offset = 0, uiOutput("AC33")),
                          column(6, numericInput("A3C", label = "", value = NULL, 
                                                 min = 0, max = 1, step = 0.01 ))
                        ),
                        #Darkolivegreen
                        fluidRow(
                          column(4, uiOutput("labeldoGRl33")),
                          column(6, offset = 0, uiOutput("BC33")),
                          column(6, numericInput("B3C", label = "", value = NULL, 
                                                 min = 0, max = 1, step = 0.01 ))
                        ),
                        #Central part
                        fluidRow(
                          column(4, uiOutput("labeldoBGRl33")),
                          column(6, offset = 0, uiOutput("ABC33")),
                          column(6, numericInput("A3BC", label = "", value = NULL, 
                                                 min = 0, max = 1, step = 0.01 ))
                        )
                      )
                      
                    )
                  ), 
                  
    ###################MAIN PANEL OF THE VENN DIAGRAM #####################
                  mainPanel(
                    fluidRow(column(1, h2('Challenge'))),
                    fluidRow(column(11, textOutput("instruction"))),
     #################1 Element Probabilities ##############################
                    conditionalPanel(
                      condition = "input.modes == 'level1'", 
                      p(textOutput("questionl1")),
                      conditionalPanel(
                        condition = "input.check1 == 'Numeric'",
                        actionButton("hintSingle1",icon("question", 
                                                        class = "myClass")),
                        
                        flowLayout(
                          div(plotOutput("enterplot1"),
                          ),
                          useShinyjs(),
                          
                          verticalLayout(
                            
                            div(style = "display: inline-block;
                                vertical-align: top; width: 200px",
                                actionButton("pic11", "Sample Answer")),
                            hidden(div(id = 'pic11_div', htmlOutput("Feed1"))))),
                        
                        p("Probability of the complement = ", 
                          textOutput("outsideNumericDiagram1", inline = TRUE)),
                        p(uiOutput("answerl11")),
                        htmlOutput("answerl11Picture"),
                        
                        
                        useShinyjs(),
                        div(style = "display: inline-block;vertical-align:top;
                            width: 200px;",
                            actionButton("next11", "Next Question")),
                        
                      ),
                      
                      
                      ######Slider Level 1 #################
                      
                      
                      #Slider Plot Level 1
                      conditionalPanel(
                        condition = "input.check1 == 'Slider'",
                        actionButton("hintSingle11",
                                     icon("question", class = "myClass")),
                        br(),
                        flowLayout(
                          
                          div(plotOutput("distPlotl1", width = "70%")),
                          
                          useShinyjs(),
                          
                          verticalLayout(
                            div(actionButton("pic1", "Sample Answer")),
                            
                            hidden(div(id = 'pic1_div', htmlOutput("Feed11"))))
                          
                        ),
                        
                        p(uiOutput("answerl1")), 
                        htmlOutput("answerl1Picture"),
                        
                        br(),
                        useShinyjs(),
                        div(actionButton("next1", "Next Question")),
                        
                        br(), br(),
                      )
                      
                      
                      
                    ),
     #################### 2 probabilities #####################################
                    conditionalPanel(
                      condition = "input.modes == 'level2'",
                      ##########Input Level 2 ###################
                      
                      p(textOutput("questionl2")),
                      conditionalPanel(
                        actionButton("hintMultiple22",
                                     icon("question", class = "myClass")),
                        condition = "input.check2 == 'Numeric2'",
                        flowLayout(
                          div(style = "display: inline-block;vertical-align:top;length: 20px", 
                              plotOutput("enterplot2"),
                          ),
                          
                          
                          useShinyjs(),
                          verticalLayout(
                            
                            
                            div(style = "display: inline-block; 
                                vertical-align: top; width: 200px" ,
                                actionButton("pic22", "Sample Answer")),
                            hidden(div(id = 'pic22_div', htmlOutput("Feed2"))))),
                        
                        p("Probability of the intersection of the complements = ",
                          textOutput("outsideNumericDiagram2", inline = TRUE)),
                        
                        
                        p(uiOutput("answerl22")),
                        htmlOutput("answerl22Picture"),
                        
                        useShinyjs(),
                        div(style = "display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next22", "Next Question")),
                        
                      ),
                      ########Slider level 2#################
                      conditionalPanel(
                        condition = "input.check2 == 'Slider2'",
                        actionButton("hintMultiple2",icon("question", 
                                                          class = "myClass")),
                        br(),
                        flowLayout(
                          div(plotOutput("distPlotl2", width = "70%")),
                          useShinyjs(),
                          verticalLayout(
                            div(actionButton("pic2", "Sample Answer")),
                            hidden(div(id = 'pic2_div', htmlOutput("Feed22")))
                          )),
                        
                        p(uiOutput("answerl2")),
                        
                        htmlOutput("answerl2Picture")
                        
                        ,
                        br(),
                        useShinyjs(),
                        
                        div(style = "display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next2", "Next Question"))
                      )
                      
                    ),
          ########################3 Probabilities #############################
                    conditionalPanel(
                      condition = "input.modes == 'level3'",
                      ##########Input 3 ####################
                      p(textOutput("questionl3")), 
                      
                      
                      conditionalPanel(
                        condition = "input.check3 == 'Numeric3'",
                        actionButton("hintMultiple33", icon("question",
                                                           class = "myClass")),
                        flowLayout(
                          div(style = "display: inline-block;vertical-align:top;",
                              plotOutput("enterplot3")
                          ),
                          
                          useShinyjs(),
                          verticalLayout(
                            div(style = "display: inline-block; vertical-align: top; width: 200px",
                                actionButton("pic33", "Sample Answer")),
                            hidden(div(id = 'pic33_div', htmlOutput("Feed3")))
                          )),
                        
                        
                        p("Probability of the intersection of the complements = ",
                          textOutput("outsideNumericDiagram3", inline = TRUE)),
                        
                        p(uiOutput("answerl33")),
                        htmlOutput("answerl33Picture"),
                        
                        br(),
                        useShinyjs(),
                        div(style = "display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next33", "Next Question"))),
                      
                      
                      
                      
                      ####### Slider Level 3 #######################
                      conditionalPanel(
                        condition = "input.check3 == 'Slider3'",
                        actionButton("hintMultiple3", icon("question", class = "myClass")),
                        br(),
                        flowLayout(
                          div(style = "display: inline-block;vertical-align:top;",
                              plotOutput("distPlotl3"), width = "70%"),
                          useShinyjs(),
                          verticalLayout(
                            div(style = "display: inline-block; vertical-align: top; width: 200px",
                                actionButton("pic3", "Sample Answer")),
                            hidden(div(id = 'pic3_div', htmlOutput("Feed33")))
                          )
                        ),
                        
                        p(uiOutput("answerl3")),
                        htmlOutput("answerl3Picture"),
                        
                        
                        br(),
                        useShinyjs(),
                        
                        
                        div(style = "display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next3", "Next Question")),
                        
                      ),
                    ),
                    
                    
    ################ HTML Circle computations #################################
                    tags$head(
                      
                      #Probability of blue circle
                      tags$style(HTML("
                                        #dol3 {
                                        background-color:#B2B2FF;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #B2B2FF;
                                        color:#000000;

                                        }

                                        ")),
                      #Probability of green circle
                      tags$style(HTML("
                                        #do2l3 {
                                        background-color:#B2FFB2;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #B2FFB2;
                                        color:#000000;

                                        }

                                        ")),
                      
                      #Probability of red circle
                      tags$style(HTML("
                                        #do3l3 {
                                        background-color:#FFB2B2;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #FFB2B2;
                                        color:#000000;
                                        }

                                        ")),
                      #Probability of cyan
                      tags$style(HTML("
                                        #do4l3 {
                                        background-color:#7CC9B2;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #7CC9B2;
                                        color:#000000;
                                        
                                        }
                                        
                                        ")),
                      
                      #Probability of purple
                      tags$style(HTML("
                                        #do5l3 {
                                        background-color:#C97CB2;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #C97CB2;
                                        color:#000000;
                                        
                                        }
                                        
                                        ")),
                      
                      #Probability of darkolivegreen
                      tags$style(HTML("
                                        #do6l3 {
                                        background-color:#C9B27C;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #C9B27C;
                                        color:#000000;
                                        
                                        }
                                        
                                        ")),
                      
                      tags$style(HTML("
                                        #do7l3 {
                                        background-color:#A48C7C;
                                        display:block;
                                        height: 50px;
                                        width: 50px;
                                        border-radius: 50%;
                                        border: 1px solid #A48C7C;
                                        color:#000000;
                                        
                                        }
                                        
                                        "))
                      
                    )
                    
                  )
                  
                )
                
        ),
   ################ References Tab ############################################
        tabItem(tabName = "references",
                withMathJax(),
                h2("References"),
                p(class = "hangingindent",
                  "Attali, D. (2020), 
                  shinyjs: Easily Improve the User Experience of Your 
                  Shiny Apps in Seconds, R package. Available from
  https://CRAN.R-project.org/package = shinyjs"),
                p(class = "hangingindent", 
                  "Bailey, E. (2015), 
                  shinyBS: Twitter Bootstrap Components for Shiny, R package. 
                  Available from
  https://CRAN.R-project.org/package = shinyBS"),
                p(class = "hangingindent",
                  "Carey, R. and Hatfield, N. (2020), 
                  boastUtils: BOAST Utilities, R package. Available from
  https://github.com/EducationShinyAppTeam/boastUtils"),
                p(class = "hangingindent",
                  "Chang, W. and Borges Ribeiro, B. (2018), 
                  shinydashboard: Create Dashboards with 'Shiny', R package.
                  Available from
  https://CRAN.R-project.org/package = shinydashboard"),
                p(class = "hangingindent",
                  "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson J. (2020), 
                  shiny: Web application framework for R, R package. Available from 
  https://CRAN.R-project.org/package = shiny"),
                p(class = "hangingindent",
                  "Lemon, J. (2006), 
                  Plotrix: a package in the red light district of R, R package.
                  Available from
  https://CRAN.R-project.org/package = plotrix"),
                p(class = "hangingindent",
                  "Perrier, V., Meyer F., and Granjon D. (2020), 
                  shinyWidgets: Custom Inputs Widgets for Shiny, R package.
                  Available from
  https://CRAN.R-project.org/package = shinyWidgets"),
                p(class = "hangingindent",
                  "Swinton, J. (2020), 
                  Vennerable: Venn and Euler area-proportional diagrams,
                  R package. Available from
  https://github.com/js229/Vennerable")
        )
      )
    )
   )
)
