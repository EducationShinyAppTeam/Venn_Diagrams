library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(Vennerable)

#fresh the whole page
jsResetCode <- "shinyjs.reset= function() {history.go(0)}"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title="Venn Diagrams",
                    titleWidth = 180,
                    tags$li(class = "dropdown", tags$a(href='https://shinyapps.science.psu.edu/',icon("home"))),
                    tags$li(class = "dropdown", actionLink("info",icon("info",class="myClass"))),
                    tags$li(class = "dropdown", actionLink("hint",icon("question",class="myClass")))
                    ),
    dashboardSidebar(
      sidebarMenu(
        id="tabs",
        menuItem("Overview", tabName = "about", icon = icon("dashboard")),
        menuItem("Venn Diagrams", tabName = "circle", icon = icon("cogs"))
        
      )
    ),
    dashboardBody(

      tags$head( 
        tags$link(rel = "stylesheet", type = "text/css", href = "color.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
      ),
      tabItems(
      #Homepage
      # tabItem(tabName = "home",
      #         tags$a(href='http://stat.psu.edu/')
      #         ),
      # About
      tabItem(tabName = "about",
              
              tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
              br(),br(),br(),
              h3(tags$b("About:")),
              h4(p("The goals of this game are to introduce probability, to help you think about the relationships 
                   between events, and how to learn to model real problems using Venn diagrams.")),
              br(),
              h3(tags$b("Instructions:")),
              h4((strong("If you want to adjust the circles in the Venn Diagram:"))),
              h4(tags$li("Move or change the size of the circles using sliders (accessed by the button).")),
              h4(tags$li("The 'Reset' button lets you try again, the 'Next' button provides a new question.")),
              h4((strong("If you want to enter the probabilities directly:"))),
              h4(tags$li("Enter your probability in the textbox.")),
              h4(tags$li("If you want to get hints, please click 'Venn Diagram for Answer' button.")),
              div(style = "text-align: center",bsButton("go", "G O !", icon("bolt"))),br(),
              h3(tags$b("Acknowledgements:")),
              h4("This app was developed and coded by Qichao Chen with input from Yuxin Zhang, Sitong Liu and Yingjie Wang in 2017."),
              h4("This app was modified by Yubaihe Zhou to improve formatting and allow the user to Numeric Input directly in 2018."),
              h4("This app was further modified by Jingjun Wang who added another different Venn diagram in Numeic Input section and reformated the layout of the whole app in 2019.")),
      # Circle Game
      tabItem(tabName = "circle",
              fluidPage(
                # fresh the whole page
                useShinyjs(),
                extendShinyjs(text = jsResetCode),
                # tags$head(
                #   tags$style(".shiny-notification {position:fixed;bottom:50px;left:25%;width:50%;} ")),
                sidebarLayout(
                  sidebarPanel(
                    #hide value of silder bar
                    tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                              visibility: hidden !important;
                                              }'))),
                    selectInput("modes", "Modes",
                                list( 
                                      "One Event" = "level1",
                                      "Two Events" = "level2",
                                      "Three Events" = "level3"
                                )
                    ),
                    bsButton("random", "Random Question"),
                    bsPopover("random", " ","Randomly get questions",
                              placement = "bottom", trigger = "hover", options = NULL),
                    conditionalPanel(
                      # level 1
                      condition="input.modes=='level1'",
                      # div(style="display: inline-block;vertical-align:top;",
                      #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                      # ),
                      # div(style="display: inline-block;vertical-align:top;",
                      #     circleButton("info1",icon = icon("info"), status = "myClass",size = "xs")
                      # ),
                      # div(style="display: inline-block;vertical-align:top;",
                      #     circleButton("hint1",icon = icon("question"), status = "myClass", size = "xs")
                      # ),
                      
                      br(),
                      #Radio Buttons
                      div(style="display: inline-block;vertical-align:top;",
                          prettyRadioButtons("check1",label = NULL,choices = c("Numeric Input" = "Numeric",
                                                                         "Slider Input" = "Slider"),
                                       selected = "Slider", inline = F, width = NULL, status = "primary")
                          ),
                      
                      conditionalPanel(
                        condition = "input.check1 == 'Numeric'",
                        fluidRow(
                          column(4, uiOutput("labeldol1")),
                          column(6, offset= 0, uiOutput("PA11")),
                          column(6, numericInput("PA",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          #column(12, verbatimTextOutput("Feed1"))
                          )
                        
                      ),
                      
                      conditionalPanel(
                        condition = "input.check1 == 'Slider'",
                        fluidRow(
                        column(5, 
                               dropdownButton(
                                 tags$h3("Blue Circle"),
                                 div(style = "position: absolute; left: 0.5em; top: 4em", h5("Small")),
                                 div(style = "position: absolute; right: 0.5em; top: 4em", h5("Large")),
                                 sliderInput("radiusl1",label = NULL,min = 0,max = 1.2,step = 0.005,value = 0.05,ticks = F),
                                 div(style = "position: absolute; left: 0.5em; top: 8em", h5("Left")),
                                 div(style = "position: absolute; right: 0.5em; top: 8em", h5("Right")),
                                 sliderInput("movel1",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.49),
                                 div(style = "position: absolute; left: 0.5em; top: 12em", h5("Down")),
                                 div(style = "position: absolute; right: 0.5em; top: 12em", h5("Up")),
                                 sliderInput("move1l1",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.5),
                                 circle = T, status = "myClass1", icon = icon("sliders"), width = "300px",up = T,
                                 tooltip = tooltipOptions(title = "Click to adjust blue circle !"))
                              ),
                        column(6, offset= 0, uiOutput("PA1")),
                        column(4, offset= 0, style='padding:0px;',verbatimTextOutput("PAl1")) 
                      )),
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
                                        ")))
                        ),
                    conditionalPanel(
                      condition = "input.modes == 'level2'",
                      # div(style="display: inline-block;vertical-align:top;",
                      #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                      # ),
                      # div(style="display: inline-block;vertical-align:top;",
                      #     circleButton("info2",icon = icon("info"), status = "myClass",size = "xs")
                      # ),
                      # div(style="display: inline-block;vertical-align:top;",
                      #     circleButton("hint2",icon = icon("question"), status = "myClass", size = "xs")
                      # ),
                      br(),
                      #Radio Buttons
                      div(style="display: inline-block;vertical-align:top;",
                          prettyRadioButtons("check2",label = NULL,choices = c("Numeric Input" = "Numeric2",
                                                                               "Slider Input" = "Slider2"),
                                             selected = "Slider2", inline = F, width = NULL, status = "primary")
                      ),
                      conditionalPanel(
                        condition = "input.check2 == 'Slider2'",
                        #Blue Circle
                        fluidRow(
                          column(5, 
                                 dropdownButton(
                                   tags$h3("Blue Circle"),
                                   div(style = "position: absolute; left: 0.5em; top: 4em", h5("Small")),
                                   div(style = "position: absolute; right: 0.5em; top: 4em", h5("Large")),
                                   sliderInput("radiusl2",label = NULL,min = 0,max = 1.2,step = 0.005,value = 0.05,ticks = F),
                                   div(style = "position: absolute; left: 0.5em; top: 8em", h5("Left")),
                                   div(style = "position: absolute; right: 0.5em; top: 8em", h5("Right")),
                                   sliderInput("movel12",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.45),
                                   div(style = "position: absolute; left: 0.5em; top: 12em", h5("Down")),
                                   div(style = "position: absolute; right: 0.5em; top: 12em", h5("Up")),
                                   sliderInput("move1l2",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.5),
                                   circle = T, status = "myClass1", icon = icon("sliders"), width = "300px", up = T,
                                   tooltip = tooltipOptions(title = "Click to adjust blue circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PA2")),
                          column(4, offset = 0, style='padding:0px;', verbatimTextOutput("PAl2"))
                        ),
                        #Green Circle
                        fluidRow(
                          column(5, 
                                 dropdownButton(
                                   tags$h3("Green Circle"),
                                   div(style = "position: absolute; left: 0.5em; top: 4em", h5("Small")),
                                   div(style = "position: absolute; right: 0.5em; top: 4em", h5("Large")),
                                   sliderInput("radius2l2",label = NULL,min = 0,max = 1.2,step = 0.005,value = 0.05,ticks = F),
                                   div(style = "position: absolute; left: 0.5em; top: 8em", h5("Left")),
                                   div(style = "position: absolute; right: 0.5em; top: 8em", h5("Right")),
                                   sliderInput("movel2",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.55),
                                   div(style = "position: absolute; left: 0.5em; top: 12em", h5("Down")),
                                   div(style = "position: absolute; right: 0.5em; top: 12em", h5("Up")),
                                   sliderInput("move2l2",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.5),
                                   circle = T, status = "myClass2", icon = icon("sliders"), width = "300px", up = T,
                                   tooltip = tooltipOptions(title = "Click to adjust green circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PB2")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("PBl2"))
                        ),
                        fluidRow(column(6, offset = 0,uiOutput("inter2"))),
                        br(),
                        fluidRow(
                          column(5,uiOutput("labeldoBGl2")),
                          column(6, offset = 0, uiOutput("AB2")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("ABl2"))
                        )
                      ),
                      conditionalPanel(
                        condition = "input.check2 == 'Numeric2'",
                        #Blue
                        fluidRow(
                          column(4,uiOutput("labeldoBl2")),
                          column(6, offset= 0, uiOutput("PA22")),
                          column(6, numericInput("P2A",label = NULL, value=NULL, min = 0.01, max = 1, step = 0.01 ))
                          ),
                        #Green
                        fluidRow(
                          column(4,uiOutput("labeldoGl2")),
                          column(6, offset= 0, uiOutput("PB22")),
                          column(6, numericInput("P2B",label = NULL, value=NULL, min = 0.01, max = 1, step = 0.01 ))
                          ),
                        #Intersection
                        fluidRow(
                          column(4,uiOutput("labeldoBGl22")),
                          column(6, offset= 0, uiOutput("AB22")),
                          column(6, numericInput("A2B",label = NULL, value=NULL, min = 0.01, max = 1, step = 0.01 ))
                          )
                      ),
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
                                        "))
                        )
                      ),
                    conditionalPanel(
                      condition = "input.modes == 'level3'",
                      # div(style="display: inline-block;vertical-align:top;",
                      #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                      # ),
                      # div(style="display: inline-block;vertical-align:top;",
                      #     circleButton("info3",icon = icon("info"), status = "myClass",size = "xs")
                      # ),
                      # div(style="display: inline-block;vertical-align:top;",
                      #     circleButton("hint3",icon = icon("question"), status = "myClass", size = "xs")
                      # ),
                      br(),
                      #Radio Buttons
                      div(style="display: inline-block;vertical-align:top;",
                          prettyRadioButtons("check3",label = NULL,choices = c("Numeric Input" = "Numeric3",
                                                                               "Slider Input" = "Slider3"),
                                             selected = "Slider3", inline = F, width = NULL, status = "primary")
                      ),
                      conditionalPanel(
                        condition = "input.check3 == 'Slider3'",
                        #Blue Circle
                        fluidRow(
                          column(5, 
                                 dropdownButton(
                                   tags$h3("Blue Circle"),
                                   div(style = "position: absolute; left: 0.5em; top: 4em", h5("Small")),
                                   div(style = "position: absolute; right: 0.5em; top: 4em", h5("Large")),
                                   sliderInput("radiusl3",label = NULL,min = 0,max = 1.2,step = 0.005,value = 0.05,ticks = F),
                                   div(style = "position: absolute; left: 0.5em; top: 8em", h5("Left")),
                                   div(style = "position: absolute; right: 0.5em; top: 8em", h5("Right")),
                                   sliderInput("movel13",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.45),
                                   div(style = "position: absolute; left: 0.5em; top: 12em", h5("Down")),
                                   div(style = "position: absolute; right: 0.5em; top: 12em", h5("Up")),
                                   sliderInput("move1l3",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.5),
                                   circle = T, status = "myClass1", icon = icon("sliders"), width = "300px", up = T,
                                   tooltip = tooltipOptions(title = "Click to adjust blue circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PA3")),
                          column(4, offset = 0, style='padding:0px;', verbatimTextOutput("PAl3"))
                        ),
                        #Green Circle
                        fluidRow(
                          column(5, 
                                 dropdownButton(
                                   tags$h3("Green Circle"),
                                   div(style = "position: absolute; left: 0.5em; top: 4em", h5("Small")),
                                   div(style = "position: absolute; right: 0.5em; top: 4em", h5("Large")),
                                   sliderInput("radius2l3",label = NULL,min = 0,max = 1.2,step = 0.005,value = 0.05,ticks = F),
                                   div(style = "position: absolute; left: 0.5em; top: 8em", h5("Left")),
                                   div(style = "position: absolute; right: 0.5em; top: 8em", h5("Right")),
                                   sliderInput("movel23",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.55),
                                   div(style = "position: absolute; left: 0.5em; top: 12em", h5("Down")),
                                   div(style = "position: absolute; right: 0.5em; top: 12em", h5("Up")),
                                   sliderInput("move2l3",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.5),
                                   circle = T, status = "myClass2", icon = icon("sliders"), width = "300px", up = T,
                                   tooltip = tooltipOptions(title = "Click to adjust green circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PB3")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("PBl3"))
                        ),
                        #Red Circle
                        fluidRow(
                          column(5, 
                                 dropdownButton(
                                   tags$h3("Red Circle"),
                                   div(style = "position: absolute; left: 0.5em; top: 4em", h5("Small")),
                                   div(style = "position: absolute; right: 0.5em; top: 4em", h5("Large")),
                                   sliderInput("radius3l3",label = NULL,min = 0,max = 1.2,step = 0.005,value = 0.05,ticks = F),
                                   div(style = "position: absolute; left: 0.5em; top: 8em", h5("Left")),
                                   div(style = "position: absolute; right: 0.5em; top: 8em", h5("Right")),
                                   sliderInput("movel33",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.5),
                                   div(style = "position: absolute; left: 0.5em; top: 12em", h5("Down")),
                                   div(style = "position: absolute; right: 0.5em; top: 12em", h5("Up")),
                                   sliderInput("move3l3",label = NULL,min=0,max=1,step=0.01,ticks = FALSE,value=0.45),
                                   circle = T, status = "myClass3", icon = icon("sliders"), width = "300px", up = T,
                                   tooltip = tooltipOptions(title = "Click to adjust green circle !"))
                          ),
                          column(6, offset = 0, uiOutput("PC3")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("PCl3"))
                        ),
                        
                        fluidRow(column(6, offset = 0,uiOutput("inter3"))),
                        br(),
                        #Cyan
                        fluidRow(
                          column(5,uiOutput("labeldoBGl3")),
                          column(6, offset = 0, uiOutput("AB3")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("ABl3"))
                        ),
                        
                        # Purple
                        fluidRow(
                          column(5,uiOutput("labeldoBRl3")),
                          column(6, offset = 0, uiOutput("AC3")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("ACl3"))
                        ),
                        
                        #darkolivegreen
                        fluidRow(
                          column(5,uiOutput("labeldoGRl3")),
                          column(6, offset = 0, uiOutput("BC3")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("BCl3"))
                        ),
                        
                        #central part
                        fluidRow(
                          column(5,uiOutput("labeldoBGRl3")),
                          column(6, offset = 0, uiOutput("ABC3")),
                          column(4, offset = 0, style='padding:0px;',verbatimTextOutput("ABCl3"))
                        )
                      ),
                      conditionalPanel(
                        condition = "input.check3 == 'Numeric3'",
                        #Blue
                        fluidRow(
                          column(4,uiOutput("labeldoBl3")),
                          column(6, offset= 0, uiOutput("PA33")),
                          column(6, numericInput("P3A",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          ),
                        #Green
                        fluidRow(
                          column(4,uiOutput("labeldoGl3")),
                          column(6, offset= 0, uiOutput("PB33")),
                          column(6, numericInput("P3B",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          ),
                        #Red
                        fluidRow(
                          column(4,uiOutput("labeldoRl3")),
                          column(6, offset= 0, uiOutput("PC33")),
                          column(6, numericInput("P3C",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          ),
                        #Intersection
                        #Cyan
                        fluidRow(
                          column(4,uiOutput("labeldoBGl33")),
                          column(6, offset= 0, uiOutput("AB33")),
                          column(6, numericInput("A3B",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          ),
                        #Purple
                        fluidRow(
                          column(4,uiOutput("labeldoBRl33")),
                          column(6, offset= 0, uiOutput("AC33")),
                          column(6, numericInput("A3C",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          ),
                        #darkolivegreen
                        fluidRow(
                          column(4,uiOutput("labeldoGRl33")),
                          column(6, offset= 0, uiOutput("BC33")),
                          column(6, numericInput("B3C",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          ),
                        #Central part
                        fluidRow(
                          column(4,uiOutput("labeldoBGRl33")),
                          column(6, offset= 0, uiOutput("ABC33")),
                          column(6, numericInput("A3BC",label = NULL, value=NULL, min = 0, max = 1, step = 0.01 ))
                          )
                      )
                      )
                    ),
                  mainPanel(
                    fluidRow(column(1,textOutput("challenge"))),
                
                    fluidRow(column(11,textOutput("instruction"))),
                    conditionalPanel(
                      condition ="input.modes == 'level1'", 
                      h4(textOutput("questionl1")),
                      
                      conditionalPanel(
                        condition = "input.check1 == 'Slider'",
                        
                        br(),
                        flowLayout(
                          
                          div(style="display: inline-block;vertical-align:top;", 
                              plotOutput("distPlotl1", width = "70%")),
                          useShinyjs(),
                          verticalLayout(
                            div(style="display: inline-block; vertical-align: top; width: 200px" , 
                                actionButton("pic1", "Sample Answer", style="color: black; background-color: #fff; ")),
                            hidden(div(id='pic1_div', htmlOutput("Feed11"))))
                          
                        ),
                        wellPanel(
                          h4(textOutput("answerl1"))
                          , style = "width : 55%;background-color: #ffffff;"
                        )
                        ,
                        br(),
                        useShinyjs(),
                        div(style="display: inline-block;vertical-align:top; width: 200px;", 
                            actionButton("feedback1", "Feedback", style="color: #fff; background-color: #337ab7")), 
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next1", "Next Question", style="color: #fff; background-color: #337ab7")),
                       
                        br(),br(),
                      
                        shinyjs::hidden(wellPanel(id = "panelS1",textOutput("fdbc1")
                                                  , style = "width : 55%;background-color: #ffffff;"))
                       
                               ),
                      conditionalPanel(
                        condition = "input.check1 == 'Numeric'",
                        verticalLayout(
                         useShinyjs(),
                          div(style="display: inline-block; vertical-align: top; width: 200px" , 
                              actionButton("pic11", "Sample Answer", style="color: black; background-color: #fff; ")),
                          hidden(div(id='pic11_div', htmlOutput("Feed1")))),
                        
                        wellPanel(
                          h4(textOutput("answerl11"))
                          , style = "width : 55%;background-color: #ffffff;"
                        )
                        ,
                        div(style="display: inline-block;vertical-align:top; width: 200px;", 
                            actionButton("feedback11", "Feedback", style="color: #fff; background-color: #337ab7")), 
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next11","Next Question")),
                        br(),br(),
                        
                        shinyjs::hidden(wellPanel(id = "panelN1",textOutput("fdbc11")
                                                  , style = "width : 55%; background-color: #ffffff;"))
                       #  div(style="display: inline-block;vertical-align:top; width: 200px;",
                       #      prettyCheckbox("pic1","Venn Diagram for Answer",value = F,status = "info",shape = "curve"))
                       # # htmlOutput("Feed11")

                      )
                      
                    ),
                    conditionalPanel(
                      condition = "input.modes == 'level2'",
                      
                      h4(textOutput("questionl2")),
                      conditionalPanel(
                        condition = "input.check2 == 'Slider2'",
                        br(),
                        flowLayout(
                          div(style="display: inline-block;vertical-align:top;", 
                              plotOutput("distPlotl2", width = "70%")),
                          useShinyjs(),
                          verticalLayout(
                            div(style="display: inline-block; vertical-align: top; width: 200px" ,
                                actionButton("pic2", "Sample Answer", style="color: black; background-color: #fff; ")),
                            hidden(div(id='pic2_div', htmlOutput("Feed22")))
                          )),
                        wellPanel(
                          h4(textOutput("answerl2"))
                          , style = "width : 55%;background-color: #ffffff;"
                        )
                        ,
                        br(),
                        useShinyjs(),
                        # div(style="display: inline-block;vertical-align:top; width: 200px;",
                        #     actionButton("submit_buttonl2","Submit", style="color: #fff; background-color: #337ab7")),
                        div(style="display: inline-block;vertical-align:top; width: 200px;", 
                            actionButton("feedback2", "Feedback", style="color: #fff; background-color: #337ab7")), 
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next2","Next Question", style="color: #fff; background-color: #337ab7")),
                        # br(),
                        # hidden( div(id='submit_buttonl2_div', h4(textOutput("correctness2")))),
                        br(), br(),
                        shinyjs::hidden(wellPanel(id = "panelS2",textOutput("fdbc2")
                                                  , style = "width : 55%;background-color: #ffffff;"))
                        
                      ) ,
                      conditionalPanel(
                        condition = "input.check2 == 'Numeric2'",
                        flowLayout(
                          div(style="display: inline-block;vertical-align:top;", 
                              plotOutput("enterplot2")),
                          
                          useShinyjs(),
                          verticalLayout(
                            div(style="display: inline-block; vertical-align: top; width: 200px" ,
                                actionButton("pic22", "Sample Answer", style="color: black; background-color: #fff; ")),
                            hidden(div(id='pic22_div', htmlOutput("Feed2")))
                          )),
                        p("The generated plot does not reflect true scale", style = "color:grey; font-size: 16px;"),
                     
                        wellPanel(
                          h4(textOutput("answerl22"))
                          , style = "width : 55%;background-color: #ffffff;"
                        ),
                        useShinyjs(),
                        div(style="display: inline-block;vertical-align:top; width: 200px;", 
                            actionButton("feedback22", "Feedback", style="color: #fff; background-color: #337ab7")),
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next22","Next Question")),
                       #  div(style="display: inline-block;vertical-align:top; width: 200px;",
                       #      prettyCheckbox("pic2","Venn Diagram for Answer",value = F,status = "info",shape = "curve"))
                       # # htmlOutput("Feed22")
                       br(), br(),
                       shinyjs::hidden(wellPanel(id = "panelN2",textOutput("fdbc22")
                                                 , style = "width : 55%;background-color: #ffffff;"))
                      )
                      ),
                    conditionalPanel(
                      condition = "input.modes =='level3'",
                      
                      h3(textOutput("questionl3")),
                      conditionalPanel(
                        condition = "input.check3 == 'Slider3'",
                        br(),
                        flowLayout(
                          div(style="display: inline-block;vertical-align:top;",
                              plotOutput("distPlotl3"), width = "70%"),
                          useShinyjs(),
                          verticalLayout(
                            div(style="display: inline-block; vertical-align: top; width: 200px" ,
                                actionButton("pic3", "Sample Answer", style="color: black; background-color: #fff; ")),
                            hidden(div(id='pic3_div', htmlOutput("Feed33")))
                          )
                        ),
                        wellPanel(
                          h4(textOutput("answerl3"))
                          , style = "width : 55%;background-color: #ffffff;"
                        )
                        ,
                        br(),
                        useShinyjs(),
                        # div(style="display: inline-block;vertical-align:top; width: 200px;",
                        #     actionButton("submit_buttonl3","Submit", style="color: #fff; background-color: #337ab7")),
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("feedback3", "Feedback", style="color: #fff; background-color: #337ab7")),
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next3","Next Question", style="color: #fff; background-color: #337ab7")),
                        br(),
                       # hidden( div(id='submit_buttonl3_div', h4(textOutput("correctness3")))),
                        br(),
                       shinyjs::hidden(wellPanel(id = "panelS3",textOutput("fdbc3")
                                                 , style = "width : 55%;background-color: #ffffff;"))
                      ),
                      
                      conditionalPanel(
                        condition = "input.check3 == 'Numeric3'",
                        flowLayout(
                          div(style="display: inline-block;vertical-align:top;",
                              plotOutput("enterplot3")),
                          useShinyjs(),
                          verticalLayout(
                            div(style="display: inline-block; vertical-align: top; width: 200px" ,
                                actionButton("pic33", "Sample Answer", style="color: black; background-color: #fff; ")),
                            hidden(div(id='pic33_div', htmlOutput("Feed3")))
                          )
                        ),
                        p("The generated plot does not reflect true scale", style = "color:grey; font-size: 16px;"),
                     
                        wellPanel(
                          h4(textOutput("answerl33"))
                          , style = "width : 70%;background-color: #ffffff;"
                        ),
                        br(),
                        useShinyjs(),
                        div(style="display: inline-block;vertical-align:top; width: 200px;", 
                            actionButton("feedback33", "Feedback", style="color: #fff; background-color: #337ab7")),
                        div(style="display: inline-block;vertical-align:top; width: 200px;",
                            actionButton("next33","Next Question")),
                        # div(style="display: inline-block;vertical-align:top; width: 200px;",
                        #     actionButton("pic3","Venn Diagram for Answer",value = F,status = "info",shape = "curve"))
                       # htmlOutput("Feed33")
                       br(),br(),
                       shinyjs::hidden(wellPanel(id = "panelN3",textOutput("fdbc33")
                                                 , style = "width : 55%;background-color: #ffffff;"))
                      ),
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
                        
                        ))
                    
                        )
                  
                        )
                        )
                      )
    )
                    )))
)
