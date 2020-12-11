# Load Libraries ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(V8)
library(Vennerable)
library(plotrix)
library(boastUtils)

#Note: variables ending in 1 are for the slider diagram with 1 event
#         Variables ending in 11 are for the input diagram with 1 event
#         variables ending in 2 are for the slider diagram with 2 events
#         Variables ending in 22 are for the input diagram with 2 event
#               and so on with 3 eve

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Venn Diagram App"
APP_DESCP  <<- paste(
  "Meant to show the probability of different combinations of events",
  "illustrated by a Venn Diagram.",
  "User can either type in values or use sliders to make them larger or smaller."
)
## End App Meta Data------------------------------------------------------------

# Define Global Constants and Functions, Read Data
## Refresh the whole page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Venn Diagrams",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Venn_Diagrams")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
      )
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem(
          "Overview",
          tabName = "Overview",
          icon = icon("tachometer-alt")
        ),
        menuItem("Venn Diagrams", tabName = "circle", icon = icon("cogs")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(class = "sidebar-logo",
               boastUtils::psu_eberly_logo("reversed"))
    ),
    dashboardBody(
      tabItems(
        #Overview Tab ----
        tabItem(
          tabName = "Overview",
          h1("Venn Diagrams"),
          p(
            "The goals of these challenges are to introduce probability, to help
            you think about the relationships between events, and how to learn to
            model real problems using Venn diagrams."
          ),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li("If you want to adjust the circles in the Venn Diagram:"),
            tags$ol(
              tags$li(
                "Move or change the size of the circles using sliders
                      (accessed by the button)."
              ),
              tags$li("Press the 'Submit' button to see if your answer is correct.")
            ),
            tags$li("If you want to enter the probabilities directly:"),
            tags$ol(
              tags$li("Enter your probability in the textbox."),
              tags$li("Press the 'Submit' button to see if your answer is correct.")
            ),
            tags$li(
              "The 'Reset' button lets you try again, while the 'Next'
                    button provides a new question."
            ),
            tags$li(
              "After attempting the problem, you can check the basic
                    relationship of the events using the 'Sample Answer' button."
            )
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go",
              label = "GO!",
              icon = icon("bolt"),
              size = "large"
            )
          ),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Qichao Chen with input from
            Yuxin Zhang, Sitong Liu and Yingjie Wang in 2017. It was then modified
            by Yubaihe Zhou to allow direct numeric input in 2018. This was further
            modified by Jingjun Wang in 2019. It was then modified by Ethan Wright
            to improve the visual presentation, feedback, and formatting in 2020."
          ),
          div(class = "updated", "Last Update: 07/05/2020 by EJW.")
        ),
        ## Main Venn Diagram Tab ----
        tabItem(
          tabName = "circle",
          useShinyjs(),
          # For refresh the whole page
          extendShinyjs(
            text = jsResetCode,
            functions = c("hidden", "disable", "enable", "toggle")
          ),
          #Place any functions using javascript inside of the c().
          sidebarLayout(
            sidebarPanel(
              #SIDEBAR PANEL OF THE
              #hides value of slider bar
              tags$head(
                tags$style(
                  HTML(
                    '.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                  visibility: hidden !important;}'
                  )
                )
              ),
              #CREATES 3 INPUTS FOR LIST, each creates one of the 3 outputs
              selectInput(
                inputId = "modes",
                label = "Number of events",
                choices = list(
                  "One event" = "level1",
                  "Two events" = "level2",
                  "Three events" = "level3"
                )
              ),
              bsButton(
                inputId = "random",
                label = "Get random question",
                size = "large"
              ),
              ### 1 Event Sidebar ----
              conditionalPanel(
                condition = "input.modes == 'level1'",
                br(),
                #Radio Buttons
                div(
                  style = "display: inline-block;vertical-align:top;",
                  radioButtons(
                    inputId = "check1",
                    label = "Input controls",
                    choices = c("Use numeric input" = "Numeric",
                                "Use slider input" = "Slider"),
                    selected = "Numeric",
                    inline = FALSE
                  )
                ),
                #### Numeric Sidebar Level 1 ----
                conditionalPanel(
                  condition = "input.check1 == 'Numeric'",
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldol1")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "PA",
                        label = uiOutput("PA11"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  )
                ),
                ### Slider Sidebar Level 1 ----
                conditionalPanel(
                  condition = "input.check1 == 'Slider'",
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        #The 3 pop up sliders
                        tags$h3("Blue Circle"),
                        div(
                          style = "position: absolute; left: 0.5em; top: 5em;",
                          p("Small")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 4.5rem;",
                          p("Large")
                        ),
                        sliderInput(
                          inputId = "radiusl1",
                          label = "",
                          min = 0,
                          max = 1.2,
                          step = 0.005,
                          value = 0.05,
                          ticks = F
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 11em;",
                          p("Left")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 11em;",
                          p("Right")
                        ),
                        sliderInput(
                          inputId = "movel1",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.49
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 17em;",
                          p("Down")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 17em;",
                          p("Up")
                        ),
                        sliderInput(
                          inputId = "move1l1",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.5
                        ),
                        circle = TRUE,
                        status = "info",
                        icon = icon("sliders"),
                        width = "300px",
                        up = FALSE,
                        tooltip = tooltipOptions(title = "Click to adjust blue circle!")
                      )
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("PA1", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("PAl1")
                    )
                  )
                ),
                tags$head(#Probability of blue circle
                  tags$style(
                    HTML(
                      "#dol1 {
                      background-color:#B2B2FF;
                      display:block;
                      height: 50px;
                      width: 50px;
                      border-radius: 50%;
                      border: 1px solid #B2B2FF;
                      color:#000000;
                      }"
                    )
                  )
                )
              ),
              ### 2 Events Sidebar ----
              conditionalPanel(
                condition = "input.modes == 'level2'",
                br(),
                #Radio Buttons
                div(
                  style = "display: inline-block; vertical-align:top;",
                  radioButtons(
                    inputId = "check2",
                    label = "Input controls",
                    choices = c("Use numeric input" = "Numeric2",
                                "Use slider Iinput" = "Slider2"),
                    selected = "Numeric2"
                  ),
                ),
                ####2 Events Slider ----
                conditionalPanel(
                  condition = "input.check2 == 'Slider2'",
                  #Blue Circle
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        tags$h3("Blue Circle"),
                        div(
                          style = "position: absolute; left: 0.5em; top: 5em;",
                          p("Small")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 5em;",
                          p("Large")
                        ),
                        sliderInput(
                          inputId = "radiusl2",
                          label = "",
                          min = 0,
                          max = 1.2,
                          step = 0.005,
                          value = 0.05,
                          ticks = F
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 11em;",
                          p("Left")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 11em;",
                          p("Right")
                        ),
                        sliderInput(
                          inputId = "movel12",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.45
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 17em;",
                          p("Down")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 17em;",
                          p("Up")
                        ),
                        sliderInput(
                          inputId = "move1l2",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.5
                        ),
                        circle = TRUE,
                        status = "info",
                        icon = icon("sliders"),
                        width = "300px",
                        up = FALSE,
                        tooltip = tooltipOptions(title = "Click to adjust blue circle!")
                      )
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("PA2", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("PAl2")
                    )
                  ),
                  br(),
                  #Green Circle
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        tags$h3("Green Circle"),
                        div(
                          style = "position: absolute; left: 0.5em; top: 5em;",
                          p("Small")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 5em;",
                          p("Large")
                        ),
                        sliderInput(
                          inputId = "radius2l2",
                          label = "",
                          min = 0,
                          max = 1.2,
                          step = 0.005,
                          value = 0.05,
                          ticks = FALSE
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 11em;",
                          p("Left")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 11em",
                          p("Right")
                        ),
                        sliderInput(
                          inputId = "movel2",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.55
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 17em;",
                          p("Down")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 17em;",
                          p("Up")
                        ),
                        sliderInput(
                          inputId = "move2l2",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.5
                        ),
                        circle = TRUE,
                        status = "success",
                        icon = icon("sliders"),
                        width = "300px",
                        up = FALSE,
                        tooltip = tooltipOptions(title = "Click to adjust green circle!")
                      )
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("PB2", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("PBl2")
                    )
                  ),
                  br(),
                  uiOutput("inter2"),
                  br(),
                  fluidRow(
                    column(width = 2,
                           uiOutput("labeldoBGl2")),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("AB2", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("ABl2")
                    )
                  )
                ),
                ####2 Event Numeric ----
                conditionalPanel(
                  condition = "input.check2 == 'Numeric2'",
                  #Blue
                  fluidRow(
                    column(width = 3,
                           uiOutput("labeldoBl2")),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "P2A",
                        label = uiOutput("PA22"),
                        value = NULL,
                        min = 0.01,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Green
                  fluidRow(
                    column(width = 3,
                           uiOutput("labeldoGl2")),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "P2B",
                        label = uiOutput("PB22"),
                        value = NULL,
                        min = 0.01,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Intersection
                  fluidRow(
                    column(width = 3,
                           uiOutput("labeldoBGl22")),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "A2B",
                        label = uiOutput("AB22"),
                        value = NULL,
                        min = 0.01,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                ),
                ############# Probability of circles ###############
                tags$head(#Probability of blue circle
                  tags$style(
                    HTML(
                      "#dol2 {
                      background-color:#B2B2FF;
                      display:block;
                      height: 50px;
                      width: 50px;
                      border-radius: 50%;
                      border: 1px solid #B2B2FF;
                      color:#000000;
                      }"
                    )
                  ),
                  #Probability of green circle
                  tags$style(
                    HTML(
                      "#do2l2 {
                      background-color:#B2FFB2;
                      display:block;
                      height: 50px;
                      width: 50px;
                      border-radius: 50%;
                      border: 1px solid #B2FFB2;
                      color:#000000;
                      }"
                    )
                  ),
                  #Probability of cyan
                  tags$style(
                    HTML(
                      "#do4l2 {
                      background-color:#7CC9B2;
                      display:block;
                      height: 50px;
                      width: 50px;
                      border-radius: 50%;
                      border: 1px solid #7CC9B2;
                      color:#000000;
                      }"
                    )
                  )
                )
              ),
              ### 3 Events ----
              conditionalPanel(
                condition = "input.modes == 'level3'",
                br(),
                #Radio Buttons
                div(
                  style = "display: inline-block; vertical-align:top;",
                  radioButtons(
                    inputId = "check3",
                    label = "Input controls",
                    choices = c("Use numeric input" = "Numeric3",
                                "Use slider input" = "Slider3"),
                    selected = "Numeric3"
                  )
                ),
                #### 3 Events Slider Input ----
                conditionalPanel(
                  condition = "input.check3 == 'Slider3'",
                  #Blue Circle
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        tags$h3("Blue Circle"),
                        div(
                          style = "position: absolute; left: 0.5em; top: 5em;",
                          p("Small")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 5em;",
                          p("Large")
                        ),
                        sliderInput(
                          inputId = "radiusl3",
                          label = "",
                          min = 0,
                          max = 1.2,
                          step = 0.005,
                          value = 0.05,
                          ticks = F
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 11em;",
                          p("Left")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 11em;",
                          p("Right")
                        ),
                        sliderInput(
                          inputId = "movel13",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.45
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 17em;",
                          p("Down")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 17em;",
                          p("Up")
                        ),
                        sliderInput(
                          inputId = "move1l3",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.5
                        ),
                        circle = TRUE,
                        status = "info",
                        icon = icon("sliders"),
                        width = "300px",
                        up = FALSE,
                        tooltip = tooltipOptions(title = "Click to adjust blue circle!")
                      )
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("PA3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("PAl3")
                    )
                  ),
                  br(),
                  #Green Circle
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        tags$h3("Green Circle"),
                        div(
                          style = "position: absolute; left: 0.5em; top: 5em;",
                          p("Small")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 5em;",
                          p("Large")
                        ),
                        sliderInput(
                          inputId = "radius2l3",
                          label = "",
                          min = 0,
                          max = 1.2,
                          step = 0.005,
                          value = 0.05,
                          ticks = FALSE
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 11em;",
                          p("Left")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 11em;",
                          p("Right")
                        ),
                        sliderInput(
                          inputId = "movel23",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.55
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 17em;",
                          p("Down")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 17em;",
                          p("Up")
                        ),
                        sliderInput(
                          inputId = "move2l3",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.5
                        ),
                        circle = T,
                        status = "success",
                        icon = icon("sliders"),
                        width = "300px",
                        up = FALSE,
                        tooltip = tooltipOptions(title = "Click to adjust green circle!")
                      )
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("PB3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("PBl3")
                    )
                  ),
                  br(),
                  #Red Circle
                  fluidRow(
                    column(
                      width = 2,
                      dropdownButton(
                        tags$h3("Red Circle"),
                        div(
                          style = "position: absolute; left: 0.5em; top: 5em;",
                          p("Small")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 5em;",
                          p("Large")
                        ),
                        sliderInput(
                          inputId = "radius3l3",
                          label = "",
                          min = 0,
                          max = 1.2,
                          step = 0.005,
                          value = 0.05,
                          ticks = F
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 11em,",
                          p("Left")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 11em;",
                          p("Right")
                        ),
                        sliderInput(
                          inputId = "movel33",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.5
                        ),
                        div(
                          style = "position: absolute; left: 0.5em; top: 17em;",
                          p("Down")
                        ),
                        div(
                          style = "position: absolute; right: 0.5em; top: 17em;",
                          p("Up")
                        ),
                        sliderInput(
                          inputId = "move3l3",
                          label = "",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          ticks = FALSE,
                          value = 0.45
                        ),
                        circle = TRUE,
                        status = "danger",
                        icon = icon("sliders"),
                        width = "300px",
                        up = FALSE,
                        tooltip = tooltipOptions(title = "Click to adjust red circle!")
                      )
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("PC3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("PCl3")
                    )
                  ),
                  br(),
                  uiOutput("inter3"),
                  br(),
                  #Cyan
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("labeldoBGl3")
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("AB3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("ABl3")
                    )
                  ),
                  br(),
                  # Purple
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("labeldoBRl3")
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("AC3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("ACl3")
                    )
                  ),
                  br(),
                  #darkolivegreen
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("labeldoGRl3")
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("BC3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("BCl3")
                    )
                  ),
                  br(),
                  #central part
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("labeldoBGRl3")
                    ),
                    column(
                      width = 6,
                      offset = 1,
                      uiOutput("ABC3", style = "text-align: right;")
                    ),
                    column(
                      width = 3,
                      offset = 0,
                      style = 'padding:0px;',
                      verbatimTextOutput("ABCl3")
                    )
                  )
                ),
                #### 3 Event Numerics ----
                conditionalPanel(
                  condition = "input.check3 == 'Numeric3'",
                  #Blue
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldoBl3")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "P3A",
                        label = uiOutput("PA33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Green
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldoGl3")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "P3B",
                        label = uiOutput("PB33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Red
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldoRl3")
                    ),
                    column(
                      width = 8,
                      numericInput(
                        inputId = "P3C",
                        label = uiOutput("PC33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Intersection
                  #Cyan
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldoBGl33")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "A3B",
                        label = uiOutput("AB33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Purple
                  fluidRow(
                    column(
                      width = 4,
                      uiOutput("labeldoBRl33")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "A3C",
                        label = uiOutput("AC33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #darkolivegreen
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldoGRl33")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "B3C",
                        label = uiOutput("BC33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  ),
                  #Central part
                  fluidRow(
                    column(
                      width = 3,
                      uiOutput("labeldoBGRl33")
                    ),
                    column(
                      width = 8,
                      offset = 1,
                      numericInput(
                        inputId = "A3BC",
                        label = uiOutput("ABC33"),
                        value = NULL,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  )
                )
              )
            ),
            ## MAIN PANEL OF THE VENN DIAGRAM ----
            mainPanel(
              h2('Challenge'),
              textOutput("instruction"),
              ###1 Element Probabilities ----
              conditionalPanel(
                condition = "input.modes == 'level1'",
                p(textOutput("questionl1")),
                conditionalPanel(
                  condition = "input.check1 == 'Numeric'",
                  bsButton(
                    inputId = "hintSingle1",
                    label = "Hint",
                    icon = icon("question"),
                    size = "small"
                  ),
                  flowLayout(
                    div(
                      plotOutput("enterplot1")
                    ),
                    verticalLayout(
                      div(
                        style = "display: inline-block; vertical-align: top; width: 200px",
                        bsButton(
                          inputId = "pic11",
                          label = "Sample Answer",
                          size = "default"
                        )
                      ),
                      hidden(
                        div(
                          id = 'pic11_div',
                          htmlOutput("Feed1")
                        )
                      )
                    )
                  ),
                  p(
                    "Probability of the complement = ",
                    textOutput("outsideNumericDiagram1", inline = TRUE)
                  ),
                  uiOutput("answerl11"),
                  uiOutput("answerl11Picture"),
                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    bsButton(
                      inputId = "next11",
                      label = "Next Question",
                      size = "large"
                    )
                  )
                ),
                ####Slider Level 1 ----
                #Slider Plot Level 1
                conditionalPanel(
                  condition = "input.check1 == 'Slider'",
                  bsButton(
                    inputId = "hintSingle11",
                    label = "Hint",
                    icon = icon("question"),
                    size = "small"
                  ),
                  br(),
                  flowLayout(
                    div(
                      plotOutput("distPlotl1", width = "70%")
                    ),
                    verticalLayout(
                      div(
                        bsButton(
                          inputId = "pic1",
                          label = "Sample Answer",
                          size = "default"
                        )
                      ),
                      hidden(
                        div(
                          id = 'pic1_div',
                          htmlOutput("Feed11")
                        )
                      )
                    )
                  ),
                  uiOutput("answerl1"),
                  uiOutput("answerl1Picture"),
                  br(),
                  bsButton(
                    inputId = "next1",
                    label = "Next Question",
                    size = "large"
                  )
                )
              ),
              ### 2 probabilities ----
              conditionalPanel(
                condition = "input.modes == 'level2'",
                #### Input Level 2 ----
                p(textOutput("questionl2")),
                conditionalPanel(
                  condition = "input.check2 == 'Numeric2'",
                  bsButton(
                    inputId = "hintMultiple22",
                    label = "Hint",
                    icon = icon("question"),
                    size = "small"
                  ),
                  flowLayout(
                    div(
                      style = "display: inline-block;vertical-align:top;length: 20px",
                      plotOutput("enterplot2")
                    ),
                    verticalLayout(
                      div(
                        style = "display: inline-block; vertical-align: top; width: 200px" ,
                        bsButton(
                          inputId = "pic22",
                          label = "Sample Answer",
                          size = "default"
                        )
                      ),
                      hidden(
                        div(
                          id = 'pic22_div',
                          htmlOutput("Feed2")
                        )
                      )
                    )
                  ),
                  p(
                    "Probability of the intersection of the complements = ",
                    textOutput("outsideNumericDiagram2", inline = TRUE)
                  ),
                  uiOutput("answerl22"),
                  uiOutput("answerl22Picture"),
                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    bsButton(
                      inputId = "next22",
                      label = "Next Question",
                      size = "large"
                    )
                  )
                ),
                #### Slider level 2 ----
                conditionalPanel(
                  condition = "input.check2 == 'Slider2'",
                  bsButton(
                    inputId = "hintMultiple2",
                    label = "Hint",
                    icon = icon("question"),
                    size = "small"
                  ),
                  br(),
                  flowLayout(
                    div(
                      plotOutput("distPlotl2", width = "70%")
                    ),
                    verticalLayout(
                      div(
                        bsButton(
                          inputId = "pic2",
                          label = "Sample Answer",
                          size = "default"
                        )
                      ),
                      hidden(
                        div(
                          id = 'pic2_div',
                          htmlOutput("Feed22")
                        )
                      )
                    )
                  ),
                  uiOutput("answerl2"),
                  uiOutput("answerl2Picture"),
                  br(),
                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    bsButton(
                      inputId = "next2",
                      label = "Next Question",
                      size = "large"
                    )
                  )
                )
              ),
              ### 3 Probabilities ----
              conditionalPanel(
                condition = "input.modes == 'level3'",
                #### Input 3 ----
                p(textOutput("questionl3")),
                conditionalPanel(
                  condition = "input.check3 == 'Numeric3'",
                  bsButton(
                    inputId = "hintMultiple33",
                    label = "Hint",
                    icon = icon("question"),
                    size = "small"
                  ),
                  flowLayout(
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      plotOutput("enterplot3")
                    ),
                    verticalLayout(
                      div(
                        style = "display: inline-block; vertical-align: top; width: 200px",
                        bsButton(
                          inputId = "pic33",
                          label = "Sample Answer",
                          size = "default"
                        )
                      ),
                      hidden(
                        div(
                          id = 'pic33_div',
                          htmlOutput("Feed3")
                        )
                      )
                    )
                  ),
                  p(
                    "Probability of the intersection of the complements = ",
                    textOutput("outsideNumericDiagram3", inline = TRUE)
                  ),
                  uiOutput("answerl33"),
                  uiOutput("answerl33Picture"),
                  br(),
                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    bsButton(
                      inputId = "next33",
                      label = "Next Question",
                      size = "large"
                    )
                  )
                ),
                #### Slider Level 3 ----
                conditionalPanel(
                  condition = "input.check3 == 'Slider3'",
                  bsButton(
                    inputId = "hintMultiple3",
                    label = "Hint",
                    icon = icon("question"),
                    size = "small"
                  ),
                  br(),
                  flowLayout(
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      plotOutput("distPlotl3", width = "70%")
                    ),
                    verticalLayout(
                      div(
                        style = "display: inline-block; vertical-align: top; width: 200px",
                        bsButton(
                          inputId = "pic3",
                          label = "Sample Answer",
                          size = "default"
                        )
                      ),
                      hidden(
                        div(
                          id = 'pic3_div',
                          htmlOutput("Feed33")
                        )
                      )
                    )
                  ),
                  uiOutput("answerl3"),
                  uiOutput("answerl3Picture"),
                  br(),
                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    bsButton(
                      inputId = "next3",
                      label = "Next Question",
                      size = "large"
                    )
                  )
                )
              ),
              ################ HTML Circle computations #################################
              tags$head(
                #Probability of blue circle
                tags$style(
                  HTML(
                    "#dol3 {
                    background-color:#B2B2FF;
                    display:block;
                    height: 50px;
                    width: 50px;
                    border-radius: 50%;
                    border: 1px solid #B2B2FF;
                    color:#000000;
                  }"
                  )
                ),
                #Probability of green circle
                tags$style(
                  HTML(
                    "#do2l3 {
                  background-color:#B2FFB2;
                  display:block;
                  height: 50px;
                  width: 50px;
                  border-radius: 50%;
                  border: 1px solid #B2FFB2;
                  color:#000000;
                }"
                  )
                ),
                #Probability of red circle
                tags$style(
                  HTML(
                    "#do3l3 {
                  background-color:#FFB2B2;
                  display:block;
                  height: 50px;
                  width: 50px;
                  border-radius: 50%;
                  border: 1px solid #FFB2B2;
                  color:#000000;
                }"
                  )
                ),
                #Probability of cyan
                tags$style(
                  HTML(
                    "#do4l3 {
                  background-color:#7CC9B2;
                  display:block;
                  height: 50px;
                  width: 50px;
                  border-radius: 50%;
                  border: 1px solid #7CC9B2;
                  color:#000000;
                }"
                  )
                ),
                #Probability of purple
                tags$style(
                  HTML(
                    "#do5l3 {
                    background-color:#C97CB2;
                    display:block;
                    height: 50px;
                    width: 50px;
                    border-radius: 50%;
                    border: 1px solid #C97CB2;
                    color:#000000;
                  }"
                  )
                ),
                #Probability of darkolivegreen
                tags$style(
                  HTML(
                    "#do6l3 {
                     background-color:#C9B27C;
                    display:block;
                    height: 50px;
                    width: 50px;
                    border-radius: 50%;
                    border: 1px solid #C9B27C;
                    color:#000000;
                  }"
                  )
                ),
                tags$style(
                  HTML(
                    "#do7l3 {
                    background-color:#A48C7C;
                    display:block;
                    height: 50px;
                    width: 50px;
                    border-radius: 50%;
                    border: 1px solid #A48C7C;
                    color:#000000;
                  }"
                  )
                )
              )
            )
          )
        ),
        ## References Tab ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, D. (2020), shinyjs: Easily Improve the User Experience of Your
          Shiny Apps in Seconds, R package. Available from
          https://CRAN.R-project.org/package=shinyjs"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter Bootstrap Components for Shiny, R
          package. Available fromhttps://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020), boastUtils: BOAST Utilities, R
          package. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2018), shinydashboard: Create
          Dashboards with 'Shiny', R package. Available from
          https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson J. (2020),
          shiny: Web application framework for R, R package. Available from
          https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Lemon, J. (2006), Plotrix: a package in the red light district of R,
          R package. Available from https://CRAN.R-project.org/package=plotrix"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer F., and Granjon D. (2020), shinyWidgets: Custom Inputs
          Widgets for Shiny, R package. Available from
          https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Swinton, J. (2020), Vennerable: Venn and Euler area-proportional diagrams,
          R package. Available from https://github.com/js229/Vennerable"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the Server ----
server <- function(session, input, output) {
  disable("pic33")
  disable("pic22")
  disable("pic11")

  #Go Button ----
  observeEvent(input$go, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "circle")
  })

  # Info button ----
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = p(
        tags$ul(
          tags$li("Using Sliders"),
          tags$ol(
            tags$li("Move or change the size of the circles using sliders
                    (accessed by the button)."
            )
          ),
          tags$li("Entering Probabilities"),
          tags$ol(
            tags$li("Enter your probability in the textbox.")
          )
        ),
        "Once the correct image appears move on to the next question",
        br(),
        br(),
        "After attempting the problem, you can check the basic relationship of
        the events using the 'Sample Answer' button.",
        br(),
        br(),
        "The 'Sample Answer' button shows a basic idea of what a correct
        diagram will look like; the 'Next' button provides a new question."
      ),
      type = "info",
      html = TRUE
    )
  })

  #The following is related to the ? mark buttons that appear in the body of
  #the tabs. that give out large pop up alerts
  observeEvent(input$hintSingle1, {
    sendSweetAlert(
      session = session,
      title = "Strategy",
      text = tags$span(
        "Understand if you are dealing with the event or the compliment of
        the event",
      )
    )
  })

  observeEvent(input$hintSingle11, {
    sendSweetAlert(
      session = session,
      title = "Strategy",
      text = tags$span(
        "Understand if you are dealing with the event or the compliment of the
        event",
        tags$br(),
        tags$br(),
        "In order to change the diagram circle click the colored circles in
        the side bar"
      )
    )
  })

  observeEvent(input$hintMultiple2, {
    outputMultipleHint2()
  })
  observeEvent(input$hintMultiple22, {
    outputMultipleHint()
  })
  observeEvent(input$hintMultiple3, {
    outputMultipleHint2()
  })
  observeEvent(input$hintMultiple33, {
    outputMultipleHint()
  })
  outputMultipleHint <- function() {
    sendSweetAlert(
      session = session,
      title = "Strategy",
      text = tags$span(
        "1. Understand how many events are involved",
        tags$br(),
        "2. Check for mutually exclusive relationships",
        tags$br(),
        "3. Check for independent relationships"
      ),
      type = "info",
      html = TRUE
    )
  }
  outputMultipleHint2 <- function() {
    sendSweetAlert(
      session = session,
      title = "Strategy",
      text = tags$span(
        "1. Understand how many events are involved",
        tags$br(),
        "2. Check for mutually exclusive relationships",
        tags$br(),
        "3. Check for independent relationships",
        tags$br(),
        tags$br(),
        "In order to change the diagram circles click the colored
        circles in the side bar"
      ),
      type = "info",
      html = TRUE
    )
  }

  output$challenge <- renderText({
    paste("Challenge")
  })

  ################# 1 Event #############################

  ######### Input Values 1 Event ##########

  #output saying if answer is correct or not
  output$answerl11 <- renderUI({
    validate(need(input$PA != "", ""),
             errorClass = "inline")

    if (input$PA == bank[numbersl1$quesanswerl1, 5]) {
      updateButton(session, "next11", disabled = F)
      p("Great! You are right!", class = "answertext")
    }

    else{
      updateButton(session, "next11", disabled = F)
      p("Please adjust your answer", class = "redtext")
    }

  })

  output$outsideNumericDiagram1 <- reactive({
    {
      validate(need(input$PA != "", "Please enter the probability"))
      if (1 - input$PA <= 1 && 1 - input$PA >= 0)
        1 - input$PA
      else
        "Impossible to exist"
    }
  })


  #Output the check or the X
  output$answerl11Picture <-
    renderUI({
      #input$SubmitNumeric1
      validate(need(input$PA != "", ""))
      if (input$PA == bank[numbersl1$quesanswerl1, 5]) {
        img(src = "correct.png",
            alt = "Correct",
            width = 30)
      }
      else{
        img(src = "incorrect.png",
            alt = "Incorrect",
            width = 30)
      }
    })

  #Graph for One Event input number
  w1 = reactive({
    #input$SubmitNumeric1
    compute.Venn(Venn(
      SetNames = c("1", "2"),
      Weight = c(
        `01` = input$PA,
        `11` = .05,
        `10` = .01
      )
    ),
    type = "circles",
    doEuler = TRUE)
  })
  output$enterplot1 <- renderPlot({
    #input$SubmitNumeric1
    validate(need(input$PA != "", "Please enter the probability"))
    isolate({
      plot(
        c(0, 1),
        c(0, 1),
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        ann = FALSE
      )
    })

    col1l1 <- rgb(
      red = 0,
      green = 0,
      blue = 1,
      alpha = 0.3
    )

    if (input$PA >= 0 && input$PA <= 1) {
      draw.circle(.5, .5, input$PA / 1.77, col = col1l1)
      enable("pic11")
    }
    else{
      "Impossible to exist"
      enable("pic11")
    }
  }, width = 350, height = 350)


  #pic1
  observeEvent(input$pic1, {
    toggle('pic1_div')
    output$Feed11 <- renderUI({
      img(
        src = bank[numbersl1$quesanswerl1, 19],
        alt = bank[numbersl1$quesanswerl1, 20],
        height = "70%",
        width = "70%"
      )
    })
  })
  #observation
  observeEvent(input$pic11,
               {
                 toggle('pic11_div')
                 output$Feed1 <- renderUI({
                   img(
                     src = bank[numbersl1$quesanswerl1, 19],
                     alt = bank[numbersl1$quesanswerl1, 20],
                     height = "70%",
                     width = "70%"
                   )
                 })
               })



  observeEvent(input$next11, {
    hide('pic11_div')
    numbersl1$quesanswerl1 <-
      sample(space[-numbersl1$quesanswerl1], 1)
    updateNumericInput(
      session,
      "PA",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateButton(session, "next11", disabled = F)
    updateCheckboxInput(session, "pic1", value = F)
    disable("pic11")
  })


  ######### Slider Values 1 Event ############
  output$distPlotl1 <- renderPlot({
    isolate({
      plot(
        c(0, 1),
        c(0, 1),
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        ann = FALSE
      )
    })

    col1l1 <- rgb(
      red = .0,
      green = 0,
      blue = 1,
      alpha = 0.3
    )
    draw.circle(input$movel1, input$move1l1, input$radiusl1, col = col1l1)
  }, width = 350, height = 350)

  # using points simulating prob
  probabilityl1 <- reactiveValues(probc1l1 = NULL)

  # Simulation Probabilties
  observe({
    subdividel1 <- 350

    xcoordl1 <- NULL
    ycoordl1 <- NULL

    for (i in 1:subdividel1) {
      xcoordl1 <- c(xcoordl1,
                    seq(
                      from = 1 / subdividel1,
                      to = 1,
                      by = 1 / subdividel1
                    ))
      ycoordl1 <- c(ycoordl1, rep(i / subdividel1, subdividel1))
    }

    samplespacel1 <- data.frame(xcoordl1, ycoordl1)
    #These use values of the 3 slidebars in slider input
    samplespacel1$radiusl1 <- input$radiusl1
    samplespacel1$xcenterl1 <- input$movel1
    samplespacel1$ycenterl1 <- input$move1l1
    #Here they seem to calculate the radius again
    samplespacel1$diffl1 <- sqrt((samplespacel1$xcenterl1
                                  - samplespacel1$xcoordl1) ^ 2
                                 + (samplespacel1$ycenterl1
                                    - samplespacel1$ycoordl1) ^ 2
    )
    samplespacel1$inc1l1 <-
      samplespacel1$diffl1 <= samplespacel1$radiusl1

    probl1 <- mean(samplespacel1$inc1l1)
    probabilityl1$probc1l1 <- signif(probl1, 2)
  })

  observe({
    output$labeldol1 <- renderUI({
      bsButton("dol1", "", class = "btn-group")
    })
  })

  observeEvent(input$reset_buttonl1, {
    updateSliderInput(
      session,
      "radiusl1",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel1",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
    updateSliderInput(
      session,
      "move1l1",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
  })
  ### random choose question
  numbersl1 <- reactiveValues(quesanswerl1 = c())
  observe({
    numbersl1$quesanswerl1 = sample(1:5, 1)
  })
  output$PA1 <- renderPrint({
    cat(bank[numbersl1$quesanswerl1, 12])
  })
  output$PA11 <- renderPrint({
    cat(bank[numbersl1$quesanswerl1, 12])
  })
  output$PAl1 <- renderPrint({
    cat(probabilityl1$probc1l1)
  })
  space <- c(1:5)
  #Reset the sliders when the next question comes up
  observeEvent(input$next1, {
    hide('pic1_div')
    numbersl1$quesanswerl1 <-
      sample(space[-numbersl1$quesanswerl1], 1)
    updateSliderInput(
      session,
      "radiusl1",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel1",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.49
    )
    updateSliderInput(
      session,
      "move1l1",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
  })
  #question
  output$questionl1 <- renderText(bank[numbersl1$quesanswerl1, 4])


  output$answerl1 <- renderUI({
    if (probabilityl1$probc1l1 == bank[numbersl1$quesanswerl1, 5]) {
      updateButton(session, "next1", disabled = F)
      p("Great! You are right!", class = "answertext")
    }
    else{
      updateButton(session, "next1", disabled = F)
      p("Keep changing the size of your circle
        to match the probability desired",
        class = "redtext")
    }
  })
  output$answerl1Picture <- renderUI({
    if (probabilityl1$probc1l1 == bank[numbersl1$quesanswerl1, 5]) {
      img(src = "correct.png",
          alt = "Correct",
          width = 30)
    }
    else{
      img(src = "incorrect.png",
          alt = "Incorrect",
          width = 30)
    }
  })

  observeEvent(input$feedback11, {
    toggle(id = "panelN1")
  })

  ################# 2 Events #################################
  ######### 2 Events input #######

  observeEvent(input$pic22, {
    toggle('pic22_div')
    output$Feed2 <- renderUI({
      img(
        src = bank[numbersl2$quesanswerl2, 19],
        alt = bank[numbersl2$quesanswerl2, 20],
        height = "70%",
        width = "70%"
      )
    })
  })

  output$answerl22 <- renderUI({
    validate(need(((input$P2A != "") &
                     (input$P2B != "") & (input$A2B != "")
    ), ""))
    if ((input$P2A == bank[numbersl2$quesanswerl2, 5]) &
        (input$P2B == bank[numbersl2$quesanswerl2, 6]) &
        (input$A2B == bank[numbersl2$quesanswerl2, 8])) {
      updateButton(session, "next22", disabled = F)
      p("Great! You are right!", class = "answertext")

    }
    else{
      updateButton(session, "next22", disabled = F)
      p("Please adjust your answer", class = "redtext")
    }
  })

  #Output the check or the X
  output$answerl22Picture <- renderUI({
    validate(need(((input$P2A != "") &
                     (input$P2B != "") & (input$A2B != "")
    ), ""))
    if ((input$P2A == bank[numbersl2$quesanswerl2, 5]) &
        (input$P2B == bank[numbersl2$quesanswerl2, 6]) &
        (input$A2B == bank[numbersl2$quesanswerl2, 8])) {
      img(src = "correct.png",
          alt = "Correct",
          width = 30)
    }
    else{
      img(src = "incorrect.png",
          alt = "Incorrect",
          width = 30)
    }
  })



  #Stuff not in the first yet
  w2 = reactive({
    compute.Venn(Venn(
      SetNames = c("", ""),
      Weight = c(
        `01` = input$P2B - input$A2B,
        `11` = input$A2B,
        `10` = input$P2A - input$A2B
      )
    ),
    type = "circles",
    doEuler = TRUE)
  })

  output$outsideNumericDiagram2 = reactive({
    validate(need(((input$P2A != "") &
                     (input$P2B != "") & (input$A2B != "")
    ),
    "Please enter the probabilities"))

    if (((min(input$P2A, input$P2B) == 0) & (input$A2B == 0)) ||
        ((input$P2A == input$P2B) & (input$P2A == input$A2B) &
         (input$P2B == input$A2B))
        || ((input$P2A + input$P2B - input$A2B <= 1) &
            (input$A2B <= min(input$P2A, input$P2B))))
      1 - (input$P2B - input$A2B) - input$A2B - (input$P2A - input$A2B)
    else
      "Impossible to exist"
  })
  output$enterplot2 = renderPlot({
    #Confirms that all 3 inputs are used.
    validate(need(((input$P2A != "") &
                     (input$P2B != "") & (input$A2B != "")
    ),
    "Please enter the probabilities"))
    #If 0 for combination and at least one 0 in a category
    #   output remind need more than 1 event
    if ((min(input$P2A, input$P2B) == 0) & (input$A2B == 0)) {
      isolate({
        plot(
          1,
          1,
          col = "white",
          type = 'n',
          xaxt = 'n',
          yaxt = 'n',
          ann = FALSE
        )
      })
      text(1,
           1,
           "Note that there are two events",
           cex = 1,
           col = "red")
    }
    #If all of the values are equal
    else if ((input$P2A == input$P2B) & (input$P2A == input$A2B) &
             (input$P2B == input$A2B)) {
      isolate({
        plot(
          1,
          1,
          col = "white",
          type = 'n',
          xaxt = 'n',
          yaxt = 'n',
          ann = FALSE
        )
      })
      enable("pic22")
      draw.circle(1, 1, .1, col = "#7cc9b2")
    }
    #If all the inputs are met
    else if ((input$P2A + input$P2B - input$A2B <= 1) &
             (input$A2B <= min(input$P2A, input$P2B))) {
      gp <- VennThemes(w2())
      gp[["Face"]][["11"]]$fill <-  "#79CAB1" #original mitegreen
      gp[["Face"]][["01"]]$fill <-  "#B6FEB5" #original green
      gp[["Face"]][["10"]]$fill <-  '#B3B2FF' #original purple
      gp[["Set"]][["Set1"]]$col <- 'black'
      gp[["Set"]][["Set2"]]$col <- 'black'
      gp[["Set"]][["Set1"]]$lwd <- 1.5
      gp[["Set"]][["Set2"]]$lwd <- 1.5
      enable("pic22")
      plot(w2(), gp = gp, show = list(SetLabels = FALSE)) #calls the plot function
    }
    else{
      #If both of these results invalid
      plot(
        1,
        1,
        col = "white",
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        ann = FALSE
      )
      text(1,
           1,
           "Error: impossible to exist",
           cex = 1,
           col = "red")
      enable("pic22")
    }
  }, width = 300, height = 280)


  observeEvent(input$next22, {
    numbersl2$quesanswerl2 <- sample(space2[-numbersl2$quesanswerl2], 1)
    updateNumericInput(
      session,
      "P2A",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "P2B",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "A2B",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateButton(session, "next22", disabled = F)
    updateCheckboxInput(session, "pic2", value = F)
    hide('pic22_div')
    disable("pic22")
  })

  ########### Slider Plot 2 Events ##################

  output$distPlotl2 <- renderPlot({
    # input$calculatel2 #Re-run when the button is clicked
    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    # progress$set(message = "Making plot", value = 10)
    isolate({
      plot(
        c(0, 1),
        c(0, 1),
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        ann = FALSE
      )

    })

    col1l2 <- rgb(
      red = 0,
      green = 0,
      blue = 1,
      alpha = 0.3
    )
    col2l2 <- rgb(
      red = 0,
      green = 1,
      blue = 0,
      alpha = 0.3
    )


    draw.circle(input$movel12, input$move1l2, input$radiusl2, col = col1l2)
    draw.circle(input$movel2, input$move2l2, input$radius2l2, col = col2l2)

  }, width = 350, height = 350)



  probabilityl2 <- reactiveValues(
    probc1l2 = NULL,
    probc2l2 = NULL,
    probintersectionl2 = NULL,
    dl2 = NULL

  )
  observe({
    probabilityl2$dl2 <- ((input$movel12 - input$movel2) ^ 2 +
                            (input$move1l2 - input$move2l2) ^ 2) ^ 0.5

    subdividel2 <- 350

    xcoordl2 <- NULL
    ycoordl2 <- NULL

    for (i in 1:subdividel2) {
      xcoordl2 <- c(xcoordl2,
                    seq(
                      from = 1 / subdividel2,
                      to = 1,
                      by = 1 / subdividel2
                    ))
      ycoordl2 <- c(ycoordl2, rep(i / subdividel2, subdividel2))

    }

    samplespacel2 <- data.frame(xcoordl2, ycoordl2)
    #Slider values
    samplespacel2$radiusc1l2 <- input$radiusl2
    samplespacel2$radiusc2l2 <- input$radius2l2
    samplespacel2$xcenterc1l2 <- input$movel12
    samplespacel2$ycenterc1l2 <- input$move1l2

    samplespacel2$xcenterc2l2 <- input$movel2
    samplespacel2$ycenterc2l2 <- input$move2l2


    samplespacel2$diffc1l2 <- sqrt((samplespacel2$xcenterc1l2
                                    - samplespacel2$xcoordl2) ^ 2
                                   + (samplespacel2$ycenterc1l2
                                      - samplespacel2$ycoordl2) ^ 2
    )
    samplespacel2$inc1l2 <-
      samplespacel2$diffc1l2 <= samplespacel2$radiusc1l2

    samplespacel2$diffc2l2 <- sqrt((samplespacel2$xcenterc2l2
                                    - samplespacel2$xcoordl2) ^ 2
                                   + (samplespacel2$ycenterc2l2
                                      - samplespacel2$ycoordl2) ^ 2
    )
    samplespacel2$inc2l2 <-
      samplespacel2$diffc2l2 <= samplespacel2$radiusc2l2

    samplespacel2$intersectionl2 <- (samplespacel2$diffc1l2 <=
                                       samplespacel2$radiusc1l2) &
      (samplespacel2$diffc2l2 <= samplespacel2$radiusc2l2)

    p1l2 <- mean(samplespacel2$inc1l2)
    probabilityl2$probc1l2 <- signif(p1l2, 2)

    p2l2 <- mean(samplespacel2$inc2l2)
    probabilityl2$probc2l2 <- signif(p2l2, 2)

    p3l2 <- mean(samplespacel2$intersectionl2)
    probabilityl2$probintersectionl2 <- signif(p3l2, 2)
  })

  observe({
    output$labeldoBl2 <- renderUI({
      bsButton("dol2", "", class = "btn-group")
    })

    output$labeldoGl2 <- renderUI({
      bsButton("do2l2", "", class = "btn-group")
    })
    output$labeldoBGl2 <- renderUI({
      bsButton("do4l2", "", class = "btn-group")
    })
    output$labeldoBGl22 <- renderUI({
      bsButton("do4l2", "", class = "btn-group")
    })
  })
  output$inter2 <- renderUI(p("Intersection"))
  output$PAl2 <- renderPrint(cat(probabilityl2$probc1l2))
  output$PBl2 <- renderPrint(cat(probabilityl2$probc2l2))
  output$ABl2 <- renderPrint(cat(probabilityl2$probintersectionl2))

  #reset
  observeEvent(input$reset_buttonl2, {
    updateSliderInput(
      session,
      "radiusl2",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel12",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.45
    )
    updateSliderInput(
      session,
      "move1l2",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )

    updateSliderInput(
      session,
      "radius2l2",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel2",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.55
    )
    updateSliderInput(
      session,
      "move2l2",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
  })
  ### random choose question
  numbersl2 <- reactiveValues(quesanswerl2 = c())

  observe({
    numbersl2$quesanswerl2 = sample(6:10, 1)
  })
  output$PA2 <- renderPrint({
    cat(bank[numbersl2$quesanswerl2, 12])
  })
  output$PA22 <- renderPrint({
    cat(bank[numbersl2$quesanswerl2, 12])
  })
  output$PB2 <- renderPrint({
    cat(bank[numbersl2$quesanswerl2, 13])
  })
  output$PB22 <- renderPrint({
    cat(bank[numbersl2$quesanswerl2, 13])
  })
  output$AB2 <- renderPrint({
    cat(bank[numbersl2$quesanswerl2, 15])
  })
  output$AB22 <- renderPrint({
    cat(bank[numbersl2$quesanswerl2, 15])
  })
  output$questionl2 <- renderText(bank[numbersl2$quesanswerl2, 4])
  space2 <- c(6:10)
  #Generate next question, reset the sliders
  observeEvent(input$next2, {
    numbersl2$quesanswerl2 <- sample(space2[-numbersl2$quesanswerl2], 1)
    updateSliderInput(
      session,
      "radiusl2",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel12",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.45
    )
    updateSliderInput(
      session,
      "move1l2",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )

    updateSliderInput(
      session,
      "radius2l2",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel2",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.55
    )
    updateSliderInput(
      session,
      "move2l2",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
    hide('pic2_div')
  })
  observeEvent(input$feedback2, {
    toggle(id = "panelS2")
  })


  observeEvent(input$feedback22, {
    toggle(id = "panelN2")
  })


  output$answerl2 <- renderUI({
    if ((probabilityl2$probc1l2 == bank[numbersl2$quesanswerl2, 5]) &
        (probabilityl2$probc2l2 == bank[numbersl2$quesanswerl2, 6]) &
        (probabilityl2$probintersectionl2 == bank[numbersl2$quesanswerl2, 8]))
    {
      updateButton(session, "next2", disabled = F)
      p("Great! You are right!", class = "answertext")
    }

    else{
      updateButton(session, "next2", disabled = F)
      p(
        "Keep changing the size and placement of your circles to match
        the probabilities desired.",
        class = "redtext"
      )
    }
  })


  output$answerl2Picture <- renderUI({
    if ((probabilityl2$probc1l2 == bank[numbersl2$quesanswerl2, 5]) &
        (probabilityl2$probc2l2 == bank[numbersl2$quesanswerl2, 6]) &
        (probabilityl2$probintersectionl2 ==
         bank[numbersl2$quesanswerl2, 8])) {
      img(src = "correct.png",
          alt = "Correct",
          width = 30)
    }
    else{
      img(src = "incorrect.png",
          alt = "Incorrect",
          width = 30)
    }
  })


  observeEvent(input$pic2, {
    toggle('pic2_div')
    output$Feed22 <- renderUI({
      img(
        src = bank[numbersl2$quesanswerl2, 19],
        alt = bank[numbersl2$quesanswerl2, 20],
        height = "70%",
        width = "70%"
      )
    })
  })

  ################# 3 Events Body #################################
  ############# 3 Events Numeric Input ###################
  observeEvent(input$pic33, {
    toggle('pic33_div')
    output$Feed3 <- renderUI({
      img(
        src = bank[numbersl3$quesanswerl3, 19],
        alt = bank[numbersl3$quesanswerl3, 20],
        height = "70%",
        width = "70%"
      )
    })
  })

  #Calculate all numbers inside of the Venn Diagram
  w3 = reactive({
    compute.Venn(Venn(
      SetNames = c("1", "2", "3"),
      Weight = c(
        `001` = round(input$P3B - input$A3B - input$B3C + input$A3BC, 4),
        `010` = round(input$P3C - input$A3C - input$B3C + input$A3BC, 4),
        `100` = round(input$P3A - input$A3B - input$A3C + input$A3BC, 4),
        `101` = round(input$A3B - input$A3BC, 4),
        `110` = round(input$A3C - input$A3BC, 4),
        `011` = round(input$B3C - input$A3BC, 4),
        `111` = round(input$A3BC, 4)
      )
    ),
    type = "circles",
    doEuler = TRUE)
  })

  output$outsideNumericDiagram3 = reactive({
    validate(need(((input$P3A != "") &
                     (input$P3B != "") & (input$P3C != "") &
                     (input$A3B != "") & (input$A3C != "") &
                     (input$B3C != "") &
                     (input$A3BC != "")
    ),
    "Please enter the probabilities"))
    if ((input$P3A + input$P3B + input$P3C - input$A3B - input$A3C
         - input$B3C  <= 1) #If all of the values combined are less than 1
        &
        (
          0 <= min(
            round(input$P3B - input$A3B - input$B3C + input$A3BC, 4),
            round(input$P3C - input$A3C - input$B3C + input$A3BC, 4),
            round(input$P3A - input$A3B - input$A3C + input$A3BC, 4),
            input$A3B - input$A3BC,
            input$A3C - input$A3BC,
            input$B3C - input$A3BC,
            input$A3BC
          )
        ))
    1 - round(input$P3B - input$A3B - input$B3C + input$A3BC, 4) -
      round(input$P3C - input$A3C - input$B3C + input$A3BC, 4) -
      round(input$P3A - input$A3B - input$A3C + input$A3BC, 4) -
      round(input$A3B - input$A3BC, 4) -
      round(input$A3C - input$A3BC, 4) - round(input$B3C - input$A3BC, 4) -
      round(input$A3BC, 4)
    else
      "Impossible to exist"
  })

  output$enterplot3 = renderPlot({
    validate(need(((input$P3A != "") &
                     (input$P3B != "") & (input$P3C != "") &
                     (input$A3B != "") & (input$A3C != "") &
                     (input$B3C != "") &
                     (input$A3BC != "")
    ),
    "Please enter the probabilities"))
    if ((min(input$P3A, input$P3B, input$P3C) == 0) &
        (input$A3B == 0) &
        (input$A3C == 0) & (input$B3C == 0) & (input$A3BC == 0))
    {
      isolate({
        plot(
          1,
          1,
          col = "white",
          type = 'n',
          xaxt = 'n',
          yaxt = 'n',
          ann = FALSE
        )
      })
      text(1,
           1,
           "Note that there are three events",
           cex = 1,
           col = "red")

    }
    else if ((input$P3A + input$P3B + input$P3C - input$A3B - input$A3C
              - input$B3C  <= 1) #If all of the values combined are less than 1
             &
             (
               0 <= min(
                 round(input$P3B - input$A3B - input$B3C + input$A3BC, 4),
                 round(input$P3C - input$A3C - input$B3C + input$A3BC, 4),
                 round(input$P3A - input$A3B - input$A3C + input$A3BC, 4),
                 input$A3B - input$A3BC,
                 input$A3C - input$A3BC,
                 input$B3C - input$A3BC,
                 input$A3BC
               )
             )) {
      gp <- VennThemes(w3())
      enable("pic33")
      #Fills the Venn Diagram with color values
      gp[["Face"]][["101"]]$fill <-  "#79CAB1"
      gp[["Face"]][["001"]]$fill <-  "#B6FEB5" #purple
      gp[["Face"]][["100"]]$fill <-  '#B3B2FF' #green
      gp[["Face"]][["010"]]$fill <-  "#FFB2B2" #pink
      gp[["Face"]][["110"]]$fill <-  "#C77DB7"
      gp[["Face"]][["011"]]$fill <-  "#C8AA77"
      gp[["Face"]][["111"]]$fill <-  "#988770"
      gp[["Set"]][["Set1"]]$col <- 'black'
      gp[["Set"]][["Set2"]]$col <- 'black'
      gp[["Set"]][["Set1"]]$lwd <- 1.5
      gp[["Set"]][["Set2"]]$lwd <- 1.5
      gp[["Set"]][["Set3"]]$col <- 'black'
      gp[["Set"]][["Set3"]]$lwd <- 1.5
      plot(w3(), gp = gp, show = list(SetLabels = FALSE))

    }
    else{
      plot(
        1,
        1,
        col = "white",
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        ann = FALSE,

      )
      #Error catching section
      text(1,
           1,
           "Error: impossible to exist",
           cex = 1,
           col = "red")
    }
  }, width = 300, height = 350)



  output$answerl33 <- renderUI({
    validate(need(((input$P3A != "") &
                     (input$P3B != "") & (input$P3C != "") &
                     (input$A3B != "") & (input$A3C != "") &
                     (input$B3C != "") &
                     (input$A3BC != "")
    ),
    ""))
    if (any(bank[numbersl3$quesanswerl3, 3] == c(11, 12, 13, 14))) {
      if ((input$P3A == bank[numbersl3$quesanswerl3, 5]) &
          (input$P3B == bank[numbersl3$quesanswerl3, 6]) &
          (input$P3C == bank[numbersl3$quesanswerl3, 7]) &
          (input$A3B == bank[numbersl3$quesanswerl3, 8]) &
          (input$B3C == bank[numbersl3$quesanswerl3, 9]) &
          (input$A3C == bank[numbersl3$quesanswerl3, 10])) {
        updateButton(session, "next33")
        p("Great! You are right!", class = "answerext")
      }
      else{
        updateButton(session, "next33", disabled = F)
        p("Please adjust your answer", class = "redtext")
      }
    }
    else if (any(bank[numbersl3$quesanswerl3, 3] == c(15))) {
      if ((input$P3A == bank[numbersl3$quesanswerl3, 5]) &
          (input$P3B == bank[numbersl3$quesanswerl3, 6]) &
          (input$P3C == bank[numbersl3$quesanswerl3, 7]) &
          (input$A3B == bank[numbersl3$quesanswerl3, 8]) &
          (input$B3C == bank[numbersl3$quesanswerl3, 9]) &
          (input$A3C == bank[numbersl3$quesanswerl3, 10]) &
          (input$A3BC == bank[numbersl3$quesanswerl3, 11])) {
        updateButton(session, "next33", disabled = F)
        p("Great! You are right!", class = "answertext")
      }
      else{
        updateButton(session, "next33", disabled = F)
        p("Please adjust your answer", class = "redtext")
      }
    }
  })

  output$answerl33Picture <- renderPrint({
    validate(need(((input$P3A != "") &
                     (input$P3B != "") & (input$P3C != "") &
                     (input$A3B != "") & (input$A3C != "") &
                     (input$B3C != "") &
                     (input$A3BC != "")
    ),
    ""))
    if (any(bank[numbersl3$quesanswerl3, 3] == c(11, 12, 13, 14))) {
      if ((input$P3A == bank[numbersl3$quesanswerl3, 5]) &
          (input$P3B == bank[numbersl3$quesanswerl3, 6]) &
          (input$P3C == bank[numbersl3$quesanswerl3, 7]) &
          (input$A3B == bank[numbersl3$quesanswerl3, 8]) &
          (input$B3C == bank[numbersl3$quesanswerl3, 9]) &
          (input$A3C == bank[numbersl3$quesanswerl3, 10])) {
        img(src = "correct.png",
            alt = "Correct",
            width = 30)
      }
      else{
        img(src = "incorrect.png",
            alt = "Incorrect",
            width = 30)
      }
    }
    else if (any(bank[numbersl3$quesanswerl3, 3] == c(15))) {
      if ((input$P3A == bank[numbersl3$quesanswerl3, 5]) &
          (input$P3B == bank[numbersl3$quesanswerl3, 6]) &
          (input$P3C == bank[numbersl3$quesanswerl3, 7]) &
          (input$A3B == bank[numbersl3$quesanswerl3, 8]) &
          (input$B3C == bank[numbersl3$quesanswerl3, 9]) &
          (input$A3C == bank[numbersl3$quesanswerl3, 10]) &
          (input$A3BC == bank[numbersl3$quesanswerl3, 11])) {
        img(src = "correct.png",
            alt = "Correct",
            width = 30)
      }
      else{
        img(src = "incorrect.png",
            alt = "Incorrect",
            width = 30)
      }
    }
  })

  observeEvent(input$next33, {
    numbersl3$quesanswerl3 <- sample(space3[-numbersl3$quesanswerl3], 1)
    updateNumericInput(
      session,
      "P3A",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "P3B",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "P3C",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "A3B",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "A3C",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "B3C",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateNumericInput(
      session,
      "A3BC",
      label = NULL,
      value = NA,
      min = 0,
      max = 1,
      step = 0.01
    )
    updateButton(session, "next33", disabled = F)
    updateCheckboxInput(session, "pic3", value = F)
    hide('pic33_div')
    disable("pic33")
  })

  #Random Question
  observeEvent(input$random, {
    val <- floor(runif(1, 0, 3))
    if (val == 0) {
      updateSelectInput(session, "modes", selected = "level1")
      numbersl1$quesanswerl1 = sample(1:5, 1)
      updateNumericInput(
        session,
        "PA",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateSliderInput(
        session,
        "radiusl1",
        min = 0,
        max = 1.2,
        step = 0.01,
        value = 0.05
      )
      updateSliderInput(
        session,
        "movel1",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )
      updateSliderInput(
        session,
        "move1l1",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )
      updateButton(session, "next11", disabled = F)
      updateButton(session, "next1", disabled = F)
    }
    else if (val == 1) {
      updateSelectInput(session, "modes", selected = "level2")
      numbersl2$quesanswerl2 = sample(6:10, 1)
      updateSliderInput(
        session,
        "radiusl2",
        min = 0,
        max = 1.2,
        step = 0.01,
        value = 0.05
      )
      updateSliderInput(
        session,
        "movel12",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.45
      )
      updateSliderInput(
        session,
        "move1l2",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )

      updateSliderInput(
        session,
        "radius2l2",
        min = 0,
        max = 1.2,
        step = 0.01,
        value = 0.05
      )
      updateSliderInput(
        session,
        "movel2",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.55
      )
      updateSliderInput(
        session,
        "move2l2",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )
      updateNumericInput(
        session,
        "P2A",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "P2B",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "A2B",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateButton(session, "next22", disabled = F)
      updateButton(session, "next2", disabled = F)
    }
    else if (val == 2) {
      updateSelectInput(session, "modes", selected = "level3")
      numbersl3$quesanswerl3 = sample(11:15, 1)
      updateSliderInput(
        session,
        "radiusl3",
        min = 0,
        max = 1.2,
        step = 0.01,
        value = 0.05
      )
      updateSliderInput(
        session,
        "movel13",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.45
      )
      updateSliderInput(
        session,
        "move1l3",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )

      updateSliderInput(
        session,
        "radius2l3",
        min = 0,
        max = 1.2,
        step = 0.01,
        value = 0.05
      )
      updateSliderInput(
        session,
        "movel23",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.55
      )
      updateSliderInput(
        session,
        "move2l3",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )

      updateSliderInput(
        session,
        "radius3l3",
        min = 0,
        max = 1.2,
        step = 0.01,
        value = 0.05
      )
      updateSliderInput(
        session,
        "movel33",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.5
      )
      updateSliderInput(
        session,
        "move3l3",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.45
      )
      updateNumericInput(
        session,
        "P3A",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "P3B",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "P3C",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "A3B",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "A3C",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "B3C",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateNumericInput(
        session,
        "A3BC",
        label = NULL,
        value = NA,
        min = 0,
        max = 1,
        step = 0.01
      )
      updateButton(session, "next33", disabled = F)
      updateButton(session, "next3", disabled = F)
    }
  })

  ########### 3 Events Slider ###############################
  output$distPlotl3 <- renderPlot({
    # input$calculate.l3 #Re-run when the button is clicked
    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    # progress$set(message = "Making plot", value = 10)

    isolate({
      plot(
        c(0, 1),
        c(0, 1),
        type = 'n',
        xaxt = 'n',
        yaxt = 'n',
        ann = FALSE
      )

    })

    col1l3 <- rgb(
      red = .0,
      green = 0,
      blue = 1,
      alpha = 0.3
    )
    col2l3 <- rgb(
      red = 0,
      green = 1,
      blue = 0,
      alpha = 0.3
    )
    col3l3 <- rgb(
      red = 1,
      green = 0,
      blue = 0,
      alpha = 0.3
    )
    #Creates the 3 circles for the plot
    draw.circle(input$movel13, input$move1l3, input$radiusl3, col = col1l3)
    draw.circle(input$movel23, input$move2l3, input$radius2l3, col = col2l3)
    draw.circle(input$movel33, input$move3l3, input$radius3l3, col = col3l3)


  }, width = 350, height = 350)


  probabilityl3 <- reactiveValues(
    probc1l3 = NULL,
    probc2l3 = NULL,
    probc3l3 = NULL,
    intersectionc12l3 = NULL,
    intersectionc23l3 = NULL,
    intersectionc13l3 = NULL,
    intersectionc123l3 = NULL,
    d12l3 = NULL,
    d23l3 = NULL,
    d13l3 = NULL

  )
  observe({
    probabilityl3$d12l3 <- ((input$movel13 - input$movel23) ^ 2 +
                              (input$move1l3 - input$move2l3) ^ 2) ^ 0.5
    probabilityl3$d23l3 <- ((input$movel33 - input$movel23) ^ 2 +
                              (input$move3l3 - input$move2l3) ^ 2) ^ 0.5
    probabilityl3$d13l3 <- ((input$movel13 - input$movel33) ^ 2 +
                              (input$move1l3 - input$move3l3) ^ 2) ^ 0.5

    subdividel3 <- 350

    xcoordl3 <- NULL
    ycoordl3 <- NULL

    for (i in 1:subdividel3) {
      xcoordl3 <- c(xcoordl3,
                    seq(
                      from = 1 / subdividel3,
                      to = 1,
                      by = 1 / subdividel3
                    ))
      ycoordl3 <- c(ycoordl3, rep(i / subdividel3, subdividel3))

    }

    samplespacel3 <- data.frame(xcoordl3, ycoordl3)


    samplespacel3$radiusc1l3 <- input$radiusl3
    samplespacel3$radiusc2l3 <- input$radius2l3
    samplespacel3$radiusc3l3 <- input$radius3l3
    samplespacel3$xcenterc1l3 <- input$movel13
    samplespacel3$ycenterc1l3 <- input$move1l3
    samplespacel3$xcenterc2l3 <- input$movel23
    samplespacel3$ycenterc2l3 <- input$move2l3
    samplespacel3$xcenterc3l3 <- input$movel33
    samplespacel3$ycenterc3l3 <- input$move3l3


    samplespacel3$diffc1l3 <- sqrt((samplespacel3$xcenterc1l3
                                    - samplespacel3$xcoordl3) ^ 2
                                   + (samplespacel3$ycenterc1l3
                                      - samplespacel3$ycoordl3) ^ 2
    )
    samplespacel3$inc1l3 <-
      samplespacel3$diffc1l3 <= samplespacel3$radiusc1l3

    samplespacel3$diffc2l3 <- sqrt((samplespacel3$xcenterc2l3
                                    - samplespacel3$xcoordl3) ^ 2
                                   + (samplespacel3$ycenterc2l3
                                      - samplespacel3$ycoordl3) ^ 2
    )
    samplespacel3$inc2l3 <-
      samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3

    samplespacel3$diffc3l3 <- sqrt((samplespacel3$xcenterc3l3
                                    - samplespacel3$xcoordl3) ^ 2
                                   + (samplespacel3$ycenterc3l3
                                      - samplespacel3$ycoordl3) ^ 2
    )
    samplespacel3$inc3l3 <-
      samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3

    samplespacel3$intersectionc12l3 <- (samplespacel3$diffc1l3
                                        <= samplespacel3$radiusc1l3) &
      (samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3)
    samplespacel3$intersectionc23l3 <- (samplespacel3$diffc3l3 <=
                                          samplespacel3$radiusc3l3) &
      (samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3)
    samplespacel3$intersectionc13l3 <- (samplespacel3$diffc1l3 <=
                                          samplespacel3$radiusc1l3) &
      (samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3)
    samplespacel3$intersectionc123l3 <- (samplespacel3$diffc1l3 <=
                                           samplespacel3$radiusc1l3) &
      (samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3) &
      (samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3)

    p1l3 <- mean(samplespacel3$inc1l3)
    probabilityl3$probc1l3 <- signif(p1l3, 2)

    p2l3 <- mean(samplespacel3$inc2l3)
    probabilityl3$probc2l3 <- signif(p2l3, 2)

    p3l3 <- mean(samplespacel3$inc3l3)
    probabilityl3$probc3l3 <- signif(p3l3, 2)

    p12l3 <- mean(samplespacel3$intersectionc12l3)
    probabilityl3$intersectionc12l3 <- signif(p12l3, 2)

    p23l3 <- mean(samplespacel3$intersectionc23l3)
    probabilityl3$intersectionc23l3 <- signif(p23l3, 2)

    p13l3 <- mean(samplespacel3$intersectionc13l3)
    probabilityl3$intersectionc13l3 <- signif(p13l3, 2)

    p123l3 <- mean(samplespacel3$intersectionc123l3)
    probabilityl3$intersectionc123l3 <- signif(p123l3, 2)
  })
  output$inter3 <- renderUI(p("Intersection"))

  #reset
  observeEvent(input$reset_buttonl3, {
    updateSliderInput(
      session,
      "radiusl3",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel13",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.45
    )
    updateSliderInput(
      session,
      "move1l3",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )

    updateSliderInput(
      session,
      "radius2l3",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel23",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.55
    )
    updateSliderInput(
      session,
      "move2l3",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )

    updateSliderInput(
      session,
      "radius3l3",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel33",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
    updateSliderInput(
      session,
      "move3l3",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.45
    )
  })
  observe({
    output$labeldoBl3 <- renderUI({
      #Output means
      actionButton("dol3", "", class = "btn-group") #blue circle action button
    })
    output$labeldoGl3 <- renderUI({
      #green circle action button
      actionButton("do2l3", "", class = "btn-group")
    })
    output$labeldoRl3 <- renderUI({
      #red circle action button
      actionButton("do3l3", "", class = "btn-group")
    })
    output$labeldoBGl3 <- renderUI({
      #cyan circle action button
      actionButton("do4l3", "", class = "btn-group")
    })
    output$labeldoBRl3 <- renderUI({
      #purple circle action button
      actionButton("do5l3", "", class = "btn-group")
    })
    output$labeldoGRl3 <-
      renderUI({
        #darkolivegreen circle action button
        actionButton("do6l3", "", class = "btn-group")
      })
    output$labeldoBGRl3 <- renderUI({
      #brown circle action button
      actionButton("do7l3", "", class = "btn-group")
    })
    output$labeldoBGl33 <- renderUI({
      #Blue Green
      actionButton("do4l3", "", class = "btn-group")
    })
    output$labeldoBRl33 <- renderUI({
      #Blue Red
      actionButton("do5l3", "", class = "btn-group")
    })
    output$labeldoGRl33 <- renderUI({
      #Green Red
      actionButton("do6l3", "", class = "btn-group")
    })
    output$labeldoBGRl33 <- renderUI({
      #Blue Green Red
      actionButton("do7l3", "", class = "btn-group")
    })
  })

  #blue button
  output$PAl3 <- renderPrint({
    cat(probabilityl3$probc1l3)
  })
  #green button
  output$PBl3 <- renderPrint({
    cat(probabilityl3$probc2l3)
  })
  #red button
  output$PCl3 <- renderPrint({
    cat(probabilityl3$probc3l3)
  })
  #cyan button
  output$ABl3 <- renderPrint({
    cat(probabilityl3$intersectionc12l3)
  })
  #purple button
  output$ACl3 <- renderPrint({
    cat(probabilityl3$intersectionc13l3)
  })
  #darkolivegreen button
  output$BCl3 <- renderPrint({
    cat(probabilityl3$intersectionc23l3)
  })
  #center button
  output$ABCl3 <-
    renderPrint({
      cat(probabilityl3$intersectionc123l3)
    })


  ### random choose question
  numbersl3 <- reactiveValues(quesanswerl3 = c())

  observe({
    numbersl3$quesanswerl3 = sample(11:15, 1)
  })
  output$PA3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 12])
  })
  output$PB3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 13])
  })
  output$PC3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 14])
  })
  output$AB3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 15])
  })
  output$BC3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 16])
  })
  output$AC3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 17])
  })
  output$ABC3 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 18])
  })
  output$PA33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 12])
  })
  output$PB33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 13])
  })
  output$PC33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 14])
  })
  output$AB33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 15])
  })
  output$BC33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 16])
  })
  output$AC33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 17])
  })
  output$ABC33 <- renderPrint({
    cat(bank[numbersl3$quesanswerl3, 18])
  })
  output$questionl3 <- renderText(bank[numbersl3$quesanswerl3, 4])
  space3 <- c(11:15)
  #Generate next question
  observeEvent(input$next3, {
    numbersl3$quesanswerl3 <- sample(space3[-numbersl3$quesanswerl3], 1)
    updateSliderInput(
      session,
      "radiusl3",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel13",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.45
    )
    updateSliderInput(
      session,
      "move1l3",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )

    updateSliderInput(
      session,
      "radius2l3",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel23",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.55
    )
    updateSliderInput(
      session,
      "move2l3",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )

    updateSliderInput(
      session,
      "radius3l3",
      min = 0,
      max = 1.2,
      step = 0.01,
      value = 0.05
    )
    updateSliderInput(
      session,
      "movel33",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.5
    )
    updateSliderInput(
      session,
      "move3l3",
      min = 0,
      max = 1,
      step = 0.01,
      value = 0.45
    )
    hide('pic3_div')
  })
  observeEvent(input$feedback3, {
    toggle(id = "panelS3")
  })


  observeEvent(input$feedback33, {
    toggle(id = "panelN3")
  })

  output$answerl3 <- renderUI({
    if (any(bank[numbersl3$quesanswerl3, 3] == c(11, 12, 13, 14))) {
      if ((probabilityl3$probc1l3 == bank[numbersl3$quesanswerl3, 5]) &
          (probabilityl3$probc2l3 == bank[numbersl3$quesanswerl3, 6]) &
          (probabilityl3$probc3l3 == bank[numbersl3$quesanswerl3, 7]) &
          (probabilityl3$intersectionc12l3 == bank[numbersl3$quesanswerl3, 8]) &
          (probabilityl3$intersectionc23l3 == bank[numbersl3$quesanswerl3, 9]) &
          (probabilityl3$intersectionc13l3 == bank[numbersl3$quesanswerl3, 10]))
      {
        updateButton(session, "next3", disabled = F)
        p("Great! You are right!", class = "answertext")
      }

      else{
        updateButton(session, "next3", disabled = F)
        p(
          "Keep changing the size and placement of your circles
          to match the probabilities desired.",
          class = "redtext"
        )
      }
    }

    else if (any(bank[numbersl3$quesanswerl3, 3] == c(15))) {
      if ((probabilityl3$probc1l3 == bank[numbersl3$quesanswerl3, 5]) &
          (probabilityl3$probc2l3 == bank[numbersl3$quesanswerl3, 6]) &
          (probabilityl3$probc3l3 == bank[numbersl3$quesanswerl3, 7]) &
          (probabilityl3$intersectionc12l3 == bank[numbersl3$quesanswerl3, 8]) &
          (probabilityl3$intersectionc23l3 == bank[numbersl3$quesanswerl3, 9]) &
          (probabilityl3$intersectionc13l3 == bank[numbersl3$quesanswerl3, 10]) &
          (probabilityl3$intersectionc123l3 == bank[numbersl3$quesanswerl3, 11]))
      {
        cat("Great! You are right!")
        updateButton(session, "next3", disabled = F)
      }
      else{
        cat(
          "Keep changing the size and placement of your
            circles to match the probabilities desired."
        )
        updateButton(session, "next3", disabled = F)
      }
    }
  })

  output$answerl3Picture <- renderPrint({
    if (any(bank[numbersl3$quesanswerl3, 3] == c(11, 12, 13, 14))) {
      if ((probabilityl3$probc1l3 == bank[numbersl3$quesanswerl3, 5]) &
          (probabilityl3$probc2l3 == bank[numbersl3$quesanswerl3, 6]) &
          (probabilityl3$probc3l3 == bank[numbersl3$quesanswerl3, 7]) &
          (probabilityl3$intersectionc12l3 == bank[numbersl3$quesanswerl3, 8]) &
          (probabilityl3$intersectionc23l3 == bank[numbersl3$quesanswerl3, 9]) &
          (probabilityl3$intersectionc13l3 == bank[numbersl3$quesanswerl3, 10]))
      {
        img(src = "correct.png",
            alt = "Correct",
            width = 30)
      }

      else{
        img(src = "incorrect.png",
            alt = "Incorrect",
            width = 30)
      }
    }

    else if (any(bank[numbersl3$quesanswerl3, 3] == c(15))) {
      if ((probabilityl3$probc1l3 == bank[numbersl3$quesanswerl3, 5]) &
          (probabilityl3$probc2l3 == bank[numbersl3$quesanswerl3, 6]) &
          (probabilityl3$probc3l3 == bank[numbersl3$quesanswerl3, 7]) &
          (probabilityl3$intersectionc12l3 == bank[numbersl3$quesanswerl3, 8]) &
          (probabilityl3$intersectionc23l3 == bank[numbersl3$quesanswerl3, 9]) &
          (probabilityl3$intersectionc13l3 == bank[numbersl3$quesanswerl3, 10]) &
          (probabilityl3$intersectionc123l3 == bank[numbersl3$quesanswerl3, 11]))
      {
        img(src = "correct.png",
            alt = "Correct",
            width = 30)
      }
      else{
        img(src = "incorrect.png",
            alt = "Incorrect",
            width = 30)
      }
    }
  })



  observeEvent(input$pic3, {
    toggle('pic3_div')
    output$Feed33 <- renderUI({
      img(
        src = bank[numbersl3$quesanswerl3, 19],
        alt = bank[numbersl3$quesanswerl3, 20],
        height = "70%",
        width = "70%"
      )

    })
  })


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)