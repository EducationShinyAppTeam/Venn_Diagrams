library(shinydashboard)
library(shiny)
library(plotrix)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(Vennerable)
library(boastUtils)
#Added these 3 thinking it may fix the error in publishing. 
#Has not worked yet
# library(BiocGenerics)
# library(BiocManager)
# library(BiocVersion)


#This may be helpful for downloading Vennerable 
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#if (!requireNamespace("devtools", quietly = TRUE))
#  install.packages("devtools")
#BiocManager::install(c("RBGL","graph"))
#devtools::install_github("js229/Vennerable")

#Error in line 219 prevents 1 Event numeric input




bank<- read.csv("questionbank.csv")
bank= data.frame(lapply(bank,as.character),stringsAsFactors = FALSE)
jsResetCode <- "shinyjs.reset= function() {history.go(0)}"

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

  #Go Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "circle")
  })
  
  #Programs the i (information) button on the top right of header
  observeEvent(input$info,{
    sendSweetAlert(
      
      session = session,
      title = "Instructions:",
      text = tags$span(
              "If you want to adjust the circles using sliders in the Venn Diagram:",
              tags$br(),
              "1. Move or change the size of the circles using sliders (accessed by the button).",
              tags$br(),
              "2. Once the correct image appears move on to the next question",
              
              tags$br(),
              tags$br(),
              "If you want to enter the probabilities directly:",
              tags$br(),
              "1. Enter your probability in the textbox.",
              tags$br(),
              "2. Once the correct image appears move on to the next question",
              tags$br(),
              
              "If you want to get hints, please click 'Sample Answer' button.",
              tags$br(),
              "The 'Sample Answer' button shows a basic idea of what a correct diagram will look like; the 'Next' button provides a new question." 
              ),                                                              
      
      type = "info",
      html = TRUE
    )
  })
  
  #Programs the ? (help) button on the top right of header
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Strategy:",
      text = tags$span(
        "1. Understand if you are dealing with one event or multiple events.",
        tags$br(),
        "2. Check if the relationship is mutually exclusive.",
        tags$br(),
        "3. Check if the relationship is independent."),
      type = "info",
      html = TRUE
    )
  })
  
  #Appears to simply output challenge when called
  output$challenge <- renderText({paste("Challenge")})
  #output$instruction <- renderText({paste("Please adjust circle(s) to create a diagram that fits the following situation:")})
  
################# 1 Event #############################
  
######### Input Values 1 Event ##########

  
  # observeEvent(input$feedback1, {
  #   toggle(id= "panelS1")
  # })
  
  #input$PA <- isolate(input$PA)

    #output saying if answer is correct or not
    output$answerl11 <- renderPrint({
  
      validate(
        need(input$PA != "", "Please enter your probability")
      )
  
      if(input$PA == bank[numbersl1$quesanswerl1,5]){
        cat("Great! You are right!")
        updateButton(session, "next11", disabled = F)
      }
      
      else{
        updateButton(session, "next11", disabled = F)
        cat("Please adjust your answer")
      }
  
    })
    
    output$outsideNumericDiagram1 <- reactive({
      {
        validate(
          need(input$PA != "", "Please enter your probability")
        )
        if(1 - input$PA <= 1 && 1 - input$PA >= 0)
          1-input$PA
        else
          "Impossible to exist"
      }
    })
  
    
    #Output the check or the X
    output$answerl11Picture <- 
      renderUI({
        #input$SubmitNumeric1
        validate(
          need(input$PA != "", "")
        )
        if(input$PA == bank[numbersl1$quesanswerl1,5]){
          img(src = "correct.png", alt = "Correct", width = 30)
        }
        else{
          img(src = "incorrect.png", alt = "Incorrect", width = 30)
        }
      })
    
    
    #Graph for One Event input number
    
    
    w1 = reactive({
      #input$SubmitNumeric1
      print('where is this')
      compute.Venn(Venn(SetNames = c("1","2"), Weight = c(`01` = input$PA,`11` = .05, `10` = .01)), type = "circles", doEuler = TRUE) #ERROR IS COMING FROM HERE
    })
    output$enterplot1 <- renderPlot({
      #input$SubmitNumeric1
      validate(
        need(input$PA != "", "Please enter your probability")
      )
      isolate({
        plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      })
      
      col1l1 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
      #draw.circle(input$movel1,input$move1l1,input$radiusl1,col=col1l1)
      if(input$PA >= 0 && input$PA <= 1)
        draw.circle(.5,.5,input$PA/1.77,col=col1l1)
      else
        "Impossible to exist"
    }, width = 350, height = 350)
    
    
    #pic1
    observeEvent(input$pic1,{
      toggle('pic1_div')
      output$Feed11 <- renderUI({
        img(src = bank[numbersl1$quesanswerl1, 19], alt = bank[numbersl1$quesanswerl1, 20],  height = "70%",  width = "70%")
      })
    })
    #observation 
    observeEvent(input$pic11,{
      print("does detect pic11")
      toggle('pic11_div')
      output$Feed1 <- renderUI({
        img(src = bank[numbersl1$quesanswerl1, 19], alt = bank[numbersl1$quesanswerl1, 20], height = "70%",  width = "70%")
      })
    })
    
    # #observation 
    # observeEvent(input$pic11,{
    #   toggle('pic11_div')
    #   output$Feed1 <- renderUI({
    #     img(src = bank[numbersl1$quesanswerl1, 19], alt = bank[numbersl1$quesanswerl1, 20], height = "25%",  width = "25%")
    #   })
    # })
    
    observeEvent(input$next11,{
      hide('pic11_div')
      numbersl1$quesanswerl1 <- sample(space[-numbersl1$quesanswerl1],1)
      updateNumericInput(session, "PA", label = NULL, value = NA,
                         min = 0, max = 1, step = 0.01)
      updateButton(session, "next11", disabled = F)
      updateCheckboxInput(session, "pic1", value = F)
    })
    print("Right above")
    #print(input$SubmitNumeric1)
    print("Right after")


  
  
  
  
  
  
  
  
  
  
  
  
  
######### Slider Values 1 Event ############
  output$distPlotl1 <- renderPlot({
    
    isolate({
      plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
    })
    
    col1l1 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
    draw.circle(input$movel1,input$move1l1,input$radiusl1,col=col1l1)
  }, width = 350, height = 350) 
  
  # using points simulating prob
  probabilityl1 <- reactiveValues(
    probc1l1= NULL
  )
  
  # Simulation Probabilties
  observe({
    subdividel1 <- 350
    
    xcoordl1 <- NULL
    ycoordl1 <- NULL
    
    for(i in 1:subdividel1){
      xcoordl1 <- c(xcoordl1,seq(from = 1/subdividel1, to=1, by = 1/subdividel1))
      ycoordl1 <- c(ycoordl1,rep(i/subdividel1,subdividel1))
    }
    
    samplespacel1 <-data.frame(xcoordl1,ycoordl1)
    #These use values of the 3 slidebars in slider input
    samplespacel1$radiusl1 <- input$radiusl1
    samplespacel1$xcenterl1 <- input$movel1 
    samplespacel1$ycenterl1 <- input$move1l1
    #Here they seem to calculate the radius again
    samplespacel1$diffl1 <- sqrt((samplespacel1$xcenterl1-samplespacel1$xcoordl1)^2
                                 +(samplespacel1$ycenterl1-samplespacel1$ycoordl1)^2)
    samplespacel1$inc1l1 <- samplespacel1$diffl1 <= samplespacel1$radiusl1 
    
    probl1 <- mean(samplespacel1$inc1l1)
    probabilityl1$probc1l1<-signif(probl1,2)
  })

  observe({
    output$labeldol1 <- renderUI({
      bsButton("dol1","", class = "btn-group")
    })
  })
  
  observeEvent(input$reset_buttonl1,{
    updateSliderInput(session, "radiusl1",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel1",min=0,max=1,step=0.01,value=0.5)
    updateSliderInput(session, "move1l1",min=0,max=1,step=0.01,value=0.5)
  })
  ### random choose question
  numbersl1 <- reactiveValues(quesanswerl1=c())
  observe({
    numbersl1$quesanswerl1=sample(1:5,1)
  })
  output$PA1<- renderPrint({
    cat(bank[numbersl1$quesanswerl1,12])
  })
  output$PA11<- renderPrint({
    cat(bank[numbersl1$quesanswerl1,12])
  })
  output$PAl1<- renderPrint({
    cat(probabilityl1$probc1l1)
  })
  space <- c(1:5)
  #Reset the sliders when the next question comes up
  observeEvent(input$next1, {
    hide('pic1_div')
    numbersl1$quesanswerl1 <- sample(space[-numbersl1$quesanswerl1],1)
    updateSliderInput(session, "radiusl1",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel1",min=0,max=1,step=0.01,value=0.49)
    updateSliderInput(session, "move1l1",min=0,max=1,step=0.01,value=0.5)
  })
  #question
  output$questionl1<-renderText(bank[numbersl1$quesanswerl1,4])

  
  output$answerl1 <- renderPrint({
    if(probabilityl1$probc1l1==bank[numbersl1$quesanswerl1,5]){

      cat("Great! You are right!")
      updateButton(session, "next1", disabled = F)
    }
    else{
      cat("Keep changing the size of your circle to match the probability desired.")
      updateButton(session, "next1", disabled = F)
    }
  })
  output$answerl1Picture <- renderUI({
    if(probabilityl1$probc1l1==bank[numbersl1$quesanswerl1,5]){
      img(src = "correct.png", alt = "Correct", width = 30)
    }
    else{
      img(src = "incorrect.png", alt = "Incorrect", width = 30)
    }
  })
  
  observeEvent(input$feedback11, {
    toggle(id= "panelN1")
  })
  


  

  
################# level 2 #################################
  
  ####### 2 Events enterPlot section ##################
  # observeEvent(input$pic11,{
  #   toggle('pic11_div')
  #   output$Feed1 <- renderUI({
  #     img(src = bank[numbersl1$quesanswerl1, 19], alt = bank[numbersl1$quesanswerl1, 20], height = "70%",  width = "25%")
  #   })
  # })
  
  
  observeEvent(input$pic22,{
    toggle('pic22_div')
    output$Feed2 <- renderUI({
      img(src = bank[numbersl2$quesanswerl2,19], alt = bank[numbersl2$quesanswerl2,20], height = "70%", width = "70%")
    })
  })
  
  output$answerl22 <- renderPrint({
    validate(
      need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please enter all your probabilities")
    )
    if((input$P2A == bank[numbersl2$quesanswerl2,5])&(input$P2B == bank[numbersl2$quesanswerl2,6])&(input$A2B == bank[numbersl2$quesanswerl2,8])){
      cat("Great! You are right!")
      updateButton(session, "next22", disabled = F)
    }
    else{
      cat("Please adjust your answer")
      updateButton(session, "next22", disabled = F)
    }
  })
  
  #Output the check or the X
  output$answerl22Picture <- renderUI({
    
    validate(
      need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")),"")
    )
    if((input$P2A == bank[numbersl2$quesanswerl2,5])&(input$P2B == bank[numbersl2$quesanswerl2,6])&(input$A2B == bank[numbersl2$quesanswerl2,8])){
      img(src = "correct.png", alt = "Correct", width = 30)
    }
    else{
      img(src = "incorrect.png", alt = "Correct", width = 30)
    }
  })
  
  
  
  #Stuff not in the first yet
  w2 = reactive({
    compute.Venn(Venn(SetNames = c("", ""), Weight = c(`01` = input$P2B-input$A2B, `11` = input$A2B, `10` = input$P2A-input$A2B)), type ="circles", doEuler=TRUE)
  })
  
  output$outsideNumericDiagram2 = reactive({
    validate(
      need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please input the numbers")
    )
    
    if(((min(input$P2A, input$P2B) == 0 ) & (input$A2B == 0) ) || ((input$P2A == input$P2B ) & (input$P2A == input$A2B) & (input$P2B == input$A2B)) 
       || ((input$P2A + input$P2B - input$A2B <=1) & (input$A2B <= min(input$P2A, input$P2B)) ))
      1 - (input$P2B-input$A2B) - input$A2B - (input$P2A-input$A2B)
    else
      "Impossible to exist"
  })
  output$enterplot2 = renderPlot({
    #Confirms that all 3 inputs are used. 
    validate(
      need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please input the numbers")
    )
    #If 0 for combination and at least one 0 in a category output remind need more than 1 event
    if ((min(input$P2A, input$P2B) == 0 ) & (input$A2B == 0) ) {
      isolate({plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)})
      text(1,1,"Note that there are two events",cex = 1, col = "red")
    }
    #If all of the values are equal
    else if ((input$P2A == input$P2B ) & (input$P2A == input$A2B) & (input$P2B == input$A2B)) {
      isolate({plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)})
      draw.circle(1,1,.1,col="#7cc9b2")
    }
    #If all the inputs are met
    else if ((input$P2A + input$P2B - input$A2B <=1) & (input$A2B <= min(input$P2A, input$P2B)) ) {
      gp <- VennThemes(w2())
      gp[["Face"]][["11"]]$fill <-  "#79CAB1" #original mitegreen
      gp[["Face"]][["01"]]$fill <-  "#B6FEB5" #original green
      gp[["Face"]][["10"]]$fill <-  '#B3B2FF' #original purple
      gp[["Set"]][["Set1"]]$col <- 'black'
      gp[["Set"]][["Set2"]]$col <- 'black'
      gp[["Set"]][["Set1"]]$lwd <- 1.5
      gp[["Set"]][["Set2"]]$lwd <- 1.5
      plot(w2(), gp = gp, show = list(SetLabels = FALSE)) #calls the plot function
    }
    else{ #If both of these results invalid
      plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      text(1,1,"Error: impossible to exist",cex = 1, col = "red")
    }
    
  },width = 300, height = 280)
  
  
  observeEvent(input$next22,{
    numbersl2$quesanswerl2 <- sample(space2[-numbersl2$quesanswerl2],1)
    updateNumericInput(session, "P2A", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "P2B", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A2B", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateButton(session, "next22", disabled = F)
    updateCheckboxInput(session, "pic2", value = F)
    hide('pic22_div')
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
      plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      
    })
    
    col1l2 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
    col2l2 <- rgb(red = 0, green = 1, blue = 0, alpha = 0.3)
    

    draw.circle(input$movel12,input$move1l2,input$radiusl2,col=col1l2)
    draw.circle(input$movel2,input$move2l2,input$radius2l2,col=col2l2)
    
  }, width = 350, height = 350)
  
  
  
  probabilityl2 <- reactiveValues(
    
    probc1l2= NULL,
    probc2l2= NULL,
    probintersectionl2= NULL,
    dl2= NULL
    
  )
  observe({
    probabilityl2$dl2 <- ((input$movel12-input$movel2)^2+(input$move1l2-input$move2l2)^2)^0.5
    
    subdividel2 <- 350
    
    xcoordl2 <- NULL
    ycoordl2 <- NULL
    
    for(i in 1:subdividel2){
      
      xcoordl2 <- c(xcoordl2,seq(from = 1/subdividel2, to=1, by = 1/subdividel2))
      ycoordl2 <- c(ycoordl2,rep(i/subdividel2,subdividel2))
      
    }
    
    samplespacel2 <-data.frame(xcoordl2,ycoordl2)
    #Slider values
    samplespacel2$radiusc1l2 <- input$radiusl2
    samplespacel2$radiusc2l2 <- input$radius2l2
    samplespacel2$xcenterc1l2<- input$movel12
    samplespacel2$ycenterc1l2 <- input$move1l2
    
    samplespacel2$xcenterc2l2 <- input$movel2
    samplespacel2$ycenterc2l2 <- input$move2l2
    
    
    samplespacel2$diffc1l2 <- sqrt((samplespacel2$xcenterc1l2-samplespacel2$xcoordl2)^2+(samplespacel2$ycenterc1l2-samplespacel2$ycoordl2)^2)
    samplespacel2$inc1l2<- samplespacel2$diffc1l2 <= samplespacel2$radiusc1l2
    
    samplespacel2$diffc2l2 <- sqrt((samplespacel2$xcenterc2l2-samplespacel2$xcoordl2)^2+(samplespacel2$ycenterc2l2-samplespacel2$ycoordl2)^2)
    samplespacel2$inc2l2<- samplespacel2$diffc2l2 <= samplespacel2$radiusc2l2
    
    samplespacel2$intersectionl2 <- (samplespacel2$diffc1l2 <= samplespacel2$radiusc1l2) & (samplespacel2$diffc2l2 <= samplespacel2$radiusc2l2)
    
    p1l2<- mean(samplespacel2$inc1l2)
    probabilityl2$probc1l2<-signif(p1l2,2)
    
    p2l2<- mean(samplespacel2$inc2l2)
    probabilityl2$probc2l2<-signif(p2l2,2)
    
    p3l2 <- mean(samplespacel2$intersectionl2)
    probabilityl2$probintersectionl2<-signif(p3l2,2)
  })
  
  observe({
    output$labeldoBl2 <- renderUI({
      bsButton("dol2","", class = "btn-group")
    })
    
    output$labeldoGl2 <- renderUI({
      bsButton("do2l2","", class = "btn-group")
    })
    output$labeldoBGl2 <- renderUI({
      bsButton("do4l2","", class = "btn-group")
    })
    output$labeldoBGl22 <- renderUI({
      bsButton("do4l2","", class = "btn-group")
    })
  })
  output$inter2 <- renderUI(
    print("Intersection")
  )
  output$PAl2<- renderPrint(cat(probabilityl2$probc1l2))
  output$PBl2<- renderPrint(cat(probabilityl2$probc2l2))
  output$ABl2<- renderPrint(cat(probabilityl2$probintersectionl2))
  
  #reset 
  observeEvent(input$reset_buttonl2,{
    updateSliderInput(session, "radiusl2",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel12",min=0,max=1,step=0.01,value=0.45)
    updateSliderInput(session, "move1l2",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius2l2",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel2",min=0,max=1,step=0.01,value=0.55)
    updateSliderInput(session, "move2l2",min=0,max=1,step=0.01,value=0.5)
  })
  ### random choose question
  numbersl2 <- reactiveValues( quesanswerl2=c())
  
  observe({
    numbersl2$quesanswerl2=sample(6:10,1)
  })
  output$PA2<- renderPrint({
    cat(bank[numbersl2$quesanswerl2,12])
  })
  output$PA22<- renderPrint({
    cat(bank[numbersl2$quesanswerl2,12])
  })
  output$PB2<- renderPrint({
    cat(bank[numbersl2$quesanswerl2,13])
  })
  output$PB22<- renderPrint({
    cat(bank[numbersl2$quesanswerl2,13])
  })
  output$AB2<- renderPrint({
    cat(bank[numbersl2$quesanswerl2,15])
  })
  output$AB22<- renderPrint({
    cat(bank[numbersl2$quesanswerl2,15])
  })
  output$questionl2<-renderText(bank[numbersl2$quesanswerl2,4])
  space2 <- c(6:10)
  #Generate next question, reset the sliders
  observeEvent(input$next2,{
    numbersl2$quesanswerl2 <- sample(space2[-numbersl2$quesanswerl2],1)
    updateSliderInput(session, "radiusl2",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel12",min=0,max=1,step=0.01,value=0.45)
    updateSliderInput(session, "move1l2",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius2l2",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel2",min=0,max=1,step=0.01,value=0.55)
    updateSliderInput(session, "move2l2",min=0,max=1,step=0.01,value=0.5)
    hide('pic2_div')
  })
observeEvent(input$feedback2, {
    toggle(id= "panelS2")
  })


observeEvent(input$feedback22, {
  toggle(id= "panelN2")
})

  
  output$answerl2 <- renderPrint({
    
    if ((probabilityl2$probc1l2==bank[numbersl2$quesanswerl2,5])&(probabilityl2$probc2l2==bank[numbersl2$quesanswerl2,6])& (probabilityl2$probintersectionl2 == bank[numbersl2$quesanswerl2,8])) 
    {cat("Great! You are right!")
      updateButton(session, "next2", disabled = F)
    }
    
    else{
      cat("Keep changing the size and placement of your circles to match the probabilities desired.")
      updateButton(session, "next2", disabled = F)
    }
  })
  
  
  output$answerl2Picture <- renderUI({
    if((probabilityl2$probc1l2==bank[numbersl2$quesanswerl2,5])&(probabilityl2$probc2l2==bank[numbersl2$quesanswerl2,6])& (probabilityl2$probintersectionl2 == bank[numbersl2$quesanswerl2,8])){
      img(src = "correct.png", "Correct", width = 30)
    }
    else{
      img(src = "incorrect.png", "Incorrect", width = 30)
    }
  })
  
  
  observeEvent(input$pic2,{
    toggle('pic2_div')
    output$Feed22 <- renderUI({
      
      img(src = bank[numbersl2$quesanswerl2,19], alt = bank[numbersl2$quesanswerl2,20], height = "70%", width = "70%")
      
    })
  })
  
  
  
  
################# 3 Events Body #################################
  

  

  
  ############# level3 enter ##############################################
  observeEvent(input$pic33,{
    toggle('pic33_div')
    output$Feed3 <- renderUI({
      
      img(src = bank[numbersl3$quesanswerl3,19], alt = bank[numbersl3$quesanswerl3,20], height = "70%", width = "70%")
      
    })
  })
  
  #Calculate all numbers inside of the Venn Diagram
  w3 = reactive({
    compute.Venn(Venn(SetNames = c("1", "2", "3"), Weight = c(
      `001` = round(input$P3B-input$A3B-input$B3C+input$A3BC,4), 
      `010` = round(input$P3C-input$A3C-input$B3C+input$A3BC,4), 
      `100` = round(input$P3A-input$A3B-input$A3C+input$A3BC,4),
      `101` = round(input$A3B-input$A3BC,4), 
      `110` = round(input$A3C-input$A3BC,4),
      `011` = round(input$B3C-input$A3BC,4),
      `111` = round(input$A3BC,4))), type ="circles", doEuler=TRUE)
  })
  
  output$outsideNumericDiagram3 = reactive({
    validate(
      need(((input$P3A != "")&(input$P3B != "")&(input$P3C != "")&(input$A3B != "")&(input$A3C != "")&(input$B3C != "")&(input$A3BC != "")), 
           "Please enter all your probabilities")
    )
    
    if(((min(input$P3A, input$P3B, input$P3C) == 0 ) & (input$A3B == 0) & (input$A3C == 0) & (input$B3C == 0) & (input$A3BC == 0)) || 
       ((input$P3A + input$P3B + input$P3C - input$A3B - input$A3C - input$B3C  <= 1) & (0 <= min(input$P3B-input$A3B-input$B3C+input$A3BC,input$P3C-input$A3C-input$B3C+input$A3BC,input$P3A-input$A3B-input$A3C+input$A3BC,
                                                                                                                                                   input$A3B-input$A3BC,input$A3C-input$A3BC,input$B3C-input$A3BC,input$A3BC))
    ))
      1 - round(input$P3B-input$A3B-input$B3C+input$A3BC,4) - round(input$P3C-input$A3C-input$B3C+input$A3BC,4) - round(input$P3A-input$A3B-input$A3C+input$A3BC,4) - round(input$A3B-input$A3BC,4) - 
      round(input$A3C-input$A3BC,4) - round(input$B3C-input$A3BC,4) - round(input$A3BC,4)
    else
      "Impossible to exist"
    })

  
  
  output$enterplot3 = renderPlot({
    validate(
      need(((input$P3A != "")&(input$P3B != "")&(input$P3C != "")&(input$A3B != "")&(input$A3C != "")&(input$B3C != "")&(input$A3BC != "")), 
           "Please enter all your probabilities")
    )
    if ((min(input$P3A, input$P3B, input$P3C) == 0 ) & (input$A3B == 0) & (input$A3C == 0) & (input$B3C == 0) & (input$A3BC == 0)) {
      isolate({plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)})
      text(1,1,"Note that there are three events",cex = 1, col = "red")
    }
    # else if ((input$P3A + input$P3B + input$P3C - input$A3B - input$A3C - input$B3C  <= 1)#- input$A3BC
    #          & (input$A3BC <= min(input$A3C, input$A3B, input$B3C)) & (input$A3B <= min(input$P3A, input$P3B)) 
    #          & (input$A3C <= min(input$P3A, input$P3C))  & (input$B3C <= min(input$P3B, input$P3C)) 
    #           ) { 
    #Rewritten version
    else if ((input$P3A + input$P3B + input$P3C - input$A3B - input$A3C - input$B3C  <= 1) #If all of the values combined are less than 1
                      & (0 <= min(input$P3B-input$A3B-input$B3C+input$A3BC,input$P3C-input$A3C-input$B3C+input$A3BC,input$P3A-input$A3B-input$A3C+input$A3BC,
                                  input$A3B-input$A3BC,input$A3C-input$A3BC,input$B3C-input$A3BC,input$A3BC))
             ) { 
      gp <- VennThemes(w3())
      
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
      
      #plot(w3(), gp = gp, show = list(SetLabels = FALSE)) 
      #Vennerable::
      plot(w3(), gp = gp, show = list(SetLabels = FALSE))     
    }
    else{
      plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE, )
      #Error catching section
      text(1,1,"Error: impossible to exist",cex = 1, col = "red")
    }
  },width = 300, height = 350)

  

  output$answerl33 <- renderPrint({
    validate(
      need(((input$P3A != "")&(input$P3B != "")&(input$P3C != "")&(input$A3B != "")&(input$A3C != "")&(input$B3C != "")&(input$A3BC != "")), 
           "Please enter all your probabilities")
    )
    if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
      if((input$P3A == bank[numbersl3$quesanswerl3,5])&(input$P3B == bank[numbersl3$quesanswerl3,6])&(input$P3C == bank[numbersl3$quesanswerl3,7])
         &(input$A3B == bank[numbersl3$quesanswerl3,8])&(input$B3C == bank[numbersl3$quesanswerl3,9])&(input$A3C == bank[numbersl3$quesanswerl3,10])){
        cat("Great! You are right!")
        updateButton(session, "next33", disabled = F)
      }
      else{
        cat("Please adjust your answer")
        updateButton(session, "next33", disabled = F)
      }
    }
    else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
      if((input$P3A == bank[numbersl3$quesanswerl3,5])&(input$P3B == bank[numbersl3$quesanswerl3,6])&(input$P3C == bank[numbersl3$quesanswerl3,7])
         &(input$A3B == bank[numbersl3$quesanswerl3,8])&(input$B3C == bank[numbersl3$quesanswerl3,9])&(input$A3C == bank[numbersl3$quesanswerl3,10])
         &(input$A3BC == bank[numbersl3$quesanswerl3,11])){
        cat("Great! You are right!")
        updateButton(session, "next33", disabled = F)
      }
      else{
        cat("Please adjust your answer")
        updateButton(session, "next33", disabled = F)
      }
    }
  })
  
  output$answerl33Picture <- renderPrint({
    validate(
      need(((input$P3A != "")&(input$P3B != "")&(input$P3C != "")&(input$A3B != "")&(input$A3C != "")&(input$B3C != "")&(input$A3BC != "")), 
           "")
    )
    if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
      if((input$P3A == bank[numbersl3$quesanswerl3,5])&(input$P3B == bank[numbersl3$quesanswerl3,6])&(input$P3C == bank[numbersl3$quesanswerl3,7])
         &(input$A3B == bank[numbersl3$quesanswerl3,8])&(input$B3C == bank[numbersl3$quesanswerl3,9])&(input$A3C == bank[numbersl3$quesanswerl3,10])){
      
        img(src = "correct.png", alt = "Correct", width = 30)
      }
      else{
        img(src = "incorrect.png", alt = "Incorrect", width = 30)
      }
    }
    else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
      if((input$P3A == bank[numbersl3$quesanswerl3,5])&(input$P3B == bank[numbersl3$quesanswerl3,6])&(input$P3C == bank[numbersl3$quesanswerl3,7])
         &(input$A3B == bank[numbersl3$quesanswerl3,8])&(input$B3C == bank[numbersl3$quesanswerl3,9])&(input$A3C == bank[numbersl3$quesanswerl3,10])
         &(input$A3BC == bank[numbersl3$quesanswerl3,11])){
        img(src = "correct.png", alt = "Correct", width = 30)
      }
      else{
        img(src = "incorrect.png", alt = "Incorrect", width = 30)
      }
    }
  })
  
  


  observeEvent(input$next33,{
    numbersl3$quesanswerl3 <- sample(space3[-numbersl3$quesanswerl3],1)
    updateNumericInput(session, "P3A", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "P3B", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "P3C", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A3B", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A3C", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "B3C", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A3BC", label = NULL, value = NA,
                       min = 0, max = 1, step = 0.01)
    updateButton(session, "next33", disabled = F)
    updateCheckboxInput(session, "pic3", value = F)
    hide('pic33_div')
  })
  
  #Random Question
  observeEvent(input$random,{
    val <- floor(runif(1,0,3))
    if(val==0){
      updateSelectInput(session,"modes", selected = "level1")
      numbersl1$quesanswerl1=sample(1:5,1)
      updateNumericInput(session, "PA", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateSliderInput(session, "radiusl1",min=0,max=1.2,step = 0.01,value = 0.05)
      updateSliderInput(session, "movel1",min=0,max=1,step=0.01,value=0.5)
      updateSliderInput(session, "move1l1",min=0,max=1,step=0.01,value=0.5)
      updateButton(session, "next11", disabled = F)
      updateButton(session, "next1", disabled = F)
    }
    else if(val==1){
      updateSelectInput(session, "modes", selected = "level2")
      numbersl2$quesanswerl2=sample(6:10,1)
      updateSliderInput(session, "radiusl2",min=0,max=1.2,step = 0.01,value = 0.05)
      updateSliderInput(session, "movel12",min=0,max=1,step=0.01,value=0.45)
      updateSliderInput(session, "move1l2",min=0,max=1,step=0.01,value=0.5)
      
      updateSliderInput(session, "radius2l2",min=0,max=1.2,step = 0.01,value = 0.05)
      updateSliderInput(session, "movel2",min=0,max=1,step=0.01,value=0.55)
      updateSliderInput(session, "move2l2",min=0,max=1,step=0.01,value=0.5)
      updateNumericInput(session, "P2A", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "P2B", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "A2B", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateButton(session, "next22", disabled = F)
      updateButton(session, "next2", disabled = F)
    }
    else if(val==2){
      updateSelectInput(session, "modes", selected = "level3")
      numbersl3$quesanswerl3=sample(11:15,1)
      updateSliderInput(session, "radiusl3",min=0,max=1.2,step = 0.01,value = 0.05)
      updateSliderInput(session, "movel13",min=0,max=1,step=0.01,value=0.45)
      updateSliderInput(session, "move1l3",min=0,max=1,step=0.01,value=0.5)
      
      updateSliderInput(session, "radius2l3",min=0,max=1.2,step = 0.01,value = 0.05)
      updateSliderInput(session, "movel23",min=0,max=1,step=0.01,value=0.55)
      updateSliderInput(session, "move2l3",min=0,max=1,step=0.01,value=0.5)
      
      updateSliderInput(session, "radius3l3",min=0,max=1.2,step = 0.01,value = 0.05)
      updateSliderInput(session, "movel33",min=0,max=1,step=0.01,value=0.5)
      updateSliderInput(session, "move3l3",min=0,max=1,step=0.01,value=0.45)
      updateNumericInput(session, "P3A", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "P3B", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "P3C", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "A3B", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "A3C", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "B3C", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
      updateNumericInput(session, "A3BC", label = NULL, value = 0,
                         min = 0, max = 1, step = 0.01)
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
      plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      
    })
    
    col1l3 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
    col2l3 <- rgb(red = 0, green = 1, blue = 0, alpha = 0.3)
    col3l3 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.3)
    #Creates the 3 circles for the plot
    draw.circle(input$movel13,input$move1l3,input$radiusl3,col=col1l3)
    draw.circle(input$movel23,input$move2l3,input$radius2l3,col=col2l3)
    draw.circle(input$movel33,input$move3l3,input$radius3l3,col=col3l3)
    
    
  }, width = 350, height = 350)
  
  
  probabilityl3 <- reactiveValues(
    
    probc1l3= NULL,
    probc2l3= NULL,
    probc3l3= NULL,
    intersectionc12l3= NULL,
    intersectionc23l3= NULL,
    intersectionc13l3= NULL,
    intersectionc123l3= NULL,
    d12l3= NULL,
    d23l3= NULL,
    d13l3= NULL
    
  )
  observe({
    
    probabilityl3$d12l3 <- ((input$movel13-input$movel23)^2+(input$move1l3-input$move2l3)^2)^0.5
    probabilityl3$d23l3 <- ((input$movel33-input$movel23)^2+(input$move3l3-input$move2l3)^2)^0.5
    probabilityl3$d13l3 <- ((input$movel13-input$movel33)^2+(input$move1l3-input$move3l3)^2)^0.5
    
    subdividel3 <- 350
    
    xcoordl3 <- NULL
    ycoordl3 <- NULL
    
    for(i in 1:subdividel3){
      
      xcoordl3 <- c(xcoordl3,seq(from = 1/subdividel3, to=1, by = 1/subdividel3))
      ycoordl3 <- c(ycoordl3,rep(i/subdividel3,subdividel3))
      
    }
    
    samplespacel3 <-data.frame(xcoordl3,ycoordl3)
    
    
    samplespacel3$radiusc1l3 <- input$radiusl3
    samplespacel3$radiusc2l3 <- input$radius2l3
    samplespacel3$radiusc3l3 <- input$radius3l3
    samplespacel3$xcenterc1l3 <- input$movel13
    samplespacel3$ycenterc1l3 <- input$move1l3
    samplespacel3$xcenterc2l3 <- input$movel23
    samplespacel3$ycenterc2l3 <- input$move2l3
    samplespacel3$xcenterc3l3 <- input$movel33
    samplespacel3$ycenterc3l3 <- input$move3l3
    
    
    samplespacel3$diffc1l3 <- sqrt((samplespacel3$xcenterc1l3-samplespacel3$xcoordl3)^2+(samplespacel3$ycenterc1l3-samplespacel3$ycoordl3)^2)
    samplespacel3$inc1l3<- samplespacel3$diffc1l3 <= samplespacel3$radiusc1l3
    
    samplespacel3$diffc2l3 <- sqrt((samplespacel3$xcenterc2l3-samplespacel3$xcoordl3)^2+(samplespacel3$ycenterc2l3-samplespacel3$ycoordl3)^2)
    samplespacel3$inc2l3<- samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3
    
    samplespacel3$diffc3l3 <- sqrt((samplespacel3$xcenterc3l3-samplespacel3$xcoordl3)^2+(samplespacel3$ycenterc3l3-samplespacel3$ycoordl3)^2)
    samplespacel3$inc3l3<- samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3
    
    samplespacel3$intersectionc12l3 <- (samplespacel3$diffc1l3 <= samplespacel3$radiusc1l3) & (samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3)
    samplespacel3$intersectionc23l3 <- (samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3) & (samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3)
    samplespacel3$intersectionc13l3 <- (samplespacel3$diffc1l3 <= samplespacel3$radiusc1l3) & (samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3)
    samplespacel3$intersectionc123l3 <- (samplespacel3$diffc1l3 <= samplespacel3$radiusc1l3) & (samplespacel3$diffc2l3 <= samplespacel3$radiusc2l3) & (samplespacel3$diffc3l3 <= samplespacel3$radiusc3l3)
    
    p1l3<- mean(samplespacel3$inc1l3)
    probabilityl3$probc1l3<-signif(p1l3,2)
    
    p2l3<- mean(samplespacel3$inc2l3)
    probabilityl3$probc2l3<-signif(p2l3,2)
    
    p3l3 <- mean(samplespacel3$inc3l3)
    probabilityl3$probc3l3<-signif(p3l3,2)
    
    p12l3 <- mean(samplespacel3$intersectionc12l3)
    probabilityl3$intersectionc12l3<-signif(p12l3,2)
    
    p23l3 <- mean(samplespacel3$intersectionc23l3)
    probabilityl3$intersectionc23l3<-signif(p23l3,2)
    
    p13l3 <- mean(samplespacel3$intersectionc13l3)
    probabilityl3$intersectionc13l3<-signif(p13l3,2)
    
    p123l3 <- mean(samplespacel3$intersectionc123l3)
    probabilityl3$intersectionc123l3<-signif(p123l3,2)
  })
  output$inter3 <- renderUI(
    print("Intersection")
  )
  
  #reset
  observeEvent(input$reset_buttonl3,{
    updateSliderInput(session, "radiusl3",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel13",min=0,max=1,step=0.01,value=0.45)
    updateSliderInput(session, "move1l3",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius2l3",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel23",min=0,max=1,step=0.01,value=0.55)
    updateSliderInput(session, "move2l3",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius3l3",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel33",min=0,max=1,step=0.01,value=0.5)
    updateSliderInput(session, "move3l3",min=0,max=1,step=0.01,value=0.45)
  })
  observe({
    output$labeldoBl3 <- renderUI({ #Output means  
      actionButton("dol3","", class = "btn-group") #blue circle action button
    })
    output$labeldoGl3 <- renderUI({
      actionButton("do2l3"," ", class = "btn-group") #green circle action button
    })
    output$labeldoRl3 <- renderUI({
      actionButton("do3l3"," ", class = "btn-group")#red circle action button
    })
    output$labeldoBGl3 <- renderUI({
      actionButton("do4l3","", class = "btn-group") #cyan circle action button
    })
    output$labeldoBRl3 <- renderUI({
      actionButton("do5l3","", class = "btn-group") #purple circle action button
    })
    output$labeldoGRl3 <- renderUI({
      actionButton("do6l3","", class = "btn-group") #darkolivegreen circle action button
    })
    output$labeldoBGRl3 <- renderUI({
      actionButton("do7l3","", class = "btn-group") #brown circle action button
    })
    output$labeldoBGl33 <- renderUI({ #Blue Green
      actionButton("do4l3","", class = "btn-group") #
    })
    output$labeldoBRl33 <- renderUI({ #Blue Red
      actionButton("do5l3","", class = "btn-group") #
    })
    output$labeldoGRl33 <- renderUI({ #Green Red
      actionButton("do6l3","", class = "btn-group") #
    })
    output$labeldoBGRl33 <- renderUI({ #Blue Green Red 
      actionButton("do7l3","", class = "btn-group") #
    })
  })
  
  #blue button
  output$PAl3<- renderPrint({cat(probabilityl3$probc1l3)})
  #green button
  output$PBl3<- renderPrint({cat(probabilityl3$probc2l3)})
  #red button
  output$PCl3<- renderPrint({cat(probabilityl3$probc3l3)})
  #cyan button
  output$ABl3<- renderPrint({cat(probabilityl3$intersectionc12l3)})
  #purple button
  output$ACl3<- renderPrint({cat(probabilityl3$intersectionc13l3)})
  #darkolivegreen button
  output$BCl3<- renderPrint({cat(probabilityl3$intersectionc23l3)})
  #center button
  output$ABCl3<- renderPrint({cat(probabilityl3$intersectionc123l3)})
  
  
  ### random choose question
  numbersl3 <- reactiveValues( quesanswerl3=c())
  
  observe({
    numbersl3$quesanswerl3=sample(11:15,1)
  })
  output$PA3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,12])
  })
  output$PB3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,13])
  })
  output$PC3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,14])
  })
  output$AB3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,15])
  })
  output$BC3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,16])
  })
  output$AC3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,17])
  })
  output$ABC3<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,18])
  })
  output$PA33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,12])
  })
  output$PB33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,13])
  })
  output$PC33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,14])
  })
  output$AB33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,15])
  })
  output$BC33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,16])
  })
  output$AC33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,17])
  })
  output$ABC33<- renderPrint({
    cat(bank[numbersl3$quesanswerl3,18])
  })
  output$questionl3<-renderText(bank[numbersl3$quesanswerl3,4])
  space3 <- c(11:15)
  #Generate next question
  observeEvent(input$next3,{
    numbersl3$quesanswerl3 <- sample(space3[-numbersl3$quesanswerl3],1)
    updateSliderInput(session, "radiusl3",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel13",min=0,max=1,step=0.01,value=0.45)
    updateSliderInput(session, "move1l3",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius2l3",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel23",min=0,max=1,step=0.01,value=0.55)
    updateSliderInput(session, "move2l3",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius3l3",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel33",min=0,max=1,step=0.01,value=0.5)
    updateSliderInput(session, "move3l3",min=0,max=1,step=0.01,value=0.45)
    hide('pic3_div')
  })
  observeEvent(input$feedback3, {
    toggle(id= "panelS3")
  })
  
  
  
  
  observeEvent(input$feedback33, {
    toggle(id= "panelN3")
  }) 
  
  
  
  output$answerl3 <- renderPrint({
    
    if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
      
      if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&
         (probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
      {cat("Great! You are right!")
        updateButton(session, "next3", disabled = F)
      }
      
      else{
        cat("Keep changing the size and placement of your circles to match the probabilities desired.")
        updateButton(session, "next3", disabled = F)
      }
    }
    
    else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
      if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&
         (probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&
         (probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
      {cat("Great! You are right!")
        updateButton(session, "next3", disabled = F)
      }
      else{
        cat("Keep changing the size and placement of your circles to match the probabilities desired.")
        updateButton(session, "next3", disabled = F)
      }
    }
  })
  
  output$answerl3Picture <- renderPrint({
    
    if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
      
      if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&
         (probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
      {
        img(src = "correct.png", alt = "Correct", width = 30)
      }
      
      else{
        img(src = "incorrect.png", alt = "Incorrect", width = 30)
      }
    }
    
    else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
      if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&
         (probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&
         (probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
      {
        img(src = "correct.png", alt = "Correct", width = 30)
      }
      else{
        img(src = "incorrect.png", alt = "Incorrect", width = 30)
      }
    }
  })
  
  
  
  observeEvent(input$pic3,{
    toggle('pic3_div')
    output$Feed33 <- renderUI({
      
      img(src = bank[numbersl3$quesanswerl3,19], alt = bank[numbersl3$quesanswerl3,20], height = "70%", width = "70%")
      
    })
  })
  
  
})

####### large segments removed ####################

{
  
  #THE FOLLOWING TWO OUTPUTS OF FDBC GIVE SLIGHLY MORE ADVICE ON MODIFYING THE CIRCLES. THERE USED TO
    #BE ONE FOR ALL 6 OF THE EXAMPLES. REMOVED BECAUSE THE INFORMATION WASN'T VERY HELPFUL AND CLUTTERED 
    #THE SCREEN. CAN BRING BACK IF YOU'D LIKE
  
  # output$fdbc33 = renderPrint({
  #   validate(
  #     need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please enter all your probabilities")
  #   )
  #   
  #   if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
  #     
  #     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
  #     {cat("Great! You are right!")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])))
  #     {cat("Probability is right, but the relationship is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
  #     {cat("The relationship is right, but the probability is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else{
  #       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #   }
  #   
  #   else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
  #     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
  #     {cat("Great! You are right!")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])|(probabilityl3$intersectionc123l3!=bank[numbersl3$quesanswerl3,11])))
  #     {cat("Probability is right, but the relationship is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
  #     {cat("The relationship is right, but the probability is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else{
  #       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #   }
  # })
  
  # output$fdbc3 = renderPrint({
  #   
  #   if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
  #     
  #     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
  #     {cat("Great! You are right!")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])))
  #     {cat("Probability is right, but the relationship is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
  #     {cat("The relationship is right, but the probability is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else{
  #       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #   }
  #   
  #   else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
  #     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
  #     {cat("Great! You are right!")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])|(probabilityl3$intersectionc123l3!=bank[numbersl3$quesanswerl3,11])))
  #     {cat("Probability is right, but the relationship is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
  #     {cat("The relationship is right, but the probability is Wrong. Try again.")
  #       updateButton(session, "next3", disabled = F)}
  #     else{
  #       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
  #       updateButton(session, "next3", disabled = F)
  #     }
  #   }
  # })
}
