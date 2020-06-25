library(shinydashboard)
library(shiny)
library(plotrix)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(Vennerable)

bank<- read.csv("questionbank.csv")
bank= data.frame(lapply(bank,as.character),stringsAsFactors = FALSE)
jsResetCode <- "shinyjs.reset= function() {history.go(0)}"

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

  #Go Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "circle")
  })
  
  
  observeEvent(input$hint,{
    sendSweetAlert(
      session = session,
      title = "Hints:",
      text = "1. Understand if you are dealing with one event or multiple events.\n
      2. Check if the relationship is mutually exclusive.\n
      3. Check if the relationship is independent.",
      type = "info"
    )
  })

  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "If you want to adjust the circles using slidrs in the Venn Diagram:\n
              1.Move or change the size of the circles using sliders (accessed by the button).\n
      2. The 'Feedbck' button shows comments to your inputs with more details; the 'Next' button provides a new question.\n
      If you want to enter the probabilities directly:\n
      1. Enter your probability in the textbox.\n
      2. If you want to get hints, please click 'Venn Diagram for Answer' button.",
      type = "info"
    )
  })
  output$challenge <- renderText({paste("Challenge")})
  #output$instruction <- renderText({paste("Please adjust circle(s) to create a diagram that fits the following situation:")})
  
  # Level 1
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
    
    samplespacel1$radiusl1 <- input$radiusl1
    samplespacel1$xcenterl1 <- input$movel1
    samplespacel1$ycenterl1 <- input$move1l1
    
    samplespacel1$diffl1 <- sqrt((samplespacel1$xcenterl1-samplespacel1$xcoordl1)^2
                                 +(samplespacel1$ycenterl1-samplespacel1$ycoordl1)^2)
    samplespacel1$inc1l1<- samplespacel1$diffl1 <= samplespacel1$radiusl1
    
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
  #Next question
  observeEvent(input$next1, {
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
observeEvent(input$feedback1, {
    toggle(id= "panelS1")
  })

output$fdbc1 = renderText({
                   
                   if(probabilityl1$probc1l1 == bank[numbersl1$quesanswerl1, 5])
                   {
                     return("Great! You are right!")
                   }
                   else if (probabilityl1$probc1l1 > bank[numbersl1$quesanswerl1, 5])
                   {
                     return("You got it too large.")
                   }
                   else 
                   {
                     return("Maybe you should make it larger.")
                   }  
                   
                 })

observeEvent(input$feedback11, {
  toggle(id= "panelN1")
}) 
output$fdbc11 = renderText({
  validate(
    need(input$PA != "", "Please enter your probability")
  )
  if(probabilityl1$probc1l1 == bank[numbersl1$quesanswerl1, 5])
  {
    return("Great! You are right!")
  }
  else if (probabilityl1$probc1l1 > bank[numbersl1$quesanswerl1, 5])
  {
    return("You got it too large.")
  }
  else 
  {
    return("Maybe you should make it larger.")
  }  
  
})

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
  
  observeEvent(input$pic1,{
    toggle('pic1_div')
    output$Feed11 <- renderUI({
      img(src = bank[numbersl1$quesanswerl1, 19],  height = "70%",  width = "70%")
    })
  })
  
  observeEvent(input$pic11,{
    toggle('pic11_div')
    output$Feed1 <- renderUI({
      img(src = bank[numbersl1$quesanswerl1, 19],  height = "25%",  width = "25%")
    })
  })
  
  observeEvent(input$next11,{
    numbersl1$quesanswerl1 <- sample(space[-numbersl1$quesanswerl1],1)
    updateNumericInput(session, "PA", label = NULL, value = 0,
                       min = 0, max = 1, step = 0.01)
    updateButton(session, "next11", disabled = F)
    updateCheckboxInput(session, "pic1", value = F)
  })
  
  # level 2
  
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
  #Generate next question
  observeEvent(input$next2,{
    numbersl2$quesanswerl2 <- sample(space2[-numbersl2$quesanswerl2],1)
    updateSliderInput(session, "radiusl2",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel12",min=0,max=1,step=0.01,value=0.45)
    updateSliderInput(session, "move1l2",min=0,max=1,step=0.01,value=0.5)
    
    updateSliderInput(session, "radius2l2",min=0,max=1.2,step = 0.01,value = 0.05)
    updateSliderInput(session, "movel2",min=0,max=1,step=0.01,value=0.55)
    updateSliderInput(session, "move2l2",min=0,max=1,step=0.01,value=0.5)
  })
observeEvent(input$feedback2, {
    toggle(id= "panelS2")
  })

output$fdbc2 = renderPrint({
                   
                   if((probabilityl2$probc1l2==bank[numbersl2$quesanswerl2,5])&(probabilityl2$probc2l2==bank[numbersl2$quesanswerl2,6])& (probabilityl2$probintersectionl2 == bank[numbersl2$quesanswerl2,8])) 
                   {cat("Great! You are right!")
                     updateButton(session, "next2", disabled = F)
                   }
                   else if((probabilityl2$probc1l2==bank[numbersl2$quesanswerl2,5])&(probabilityl2$probc2l2==bank[numbersl2$quesanswerl2,6])&(probabilityl2$dl2 >= bank[numbersl2$quesanswerl2,8])) {
                     cat("Probability is right, but the relationship is Wrong. Try again.")
                     updateButton(session, "next2", disabled = F)
                   }
                   else if(((probabilityl2$probc1l2!=bank[numbersl2$quesanswerl2,5]) | (probabilityl2$probc2l2!=bank[numbersl2$quesanswerl2,6]) )&(probabilityl2$dl2 == bank[numbersl2$quesanswerl2,8])){
                     cat("The relationship is right, but the probability is Wrong. Try again.")
                     updateButton(session, "next2", disabled = F)
                   }
                   else{
                     cat("Keep changing the size and placement of your circles to match the probabilities desired.")
                     updateButton(session, "next2", disabled = F)
                   }
                 })
observeEvent(input$feedback22, {
  toggle(id= "panelN2")
})

output$fdbc22 = renderPrint({
  validate(
    need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please enter all your probabilities")
  )

                   if((probabilityl2$probc1l2==bank[numbersl2$quesanswerl2,5])&(probabilityl2$probc2l2==bank[numbersl2$quesanswerl2,6])& (probabilityl2$probintersectionl2 == bank[numbersl2$quesanswerl2,8])) 
                   {cat("Great! You are right!")
                     updateButton(session, "next2", disabled = F)
                   }
                   
                   else if((probabilityl2$probc1l2==bank[numbersl2$quesanswerl2,5])&(probabilityl2$probc2l2==bank[numbersl2$quesanswerl2,6])&(probabilityl2$dl2 >= bank[numbersl2$quesanswerl2,8])) {
                     cat("Probability is right, but the relationship is Wrong. Try again.")
                     updateButton(session, "next2", disabled = F)
                   }
                   else if(((probabilityl2$probc1l2!=bank[numbersl2$quesanswerl2,5]) | (probabilityl2$probc2l2!=bank[numbersl2$quesanswerl2,6]) )&(probabilityl2$dl2 == bank[numbersl2$quesanswerl2,8])){
                     cat("The relationship is right, but the probability is Wrong. Try again.")
                     updateButton(session, "next2", disabled = F)
                   }
                   else{
                     cat("Keep changing the size and placement of your circles to match the probabilities desired.")
                     updateButton(session, "next2", disabled = F)
                   }
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
  
  observeEvent(input$pic2,{
    toggle('pic2_div')
    output$Feed22 <- renderUI({
      
      img(src = bank[numbersl2$quesanswerl2,19], height = "70%", width = "70%")
      
    })
  })
## level2 enter
  observeEvent(input$pic22,{
    toggle('pic22_div')
    output$Feed2 <- renderUI({
      
      img(src = bank[numbersl2$quesanswerl2,19], height = "70%", width = "70%")
      
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

  w2 = reactive({
    compute.Venn(Venn(SetNames = c("", ""), Weight = c(`01` = input$P2B-input$A2B, `11` = input$A2B, `10` = input$P2A-input$A2B)), type ="circles", doEuler=TRUE)
  })
  
  output$enterplot2 = renderPlot({
    validate(
      need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please input the numbers")
    )
    if ((min(input$P2A, input$P2B) == 0 ) & (input$A2B == 0) ) {
      isolate({plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)})
      text(1,1,"Note that there are two events",cex = 1, col = "red")
    }
    else if ((input$P2A == input$P2B ) & (input$P2A == input$A2B) & (input$P2B == input$A2B)) {
      isolate({plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)})
      draw.circle(1,1,.1,col="#7cc9b2")
    }
    else if ((input$P2A + input$P2B - input$A2B <=1) & (input$A2B <= min(input$P2A, input$P2B)) ) {
      gp <- VennThemes(w2())
      gp[["Face"]][["11"]]$fill <-  "#79CAB1" #original mitegreen
      gp[["Face"]][["01"]]$fill <-  "#B6FEB5" #original green
      gp[["Face"]][["10"]]$fill <-  '#B3B2FF' #original purple
      gp[["Set"]][["Set1"]]$col <- 'black'
      gp[["Set"]][["Set2"]]$col <- 'black'
      gp[["Set"]][["Set1"]]$lwd <- 1.5
      gp[["Set"]][["Set2"]]$lwd <- 1.5
      plot(w2(), gp = gp, show = list(SetLabels = FALSE))
    }
    else{
      plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      text(1,1,"Error: impossible to exist",cex = 1, col = "red")
    }
    
  },width = 300, height = 280)


  observeEvent(input$next22,{
    numbersl2$quesanswerl2 <- sample(space2[-numbersl2$quesanswerl2],1)
    updateNumericInput(session, "P2A", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "P2B", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A2B", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateButton(session, "next22", disabled = F)
    updateCheckboxInput(session, "pic2", value = F)
  })
  
  # level 3
  
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
    output$labeldoBl3 <- renderUI({
      actionButton("dol3","", class = "btn-group")
    })
    output$labeldoGl3 <- renderUI({
      actionButton("do2l3"," ", class = "btn-group")
    })
    output$labeldoRl3 <- renderUI({
      actionButton("do3l3"," ", class = "btn-group")
    })
    output$labeldoBGl3 <- renderUI({
      actionButton("do4l3","", class = "btn-group")
    })
    output$labeldoBRl3 <- renderUI({
      actionButton("do5l3","", class = "btn-group")
    })
    output$labeldoGRl3 <- renderUI({
      actionButton("do6l3","", class = "btn-group")
    })
    output$labeldoBGRl3 <- renderUI({
      actionButton("do7l3","", class = "btn-group")
    })
    output$labeldoBGl33 <- renderUI({
      actionButton("do4l3","", class = "btn-group")
    })
    output$labeldoBRl33 <- renderUI({
      actionButton("do5l3","", class = "btn-group")
    })
    output$labeldoGRl33 <- renderUI({
      actionButton("do6l3","", class = "btn-group")
    })
    output$labeldoBGRl33 <- renderUI({
      actionButton("do7l3","", class = "btn-group")
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
   
  })
observeEvent(input$feedback3, {
    toggle(id= "panelS3")
  })
  

output$fdbc3 = renderPrint({
                   
                   if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
                     
                     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
                     {cat("Great! You are right!")
                       updateButton(session, "next3", disabled = F)
                     }
                     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])))
                     {cat("Probability is right, but the relationship is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
                     {cat("The relationship is right, but the probability is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else{
                       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
                       updateButton(session, "next3", disabled = F)
                     }
                   }
                   
                   else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
                     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
                     {cat("Great! You are right!")
                       updateButton(session, "next3", disabled = F)
                     }
                     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])|(probabilityl3$intersectionc123l3!=bank[numbersl3$quesanswerl3,11])))
                     {cat("Probability is right, but the relationship is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
                     {cat("The relationship is right, but the probability is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else{
                       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
                       updateButton(session, "next3", disabled = F)
                     }
                   }
                 })
                 
  observeEvent(input$feedback33, {
                   toggle(id= "panelN3")
                 }) 

output$fdbc33 = renderPrint({
  validate(
    need(((input$P2A != "")&(input$P2B != "")&(input$A2B != "")), "Please enter all your probabilities")
  )
                   
                   if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
                     
                     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
                     {cat("Great! You are right!")
                       updateButton(session, "next3", disabled = F)
                     }
                     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])))
                     {cat("Probability is right, but the relationship is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
                     {cat("The relationship is right, but the probability is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else{
                       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
                       updateButton(session, "next3", disabled = F)
                     }
                   }
                   
                   else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
                     if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
                     {cat("Great! You are right!")
                       updateButton(session, "next3", disabled = F)
                     }
                     else if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&((probabilityl3$intersectionc12l3!=bank[numbersl3$quesanswerl3,8])|(probabilityl3$intersectionc23l3!=bank[numbersl3$quesanswerl3,9])|(probabilityl3$intersectionc13l3!=bank[numbersl3$quesanswerl3,10])|(probabilityl3$intersectionc123l3!=bank[numbersl3$quesanswerl3,11])))
                     {cat("Probability is right, but the relationship is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else if(((probabilityl3$probc1l3!=bank[numbersl3$quesanswerl3,5])|(probabilityl3$probc2l3!=bank[numbersl3$quesanswerl3,6])|(probabilityl3$probc3l3!=bank[numbersl3$quesanswerl3,7]))&(probabilityl3$intersectionc12l3==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3== bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
                     {cat("The relationship is right, but the probability is Wrong. Try again.")
                       updateButton(session, "next3", disabled = F)}
                     else{
                       cat("Keep changing the size and placement of your circles to match the probabilities desired.")
                       updateButton(session, "next3", disabled = F)
                     }
                   }
                 })
  
  output$answerl3 <- renderPrint({
      
      if(any(bank[numbersl3$quesanswerl3,3]==c(11,12,13,14))){
        
        if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10]))
        {cat("Great! You are right!")
          updateButton(session, "next3", disabled = F)
          }
       
        else{
          cat("Keep changing the size and placement of your circles to match the probabilities desired.")
          updateButton(session, "next3", disabled = F)
        }
      }
      
      else if(any(bank[numbersl3$quesanswerl3,3]==c(15))){
        if((probabilityl3$probc1l3==bank[numbersl3$quesanswerl3,5])&(probabilityl3$probc2l3==bank[numbersl3$quesanswerl3,6])&(probabilityl3$probc3l3==bank[numbersl3$quesanswerl3,7])&(probabilityl3$intersectionc12l3 ==bank[numbersl3$quesanswerl3,8])&(probabilityl3$intersectionc23l3==bank[numbersl3$quesanswerl3,9])&(probabilityl3$intersectionc13l3==bank[numbersl3$quesanswerl3,10])&(probabilityl3$intersectionc123l3==bank[numbersl3$quesanswerl3,11]))
        {cat("Great! You are right!")
          updateButton(session, "next3", disabled = F)
          }
        else{
          cat("Keep changing the size and placement of your circles to match the probabilities desired.")
          updateButton(session, "next3", disabled = F)
        }
      }
  })
  
  observeEvent(input$pic3,{
    toggle('pic3_div')
    output$Feed33 <- renderUI({
      
      img(src = bank[numbersl3$quesanswerl3,19], height = "70%", width = "70%")
      
    })
  })
  

  
  ## level3 enter
  observeEvent(input$pic33,{
    toggle('pic33_div')
    output$Feed3 <- renderUI({
      
      img(src = bank[numbersl3$quesanswerl3,19], height = "70%", width = "70%")
      
    })
  })
  
  w3 = reactive({
    compute.Venn(Venn(SetNames = c("1", "2", "3"), Weight = c(
      `001` = input$P3B-input$A3B-input$B3C+input$A3BC, 
      `010` = input$P3C-input$A3C-input$B3C+input$A3BC, 
      `100` = input$P3A-input$A3B-input$A3C+input$A3BC,
      `101` = input$A3B-input$A3BC, 
      `110` = input$A3C-input$A3BC,
      `011` = input$B3C-input$A3BC,
      `111` = input$A3BC )), type ="circles", doEuler=TRUE)
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
    else if ((input$P3A + input$P3B + input$P3C - input$A3B - input$A3C - input$B3C  <= 1)#- input$A3BC
             & (input$A3BC <= min(input$A3C, input$A3B, input$B3C)) & (input$A3B <= min(input$P3A, input$P3B)) 
             & (input$A3C <= min(input$P3A, input$P3C))  & (input$B3C <= min(input$P3B, input$P3C)) 
              ) { 
      gp <- VennThemes(w3())
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
      plot(1,1,col="white", type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      text(1,1,"Error: impossible to exist",cex = 1, col = "red")
    }
  },width = 300, height = 280)

  

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


  observeEvent(input$next33,{
    numbersl3$quesanswerl3 <- sample(space3[-numbersl3$quesanswerl3],1)
    updateNumericInput(session, "P3A", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "P3B", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "P3C", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A3B", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A3C", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "B3C", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateNumericInput(session, "A3BC", label = NULL, value = NULL,
                       min = 0, max = 1, step = 0.01)
    updateButton(session, "next33", disabled = F)
    updateCheckboxInput(session, "pic3", value = F)
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
})

