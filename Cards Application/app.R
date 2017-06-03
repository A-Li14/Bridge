###Attempt to create Card playing application from scratch
#setwd("~/R/Bridge/Basic Version")
require(shiny)
require(stringr)
require(data.table)
require(ids)
require(DT)

###To Do List
###Enable multiple people to view one set of cards
###Turn visual display for cards into buttons


# Create deck of cards
# if(file.exists("deck.Rds")) {
#     deck = readRDS("deck.Rds")
# } else {
# suits = c("Diamonds","Clovers","Hearts","Spades")
# cards = factor(c(as.character(2:10),"J","Q","K","A"))
# deck = data.table(card=character(52),number=character(52),suit=character(52))
# for(i in 1:4) {
#     deck[((i-1)*13+1):((i)*13),card:=paste(cards,suits[i])]
# }
# deck[,number:=factor(str_extract(card,"^[0-9a-zA-Z]{1,2}"),levels=cards)]
# deck[,suit:=factor(str_extract(card,"[a-zA-Z]+$"),levels=suits)]

    # saveRDS(deck,"deck.Rds")
# }

deck = readRDS("data/deck2.RDS")
source("CardButtons.R")
width = "72"
height = "100"

generateHands = function() {
    vars$hands = vector("list",4)
    dist = sample(1:52,replace=F)
    
    for(i in 1:4) {
        vars$hands[[i]] = deck[dist[((i-1)*13+1):(i*13)]][order(suit,number)]
    }
}

initialHands = vector("list",4)
dist = sample(1:52,replace=F)
for(i in 1:4) {
    initialHands[[i]] = deck[dist[((i-1)*13+1):(i*13)]][order(suit,number)]
}

# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(users=NULL, hands=initialHands, table=NULL, 
                       availHands=1:4, 
                       #lastPlays = data.table(User=character(),Play=tags$img()), 
                       lastPlays = data.table(User=character(),Play=character()), 
                       turn = 1,
                       resetCount=1)
###resetCount is a dummy variable to enable other players to have select options




# Restore sets of hands or generate them
# if(file.exists("hands.Rds")) {
#     vars$hands <- readRDS("hands.Rds")
# } else {
#     generateHands()
# }

# Restore table from previous game
# if(file.exists("table.Rds")) {
#     vars$table <- readRDS("table.Rds")
# }


server = shinyServer(function(input, output, session) {
    
    # Create a spot for reactive variables specific to this particular session
    sessionVars <- reactiveValues(username = "",handNum = 0,buttonVals=numeric(13))
    
    ###Tracking whether an identity has been created on the application
    init = FALSE
    
    ###Session termination procedure
    session$onSessionEnded(function() {
        isolate({
            vars$users <- vars$users[vars$users != sessionVars$username]
            if(sessionVars$handNum!=0)
                vars$availHands <- c(vars$availHands,sessionVars$handNum)
        })
    })
    
    # Observer to handle changes to the username
    observe({
        # We want a reactive dependency on this variable, so we'll just list it here.
        input$user
        
        if (!init){
            # Seed initial username
            sessionVars$username <- adjective_animal()
            # isolate({
            #     vars$chat <<- c(vars$chat, paste0(linePrefix(),
            #                                       tags$span(class="user-enter",
            #                                                 sessionVars$username,
            #                                                 "entered the room.")))
            # })
            
            ###Automatically assign an available hand to new session
            # if(length(vars$availHands)>0) {
            #     pick = sample(vars$availHands,1)
            #     sessionVars$handNum <- pick
            #     vars$availHands = vars$availHands[which(vars$availHands!=pick)]
            # }
            ###If a person isn't assigned a hand on initialization, remove 
            ###user interface and add an option to obtain an available hand
            
            #vars$users = c(vars$users,sessionVars$username)
            
            init <<- TRUE
        } else{
            # A previous username was already given
            isolate({
                if (input$user == sessionVars$username || input$user == ""){
                    # No change. Just return.
                    return()
                }
                
                # Updating username      
                # First, remove the old one
                vars$users <- vars$users[vars$users != sessionVars$username]
                
                # Note the change in the chat log
                # vars$chat <<- c(vars$chat, paste0(linePrefix(),
                #                                   tags$span(class="user-change",
                #                                             paste0("\"", sessionVars$username, "\""),
                #                                             " -> ",
                #                                             paste0("\"", input$user, "\""))))
                # 
                # Now update with the new one
                sessionVars$username <- input$user
            })
        }
        # Add this user to the global list of users
        isolate(vars$users <- c(vars$users, sessionVars$username))
    })
    
    
    # Keep the username updated with whatever sanitized/assigned username we have
    observe({
        updateTextInput(session, "user", 
                        value=sessionVars$username)    
    })
    
    
    
    ###Rest hands when the reset button is clicked
    ###########Error
    ###########Generating hands in this manner enables card selection only for
    ###########the individual who clicked reset
    # observeEvent(input$reset,{
    #     generateHands()
    #     updateSelectInput(session,"card_choice",label="Select a Card",
    #                       choices = yourHand()$card
    #                       )
    # 
    # })
    
    observeEvent(input$reset,{
        generateHands()
        vars$resetCount = vars$resetCount + 1
    })
    
    observe({
        #input$reset
        vars$hands
        #if(!is.null(input$card_choice)) {
        if(input$card_choice!="") {
            #print("not null card choice")
            #print(input$card_choice)
            if(input$reset>=1|vars$resetCount>=1) {
                
                updateSelectInput(session,"card_choice",label="Select a Card",
                                  choices = yourHand()$card,selected=input$card_choice
                )
                
            }
        }
        else {
            #print("is null card choice")
            if(input$reset>=1|vars$resetCount>=1) {
                updateSelectInput(session,"card_choice",label="Select a Card",
                                  choices = yourHand()$card
                )
            }
        }
    })
    
    ###Plays a card from your hand: reads your selection and removes the card
    observeEvent(input$play,{
        
        ###If it is this player's turn, play a card
        # if(vars$users[vars$turn%%4]==sessionVars$username) {
        #     vars$turn = vars$turn + 1
            
        ###Alternate: if the player doesn't have a card out, play it. 
        if(input$card_choice!=""){
            if(!sessionVars$username%in%vars$lastPlays[,User]&nrow(yourHand())>0) {
                played = which(yourHand()$card==input$card_choice)
                vars$lastPlays = rbind(vars$lastPlays,data.table(User=sessionVars$username,Play=yourHand()[played,card]))
                #vars$lastPlays = rbind(vars$lastPlays,data.table(User=sessionVars$username,Play=yourHand()[played,tags$img(path)]))
                
                vars$hands[[sessionVars$handNum]] = vars$hands[[sessionVars$handNum]][-played]
                
                updateSelectInput(session,"card_choice",label="Select a Card",
                                  choices = yourHand()$card
                )
            }
        }
    
        # }
    })
    
    ##clears card table
    observeEvent(input$nextTurn,{
        vars$lastPlays = data.table(User=character(),Play=character())
        #vars$lastPlays = data.table(User=character(),Play=tags$img())
    })
    
    ##your hand
    yourHand = reactive({
        if(sessionVars$handNum!=0)
            ds = data.table(vars$hands[[sessionVars$handNum]])
        else
            ds = data.table()
        ds
        
    })
    
    observeEvent(input$quit,{
        vars$availHands = sort(c(vars$availHands,sessionVars$handNum))
        sessionVars$handNum = 0
        updateSelectInput(session,"hand_choice","Take Hand Number",vars$availHands)
        
    })
    
    observeEvent(input$join,{
        ###Take a random available hand.
        #pick = sample(vars$availHands,1)
        #print(paste("random choice",pick))
        pick = as.numeric(input$hand_choice) ###Need to turn it into a numeric...
        #print(class(input$hand_choice))
        #print(paste("selected choice",pick))        
        sessionVars$handNum <- pick
        vars$availHands = vars$availHands[which(vars$availHands!=pick)]
        
        updateSelectInput(session,"hand_choice","Take Hand Number",vars$availHands)
        
    })
    
    ###Game displays/mechanics helpers
    
    ##card table
    output$playedDisplay = renderDataTable({
        datatable(vars$lastPlays,escape=F,options=list(paging=F,searching=F,info=F,autoWidth=F,columns.searchable=F))
        # dt = vars$lastPlays
        # setnames(dt,c("User","Plays"))
        # dt
    })
    
    ##for debug purposes; hands which are not yet assigned to a player
    output$openHands = renderTable({
        data.table(vars$availHands)
    })
    
    ##Your hand
    # output$handDisplay = renderDataTable({
    #     #datatable(yourHand()[,.(Image=path)],escape=F)
    #     display = NULL
    #     if(sessionVars$handNum!=0)
    #         display=datatable(t(yourHand()$path),escape=F,rownames=F,options=list(paging=F,searching=F,info=F,autoWidth=F,columns.searchable=F))
    #     display
    #     # dt = yourHand()
    #     # setnames(dt,"card","Your_Hand")
    #     # dt[,Your_Hand]
    #     
    # })
    
    ###ActionButton Version of your hand
    output$handDisplay = renderUI({
        display=NULL
        if(sessionVars$handNum!=0){
            display=lapply(1:nrow(yourHand()),function(i){
                cardButton2(yourHand()[i,],height,width)
            })
        }
        display
    })
    
    ###Delete cards from yourHand via the button interface
    observe({
        if(nrow(yourHand())>0) { 
            buttonVals=sapply(1:nrow(yourHand()),function(i){
                input[[yourHand()[i,card]]]
            })
            
            if(class(buttonVals)!="list"){
                if(!sessionVars$username%in%vars$lastPlays[,User]&nrow(yourHand())>0) {
                    remove = which(buttonVals>sessionVars$buttonVals)
                    if(length(remove)>0) {
                        
                        #print(paste("remove:",remove))
                        #print(paste("buttonVals:",buttonVals))
                        vars$lastPlays = rbind(vars$lastPlays,data.table(User=sessionVars$username,Play=yourHand()[remove,card]))
                        #vars$lastPlays = rbind(vars$lastPlays,data.table(User=sessionVars$username,Play=yourHand()[remove,tags$img()]))
                        vars$hands[[sessionVars$handNum]]=vars$hands[[sessionVars$handNum]][-remove]
                        
                        
                        
                    }
                }
                sessionVars$buttonVals = buttonVals
            }
        }
    })
    
    ##Active player: var$turn is currently not in use
    # output$active_player = renderText({
    #     paste0(vars$users[vars$turn%%4],"'s Turn")
    # })
     
    ###Player List
    output$player_list = renderTable({
        dt = data.table(Players=vars$users)
        dt
    })
      
    output$handNumber = renderText({
        sessionVars$handNum
    })
    
    ###Didn't work
    # output$gameControls = renderUI({
    #     if(sessionVars$handNum!=0) {
    #         taglist(
    #             
    #             actionButton("reset","Reset Hands"),
    #             actionButton("quit","Drop Hand"),
    #             textInput("user", "Your User ID:", value=""),
    #             
    #             selectInput("card_choice","Select a Card",choices=NULL),
    #             actionButton("play","Play Selected Card"),
    #             
    #             actionButton("nextTurn","Next Turn"),
    #             tableOutput("player_list")
    #             
    #         )
    #     }
    #     else {
    #         actionButton("join","Take Available Hand")
    #     }
    # })
    
})

ui = fluidPage(
    
    sidebarPanel(
        
        helpText("Pick a hand of 13 cards. Play 1 card per turn. Hit 'Next Turn' to clear the table."),
        
        helpText("Your Hand Number:"),
        textOutput("handNumber"),
        
        ###Debug
        #tableOutput("openHands"),
        
        # uiOutput("gameControls")
        
        #conditionalPanel("output.handNum!=0",
        conditionalPanel("output.handNumber!='0'",
                         actionButton("reset","Reset Hands"),
                         actionButton("quit","Drop Hand"),
                         textInput("user", "Your User ID:", value=""),

                         selectInput("card_choice","Select a Card",choices=NULL),
                         actionButton("play","Play Selected Card"),

                         actionButton("nextTurn","Next Turn"),
                         tableOutput("player_list")
        ),
        conditionalPanel("output.handNumber=='0'",
                         actionButton("join","Take Available Hand"),
                         selectInput("hand_choice","Take Hand Number",1:4)#,
                         #actionButton("request_view","Request Hand View"),
                         #selectInput("hand_view_choice","View Number",1:4)
        )
    ),
    
    #textOutput("active_player"),
    
    #actionButton("choose","Choose Hand")
    #selectInput("handSelection","Choose your hand",c(1:4,"Make a Selection"),selected="Make a Selection"),
    #tableOutput("openHands"),
    mainPanel(
        fluidRow(
            h3("The Table"),
            dataTableOutput("playedDisplay"),
            h3("Your Hand"),
            #dataTableOutput("handDisplay")
            uiOutput("handDisplay")
            
            
        )
    )
    
    
)

shinyApp(ui,server)
