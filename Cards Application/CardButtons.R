###Toy Script for identifying methods to use in the cards app

###Create a function or object that builds actionbuttons connected to a piece of data
###Aim is to create an interactive hand display for the cards app

require(shiny)
require(stringr)
require(data.table)
require(ids)
require(DT)




cardButton = function(name = NULL,label=NULL,icon=NULL,width=NULL) {
    
    actionButton(name,label,icon,width)
}

cardButton2 = function(deck,height=NULL,width=NULL) {
    
    actionButton(inputId=deck$card,
                 tags$img(src=deck$path,
                 width=width,
                 height=height))
    
}


# server=shinyServer(function(input,output,session){
#     
#     
# })
# 
# 
# ui = fluidPage(
#     createButton("foo","foo2")
# )
# 
# shinyApp(ui,server)

