###Create combined file for card definitions and images
require(data.table)
require(stringr)

deck = readRDS("Cards Application/data/deck.RDS")
image_paths = readRDS("Cards Application/data/image_paths.RDS")
#image_paths = paste('<img src="',files,'" height = "100" width = "72"></img>',sep="")

image_dt = data.table(path=image_paths,path_tag=paste('<img src="',image_paths,'" height = "100" width = "72"></img>',sep=""))
image_dt[,suit:="Spades"]
image_dt[grep("heart",path),suit:="Hearts"]
image_dt[grep("clubs",path),suit:="Clovers"]
image_dt[grep("diamonds",path),suit:="Diamonds"]

image_dt[,number:=toupper(str_extract(str_extract(path,"[0-9a-z]{1,5}_of"),"[0-9]{1,2}|[a-z]"))]
image_dt
deck = deck[image_dt,on=.(number,suit)]
deck[,number:=factor(number,levels = c(2:10,"J","Q","K","A"))]
deck[,suit:=factor(suit,levels=c("Diamonds","Clovers","Hearts","Spades"))]
setkey(deck,suit,number)
saveRDS(deck,"Cards Application/data/deck2.RDS")



