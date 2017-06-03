###Gather all card image files into a data.table for easy loading
require(data.table)
files = list.files("Cards Application",pattern="png|PNG",recursive=T)
files = files[grep("www",files,invert=T)]
files = files[grep("joker",files,invert = T)]
royals = files[grep("2.png",files)]
royals = royals[grep("ace",royals,invert=T)]

files = files[grep("king|jack|queen|spades2",files,invert=T)]
files = c(files,royals)

###Alternative - 
files = gsub("/","",str_extract(files,"/.{10,20}\\.png"))

#image_paths = paste('<img src="',files,'" height = "100" width = "72"></img>',sep="")
#saveRDS(image_paths,"Cards Application/data/image_paths.RDS")
saveRDS(files,"Cards Application/data/image_paths.RDS")
