####Intro2R - Reading and Writing Data ####


####  Reading Files ----

# From Source

getwd() # See the current working directory
Data = read.csv("Data.csv", header=T)
Data = read.table("Data.txt", header=T)

# file.choose()
Data = read.csv(file.choose(), header=T)

# Using Rstudio 

#### Writing Files ----
 
write.csv(Data, "NewData.csv", row.names = F)
write.table(Data, "NewData.txt", row.names = F)

#### Save Workspace Image ----

save.imsge("Data.Rdata")
load("Data.Rdata")
