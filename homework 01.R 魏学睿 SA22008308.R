#delete output
rm(list = ls())

#install package "ade4" and "tidyverse"
install.packages("ade4")
install.packages("tidyverse")
#load"tidyverse" and "ade4"
library("tidyverse")
library("ade4")
#read database "doubs" of package "ade4"
data(doubs,package="ade4")
#checking what the data looks like and the class of the data
class(doubs)
#extract env data
env<-doubs$env
class(env)
env
#extract rownames of "env"
rownames(env)
site<-rownames(env)
site
#Turning the row names into a column called site
env<-cbind(site,env)
env
#convert the data frame to a tibble  named it "env_tb"
env_tb<-tibble(env)
#check if succeed
env_tb
env_tb[[2]]

#3.1	One of the columns is dfs. It indicates the distance from sources. 
#Extract and remain the data of the dfs with more than 1000 km.
P1 <- function(env_tb){
  dfs<-env_tb[[2]]
  dfs1000<-which(dfs>1000)
}
#3.2 Only interested in these columns: site, dfs, slo, flo, pH, nit, oxy. 
#Select these columns for further analysis.
P2<- function(env_tb){
  env_tb1<-env_tb[,c(1,2,4,5,6,9,11)]
  return(env_tb1)
}
#3.3	Some column names are not intuitive. 
#Rename them as follows: dfs to distsour, slo to slope, flo to flowrate, nit to nitrogen, oxy to oxygen.
P3<- function(env_tb1){
  colnames(env_tb1)<-c("site","distsour","slope","flowrate","pH","nitrogen","oxygen")
  return(env_tb1)
}

#3.4	Order the data.
# Arrange the data first by slope in ascending order, and then by pH in descending order.
P4<- function(env_tb1){
  
  env_final<-env_tb1[order(env_tb1$slope),]
  env_final<-env_final[order(-env_final$pH),]
  return(env_final)
  
}

#Concatenating several steps with %>% pipe, and name the final variable as env_final.
env_final<-(((env_tb %>% P2()) %>% P3()) %>% P4())