###read the raw data
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
pacman::p_load(readxl,vioplot,ggplot2,tidyverse,stringr)


###data wrangling
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
mydata <- read_excel("Saw Data Collection-2.xlsx",sheet = 2,col_names = TRUE,skip = 1)
mydata <- mydata[-13]
mydata <- mydata[-13]
as.data.frame(mydata)
mydata <- dplyr::mutate(mydata,Saw = substr(`Saw ID`, 1,1))
#filter with hand saw
subdata <- filter(mydata,str_detect(Saw, "A|B|C|D|E|F|G|J|K|L|M"))


###EDA
# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
#Unrestrained vs restrained (hand)
datasplit <- split(mydata,mydata$`Restrained/Unrestrained`)
x1 <- datasplit[[1]]
x2 <- datasplit[[2]]
vioplot(x1$Minimum,x2$Minimum,names = "Restrained", "Unrestrained", col = "gold")
title("Violin Plots of Restrained and Unrestrained")

#scatter plot
p1 <- ggplot(subdata,aes(x = subdata$`Restrained/Unrestrained`,y = Minimum)) +geom_point(aes(color = subdata$`Saw`))+
  labs(title = "Minimum Width \nRestrained vs Unrestrained\nscatter plot",x="Hand Saw", y="Minimum Width") +
  theme(text = element_text(face = "bold",color = "steelblue"))+
  scale_color_discrete(name = "Hand Saw Type")
p1

#boxplot
#Upper bound: 75th Percentile; Lower bound: 25 Percentile; The "Notch": 95% confidence interval of the Median
p2 <- ggplot(subdata, aes(x = subdata$`Restrained/Unrestrained`, subdata$Minimum))+
  geom_boxplot(aes(fill =`Restrained/Unrestrained`),notch = T)+
  labs(title = "Minimum Width \nRestrained vs Unrestrained\nboxplot", x="Hand Saw", y="Minimum Width")+
  theme(text = element_text(face = "bold",color = "steelblue"))
p2

#Violin
#Violin plot shows the entire distribution of the data.
p3 <- ggplot(subdata,aes(x = subdata$`Restrained/Unrestrained`, subdata$Minimum))+
  geom_violin(aes(fill =`Restrained/Unrestrained`))+
  labs(title = "Minimum Width \nRestrained vs Unrestrained\nViolin", x="Hand Saw", y="Minimum Width")+
  theme(text = element_text(face = "bold",color = "steelblue"))
p3

