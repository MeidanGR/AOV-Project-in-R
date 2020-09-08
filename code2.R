library(tidyverse)
library(ISLR)
library(ggplot2)
library(outliers)
library(agricolae)
library(car)

infodata <- read.csv("infoyeshuv.csv",sep=",", header=TRUE, stringsAsFactors = FALSE)
infodata$OnlyYear <-  substr(infodata$Month,1,4)  #Only Year new column
infodata$HasamotPerJob <- infodata$Placement.from.reference / infodata$Total.jobseekers ##הוספת עמודת אחוזי השמה

#Selecting values
totaldata <- infodata %>%
  group_by(Cbs.district, OnlyYear) %>%
  summarise(Hasamot_Mean = mean(HasamotPerJob)) %>%
  arrange(Cbs.district)

#ggPlot
ggplot(totaldata, aes(x=totaldata$Cbs.district, y=totaldata$Hasamot_Mean)) +
  geom_bar(stat = "identity", aes(x=totaldata$Cbs.district), color="gray95") + theme_minimal() +
  xlab("District")+ylab("% of Hasamot") +ggtitle("% of Hasamot per district: Average of 2010-2019")
#Creating table
datatable<-xtabs(totaldata$Hasamot_Mean ~ totaldata$Cbs.district + totaldata$OnlyYear)
names(dimnames(datatable)) <- c("District", "Year")
print.table(datatable)

#Data
mean(rowMeans(datatable))   #Yi.
mean(colMeans(datatable))   #Y.j
#Yi.=Y.j = Y..

c<-0
for(j in 1:10) {
c <- c+ (mean(datatable[,j] - mean(colMeans(datatable))))
}
c



si<-0
for(i in 1:7) { #b=7
  si <- si+ (mean(datatable[i,] - mean(rowMeans(datatable))))
}
si 
#Normal distribution tests
plot(anova.final,2, pch=10,col="coral")   ##Q-Q Plot

qqPlot(totaldata$Hasamot_Mean)

qqnorm(totaldata$Hasamot_Mean, pch = 1, frame = FALSE)
qqline(totaldata$Hasamot_Mean, col = "steelblue", lwd = 2)

ks.test(totaldata$Hasamot_Mean, pnorm)  #KS Test


#F test - Var equals
anova.ftest <- aov(totaldata$Hasamot_Mean~ totaldata$Cbs.district)
summary(anova.ftest)



plot(anova.final, pch=10,col="green3")

ks.test(totaldata$Hasamot_Mean, pnorm)


#Cochran test
varj.vector <- c(var(datatable[,1]), var(datatable[,2]), var(datatable[,3]), var(datatable[,4]),
                 var(datatable[,5]),var(datatable[,6]),var(datatable[,7]),var(datatable[,8]),
                 var(datatable[,9]),var(datatable[,10]))

#Logical testing
max(varj.vector)/sum(varj.vector) >  0.3308 # G0.05,10,6

#ANOVA
anova.final <- aov(formula = totaldata$Hasamot_Mean ~ totaldata$OnlyYear + 
                     totaldata$Cbs.district, data=totaldata)
summary(anova.final)

#Fcbs.district > F1 = F6,54 (0.001) =~4.4
#FonlyYear > F2 = F8,54 (0.001) =~ 4.0

#DFError = 54, MSError = .000046
#Duncan test

out<- duncan.test(anova.final, "totaldata$Cbs.district", console =TRUE)
plot(out,variation="IQR")

