library(readr)
library(lessR)
library(dplyr)
library(ggplot2)
library(xtable)
library(multcompView)
library(car)
library(tidyverse)
library(ggpubr)
library(rstatix)

#version of packages
sessionInfo()

#setwd("C:/Users") D:\TU Dortmun WS\ics SS23\2
surveydata <- read.csv("D:/TU Dortmun WS/ics SS23/2/babies.csv", TRUE)
View(surveydata)
surveydata1 <-surveydata
View(surveydata1)

# count the missing values
sum(is.na(surveydata1)) #10 missing value in wt
colnames(surveydata1)

#impute missing value
surveydata1$wt[is.na(surveydata1$wt)] <- mean(round(surveydata1$wt), nsmall=0, na.rm = T)
#surveydata1$wt[is.na(surveydata1$wt)] = 999
View(surveydata1)
sum(is.na(surveydata1))
nrow(surveydata1) #1236

#change the name
surveydata1$smoke[which(surveydata1$smoke == "0")] <- "never"
surveydata1$smoke[which(surveydata1$smoke == "1")] <- "smokes now"
surveydata1$smoke[which(surveydata1$smoke == "2")] <- "until current pregnancy"
surveydata1$smoke[which(surveydata1$smoke == "3")] <- "once did, not now"
surveydata1$smoke[which(surveydata1$smoke == "9")] <- "unknown"

surveydata1 <- filter(surveydata1, smoke != "unknown")

View(surveydata1)

### Descriptive analysis
# summary of each category
table(surveydata1$smoke) #list of smoke
table1<- group_by(surveydata1, smoke) %>%
  summarise(
    count = n(),
    mean = mean(wt, na.rm = TRUE),
    var = var(wt, na.rm = TRUE),
    IQR = quantile(wt, 0.75)-quantile(wt, 0.25)
  )
table1<-table1[c(order(-table1$mean)),]
rownames(table1)<-NULL
table1<-xtable(table1)

View(table1)

#boxplot for each group
ggplot(surveydata1, aes(y=smoke, x=wt, fill=smoke))+
  geom_boxplot()+theme_bw()+theme(text = element_text(size=17))+theme(legend.position='none')+
  xlab("Babies weight") + ylab("Smoke")+
  stat_summary(fun.y = mean, geom = "point", shape=20, size=3, color="red", fill="red")+
  scale_y_discrete(limits=c("never", "smokes now", "until current pregnancy","once did, not now"))



#check equality variance
# Levene's test with one independent variable
leveneTest(wt ~ smoke, data = surveydata1)



#Check Normality
never <- surveydata1[surveydata1$smoke=="never",]
smokes_now <- surveydata1[surveydata1$smoke=="smokes now",]
current <- surveydata1[surveydata1$smoke=="until current pregnancy",]
once_did <- surveydata1[surveydata1$smoke=="once did, not now",]
#unknown <- surveydata1[surveydata1$smoke=="unknown",]


#check sample size
nrow(never)#544
nrow(smokes_now)#482
nrow(current)#99
nrow(once_did)#101
#nrow(unknown)#10

#View(never)

#never<- unlist(never)
surveydata2 <- as.numeric(surveydata1$wt)
surveydata3 <- as.numeric(surveydata1$smoke)
#View(surveydata1)


#saphiro test

#shapiro.test(never$wt)
#shapiro.test(smokes_now$wt)
#shapiro.test(current$wt)
#shapiro.test(once_did$wt)
#shapiro.test(unknown$wt)

#qq Plot 
qqnorm(y=never$wt,x=as.numeric(current$smoke), main=NULL, cex.axis=1.5, ann=FALSE)+
  title(xlab="Theoretical Quantiles", ylab = "Sample Quantiles", cex.lab=1.5)
qqline(y=never$wt,x=never$smoke, col=2)

qqnorm(y=smokes_now$wt,x=smokes_now$smoke, main=NULL, cex.axis=1.5, ann=FALSE)+
  title(xlab="Theoretical Quantiles", ylab = "Sample Quantiles", cex.lab=1.5)
qqline(y=smokes_now$wt,x=as.numeric(smokes_now$smoke), col=2)

qqnorm(y=current$wt,x=current$smoke, main=NULL, cex.axis=1.5, ann=FALSE)+
  title(xlab="Theoretical Quantiles", ylab = "Sample Quantiles", cex.lab=1.5)
qqline(y=current$wt,x=as.numeric(current$smoke), col=2)

qqnorm(y=once_did$wt,x=as.numeric(once_did$smoke), main=NULL, cex.axis=1.5, ann=FALSE)+
  title(xlab="Theoretical Quantiles", ylab = "Sample Quantiles", cex.lab=1.5)
qqline(y=once_did$wt,x=as.numeric(once_did$wt), col=2)

#qqnorm(y=unknown$wt,x=as.numeric(unknown$smoke), main=NULL, cex.axis=1.5, ann=FALSE)+
#  title(xlab="Theoretical Quantiles", ylab = "Sample Quantiles", cex.lab=1.5)
#qqline(y=unknown$wt,x=as.numeric(unknown$smoke), col=2)

#colnames(surveydata)

#we will do shapiro.test in order to test the normality of the distribution
#H0:Data is normally distributed
#H1:Data is not normally distribited

#One-way ANOVA



#category should be a factor
#surveydata1$smoke <- as.factor(surveydata1$smoke)
#checked the column now is a factor
#class(surveydata1)



anova_one_way <- aov(wt~smoke, data = surveydata1)
#anova_one_way <- aov(Time~as.factor(Category), data = surveydata1)
summary(anova_one_way)

print(typeof(anova_one_way))
table2<-summary(anova_one_way)
table2<-xtable(table2)

View(table2)

#post hoc none
table_n<-pairwise.t.test(surveydata1$wt, surveydata1$smoke, p.adjust.method = "none")
#table_n
table_n<-xtable(table_n$p.value, caption = table_n$method, digits = 6)
#n is none
View(table_n)


#post hoc with bonferonni
table_b<-pairwise.t.test(surveydata1$wt, surveydata1$smoke, p.adjust.method = "bonf")
#table_b
table_b<-xtable(table_b$p.value, caption = table_b$method, digits = 6)
#b with bonferroni
View(table_b)

#turkey , scipen = 300
tukey <- TukeyHSD(aov(wt ~ smoke, data=surveydata1), ordered=FALSE)$smoke %>%
  as.data.frame()

tukey$difference_yes <- tukey[,4] < 0.05
tukey <- rownames_to_column(tukey, var="smoke pair") %>% as_tibble() %>%
  mutate(across(where(is.numeric),~round(.x, digits=3)))

tukey


Turki.b <- TukeyHSD(anova_one_way, conf.level=0.95, digits=22)
Turki.b
