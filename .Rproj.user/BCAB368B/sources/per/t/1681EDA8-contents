library(readr)
drinking<-read.delim("./data/drinking_assignment.txt")


#EDA 
hist(drinking$packYears)
hist(drinking$glass_week)
barplot(table(drinking$sex)) 
table(drinking$sex)
colSums(is.na(drinking))



#both are right tailed. 
#the majority of participants had a smoking pack history of 0-40 packs
#The galls per week is more spread but majority still remain in the lower thresholds.
#There is a higher frequency of male participants 

library(doBy)
summaryBy(packYears ~ sex,data = drinking,
          FUN = c(mean,sd, median), na.rm = TRUE)

#we had to remove missing data
#females have a lower median(is it normal?), tighter variance than males

summaryBy(glass_week ~ sex,data = drinking,
          FUN = c(mean,sd,median))


drinks_female<-subset(drinking,sex == 'female')
drinks_male<-subset(drinking,sex == 'male')
colSums(is.na(drinke_female))
colSums(is.na(drinks_male))


pca_male <- prcomp(na.omit(drinks_male)[, c('glass_week', "packYears")], center = TRUE, scale. = TRUE)
biplot(pca_male)
summary(pca_male)
pca_female <- prcomp(drinks_female[, c('glass_week', "packYears")], center = TRUE, scale. = TRUE)

pca <- prcomp(drinking$packYears,glass_week)
#again women have a lower mean and median and tighter variance. Are men inflating the data? as there are more male participants will that drag the scores up

#more EDA
library(ggplot2)
library(hrbrthemes)
ggplot(drinking, aes(packYears, glass_week)) +
  geom_point() + geom_smooth(na.rm=TRUE) + labs(caption = 'smooth line') # can fit other types of distribution. Need linearity to proceed with regression

ggplot(drinking, aes(packYears, glass_week)) +
  geom_point() + geom_smooth(method = lm, na.rm=TRUE) + labs(caption = 'Linear model')

ggplot(drinking, aes(packYears, glass_week, color=sex)) +
  geom_point() + geom_smooth(method=lm) + labs(caption = 'linear model')


ggplot(drinking, aes(packYears, glass_week, color = sex)) +
  geom_point() + geom_smooth()+ labs(caption = 'smooth line')
#
#Q4

model1 <- glm(glass_week ~ sex, family = "poisson", data = drinking)
summary(model1)
confint(model1)

model2 <- glm(glass_week ~packYears, family = "poisson", data = drinking)
summary(model2)
confint(model2)
#natural log

1.556 *100
1.025* 100

ggplot(data=drinking, aes(x=packYears, y=glass_week, color=sex)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args=list(family=('poisson')))
#steady for men vs women

plot(fitted(na.omit(model)), residuals(na.omit(model)), main = "Residual Plot", 
     xlab = "Fitted Values", ylab = "Residuals")

#these check the monotancity of the data. It is held here as both the lines trend upwards consistently. 

drinking$sex<-factor(drinking$sex,
                     levels = c(0, 1),
                     labels=c("female", "male"))

library("ggpubr")
ggline(drinking, x = "sex", y = "packYears", 
       add = c("median","jitter"),
       order = c("female", "male"),
       ylab = "packYear", xlab = "sex") 

ggline(drinking, x = "sex", y = "glass_week", 
       add = c("mean","jitter"),
       order = c("female", "male"),
       ylab = "glass_week", xlab = "sex") 


#we can see that when separated by gender that male mean is higher on both occasions. So not separating the data influences the results. Will we see this in the regression? 

model1 <- glm(glass_week ~ packYears, family = "poisson", data = drinking)
summary(model1)

model2 <- glm(glass_week ~ packYears,family = 'gaussian', data = drinking)
summary(model1)

?glm

#Q5
model3 <- glm(glass_week ~ packYears*sex, family = "poisson", data = drinking)
summary(model3)
confint(model3)


ggplot(data=drinking, aes(x=packYears*sex, y=galss_week, color=sex)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args=list(family=('poisson')))
#???? 


pchisq(model4$deviance, df=model4$df.residual, lower.tail=FALSE)# very significant 
qchisq(0.95, df.residual(model4))
deviance(model3)
#the deviance is not smalles than the 0.95 critical level so it is not a good fit of the data, we saw this in the chi sq of the deviances. 

model5 <- glm(glass_week ~ packYears*sex, family = "quasipoisson", data = drinking)
summary(model5)
confint(model5)

pchisq(model5$deviance, df=model5$df.residual, lower.tail=FALSE)
qchisq(0.95, df.residual(model5))
deviance(model5)


ggplot(data=drinking, aes(x=packYears, y=glass_week, color=sex)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args=list(family=('poisson')))

8216.3/1307
library(AER)
AER::dispersiontest(model4)

pchisq(model4$deviance, df=model4$df.residual, lower.tail=FALSE)
qchisq(0.95, df.residual(model4))
deviance(model4)

8216.3/1392.2
