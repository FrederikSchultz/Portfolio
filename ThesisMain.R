setwd("C:/Users/Frede/Desktop/Speciale/ThesisDirectory")

install.packages("lme4")
install.packages("lmerTest")
install.packages("medflex")
install.packages("mediation")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("BoutrosLab.plotting.general")
install.packages("pracma")
install.packages("NSM3")
library(NSM3)
library(pracma)
library(dplyr)
library(lme4)
library(lmerTest)
library(splines)
library(medflex)
library(mediation)
library(ggplot2)
library(BoutrosLab.plotting.general)

data <- read.delim("db_effecttagging.txt", header = TRUE)

data$minute <- as.numeric(substr(data$Posixct, 15, 16))
data$hour <- as.numeric(substr(data$Posixct, 12, 13))
data$hourminute <- data$hour + data$minute/60
data$negDepth <- data$Depth
data$Depth = -data$Depth


asgedata <- subset(data, Ind == "Asgeir")
balddata <- subset(data, Ind == "Balder") 
eistdata <- subset(data, Ind == "Eistla")
freddata <- subset(data, Ind == "Frederik")
freydata <- subset(data, Ind == "Freya")
friddata <- subset(data, Ind == "Frida")
helgdata <- subset(data, Ind == "Helge") 
he18data <- subset(data, Ind == "Helge18")
kyrrdata <- subset(data, Ind == "Kyrri")
maradata <- subset(data, Ind == "Mara")
muttdata <- subset(data, Ind == "Mutti")
nemodata <- subset(data, Ind == "Nemo") 
siggdata <- subset(data, Ind == "Siggi")
thordata <- subset(data, Ind == "Thor")
thoradata <- subset(data, Ind == "Thora")
thordata[,'Buzz'][is.na(thordata[,"Buzz"])] <- 0



asgedata$time_since_tagging  <- (1:dim(asgedata)[1])/(60*60)
balddata$time_since_tagging  <- (1:dim(balddata)[1])/(60*60)
eistdata$time_since_tagging  <- (1:dim(eistdata)[1])/(60*60)
freddata$time_since_tagging  <- (1:dim(freddata)[1])/(60*60)
freydata$time_since_tagging  <- (1:dim(freydata)[1])/(60*60)
friddata$time_since_tagging  <- (1:dim(friddata)[1])/(60*60)
helgdata$time_since_tagging  <- (1:dim(helgdata)[1])/(60*60)
he18data$time_since_tagging  <- (1:dim(he18data)[1])/(60*60)
kyrrdata$time_since_tagging  <- (1:dim(kyrrdata)[1])/(60*60)
maradata$time_since_tagging  <- (1:dim(maradata)[1])/(60*60)
muttdata$time_since_tagging  <- (1:dim(muttdata)[1])/(60*60)
nemodata$time_since_tagging  <- (1:dim(nemodata)[1])/(60*60)
siggdata$time_since_tagging  <- (1:dim(siggdata)[1])/(60*60)
thordata$time_since_tagging  <- (1:dim(thordata)[1])/(60*60)
thoradata$time_since_tagging <- (1:dim(thoradata)[1])/(60*60)

asgedata$cBuzz<- cumsum(asgedata$Buzz)
balddata$cBuzz<- cumsum(balddata$Buzz)
eistdata$cBuzz<- cumsum(eistdata$Buzz)
freddata$cBuzz<- cumsum(freddata$Buzz)
freydata$cBuzz<- cumsum(freydata$Buzz)
friddata$cBuzz<- cumsum(friddata$Buzz)
helgdata$cBuzz<- cumsum(helgdata$Buzz)
he18data$cBuzz<- cumsum(he18data$Buzz)
kyrrdata$cBuzz<- cumsum(kyrrdata$Buzz)
maradata$cBuzz<- cumsum(maradata$Buzz)
muttdata$cBuzz<- cumsum(muttdata$Buzz)
nemodata$cBuzz<- cumsum(nemodata$Buzz)
siggdata$cBuzz<- cumsum(siggdata$Buzz)
thordata$cBuzz<- cumsum(thordata$Buzz)
thoradata$cBuzz<-cumsum(thoradata$Buzz)



asgedata$htime  <- "short"
balddata$htime  <- "long"
eistdata$htime  <- "long"
freddata$htime  <- "short"
freydata$htime  <- "medium"
friddata$htime  <- "short"
helgdata$htime  <- "long"
he18data$htime  <- "short"
kyrrdata$htime  <- "long"
maradata$htime  <- "medium"
muttdata$htime  <- "long"
nemodata$htime  <- "medium"
siggdata$htime  <- "short"
thordata$htime  <- "medium"
thoradata$htime <- "medium"


asgedata$length <- "med"
balddata$length <- "little"
eistdata$length <- "little"
freddata$length <- "little"
freydata$length <- "med"
friddata$length <- "little"
helgdata$length <- "large"
he18data$length <- "large"
kyrrdata$length <- "med"
maradata$length <- "large"
muttdata$length <- "large"
nemodata$length <- "med"
siggdata$length <- "large"
thordata$length <- "med"
thoradata$length <-"little"



asgedata$gender <- "M"
balddata$gender <- "F"
eistdata$gender <- "M"
freddata$gender <- "M"
freydata$gender <- "F"
friddata$gender <- "F"
helgdata$gender <- "M"
he18data$gender <- "M"
kyrrdata$gender <- "M"
maradata$gender <- "F"
muttdata$gender <- "F"
nemodata$gender <- "M"
siggdata$gender <- "M"
thordata$gender <- "M"
thoradata$gender <- "F"



data1 <- rbind(asgedata ,
               balddata ,
               eistdata ,
               freddata ,
               freydata ,
               friddata ,
               helgdata ,
               he18data ,
               kyrrdata ,
               maradata ,
               muttdata ,
               nemodata ,
               siggdata ,
               thordata ,
               thoradata)

data1$Ind <- as.factor(data1$Ind)
data1$gender <- as.factor(data1$gender)
data1$length <- as.factor(data1$length)
data1$htime <- as.factor(data1$htime)




data2 <- subset(data1, time_since_tagging >= 24)


m0 <- glmer(Buzz ~ ns(Depth, 3) + (1|Ind), family = poisson, data = data2)
#m1 <- glmer(Buzz ~ I(exp(-time_since_tagging)) + ns(Depth, 3) + (1| Ind), family = poisson, data = data1)
#m2 <- glmer(Buzz ~ I(1/time_since_tagging^(1/2)) + ns(Depth, 3)  + (1|Ind), family = poisson, data = data1)
#m3 <- glmer(Buzz ~ I(1/time_since_tagging) + ns(Depth, 3)  + (1|Ind), family = poisson, data = data1)
#m4 <- glmer(Buzz ~ I(1/time_since_tagging^2) + ns(Depth, 3)  + (1|Ind), family = poisson, data = data1)
#

#mm1 <- glmer(Buzz ~ I(exp(-time_since_tagging)) + (1| Ind), family = poisson, data = data1)
#mm2 <- glmer(Buzz ~ I(1/time_since_tagging) + (1|Ind), family = poisson, data = data1)
#mm3 <- glmer(Buzz ~ I(1/time_since_tagging^2) + (1|Ind), family = poisson, data = data1)
#mm4 <- glmer(Buzz ~ I(1/time_since_tagging^(1/2)) + (1|Ind), family = poisson, data = data1)
#
#save(mm1, file = "mm1")
#save(mm2, file = "mm2")
#save(mm3, file = "mm3")
#save(mm4, file = "mm4")




#save(m0, file = "null.model")
#save(m1, file = "exp.model")
#save(m2, file = "frachalf.model")
#save(m3, file = "frac.model")
#save(m4, file = "fractwo.model")



#save(fit.mediator, file = "med.fit")
load("null.model")
load("exp.model")
load("frac.model")
load("frachalf.model")
load("fractwo.model")

load("mm1")
load("mm2")
load("mm3")
load("mm4")
#summary(m1)
#summary(mm1)
#
#summary(m2)
#summary(mm2)
#
#summary(m3)
#summary(mm3)
#
#summary(m4)
#summary(mm4)



summary(m4)
Ind <- c("Asgeir", "Balder", "Eistla", "Frederik", "Freya", "Frida", "Helge",
         "Helge18","Kyrri", "Mutti", "Nemo", "Siggi", "Thor", "Thora")
length()
Ind <- levels(data1$Ind)
summary(m0)
Ind
plotdata <- NULL
plotdatatemp <- NULL

for (i in 1:length(Ind)){
predict0 <- predict(m0, data.frame(posDepth = 500, Ind = Ind[i]), type = "response")*60
predict1 <- predict(m1, data.frame(time_since_tagging = seq(1,50, 1), Depth = 500, Ind = Ind[i]), type = "response")*60
predict2 <- predict(m2, data.frame(time_since_tagging = seq(1,50, 1), Depth = 500, Ind = Ind[i]), type = "response")*60
predict3 <- predict(m3, data.frame(time_since_tagging = seq(1,50, 1), Depth = 500, Ind = Ind[i]), type = "response")*60
predict4 <- predict(m4, data.frame(time_since_tagging = seq(1,50, 1), Depth = 500, Ind = Ind[i]), type = "response")*60


plotdatatemp <- data.frame(point = rep(predict0, 200), time_since_tagging = rep(seq(1, 50, 1), 4),
                                    preds = c(predict1, predict2, predict3, 
                                    predict4), Whale = rep(Ind[i], 4), model = c(rep("m1", 50),
                                                                                 rep("m2", 50),
                                                                                 rep("m3", 50),
                                                                                 rep("m4", 50)))
plotdata <- rbind.data.frame(plotdata, plotdatatemp)

}

p1 <- ggplot(plotdata, aes(x = time_since_tagging, y = preds)) +
                       geom_line(aes(col = Whale)) + 
                       facet_wrap(~model, ncol = 1) + 
                       geom_point(aes(x = 51, y = point, col = Whale), shape = "X") +
                       labs(x = "Hours since tagging", y = "Buzzes")
p1

newdata <-data.frame(time_since_tagging = seq(1,50, 1), Depth = 500)
predictall0 <- predict(m0, newdata, re.form = ~0, type = "response")*60
predictall1 <- predict(m1, newdata, re.form = ~0, type = "response")*60
predictall2 <- predict(m2, newdata, re.form = ~0, type = "response")*60
predictall3 <- predict(m3, newdata, re.form = ~0, type = "response")*60
predictall4 <- predict(m4, newdata, re.form = ~0, type = "response")*60

plotdataall <- data.frame(point = rep(predictall0, 200), time_since_tagging = rep(seq(1, 50, 1), 4), 
                       preds = c(predictall1, predictall2, predictall3, predictall4),model = c(rep("m1", 50),
                                                                                               rep("m2", 50),
                                                                                               rep("m3", 50),
                                                                                               rep("m4", 50)))






ecdf

sqrt(length(z4asge))*0.15465


p2 <- ggplot(plotdataall, aes(x = time_since_tagging, y = preds)) +
                        geom_line(aes(col = model)) +
                        geom_point(aes(x = 51, y = point )) +
                        labs(x = "Hours since tagging", y = "Buzzes")
p2

#anova(m1, m2, m3, m4)
ks.test(z4asge, "punif")

summary(mm4)
load("mm1")
load("mm2")
load("mm3")
load("mm4")

(l1 <- summary(m1)$coefficients[2] )
(s1 <- summary(mm1)$coefficients[2])
(s1-l1)/s1

(l2 <- summary(m2)$coefficients[2])
(s2 <- summary(mm4)$coefficients[2])
(s2-l2)/s2

(l3 <- summary(m3)$coefficients [2])
(s3 <- summary(mm2)$coefficients[2])
(s3-l3)/s3

(l4 <- summary(m4)$coefficients [2])
(s4 <- summary(mm3)$coefficients[2])
(s4-l4)/s4




med1 <-l1/s1
med2 <-l2/s2
med3 <-l3/s3
med4 <-l4/s4
 

med1

newdata <-data.frame(time_since_tagging = seq(1,50, 1), Depth = 500)
predictall0 <- predict(m0, newdata, re.form = ~0, type = "response")*60
predictall1 <- predict(m1, newdata, re.form = ~0, type = "response")*60
predictall2 <- predict(m2, newdata, re.form = ~0, type = "response")*60
predictall3 <- predict(m3, newdata, re.form = ~0, type = "response")*60
predictall4 <- predict(m4, newdata, re.form = ~0, type = "response")*60






allpredm1 <- predict(m1, type = "response")
allpredm1 <- data.frame(allpredm1)

allpredm2 <- predict(m2, type = "response")
allpredm2 <- data.frame(allpredm2)

allpredm3 <- predict(m3, type = "response")
allpredm3 <- data.frame(allpredm3)

allpredm4 <- predict(m4, type = "response")
allpredm4 <- data.frame(allpredm4)




allpredasge1<- allpredm1[1:200389,]
allpredbald1<- allpredm1[200390:805190,]
allpredeist1<- allpredm1[805191:1174279,]
allpredfred1<- allpredm1[1174280:1277493,]
allpredfrey1<- allpredm1[1277494:1523079,]
allpredfrid1<- allpredm1[1523080:1820681,]
allpredhelg1<- allpredm1[1820682:2104900,]
allpredhe181<- allpredm1[2104901:2308004,]
allpredkyrr1<- allpredm1[2308005:2533489,]
allpredmara1<- allpredm1[2533490:2569453,]
allpredmutt1<- allpredm1[2569454:2935262,]
allprednemo1<- allpredm1[2935263:3055196,]
allpredsigg1<- allpredm1[3055197:3173950,]
allpredthor1<- allpredm1[3173951:3456474,]
allpredthora1 <- allpredm1[3456475:3826480,]


allpredasge2<- allpredm2[1:200389,]
allpredbald2<- allpredm2[200390:805190,]
allpredeist2<- allpredm2[805191:1174279,]
allpredfred2<- allpredm2[1174280:1277493,]
allpredfrey2<- allpredm2[1277494:1523079,]
allpredfrid2<- allpredm2[1523080:1820681,]
allpredhelg2<- allpredm2[1820682:2104900,]
allpredhe182<- allpredm2[2104901:2308004,]
allpredkyrr2<- allpredm2[2308005:2533489,]
allpredmara2<- allpredm2[2533490:2569453,]
allpredmutt2<- allpredm2[2569454:2935262,]
allprednemo2<- allpredm2[2935263:3055196,]
allpredsigg2<- allpredm2[3055197:3173950,]
allpredthor2<- allpredm2[3173951:3456474,]
allpredthora2 <- allpredm2[3456475:3826480,]


allpredasge3<- allpredm3[1:200389,]
allpredbald3<- allpredm3[200390:805190,]
allpredeist3<- allpredm3[805191:1174279,]
allpredfred3<- allpredm3[1174280:1277493,]
allpredfrey3<- allpredm3[1277494:1523079,]
allpredfrid3<- allpredm3[1523080:1820681,]
allpredhelg3<- allpredm3[1820682:2104900,]
allpredhe183<- allpredm3[2104901:2308004,]
allpredkyrr3<- allpredm3[2308005:2533489,]
allpredmara3<- allpredm3[2533490:2569453,]
allpredmutt3<- allpredm3[2569454:2935262,]
allprednemo3<- allpredm3[2935263:3055196,]
allpredsigg3<- allpredm3[3055197:3173950,]
allpredthor3<- allpredm3[3173951:3456474,]
allpredthora3 <- allpredm3[3456475:3826480,]


allpredasge4<- allpredm4[1:200389,]
allpredbald4<- allpredm4[200390:805190,]
allpredeist4<- allpredm4[805191:1174279,]
allpredfred4<- allpredm4[1174280:1277493,]
allpredfrey4<- allpredm4[1277494:1523079,]
allpredfrid4<- allpredm4[1523080:1820681,]
allpredhelg4<- allpredm4[1820682:2104900,]
allpredhe184<- allpredm4[2104901:2308004,]
allpredkyrr4<- allpredm4[2308005:2533489,]
allpredmara4<- allpredm4[2533490:2569453,]
allpredmutt4<- allpredm4[2569454:2935262,]
allprednemo4<- allpredm4[2935263:3055196,]
allpredsigg4<- allpredm4[3055197:3173950,]
allpredthor4<- allpredm4[3173951:3456474,]
allpredthora4 <- allpredm4[3456475:3826480,]


rescale <- function(data, preds){
  res <- NULL
  a <- NULL
  for (i in 1:dim(data)[1]){
    if (data$Buzz[i] == 1) {a <- append(a, i)}
  }
  for (j in 1:(length(a)-1)){
    temp <- NULL
      
    for (k in (a[j]:a[j+1])){
      predict1 <- preds[k]
      predict2 <- preds[k+1]
      temp <- append(temp, (predict1 + predict2)/2)
    }
    
    res[j] <- sum(temp)
  }
  return(1-exp(-res))
}


z1asge <- rescale(asgedata , allpredasge1) 
z1bald <- rescale(balddata , allpredbald1) 
z1eist <- rescale(eistdata , allpredeist1) 
z1fred <- rescale(freddata , allpredfred1) 
z1frey <- rescale(freydata , allpredfrey1) 
z1frid <- rescale(friddata , allpredfrid1) 
z1helg <- rescale(helgdata , allpredhelg1) 
z1he18 <- rescale(he18data , allpredhe181) 
z1kyrr <- rescale(kyrrdata , allpredkyrr1) 
z1mara <- rescale(maradata , allpredmara1) 
z1mutt <- rescale(muttdata , allpredmutt1) 
z1nemo <- rescale(nemodata , allprednemo1) 
z1sigg <- rescale(siggdata , allpredsigg1) 
z1thor <- rescale(thordata , allpredthor1) 
z1thora <- rescale(thoradata, allpredthora1)


z2asge <- rescale(asgedata , allpredasge2) 
z2bald <- rescale(balddata , allpredbald2) 
z2eist <- rescale(eistdata , allpredeist2) 
z2fred <- rescale(freddata , allpredfred2) 
z2frey <- rescale(freydata , allpredfrey2) 
z2frid <- rescale(friddata , allpredfrid2) 
z2helg <- rescale(helgdata , allpredhelg2) 
z2he18 <- rescale(he18data , allpredhe182) 
z2kyrr <- rescale(kyrrdata , allpredkyrr2) 
z2mara <- rescale(maradata , allpredmara2) 
z2mutt <- rescale(muttdata , allpredmutt2) 
z2nemo <- rescale(nemodata , allprednemo2) 
z2sigg <- rescale(siggdata , allpredsigg2) 
z2thor <- rescale(thordata , allpredthor2) 
z2thora <- rescale(thoradata, allpredthora2)

z3asge <- rescale(asgedata , allpredasge3) 
z3bald <- rescale(balddata , allpredbald3) 
z3eist <- rescale(eistdata , allpredeist3) 
z3fred <- rescale(freddata , allpredfred3) 
z3frey <- rescale(freydata , allpredfrey3) 
z3frid <- rescale(friddata , allpredfrid3) 
z3helg <- rescale(helgdata , allpredhelg3) 
z3he18 <- rescale(he18data , allpredhe183) 
z3kyrr <- rescale(kyrrdata , allpredkyrr3) 
z3mara <- rescale(maradata , allpredmara3) 
z3mutt <- rescale(muttdata , allpredmutt3) 
z3nemo <- rescale(nemodata , allprednemo3) 
z3sigg <- rescale(siggdata , allpredsigg3) 
z3thor <- rescale(thordata , allpredthor3) 
z3thora <- rescale(thoradata, allpredthora3)

z4asge <- rescale(asgedata , allpredasge4) 
z4bald <- rescale(balddata , allpredbald4) 
z4eist <- rescale(eistdata , allpredeist4) 
z4fred <- rescale(freddata , allpredfred4) 
z4frey <- rescale(freydata , allpredfrey4) 
z4frid <- rescale(friddata , allpredfrid4) 
z4helg <- rescale(helgdata , allpredhelg4) 
z4he18 <- rescale(he18data , allpredhe184) 
z4kyrr <- rescale(kyrrdata , allpredkyrr4) 
z4mara <- rescale(maradata , allpredmara4) 
z4mutt <- rescale(muttdata , allpredmutt4) 
z4nemo <- rescale(nemodata , allprednemo4) 
z4sigg <- rescale(siggdata , allpredsigg4) 
z4thor <- rescale(thordata , allpredthor4) 
z4thora <- rescale(thoradata, allpredthora4)



dfz1asge <- data.frame( u = ecdf.ks.CI(z1asge )$upper,l = ecdf.ks.CI(z1asge )$lower, z = unique(z1asge ) ,  s = sort(unique(z1asge )), model = "m1", whale = "Asgeir")
dfz1bald <- data.frame( u = ecdf.ks.CI(z1bald )$upper,l = ecdf.ks.CI(z1bald )$lower, z = unique(z1bald ) ,  s = sort(unique(z1bald )), model = "m1", whale = "Balder")
dfz1eist <- data.frame( u = ecdf.ks.CI(z1eist )$upper,l = ecdf.ks.CI(z1eist )$lower, z = unique(z1eist ) ,  s = sort(unique(z1eist )), model = "m1", whale = "Eistla")
dfz1fred <- data.frame( u = ecdf.ks.CI(z1fred )$upper,l = ecdf.ks.CI(z1fred )$lower, z = unique(z1fred ) ,  s = sort(unique(z1fred )), model = "m1", whale = "Frederik")
dfz1frey <- data.frame( u = ecdf.ks.CI(z1frey )$upper,l = ecdf.ks.CI(z1frey )$lower, z = unique(z1frey ) ,  s = sort(unique(z1frey )), model = "m1", whale = "Freya")
dfz1frid <- data.frame( u = ecdf.ks.CI(z1frid )$upper,l = ecdf.ks.CI(z1frid )$lower, z = unique(z1frid ) ,  s = sort(unique(z1frid )), model = "m1", whale = "Frida")
dfz1helg <- data.frame( u = ecdf.ks.CI(z1helg )$upper,l = ecdf.ks.CI(z1helg )$lower, z = unique(z1helg ) ,  s = sort(unique(z1helg )), model = "m1", whale = "Helge")
dfz1he18 <- data.frame( u = ecdf.ks.CI(z1he18 )$upper,l = ecdf.ks.CI(z1he18 )$lower, z = unique(z1he18 ) ,  s = sort(unique(z1he18 )), model = "m1", whale = "Helge18")
dfz1kyrr <- data.frame( u = ecdf.ks.CI(z1kyrr )$upper,l = ecdf.ks.CI(z1kyrr )$lower, z = unique(z1kyrr ) ,  s = sort(unique(z1kyrr )), model = "m1", whale = "Kyrri")
dfz1mara <- data.frame( u = ecdf.ks.CI(z1mara )$upper,l = ecdf.ks.CI(z1mara )$lower, z = unique(z1mara ) ,  s = sort(unique(z1mara )), model = "m1", whale = "Mara")
dfz1mutt <- data.frame( u = ecdf.ks.CI(z1mutt )$upper,l = ecdf.ks.CI(z1mutt )$lower, z = unique(z1mutt ) ,  s = sort(unique(z1mutt )), model = "m1", whale = "Mutti")
dfz1nemo <- data.frame( u = ecdf.ks.CI(z1nemo )$upper,l = ecdf.ks.CI(z1nemo )$lower, z = unique(z1nemo ) ,  s = sort(unique(z1nemo )), model = "m1", whale = "Nemo")
dfz1sigg <- data.frame( u = ecdf.ks.CI(z1sigg )$upper,l = ecdf.ks.CI(z1sigg )$lower, z = unique(z1sigg ) ,  s = sort(unique(z1sigg )), model = "m1", whale = "Siggi")
dfz1thor <- data.frame( u = ecdf.ks.CI(z1thor )$upper,l = ecdf.ks.CI(z1thor )$lower, z = unique(z1thor ) ,  s = sort(unique(z1thor )), model = "m1", whale = "Thor")
dfz1thora<- data.frame( u = ecdf.ks.CI(z1thora)$upper,l = ecdf.ks.CI(z1thora)$lower, z = unique(z1thora) ,  s = sort(unique(z1thora)), model = "m1", whale = "Thora")

dfz2asge <- data.frame( u = ecdf.ks.CI(z2asge )$upper,l = ecdf.ks.CI(z2asge )$lower, z = unique(z2asge ) ,  s = sort(unique(z2asge )), model = "m2", whale = "Asgeir")
dfz2bald <- data.frame( u = ecdf.ks.CI(z2bald )$upper,l = ecdf.ks.CI(z2bald )$lower, z = unique(z2bald ) ,  s = sort(unique(z2bald )), model = "m2", whale = "Balder")
dfz2eist <- data.frame( u = ecdf.ks.CI(z2eist )$upper,l = ecdf.ks.CI(z2eist )$lower, z = unique(z2eist ) ,  s = sort(unique(z2eist )), model = "m2", whale = "Eistla")
dfz2fred <- data.frame( u = ecdf.ks.CI(z2fred )$upper,l = ecdf.ks.CI(z2fred )$lower, z = unique(z2fred ) ,  s = sort(unique(z2fred )), model = "m2", whale = "Frederik")
dfz2frey <- data.frame( u = ecdf.ks.CI(z2frey )$upper,l = ecdf.ks.CI(z2frey )$lower, z = unique(z2frey ) ,  s = sort(unique(z2frey )), model = "m2", whale = "Freya")
dfz2frid <- data.frame( u = ecdf.ks.CI(z2frid )$upper,l = ecdf.ks.CI(z2frid )$lower, z = unique(z2frid ) ,  s = sort(unique(z2frid )), model = "m2", whale = "Frida")
dfz2helg <- data.frame( u = ecdf.ks.CI(z2helg )$upper,l = ecdf.ks.CI(z2helg )$lower, z = unique(z2helg ) ,  s = sort(unique(z2helg )), model = "m2", whale = "Helge")
dfz2he18 <- data.frame( u = ecdf.ks.CI(z2he18 )$upper,l = ecdf.ks.CI(z2he18 )$lower, z = unique(z2he18 ) ,  s = sort(unique(z2he18 )), model = "m2", whale = "Helge18")
dfz2kyrr <- data.frame( u = ecdf.ks.CI(z2kyrr )$upper,l = ecdf.ks.CI(z2kyrr )$lower, z = unique(z2kyrr ) ,  s = sort(unique(z2kyrr )), model = "m2", whale = "Kyrri")
dfz2mara <- data.frame( u = ecdf.ks.CI(z2mara )$upper,l = ecdf.ks.CI(z2mara )$lower, z = unique(z2mara ) ,  s = sort(unique(z2mara )), model = "m2", whale = "Mara")
dfz2mutt <- data.frame( u = ecdf.ks.CI(z2mutt )$upper,l = ecdf.ks.CI(z2mutt )$lower, z = unique(z2mutt ) ,  s = sort(unique(z2mutt )), model = "m2", whale = "Mutti")
dfz2nemo <- data.frame( u = ecdf.ks.CI(z2nemo )$upper,l = ecdf.ks.CI(z2nemo )$lower, z = unique(z2nemo ) ,  s = sort(unique(z2nemo )), model = "m2", whale = "Nemo")
dfz2sigg <- data.frame( u = ecdf.ks.CI(z2sigg )$upper,l = ecdf.ks.CI(z2sigg )$lower, z = unique(z2sigg ) ,  s = sort(unique(z2sigg )), model = "m2", whale = "Siggi")
dfz2thor <- data.frame( u = ecdf.ks.CI(z2thor )$upper,l = ecdf.ks.CI(z2thor )$lower, z = unique(z2thor ) ,  s = sort(unique(z2thor )), model = "m2", whale = "Thor")
dfz2thora<- data.frame( u = ecdf.ks.CI(z2thora)$upper,l = ecdf.ks.CI(z2thora)$lower, z = unique(z2thora) ,  s = sort(unique(z2thora)), model = "m2", whale = "Thora")

dfz3asge <- data.frame( u = ecdf.ks.CI(z3asge )$upper,l = ecdf.ks.CI(z3asge )$lower, z = unique(z3asge ) ,  s = sort(unique(z3asge )), model = "m3", whale = "Asgeir")
dfz3bald <- data.frame( u = ecdf.ks.CI(z3bald )$upper,l = ecdf.ks.CI(z3bald )$lower, z = unique(z3bald ) ,  s = sort(unique(z3bald )), model = "m3", whale = "Balder")
dfz3eist <- data.frame( u = ecdf.ks.CI(z3eist )$upper,l = ecdf.ks.CI(z3eist )$lower, z = unique(z3eist ) ,  s = sort(unique(z3eist )), model = "m3", whale = "Eistla")
dfz3fred <- data.frame( u = ecdf.ks.CI(z3fred )$upper,l = ecdf.ks.CI(z3fred )$lower, z = unique(z3fred ) ,  s = sort(unique(z3fred )), model = "m3", whale = "Frederik")
dfz3frey <- data.frame( u = ecdf.ks.CI(z3frey )$upper,l = ecdf.ks.CI(z3frey )$lower, z = unique(z3frey ) ,  s = sort(unique(z3frey )), model = "m3", whale = "Freya")
dfz3frid <- data.frame( u = ecdf.ks.CI(z3frid )$upper,l = ecdf.ks.CI(z3frid )$lower, z = unique(z3frid ) ,  s = sort(unique(z3frid )), model = "m3", whale = "Frida")
dfz3helg <- data.frame( u = ecdf.ks.CI(z3helg )$upper,l = ecdf.ks.CI(z3helg )$lower, z = unique(z3helg ) ,  s = sort(unique(z3helg )), model = "m3", whale = "Helge")
dfz3he18 <- data.frame( u = ecdf.ks.CI(z3he18 )$upper,l = ecdf.ks.CI(z3he18 )$lower, z = unique(z3he18 ) ,  s = sort(unique(z3he18 )), model = "m3", whale = "Helge18")
dfz3kyrr <- data.frame( u = ecdf.ks.CI(z3kyrr )$upper,l = ecdf.ks.CI(z3kyrr )$lower, z = unique(z3kyrr ) ,  s = sort(unique(z3kyrr )), model = "m3", whale = "Kyrri")
dfz3mara <- data.frame( u = ecdf.ks.CI(z3mara )$upper,l = ecdf.ks.CI(z3mara )$lower, z = unique(z3mara ) ,  s = sort(unique(z3mara )), model = "m3", whale = "Mara")
dfz3mutt <- data.frame( u = ecdf.ks.CI(z3mutt )$upper,l = ecdf.ks.CI(z3mutt )$lower, z = unique(z3mutt ) ,  s = sort(unique(z3mutt )), model = "m3", whale = "Mutti")
dfz3nemo <- data.frame( u = ecdf.ks.CI(z3nemo )$upper,l = ecdf.ks.CI(z3nemo )$lower, z = unique(z3nemo ) ,  s = sort(unique(z3nemo )), model = "m3", whale = "Nemo")
dfz3sigg <- data.frame( u = ecdf.ks.CI(z3sigg )$upper,l = ecdf.ks.CI(z3sigg )$lower, z = unique(z3sigg ) ,  s = sort(unique(z3sigg )), model = "m3", whale = "Siggi")
dfz3thor <- data.frame( u = ecdf.ks.CI(z3thor )$upper,l = ecdf.ks.CI(z3thor )$lower, z = unique(z3thor ) ,  s = sort(unique(z3thor )), model = "m3", whale = "Thor")
dfz3thora<- data.frame( u = ecdf.ks.CI(z3thora)$upper,l = ecdf.ks.CI(z3thora)$lower, z = unique(z3thora) ,  s = sort(unique(z3thora)), model = "m3", whale = "Thora")

dfz4asge <- data.frame(u = ecdf.ks.CI(z4asge )$upper,l = ecdf.ks.CI(z4asge )$lower,  z = unique(z4asge ) ,  s = sort(unique(z4asge )), model = "m4", whale = "Asgeir")
dfz4bald <- data.frame(u = ecdf.ks.CI(z4bald )$upper,l = ecdf.ks.CI(z4bald )$lower,  z = unique(z4bald ) ,  s = sort(unique(z4bald )), model = "m4", whale = "Balder")
dfz4eist <- data.frame(u = ecdf.ks.CI(z4eist )$upper,l = ecdf.ks.CI(z4eist )$lower,  z = unique(z4eist ) ,  s = sort(unique(z4eist )), model = "m4", whale = "Eistla")
dfz4fred <- data.frame(u = ecdf.ks.CI(z4fred )$upper,l = ecdf.ks.CI(z4fred )$lower,  z = unique(z4fred ) ,  s = sort(unique(z4fred )), model = "m4", whale = "Frederik")
dfz4frey <- data.frame(u = ecdf.ks.CI(z4frey )$upper,l = ecdf.ks.CI(z4frey )$lower,  z = unique(z4frey ) ,  s = sort(unique(z4frey )), model = "m4", whale = "Freya")
dfz4frid <- data.frame(u = ecdf.ks.CI(z4frid )$upper,l = ecdf.ks.CI(z4frid )$lower,  z = unique(z4frid ) ,  s = sort(unique(z4frid )), model = "m4", whale = "Frida")
dfz4helg <- data.frame(u = ecdf.ks.CI(z4helg )$upper,l = ecdf.ks.CI(z4helg )$lower,  z = unique(z4helg ) ,  s = sort(unique(z4helg )), model = "m4", whale = "Helge")
dfz4he18 <- data.frame(u = ecdf.ks.CI(z4he18 )$upper,l = ecdf.ks.CI(z4he18 )$lower,  z = unique(z4he18 ) ,  s = sort(unique(z4he18 )), model = "m4", whale = "Helge18")
dfz4kyrr <- data.frame(u = ecdf.ks.CI(z4kyrr )$upper,l = ecdf.ks.CI(z4kyrr )$lower,  z = unique(z4kyrr ) ,  s = sort(unique(z4kyrr )), model = "m4", whale = "Kyrri")
dfz4mara <- data.frame(u = ecdf.ks.CI(z4mara )$upper,l = ecdf.ks.CI(z4mara )$lower,  z = unique(z4mara ) ,  s = sort(unique(z4mara )), model = "m4", whale = "Mara")
dfz4mutt <- data.frame(u = ecdf.ks.CI(z4mutt )$upper,l = ecdf.ks.CI(z4mutt )$lower,  z = unique(z4mutt ) ,  s = sort(unique(z4mutt )), model = "m4", whale = "Mutti")
dfz4nemo <- data.frame(u = ecdf.ks.CI(z4nemo )$upper,l = ecdf.ks.CI(z4nemo )$lower,  z = unique(z4nemo ) ,  s = sort(unique(z4nemo )), model = "m4", whale = "Nemo")
dfz4sigg <- data.frame(u = ecdf.ks.CI(z4sigg )$upper,l = ecdf.ks.CI(z4sigg )$lower,  z = unique(z4sigg ) ,  s = sort(unique(z4sigg )), model = "m4", whale = "Siggi")
dfz4thor <- data.frame(u = ecdf.ks.CI(z4thor )$upper,l = ecdf.ks.CI(z4thor )$lower,  z = unique(z4thor ) ,  s = sort(unique(z4thor )), model = "m4", whale = "Thor")
dfz4thora<- data.frame(u = ecdf.ks.CI(z4thora)$upper,l = ecdf.ks.CI(z4thora)$lower,  z = unique(z4thora) ,  s = sort(unique(z4thora)), model = "m4", whale = "Thora")



zpd1 <- rbind.data.frame(dfz1asge,
                         dfz1bald,
                         dfz1eist,
                         dfz1fred,
                         dfz1frey,
                         dfz1frid,
                         dfz1helg,
                         dfz1he18,
                         dfz1kyrr,
                         dfz1mara,
                         dfz1mutt,
                         dfz1nemo,
                         dfz1sigg,
                         dfz1thor,
                         dfz1thora)

zpd2 <- rbind.data.frame(dfz2asge,
                         dfz2bald,
                         dfz2eist,
                         dfz2fred,
                         dfz2frey,
                         dfz2frid,
                         dfz2helg,
                         dfz2he18,
                         dfz2kyrr,
                         dfz2mara,
                         dfz2mutt,
                         dfz2nemo,
                         dfz2sigg,
                         dfz2thor,
                         dfz2thora)

zpd3 <- rbind.data.frame(dfz3asge,
                         dfz3bald,
                         dfz3eist,
                         dfz3fred,
                         dfz3frey,
                         dfz3frid,
                         dfz3helg,
                         dfz3he18,
                         dfz3kyrr,
                         dfz3mara,
                         dfz3mutt,
                         dfz3nemo,
                         dfz3sigg,
                         dfz3thor,
                         dfz3thora)

zpd4 <- rbind.data.frame(dfz4asge,
                         dfz4bald,
                         dfz4eist,
                         dfz4fred,
                         dfz4frey,
                         dfz4frid,
                         dfz4helg,
                         dfz4he18,
                         dfz4kyrr,
                         dfz4mara,
                         dfz4mutt,
                         dfz4nemo,
                         dfz4sigg,
                         dfz4thor,
                         dfz4thora)




z1asge
ecdf(runif(length(asgedata)))

zp1 <- ggplot(zpd1, aes(x = s, y = l)) + geom_line(col = "red") + 
       geom_line(aes(x = s, y = u), col = "red") + 
       facet_wrap(~whale, nrow = 5) + 
       geom_abline(intercept = 0, slope = 1,linetype="dashed", size=1) +
       labs(x = "z", y = "CDF(z)")
zp1

test <- data.frame(s = sort(z2asge), pos = sort(z2asge)+(1.36/sqrt(length(z2asge))), neg = sort(z2asge) - (1.36/sqrt(length(z2asge))))
head(test)

plot(test, type = "l")

ggplot(test, aes(x = s, y = pos)) + geom_line(col = "red") +
      geom_line(aes(x = s, y = neg))




zp2 <- ggplot(zpd2, aes(x = s, y = l)) + geom_line(col = "red") + 
       geom_line(aes(x = s, y = u), col = "red") + 
       facet_wrap(~whale, nrow = 6) +
       geom_abline(intercept = 0, slope = 1,linetype="dashed", size=1) +
       labs(x = "z", y = "CDF(z)")

zp2


zp3 <- ggplot(zpd3, aes(x = s, y = l)) +
       geom_line(col = "red") + 
       geom_line(aes(x = s, y = u), col = "red") + 
       facet_wrap(~whale, nrow = 5) + 
       geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1) +
       labs(x = "z", y = "CDF(z)")

zp3

zp4 <- ggplot(zpd4, aes(x = s, y = l)) + geom_line(col = "red") + 
       geom_line(aes(x = s, y = u), col = "red") + 
       facet_wrap(~whale, nrow = 5) + 
       geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1) +
       labs(x = "z", y = "CDF(z)")
zp4 



pd <- function(data, preddata){
  predd <- data.frame(predv = preddata, time_since_tagging = 1:length(preddata))
  predd$time_since_tagging <- predd$time_since_tagging/(60*60)
  count <- NULL
  hour <- NULL
  pred <- NULL
  for (i in 1:max(ceil(data$time_since_tagging))-1){
    count[i] <- sum(data[(data["time_since_tagging"]>=i) & (data["time_since_tagging"]<= i+1),]$Buzz)
    hour[i] <- i
    pred[i] <- sum(predd[(predd["time_since_tagging"]>=i) & (predd["time_since_tagging"]<= i+1),]$predv)
    
  }
  return(data.frame(hour, count, pred))
}


anova(m1, m2, m3, m4)

pd1asge <- pd(asgedata , allpredasge1) 
pd1bald <- pd(balddata , allpredbald1) 
pd1eist <- pd(eistdata , allpredeist1) 
pd1fred <- pd(freddata , allpredfred1) 
pd1frey <- pd(freydata , allpredfrey1) 
pd1frid <- pd(friddata , allpredfrid1) 
pd1helg <- pd(helgdata , allpredhelg1) 
pd1he18 <- pd(he18data , allpredhe181) 
pd1kyrr <- pd(kyrrdata , allpredkyrr1) 
pd1mara <- pd(maradata , allpredmara1) 
pd1mutt <- pd(muttdata , allpredmutt1) 
pd1nemo <- pd(nemodata , allprednemo1) 
pd1sigg <- pd(siggdata , allpredsigg1) 
pd1thor <- pd(thordata , allpredthor1) 
pd1thora <-pd(thoradata, allpredthora1)


pd2asge <- pd(asgedata , allpredasge2) 
pd2bald <- pd(balddata , allpredbald2) 
pd2eist <- pd(eistdata , allpredeist2) 
pd2fred <- pd(freddata , allpredfred2) 
pd2frey <- pd(freydata , allpredfrey2) 
pd2frid <- pd(friddata , allpredfrid2) 
pd2helg <- pd(helgdata , allpredhelg2) 
pd2he18 <- pd(he18data , allpredhe182) 
pd2kyrr <- pd(kyrrdata , allpredkyrr2) 
pd2mara <- pd(maradata , allpredmara2) 
pd2mutt <- pd(muttdata , allpredmutt2) 
pd2nemo <- pd(nemodata , allprednemo2) 
pd2sigg <- pd(siggdata , allpredsigg2) 
pd2thor <- pd(thordata , allpredthor2) 
pd2thora <-pd(thoradata, allpredthora2)

pd3asge <- pd(asgedata , allpredasge3) 
pd3bald <- pd(balddata , allpredbald3) 
pd3eist <- pd(eistdata , allpredeist3) 
pd3fred <- pd(freddata , allpredfred3) 
pd3frey <- pd(freydata , allpredfrey3) 
pd3frid <- pd(friddata , allpredfrid3) 
pd3helg <- pd(helgdata , allpredhelg3) 
pd3he18 <- pd(he18data , allpredhe183) 
pd3kyrr <- pd(kyrrdata , allpredkyrr3) 
pd3mara <- pd(maradata , allpredmara3) 
pd3mutt <- pd(muttdata , allpredmutt3) 
pd3nemo <- pd(nemodata , allprednemo3) 
pd3sigg <- pd(siggdata , allpredsigg3) 
pd3thor <- pd(thordata , allpredthor3) 
pd3thora <-pd(thoradata, allpredthora3)

pd4asge <- pd(asgedata , allpredasge4) 
pd4bald <- pd(balddata , allpredbald4) 
pd4eist <- pd(eistdata , allpredeist4) 
pd4fred <- pd(freddata , allpredfred4) 
pd4frey <- pd(freydata , allpredfrey4) 
pd4frid <- pd(friddata , allpredfrid4) 
pd4helg <- pd(helgdata , allpredhelg4) 
pd4he18 <- pd(he18data , allpredhe184) 
pd4kyrr <- pd(kyrrdata , allpredkyrr4) 
pd4mara <- pd(maradata , allpredmara4) 
pd4mutt <- pd(muttdata , allpredmutt4) 
pd4nemo <- pd(nemodata , allprednemo4) 
pd4sigg <- pd(siggdata , allpredsigg4) 
pd4thor <- pd(thordata , allpredthor4) 
pd4thora <-pd(thoradata, allpredthora4)



dfp1asge <- data.frame(count = pd1asge$count, hour =pd1asge$hour, pred = pd1asge$pred, model = "m1", whale = "Asgeir")
dfp1bald <- data.frame(count = pd1bald$count, hour =pd1bald$hour, pred = pd1bald$pred, model = "m1", whale = "Balder")
dfp1eist <- data.frame(count = pd1eist$count, hour =pd1eist$hour, pred = pd1eist$pred, model = "m1", whale = "Eistla")
dfp1fred <- data.frame(count = pd1fred$count, hour =pd1fred$hour, pred = pd1fred$pred, model = "m1", whale = "Frederik")
dfp1frey <- data.frame(count = pd1frey$count, hour =pd1frey$hour, pred = pd1frey$pred, model = "m1", whale = "Freya")
dfp1frid <- data.frame(count = pd1frid$count, hour =pd1frid$hour, pred = pd1frid$pred, model = "m1", whale = "Frida")
dfp1helg <- data.frame(count = pd1helg$count, hour =pd1helg$hour, pred = pd1helg$pred, model = "m1", whale = "Helge")
dfp1he18 <- data.frame(count = pd1he18$count, hour =pd1he18$hour, pred = pd1he18$pred, model = "m1", whale = "Helge18")
dfp1kyrr <- data.frame(count = pd1kyrr$count, hour =pd1kyrr$hour, pred = pd1kyrr$pred, model = "m1", whale = "Kyrri")
dfp1mara <- data.frame(count = pd1mara$count, hour =pd1mara$hour, pred = pd1mara$pred, model = "m1", whale = "Mara")
dfp1mutt <- data.frame(count = pd1mutt$count, hour =pd1mutt$hour, pred = pd1mutt$pred, model = "m1", whale = "Mutti")
dfp1nemo <- data.frame(count = pd1nemo$count, hour =pd1nemo$hour, pred = pd1nemo$pred, model = "m1", whale = "Nemo")
dfp1sigg <- data.frame(count = pd1sigg$count, hour =pd1sigg$hour, pred = pd1sigg$pred, model = "m1", whale = "Siggi")
dfp1thor <- data.frame(count = pd1thor$count, hour =pd1thor$hour, pred = pd1thor$pred, model = "m1", whale = "Thor")
dfp1thora<- data.frame(count = pd1thora$count,hour=pd1thora$hour, pred = pd1thora$pred, model = "m1", whale = "Thora")

dfp2asge <- data.frame(count = pd2asge$count, hour =pd2asge$hour, pred = pd2asge$pred, model = "m2", whale = "Asgeir")
dfp2bald <- data.frame(count = pd2bald$count, hour =pd2bald$hour, pred = pd2bald$pred, model = "m2", whale = "Balder")
dfp2eist <- data.frame(count = pd2eist$count, hour =pd2eist$hour, pred = pd2eist$pred, model = "m2", whale = "Eistla")
dfp2fred <- data.frame(count = pd2fred$count, hour =pd2fred$hour, pred = pd2fred$pred, model = "m2", whale = "Frederik")
dfp2frey <- data.frame(count = pd2frey$count, hour =pd2frey$hour, pred = pd2frey$pred, model = "m2", whale = "Freya")
dfp2frid <- data.frame(count = pd2frid$count, hour =pd2frid$hour, pred = pd2frid$pred, model = "m2", whale = "Frida")
dfp2helg <- data.frame(count = pd2helg$count, hour =pd2helg$hour, pred = pd2helg$pred, model = "m2", whale = "Helge")
dfp2he18 <- data.frame(count = pd2he18$count, hour =pd2he18$hour, pred = pd2he18$pred, model = "m2", whale = "Helge18")
dfp2kyrr <- data.frame(count = pd2kyrr$count, hour =pd2kyrr$hour, pred = pd2kyrr$pred, model = "m2", whale = "Kyrri")
dfp2mara <- data.frame(count = pd2mara$count, hour =pd2mara$hour, pred = pd2mara$pred, model = "m2", whale = "Mara")
dfp2mutt <- data.frame(count = pd2mutt$count, hour =pd2mutt$hour, pred = pd2mutt$pred, model = "m2", whale = "Mutti")
dfp2nemo <- data.frame(count = pd2nemo$count, hour =pd2nemo$hour, pred = pd2nemo$pred, model = "m2", whale = "Nemo")
dfp2sigg <- data.frame(count = pd2sigg$count, hour =pd2sigg$hour, pred = pd2sigg$pred, model = "m2", whale = "Siggi")
dfp2thor <- data.frame(count = pd2thor$count, hour =pd2thor$hour, pred = pd2thor$pred, model = "m2", whale = "Thor")
dfp2thora<- data.frame(count = pd2thora$count,hour= pd2thora$hour, pred =pd2thora$pred, model = "m2", whale = "Thora")

dfp3asge <- data.frame(count = pd3asge$count, hour =pd3asge$hour, pred = pd3asge$pred, model = "m3", whale = "Asgeir")
dfp3bald <- data.frame(count = pd3bald$count, hour =pd3bald$hour, pred = pd3bald$pred, model = "m3", whale = "Balder")
dfp3eist <- data.frame(count = pd3eist$count, hour =pd3eist$hour, pred = pd3eist$pred, model = "m3", whale = "Eistla")
dfp3fred <- data.frame(count = pd3fred$count, hour =pd3fred$hour, pred = pd3fred$pred, model = "m3", whale = "Frederik")
dfp3frey <- data.frame(count = pd3frey$count, hour =pd3frey$hour, pred = pd3frey$pred, model = "m3", whale = "Freya")
dfp3frid <- data.frame(count = pd3frid$count, hour =pd3frid$hour, pred = pd3frid$pred, model = "m3", whale = "Frida")
dfp3helg <- data.frame(count = pd3helg$count, hour =pd3helg$hour, pred = pd3helg$pred, model = "m3", whale = "Helge")
dfp3he18 <- data.frame(count = pd3he18$count, hour =pd3he18$hour, pred = pd3he18$pred, model = "m3", whale = "Helge18")
dfp3kyrr <- data.frame(count = pd3kyrr$count, hour =pd3kyrr$hour, pred = pd3kyrr$pred, model = "m3", whale = "Kyrri")
dfp3mara <- data.frame(count = pd3mara$count, hour =pd3mara$hour, pred = pd3mara$pred, model = "m3", whale = "Mara")
dfp3mutt <- data.frame(count = pd3mutt$count, hour =pd3mutt$hour, pred = pd3mutt$pred, model = "m3", whale = "Mutti")
dfp3nemo <- data.frame(count = pd3nemo$count, hour =pd3nemo$hour, pred = pd3nemo$pred, model = "m3", whale = "Nemo")
dfp3sigg <- data.frame(count = pd3sigg$count, hour =pd3sigg$hour, pred = pd3sigg$pred, model = "m3", whale = "Siggi")
dfp3thor <- data.frame(count = pd3thor$count, hour =pd3thor$hour, pred = pd3thor$pred, model = "m3", whale = "Thor")
dfp3thora<- data.frame(count = pd3thora$count,hour= pd3thora$hour, pred = pd3thora$pred, model = "m3", whale = "Thora")

dfp4asge <- data.frame(count = pd4asge$count, hour =pd4asge$hour, pred = pd4asge$pred, model = "m4", whale = "Asgeir")
dfp4bald <- data.frame(count = pd4bald$count, hour =pd4bald$hour, pred = pd4bald$pred, model = "m4", whale = "Balder")
dfp4eist <- data.frame(count = pd4eist$count, hour =pd4eist$hour, pred = pd4eist$pred, model = "m4", whale = "Eistla")
dfp4fred <- data.frame(count = pd4fred$count, hour =pd4fred$hour, pred = pd4fred$pred, model = "m4", whale = "Frederik")
dfp4frey <- data.frame(count = pd4frey$count, hour =pd4frey$hour, pred = pd4frey$pred, model = "m4", whale = "Freya")
dfp4frid <- data.frame(count = pd4frid$count, hour =pd4frid$hour, pred = pd4frid$pred, model = "m4", whale = "Frida")
dfp4helg <- data.frame(count = pd4helg$count, hour =pd4helg$hour, pred = pd4helg$pred, model = "m4", whale = "Helge")
dfp4he18 <- data.frame(count = pd4he18$count, hour =pd4he18$hour, pred = pd4he18$pred, model = "m4", whale = "Helge18")
dfp4kyrr <- data.frame(count = pd4kyrr$count, hour =pd4kyrr$hour, pred = pd4kyrr$pred, model = "m4", whale = "Kyrri")
dfp4mara <- data.frame(count = pd4mara$count, hour =pd4mara$hour, pred = pd4mara$pred, model = "m4", whale = "Mara")
dfp4mutt <- data.frame(count = pd4mutt$count, hour =pd4mutt$hour, pred = pd4mutt$pred, model = "m4", whale = "Mutti")
dfp4nemo <- data.frame(count = pd4nemo$count, hour =pd4nemo$hour, pred = pd4nemo$pred, model = "m4", whale = "Nemo")
dfp4sigg <- data.frame(count = pd4sigg$count, hour =pd4sigg$hour, pred = pd4sigg$pred, model = "m4", whale = "Siggi")
dfp4thor <- data.frame(count = pd4thor$count, hour =pd4thor$hour, pred = pd4thor$pred, model = "m4", whale = "Thor")
dfp4thora<- data.frame(count = pd4thora$count,hour= pd4thora$hour, pred =pd4thora$pred, model = "m4", whale = "Thora")




ppd1 <- rbind.data.frame(dfp1asge,dfp1bald,dfp1eist,dfp1fred,dfp1frey,dfp1frid,dfp1helg,dfp1he18,dfp1kyrr,dfp1mara,dfp1mutt,dfp1nemo,
                         dfp1sigg,dfp1thor,dfp1thora)

ppd2 <- rbind.data.frame(dfp2asge,dfp2bald,dfp2eist,dfp2fred,dfp2frey,dfp2frid,dfp2helg,dfp2he18,dfp2kyrr,dfp2mara,dfp2mutt,dfp2nemo,
                         dfp2sigg,dfp2thor,dfp2thora)

ppd3 <- rbind.data.frame(dfp3asge,dfp3bald,dfp3eist,dfp3fred,dfp3frey,dfp3frid,dfp3helg,dfp3he18,dfp3kyrr,dfp3mara,dfp3mutt,dfp3nemo,
                         dfp3sigg,dfp3thor,dfp3thora)

ppd4 <- rbind.data.frame(dfp4asge,dfp4bald,dfp4eist,dfp4fred,dfp4frey,dfp4frid,dfp4helg,dfp4he18,dfp4kyrr,dfp4mara,dfp4mutt,dfp4nemo, 
                         dfp4sigg,dfp4thor,dfp4thora)


pp1 <- ggplot(ppd1, aes(x = hour, y = pred)) +
  geom_line( col = "red") +
  geom_point(aes(x = hour, y = count), alpha = .2) + 
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours since tagging", y = "Counts")

pp1

pp2 <- ggplot(ppd2, aes(x = hour, y = pred)) +
  geom_line( col = "red") +
  geom_point(aes(x = hour, y = count), alpha = .2) +
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours since tagging", y = "Intensities")

pp2

pp3 <- ggplot(ppd3, aes(x = hour, y = pred)) +
  geom_line( col = "red") +
  geom_point(aes(x = hour, y = count), alpha = .2) +
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours since tagging", y = "Intensities")

pp3

pp4 <- ggplot(ppd4, aes(x = hour, y = pred)) +
  geom_line( col = "red") + 
  geom_point(aes(x = hour, y = count), alpha = .2) +
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours since tagging", y = "Intensities")
pp4




anova(m1, m2, m3, m4, test = "chisq")


install.packages("ggcorrplot")
library(ggcorrplot)


nums <- unlist(lapply(data1, is.numeric))  
nums <- data1[, nums]
ggcorrplot(cor(nums))



ggplot(data1) + geom_bar(aes(x = Ind))

data1 %>%
  count(Ind)



ggplot(data1, aes(x = time_since_tagging, y = negDepth)) + 
    geom_line() + facet_wrap(~Ind, ncol = 1)

data1
ggplot(data1) +
  geom_histogram(aes(x = Depth), binwidth = 25) +
  labs(x = "Depth", y = "Frequency")


ggplot(data1, aes(x=time_since_tagging, y = negDepth)) +
  geom_line(alpha = 0.5) +
  geom_point(data=subset(data1, Buzz == 1), color = "red", size = 0.8) +
  facet_wrap(~ Ind, ncol = 3) + 
  labs(x = "Time Since Tagging", y = "Depth")




ggplot(bald, aes(x = Depth, colour = Ind)) + 
  geom_freqpoly(binwidth = 10) +
  labs(x = "Depth", y = "Frequency")

ggplot(data1, aes(x = Depth, colour = Ind)) + 
  geom_freqpoly(binwidth = 10) +
  labs(x = "Depth", y = "Frequency")



plot(ecdf(data3$Buzz))

plot(data3$time_since_tagging, cumsum(data3$Buzz))


data3$Buzz <- as.numeric(data3$Buzz)
ggplot(data3, aes(x = Buzz, y = Depth)) + geom_boxplot() + labs(x = "Buzz", y = "Depth")
ggplot(data3, aes(x = Buzz, y = time_since_tagging)) + geom_boxplot() + labs(x = "Buzz", y = "time_since_tagging")



data1$Depthkm <- data1$Depth/1000
m5 <- glmer(Buzz ~ I(exp(-time_since_tagging)):htime + ns(Depth, 3) + gender + length + (1|Ind),
            data = data1, family = "poisson", start = theta)
m6 <- glmer(Buzz ~ I(1/time_since_tagging^(1/2)):htime + ns(Depth, 3)  + gender + length + (1|Ind),
            family = "poisson", data = data1)

m7 <- glmer(Buzz ~ I(1/time_since_tagging):htime + ns(Depth, 3)  + gender + length + (1|Ind),
            family = "poisson", data = data1)
theta = c(-71.6209, 8.5296)
m8 <- glmer(Buzz ~ I(1/(time_since_tagging^2)):htime + ns(I(Depth), 3) + gender + length + (1|Ind),
            family = "poisson", data = data1)
rm(m4)
summary(m5)
summary(m4)
theta
load("frachalf.model")

ss5 <- getME(m5,c("theta","fixef"))
m5 <- update(m5,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

ss6 <- getME(m6,c("theta","fixef"))
m6 <- update(m6,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

ss7 <- getME(m7,c("theta","fixef"))
m7 <- update(m7,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

ss8 <- getME(m8,c("theta","fixef"))
m8 <- update(m8,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


summary(m5)
summary(m8)

rename(m3, m2temp)

summary(m3)
theta <- c(1.683e+01, 6.091e+00, -9.585e-02, 4.844e-02  , 1.605e-01  , -2.855e+07, -8.131e+03, -3.504e+01)
theta <- list(theta)
theta
rm(m1)
rm(m2)
rm(m3)
rm(m4)
summary(m1)
theta <- list(c(8.523e+00, 1.683e+01, 6.091e+00, 0, 0, 0, -106.4))
#save(m5, file = "m1x")
#save(m6, file = "m2x")
#save(m7, file = "m3x")
#save(m8, file = "m4x")
summary(m5)
load("m1x")
load("m2x")
load("m4x")

allpredm5 <- predict(m5, type = "response")
allpredm5 <- data.frame(allpredm5)

allpredm6 <- predict(m6, type = "response")
allpredm6 <- data.frame(allpredm6)

allpredm8 <- predict(m8, type = "response")
allpredm8 <- data.frame(allpredm8)



allpredasge5<- allpredm5[1:200389,]
allpredbald5<- allpredm5[200390:805190,]
allpredeist5<- allpredm5[805191:1174279,]
allpredfred5<- allpredm5[1174280:1277493,]
allpredfrey5<- allpredm5[1277494:1523079,]
allpredfrid5<- allpredm5[1523080:1820681,]
allpredhelg5<- allpredm5[1820682:2104900,]
allpredhe185<- allpredm5[2104901:2308004,]
allpredkyrr5<- allpredm5[2308005:2533489,]
allpredmara5<- allpredm5[2533490:2569453,]
allpredmutt5<- allpredm5[2569454:2935262,]
allprednemo5<- allpredm5[2935263:3055196,]
allpredsigg5<- allpredm5[3055197:3173950,]
allpredthor5<- allpredm5[3173951:3456474,]
allpredthora5 <- allpredm5[3456475:3826480,]


allpredasge6<- allpredm6[1:200389,]
allpredbald6<- allpredm6[200390:805190,]
allpredeist6<- allpredm6[805191:1174279,]
allpredfred6<- allpredm6[1174280:1277493,]
allpredfrey6<- allpredm6[1277494:1523079,]
allpredfrid6<- allpredm6[1523080:1820681,]
allpredhelg6<- allpredm6[1820682:2104900,]
allpredhe186<- allpredm6[2104901:2308004,]
allpredkyrr6<- allpredm6[2308005:2533489,]
allpredmara6<- allpredm6[2533490:2569453,]
allpredmutt6<- allpredm6[2569454:2935262,]
allprednemo6<- allpredm6[2935263:3055196,]
allpredsigg6<- allpredm6[3055197:3173950,]
allpredthor6<- allpredm6[3173951:3456474,]
allpredthora6 <- allpredm6[3456475:3826480,]


allpredasge8<- allpredm8[1:200389,]
allpredbald8<- allpredm8[200390:805190,]
allpredeist8<- allpredm8[805191:1174279,]
allpredfred8<- allpredm8[1174280:1277493,]
allpredfrey8<- allpredm8[1277494:1523079,]
allpredfrid8<- allpredm8[1523080:1820681,]
allpredhelg8<- allpredm8[1820682:2104900,]
allpredhe188<- allpredm8[2104901:2308004,]
allpredkyrr8<- allpredm8[2308005:2533489,]
allpredmara8<- allpredm8[2533490:2569453,]
allpredmutt8<- allpredm8[2569454:2935262,]
allprednemo8<- allpredm8[2935263:3055196,]
allpredsigg8<- allpredm8[3055197:3173950,]
allpredthor8<- allpredm8[3173951:3456474,]
allpredthora8 <- allpredm8[3456475:3826480,]


z5asge <- rescale(asgedata , allpredasge5) 
z5bald <- rescale(balddata , allpredbald5) 
z5eist <- rescale(eistdata , allpredeist5) 
z5fred <- rescale(freddata , allpredfred5) 
z5frey <- rescale(freydata , allpredfrey5) 
z5frid <- rescale(friddata , allpredfrid5) 
z5helg <- rescale(helgdata , allpredhelg5) 
z5he18 <- rescale(he18data , allpredhe185) 
z5kyrr <- rescale(kyrrdata , allpredkyrr5) 
z5mara <- rescale(maradata , allpredmara5) 
z5mutt <- rescale(muttdata , allpredmutt5) 
z5nemo <- rescale(nemodata , allprednemo5) 
z5sigg <- rescale(siggdata , allpredsigg5) 
z5thor <- rescale(thordata , allpredthor5) 
z5thora <- rescale(thoradata, allpredthora5)


z6asge <- rescale(asgedata , allpredasge6) 
z6bald <- rescale(balddata , allpredbald6) 
z6eist <- rescale(eistdata , allpredeist6) 
z6fred <- rescale(freddata , allpredfred6) 
z6frey <- rescale(freydata , allpredfrey6) 
z6frid <- rescale(friddata , allpredfrid6) 
z6helg <- rescale(helgdata , allpredhelg6) 
z6he18 <- rescale(he18data , allpredhe186) 
z6kyrr <- rescale(kyrrdata , allpredkyrr6) 
z6mara <- rescale(maradata , allpredmara6) 
z6mutt <- rescale(muttdata , allpredmutt6) 
z6nemo <- rescale(nemodata , allprednemo6) 
z6sigg <- rescale(siggdata , allpredsigg6) 
z6thor <- rescale(thordata , allpredthor6) 
z6thora <- rescale(thoradata, allpredthora6)

z8asge <- rescale(asgedata , allpredasge8) 
z8bald <- rescale(balddata , allpredbald8) 
z8eist <- rescale(eistdata , allpredeist8) 
z8fred <- rescale(freddata , allpredfred8) 
z8frey <- rescale(freydata , allpredfrey8) 
z8frid <- rescale(friddata , allpredfrid8) 
z8helg <- rescale(helgdata , allpredhelg8) 
z8he18 <- rescale(he18data , allpredhe188) 
z8kyrr <- rescale(kyrrdata , allpredkyrr8) 
z8mara <- rescale(maradata , allpredmara8) 
z8mutt <- rescale(muttdata , allpredmutt8) 
z8nemo <- rescale(nemodata , allprednemo8) 
z8sigg <- rescale(siggdata , allpredsigg8) 
z8thor <- rescale(thordata , allpredthor8) 
z8thora <- rescale(thoradata, allpredthora8)





dfz5asge <- data.frame( u = ecdf.ks.CI(z5asge )$upper,l = ecdf.ks.CI(z5asge )$lower, z = unique(z5asge)  ,  s = sort(unique(z5asge )), model = "m5", whale = "Asgeir")
dfz5bald <- data.frame( u = ecdf.ks.CI(z5bald )$upper,l = ecdf.ks.CI(z5bald )$lower, z = unique(z5bald)  ,  s = sort(unique(z5bald )), model = "m5", whale = "Balder")
dfz5eist <- data.frame( u = ecdf.ks.CI(z5eist )$upper,l = ecdf.ks.CI(z5eist )$lower, z = unique(z5eist)  ,  s = sort(unique(z5eist )), model = "m5", whale = "Eistla")
dfz5fred <- data.frame( u = ecdf.ks.CI(z5fred )$upper,l = ecdf.ks.CI(z5fred )$lower, z = unique(z5fred)  ,  s = sort(unique(z5fred )), model = "m5", whale = "Frederik")
dfz5frey <- data.frame( u = ecdf.ks.CI(z5frey )$upper,l = ecdf.ks.CI(z5frey )$lower, z = unique(z5frey)  ,  s = sort(unique(z5frey )), model = "m5", whale = "Freya")
dfz5frid <- data.frame( u = ecdf.ks.CI(z5frid )$upper,l = ecdf.ks.CI(z5frid )$lower, z = unique(z5frid)  ,  s = sort(unique(z5frid )), model = "m5", whale = "Frida")
dfz5helg <- data.frame( u = ecdf.ks.CI(z5helg )$upper,l = ecdf.ks.CI(z5helg )$lower, z = unique(z5helg)  ,  s = sort(unique(z5helg )), model = "m5", whale = "Helge")
dfz5he18 <- data.frame( u = ecdf.ks.CI(z5he18 )$upper,l = ecdf.ks.CI(z5he18 )$lower, z = unique(z5he18)  ,  s = sort(unique(z5he18 )), model = "m5", whale = "Helge18")
dfz5kyrr <- data.frame( u = ecdf.ks.CI(z5kyrr )$upper,l = ecdf.ks.CI(z5kyrr )$lower, z = unique(z5kyrr)  ,  s = sort(unique(z5kyrr )), model = "m5", whale = "Kyrri")
dfz5mara <- data.frame( u = ecdf.ks.CI(z5mara )$upper,l = ecdf.ks.CI(z5mara )$lower, z = unique(z5mara)  ,  s = sort(unique(z5mara )), model = "m5", whale = "Mara")
dfz5mutt <- data.frame( u = ecdf.ks.CI(z5mutt )$upper,l = ecdf.ks.CI(z5mutt )$lower, z = unique(z5mutt)  ,  s = sort(unique(z5mutt )), model = "m5", whale = "Mutti")
dfz5nemo <- data.frame( u = ecdf.ks.CI(z5nemo )$upper,l = ecdf.ks.CI(z5nemo )$lower, z = unique(z5nemo)  ,  s = sort(unique(z5nemo )), model = "m5", whale = "Nemo")
dfz5sigg <- data.frame( u = ecdf.ks.CI(z5sigg )$upper,l = ecdf.ks.CI(z5sigg )$lower, z = unique(z5sigg)  ,  s = sort(unique(z5sigg )), model = "m5", whale = "Siggi")
dfz5thor <- data.frame( u = ecdf.ks.CI(z5thor )$upper,l = ecdf.ks.CI(z5thor )$lower, z = unique(z5thor)  ,  s = sort(unique(z5thor )), model = "m5", whale = "Thor")
dfz5thora<- data.frame( u = ecdf.ks.CI(z5thora)$upper,l = ecdf.ks.CI(z5thora)$lower, z = unique(z5thora) ,  s = sort(unique(z5thora)), model = "m5", whale = "Thora")

dfz6asge <- data.frame( u = ecdf.ks.CI(z6asge )$upper,l = ecdf.ks.CI(z6asge )$lower, z = unique(z6asge)  ,  s = sort(unique(z6asge )), model = "m2", whale = "Asgeir")
dfz6bald <- data.frame( u = ecdf.ks.CI(z6bald )$upper,l = ecdf.ks.CI(z6bald )$lower, z = unique(z6bald)  ,  s = sort(unique(z6bald )), model = "m2", whale = "Balder")
dfz6eist <- data.frame( u = ecdf.ks.CI(z6eist )$upper,l = ecdf.ks.CI(z6eist )$lower, z = unique(z6eist)  ,  s = sort(unique(z6eist )), model = "m2", whale = "Eistla")
dfz6fred <- data.frame( u = ecdf.ks.CI(z6fred )$upper,l = ecdf.ks.CI(z6fred )$lower, z = unique(z6fred)  ,  s = sort(unique(z6fred )), model = "m2", whale = "Frederik")
dfz6frey <- data.frame( u = ecdf.ks.CI(z6frey )$upper,l = ecdf.ks.CI(z6frey )$lower, z = unique(z6frey)  ,  s = sort(unique(z6frey )), model = "m2", whale = "Freya")
dfz6frid <- data.frame( u = ecdf.ks.CI(z6frid )$upper,l = ecdf.ks.CI(z6frid )$lower, z = unique(z6frid)  ,  s = sort(unique(z6frid )), model = "m2", whale = "Frida")
dfz6helg <- data.frame( u = ecdf.ks.CI(z6helg )$upper,l = ecdf.ks.CI(z6helg )$lower, z = unique(z6helg)  ,  s = sort(unique(z6helg )), model = "m2", whale = "Helge")
dfz6he18 <- data.frame( u = ecdf.ks.CI(z6he18 )$upper,l = ecdf.ks.CI(z6he18 )$lower, z = unique(z6he18)  ,  s = sort(unique(z6he18 )), model = "m2", whale = "Helge18")
dfz6kyrr <- data.frame( u = ecdf.ks.CI(z6kyrr )$upper,l = ecdf.ks.CI(z6kyrr )$lower, z = unique(z6kyrr)  ,  s = sort(unique(z6kyrr )), model = "m2", whale = "Kyrri")
dfz6mara <- data.frame( u = ecdf.ks.CI(z6mara )$upper,l = ecdf.ks.CI(z6mara )$lower, z = unique(z6mara)  ,  s = sort(unique(z6mara )), model = "m2", whale = "Mara")
dfz6mutt <- data.frame( u = ecdf.ks.CI(z6mutt )$upper,l = ecdf.ks.CI(z6mutt )$lower, z = unique(z6mutt)  ,  s = sort(unique(z6mutt )), model = "m2", whale = "Mutti")
dfz6nemo <- data.frame( u = ecdf.ks.CI(z6nemo )$upper,l = ecdf.ks.CI(z6nemo )$lower, z = unique(z6nemo)  ,  s = sort(unique(z6nemo )), model = "m2", whale = "Nemo")
dfz6sigg <- data.frame( u = ecdf.ks.CI(z6sigg )$upper,l = ecdf.ks.CI(z6sigg )$lower, z = unique(z6sigg)  ,  s = sort(unique(z6sigg )), model = "m2", whale = "Siggi")
dfz6thor <- data.frame( u = ecdf.ks.CI(z6thor )$upper,l = ecdf.ks.CI(z6thor )$lower, z = unique(z6thor)  ,  s = sort(unique(z6thor )), model = "m2", whale = "Thor")
dfz6thora<- data.frame( u = ecdf.ks.CI(z6thora)$upper,l = ecdf.ks.CI(z6thora)$lower, z = unique(z6thora) ,  s = sort(unique(z6thora)), model = "m2", whale = "Thora")

dfz8asge <- data.frame( u = ecdf.ks.CI(z8asge )$upper,l = ecdf.ks.CI(z8asge )$lower, z = unique(z8asge)  ,  s = sort(unique(z8asge )), model = "m3", whale = "Asgeir")
dfz8bald <- data.frame( u = ecdf.ks.CI(z8bald )$upper,l = ecdf.ks.CI(z8bald )$lower, z = unique(z8bald)  ,  s = sort(unique(z8bald )), model = "m3", whale = "Balder")
dfz8eist <- data.frame( u = ecdf.ks.CI(z8eist )$upper,l = ecdf.ks.CI(z8eist )$lower, z = unique(z8eist)  ,  s = sort(unique(z8eist )), model = "m3", whale = "Eistla")
dfz8fred <- data.frame( u = ecdf.ks.CI(z8fred )$upper,l = ecdf.ks.CI(z8fred )$lower, z = unique(z8fred)  ,  s = sort(unique(z8fred )), model = "m3", whale = "Frederik")
dfz8frey <- data.frame( u = ecdf.ks.CI(z8frey )$upper,l = ecdf.ks.CI(z8frey )$lower, z = unique(z8frey)  ,  s = sort(unique(z8frey )), model = "m3", whale = "Freya")
dfz8frid <- data.frame( u = ecdf.ks.CI(z8frid )$upper,l = ecdf.ks.CI(z8frid )$lower, z = unique(z8frid)  ,  s = sort(unique(z8frid )), model = "m3", whale = "Frida")
dfz8helg <- data.frame( u = ecdf.ks.CI(z8helg )$upper,l = ecdf.ks.CI(z8helg )$lower, z = unique(z8helg)  ,  s = sort(unique(z8helg )), model = "m3", whale = "Helge")
dfz8he18 <- data.frame( u = ecdf.ks.CI(z8he18 )$upper,l = ecdf.ks.CI(z8he18 )$lower, z = unique(z8he18)  ,  s = sort(unique(z8he18 )), model = "m3", whale = "Helge18")
dfz8kyrr <- data.frame( u = ecdf.ks.CI(z8kyrr )$upper,l = ecdf.ks.CI(z8kyrr )$lower, z = unique(z8kyrr)  ,  s = sort(unique(z8kyrr )), model = "m3", whale = "Kyrri")
dfz8mara <- data.frame( u = ecdf.ks.CI(z8mara )$upper,l = ecdf.ks.CI(z8mara )$lower, z = unique(z8mara)  ,  s = sort(unique(z8mara )), model = "m3", whale = "Mara")
dfz8mutt <- data.frame( u = ecdf.ks.CI(z8mutt )$upper,l = ecdf.ks.CI(z8mutt )$lower, z = unique(z8mutt)  ,  s = sort(unique(z8mutt )), model = "m3", whale = "Mutti")
dfz8nemo <- data.frame( u = ecdf.ks.CI(z8nemo )$upper,l = ecdf.ks.CI(z8nemo )$lower, z = unique(z8nemo)  ,  s = sort(unique(z8nemo )), model = "m3", whale = "Nemo")
dfz8sigg <- data.frame( u = ecdf.ks.CI(z8sigg )$upper,l = ecdf.ks.CI(z8sigg )$lower, z = unique(z8sigg)  ,  s = sort(unique(z8sigg )), model = "m3", whale = "Siggi")
dfz8thor <- data.frame( u = ecdf.ks.CI(z8thor )$upper,l = ecdf.ks.CI(z8thor )$lower, z = unique(z8thor)  ,  s = sort(unique(z8thor )), model = "m3", whale = "Thor")
dfz8thora<- data.frame( u = ecdf.ks.CI(z8thora)$upper,l = ecdf.ks.CI(z8thora)$lower, z = unique(z8thora) ,  s = sort(unique(z8thora)), model = "m3", whale = "Thora")




zpd5 <- rbind.data.frame(dfz5asge,
                         dfz5bald,
                         dfz5eist,
                         dfz5fred,
                         dfz5frey,
                         dfz5frid,
                         dfz5helg,
                         dfz5he18,
                         dfz5kyrr,
                         dfz5mara,
                         dfz5mutt,
                         dfz5nemo,
                         dfz5sigg,
                         dfz5thor,
                         dfz5thora)

zpd6 <- rbind.data.frame(dfz6asge,
                         dfz6bald,
                         dfz6eist,
                         dfz6fred,
                         dfz6frey,
                         dfz6frid,
                         dfz6helg,
                         dfz6he18,
                         dfz6kyrr,
                         dfz6mara,
                         dfz6mutt,
                         dfz6nemo,
                         dfz6sigg,
                         dfz6thor,
                         dfz6thora)

zpd8 <- rbind.data.frame(dfz8asge,
                         dfz8bald,
                         dfz8eist,
                         dfz8fred,
                         dfz8frey,
                         dfz8frid,
                         dfz8helg,
                         dfz8he18,
                         dfz8kyrr,
                         dfz8mara,
                         dfz8mutt,
                         dfz8nemo,
                         dfz8sigg,
                         dfz8thor,
                         dfz8thora)


zp5 <- ggplot(zpd5, aes(x = s, y = l)) +
  geom_line(col = "red") + 
  geom_line(aes(x = s, y = u), col = "red") + 
  facet_wrap(~whale, nrow = 5) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size=1) +
  labs(x = "z", y = "CDF(z)")

zp5


zp6 <- ggplot(zpd6, aes(x = s, y = l)) + geom_line(col = "red") + 
  geom_line(aes(x = s, y = u), col = "red") + 
  facet_wrap(~whale, nrow = 5) + 
  geom_abline(intercept = 0, slope = 1,linetype="dashed", size=1) +
  labs(x = "z", y = "CDF(z)")
zp6


zp8 <- ggplot(zpd8, aes(x = s, y = l)) + geom_line(col = "red") + 
  geom_line(aes(x = s, y = u), col = "red") + 
  facet_wrap(~whale, ncol = 3) + 
  geom_abline(intercept = 0, slope = 1,linetype="dashed", size=1) +
  labs(x = "z", y = "CDF(z)")

zp8

zp1
zp2
zp3
zp4
zp5
zp6
zp8

ppd5 <- rbind.data.frame(dfp5asge,dfp5bald,dfp5eist,dfp5fred,dfp5frey,dfp5frid,dfp5helg,dfp5he18,dfp5kyrr,dfp5mara,dfp5mutt,dfp5nemo,
                         dfp5sigg,dfp5thor,dfp5thora)

ppd6 <- rbind.data.frame(dfp6asge,dfp6bald,dfp6eist,dfp6fred,dfp6frey,dfp6frid,dfp6helg,dfp6he18,dfp6kyrr,dfp6mara,dfp6mutt,dfp6nemo,
                         dfp6sigg,dfp6thor,dfp6thora)

ppd8 <- rbind.data.frame(dfp8asge,dfp8bald,dfp8eist,dfp8fred,dfp8frey,dfp8frid,dfp8helg,dfp8he18,dfp8kyrr,dfp8mara,dfp8mutt,dfp8nemo,
                         dfp8sigg,dfp8thor,dfp8thora)
























pd5asge <- pd(asgedata , allpredasge5) 
pd5bald <- pd(balddata , allpredbald5) 
pd5eist <- pd(eistdata , allpredeist5) 
pd5fred <- pd(freddata , allpredfred5) 
pd5frey <- pd(freydata , allpredfrey5) 
pd5frid <- pd(friddata , allpredfrid5) 
pd5helg <- pd(helgdata , allpredhelg5) 
pd5he18 <- pd(he18data , allpredhe185) 
pd5kyrr <- pd(kyrrdata , allpredkyrr5) 
pd5mara <- pd(maradata , allpredmara5) 
pd5mutt <- pd(muttdata , allpredmutt5) 
pd5nemo <- pd(nemodata , allprednemo5) 
pd5sigg <- pd(siggdata , allpredsigg5) 
pd5thor <- pd(thordata , allpredthor5) 
pd5thora <-pd(thoradata, allpredthora5)

pd6asge <- pd(asgedata , allpredasge6) 
pd6bald <- pd(balddata , allpredbald6) 
pd6eist <- pd(eistdata , allpredeist6) 
pd6fred <- pd(freddata , allpredfred6) 
pd6frey <- pd(freydata , allpredfrey6) 
pd6frid <- pd(friddata , allpredfrid6) 
pd6helg <- pd(helgdata , allpredhelg6) 
pd6he18 <- pd(he18data , allpredhe186) 
pd6kyrr <- pd(kyrrdata , allpredkyrr6) 
pd6mara <- pd(maradata , allpredmara6) 
pd6mutt <- pd(muttdata , allpredmutt6) 
pd6nemo <- pd(nemodata , allprednemo6) 
pd6sigg <- pd(siggdata , allpredsigg6) 
pd6thor <- pd(thordata , allpredthor6) 
pd6thora <-pd(thoradata, allpredthora6)

pd8asge <- pd(asgedata , allpredasge8) 
pd8bald <- pd(balddata , allpredbald8) 
pd8eist <- pd(eistdata , allpredeist8) 
pd8fred <- pd(freddata , allpredfred8) 
pd8frey <- pd(freydata , allpredfrey8) 
pd8frid <- pd(friddata , allpredfrid8) 
pd8helg <- pd(helgdata , allpredhelg8) 
pd8he18 <- pd(he18data , allpredhe188) 
pd8kyrr <- pd(kyrrdata , allpredkyrr8) 
pd8mara <- pd(maradata , allpredmara8) 
pd8mutt <- pd(muttdata , allpredmutt8) 
pd8nemo <- pd(nemodata , allprednemo8) 
pd8sigg <- pd(siggdata , allpredsigg8) 
pd8thor <- pd(thordata , allpredthor8) 
pd8thora <-pd(thoradata, allpredthora8)

dfp5asge <- data.frame(count = pd5asge$count, hour =pd5asge$hour, pred = pd5asge$pred, model = "m5", whale = "Asgeir")
dfp5bald <- data.frame(count = pd5bald$count, hour =pd5bald$hour, pred = pd5bald$pred, model = "m5", whale = "Balder")
dfp5eist <- data.frame(count = pd5eist$count, hour =pd5eist$hour, pred = pd5eist$pred, model = "m5", whale = "Eistla")
dfp5fred <- data.frame(count = pd5fred$count, hour =pd5fred$hour, pred = pd5fred$pred, model = "m5", whale = "Frederik")
dfp5frey <- data.frame(count = pd5frey$count, hour =pd5frey$hour, pred = pd5frey$pred, model = "m5", whale = "Freya")
dfp5frid <- data.frame(count = pd5frid$count, hour =pd5frid$hour, pred = pd5frid$pred, model = "m5", whale = "Frida")
dfp5helg <- data.frame(count = pd5helg$count, hour =pd5helg$hour, pred = pd5helg$pred, model = "m5", whale = "Helge")
dfp5he18 <- data.frame(count = pd5he18$count, hour =pd5he18$hour, pred = pd5he18$pred, model = "m5", whale = "Helge18")
dfp5kyrr <- data.frame(count = pd5kyrr$count, hour =pd5kyrr$hour, pred = pd5kyrr$pred, model = "m5", whale = "Kyrri")
dfp5mara <- data.frame(count = pd5mara$count, hour =pd5mara$hour, pred = pd5mara$pred, model = "m5", whale = "Mara")
dfp5mutt <- data.frame(count = pd5mutt$count, hour =pd5mutt$hour, pred = pd5mutt$pred, model = "m5", whale = "Mutti")
dfp5nemo <- data.frame(count = pd5nemo$count, hour =pd5nemo$hour, pred = pd5nemo$pred, model = "m5", whale = "Nemo")
dfp5sigg <- data.frame(count = pd5sigg$count, hour =pd5sigg$hour, pred = pd5sigg$pred, model = "m5", whale = "Siggi")
dfp5thor <- data.frame(count = pd5thor$count, hour =pd5thor$hour, pred = pd5thor$pred, model = "m5", whale = "Thor")
dfp5thora<- data.frame(count = pd5thora$count,hour= pd5thora$hour, pred =pd5thora$pred, model = "m5", whale = "Thora")


dfp6asge <- data.frame(count = pd6asge$count, hour =pd6asge$hour, pred = pd6asge$pred, model = "m6", whale = "Asgeir")
dfp6bald <- data.frame(count = pd6bald$count, hour =pd6bald$hour, pred = pd6bald$pred, model = "m6", whale = "Balder")
dfp6eist <- data.frame(count = pd6eist$count, hour =pd6eist$hour, pred = pd6eist$pred, model = "m6", whale = "Eistla")
dfp6fred <- data.frame(count = pd6fred$count, hour =pd6fred$hour, pred = pd6fred$pred, model = "m6", whale = "Frederik")
dfp6frey <- data.frame(count = pd6frey$count, hour =pd6frey$hour, pred = pd6frey$pred, model = "m6", whale = "Freya")
dfp6frid <- data.frame(count = pd6frid$count, hour =pd6frid$hour, pred = pd6frid$pred, model = "m6", whale = "Frida")
dfp6helg <- data.frame(count = pd6helg$count, hour =pd6helg$hour, pred = pd6helg$pred, model = "m6", whale = "Helge")
dfp6he18 <- data.frame(count = pd6he18$count, hour =pd6he18$hour, pred = pd6he18$pred, model = "m6", whale = "Helge18")
dfp6kyrr <- data.frame(count = pd6kyrr$count, hour =pd6kyrr$hour, pred = pd6kyrr$pred, model = "m6", whale = "Kyrri")
dfp6mara <- data.frame(count = pd6mara$count, hour =pd6mara$hour, pred = pd6mara$pred, model = "m6", whale = "Mara")
dfp6mutt <- data.frame(count = pd6mutt$count, hour =pd6mutt$hour, pred = pd6mutt$pred, model = "m6", whale = "Mutti")
dfp6nemo <- data.frame(count = pd6nemo$count, hour =pd6nemo$hour, pred = pd6nemo$pred, model = "m6", whale = "Nemo")
dfp6sigg <- data.frame(count = pd6sigg$count, hour =pd6sigg$hour, pred = pd6sigg$pred, model = "m6", whale = "Siggi")
dfp6thor <- data.frame(count = pd6thor$count, hour =pd6thor$hour, pred = pd6thor$pred, model = "m6", whale = "Thor")
dfp6thora<- data.frame(count = pd6thora$count,hour= pd6thora$hour, pred =pd6thora$pred, model = "m6", whale = "Thora")


dfp8asge <- data.frame(count = pd8asge$count, hour =pd8asge$hour, pred = pd8asge$pred, model = "m8", whale = "Asgeir")
dfp8bald <- data.frame(count = pd8bald$count, hour =pd8bald$hour, pred = pd8bald$pred, model = "m8", whale = "Balder")
dfp8eist <- data.frame(count = pd8eist$count, hour =pd8eist$hour, pred = pd8eist$pred, model = "m8", whale = "Eistla")
dfp8fred <- data.frame(count = pd8fred$count, hour =pd8fred$hour, pred = pd8fred$pred, model = "m8", whale = "Frederik")
dfp8frey <- data.frame(count = pd8frey$count, hour =pd8frey$hour, pred = pd8frey$pred, model = "m8", whale = "Freya")
dfp8frid <- data.frame(count = pd8frid$count, hour =pd8frid$hour, pred = pd8frid$pred, model = "m8", whale = "Frida")
dfp8helg <- data.frame(count = pd8helg$count, hour =pd8helg$hour, pred = pd8helg$pred, model = "m8", whale = "Helge")
dfp8he18 <- data.frame(count = pd8he18$count, hour =pd8he18$hour, pred = pd8he18$pred, model = "m8", whale = "Helge18")
dfp8kyrr <- data.frame(count = pd8kyrr$count, hour =pd8kyrr$hour, pred = pd8kyrr$pred, model = "m8", whale = "Kyrri")
dfp8mara <- data.frame(count = pd8mara$count, hour =pd8mara$hour, pred = pd8mara$pred, model = "m8", whale = "Mara")
dfp8mutt <- data.frame(count = pd8mutt$count, hour =pd8mutt$hour, pred = pd8mutt$pred, model = "m8", whale = "Mutti")
dfp8nemo <- data.frame(count = pd8nemo$count, hour =pd8nemo$hour, pred = pd8nemo$pred, model = "m8", whale = "Nemo")
dfp8sigg <- data.frame(count = pd8sigg$count, hour =pd8sigg$hour, pred = pd8sigg$pred, model = "m8", whale = "Siggi")
dfp8thor <- data.frame(count = pd8thor$count, hour =pd8thor$hour, pred = pd8thor$pred, model = "m8", whale = "Thor")
dfp8thora<- data.frame(count = pd8thora$count,hour= pd8thora$hour, pred =pd8thora$pred, model = "m8", whale = "Thora")





ppd5 <- rbind.data.frame(dfp5asge,dfp5bald,dfp5eist,dfp5fred,dfp5frey,dfp5frid,dfp5helg,dfp5he18,dfp5kyrr,dfp5mara,dfp5mutt,dfp5nemo, 
                         dfp5sigg,dfp5thor,dfp5thora)

ppd6 <- rbind.data.frame(dfp6asge,dfp6bald,dfp6eist,dfp6fred,dfp6frey,dfp6frid,dfp6helg,dfp6he18,dfp6kyrr,dfp6mara,dfp6mutt,dfp6nemo, 
                         dfp6sigg,dfp6thor,dfp6thora)

ppd8 <- rbind.data.frame(dfp8asge,dfp8bald,dfp8eist,dfp8fred,dfp8frey,dfp8frid,dfp8helg,dfp8he18,dfp8kyrr,dfp8mara,dfp8mutt,dfp8nemo, 
                         dfp8sigg,dfp8thor,dfp8thora)

pp5 <- ggplot(ppd5, aes(x = hour, y = pred)) +
  geom_line( col = "red") +
  geom_point(aes(x = hour, y = count), alpha = .2) +
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours after tagging", y = "Counts") +
  labs(x = "z", y = "CDF(z)")

pp5

pp6 <- ggplot(ppd6, aes(x = hour, y = pred)) +
  geom_line( col = "red") +
  geom_point(aes(x = hour, y = count), alpha = .2) +
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours after tagging", y = "Counts") +
  labs(x = "z", y = "CDF(z)")
pp6


pp8 <- ggplot(ppd8, aes(x = hour, y = pred)) +
  geom_line( col = "red") +
  geom_point(aes(x = hour, y = count), alpha = .2) +
  facet_wrap(~whale, ncol = 3) +
  labs(x = "Hours after tagging", y = "Counts") +
  labs(x = "z", y = "CDF(z)")
pp8




head(data.frame(allpredm5))

ggplot(data1, aes(x = time_since_tagging, y = cBuzz)) + geom_line() + facet_wrap(~Ind)

head(predm5)

a123 <- ggplot(ppd5, aes(x = time_since_tagging, y = preds)) + geom_line(aes(col = Whale)) + facet_wrap(~model) + geom_point(aes(x = 51, y = point, col = Whale))




head(z1asge)


z1sasge <- data.frame(z1s = z1asge[1:length(z1asge)-1], z2s = z1asge[2:length(z1asge)], whale = "Asgeir"  , model = "m1")
z1sbald <- data.frame(z1s = z1bald[1:length(z1bald)-1], z2s = z1bald[2:length(z1bald)], whale = "Balder"  , model = "m1")
z1seist <- data.frame(z1s = z1eist[1:length(z1eist)-1], z2s = z1eist[2:length(z1eist)], whale = "Eistla"  , model = "m1")
z1sfred <- data.frame(z1s = z1fred[1:length(z1fred)-1], z2s = z1fred[2:length(z1fred)], whale = "Frederik", model = "m1")
z1sfrey <- data.frame(z1s = z1frey[1:length(z1frey)-1], z2s = z1frey[2:length(z1frey)], whale = "Freya"   , model = "m1")
z1sfrid <- data.frame(z1s = z1frid[1:length(z1frid)-1], z2s = z1frid[2:length(z1frid)], whale = "Frida"   , model = "m1")
z1shelg <- data.frame(z1s = z1helg[1:length(z1helg)-1], z2s = z1helg[2:length(z1helg)], whale = "Helge"   , model = "m1")
z1she18 <- data.frame(z1s = z1he18[1:length(z1he18)-1], z2s = z1he18[2:length(z1he18)], whale = "Helge18" , model = "m1")
z1skyrr <- data.frame(z1s = z1kyrr[1:length(z1kyrr)-1], z2s = z1kyrr[2:length(z1kyrr)], whale = "Kyrri"   , model = "m1")
z1smara <- data.frame(z1s = z1mara[1:length(z1mara)-1], z2s = z1mara[2:length(z1mara)], whale = "Mara"    , model = "m1")
z1smutt <- data.frame(z1s = z1mutt[1:length(z1mutt)-1], z2s = z1mutt[2:length(z1mutt)], whale = "Mutti"   , model = "m1")
z1snemo <- data.frame(z1s = z1nemo[1:length(z1nemo)-1], z2s = z1nemo[2:length(z1nemo)], whale = "Nemo"    , model = "m1")
z1ssigg <- data.frame(z1s = z1sigg[1:length(z1sigg)-1], z2s = z1sigg[2:length(z1sigg)], whale = "Siggi"   , model = "m1")
z1sthor <- data.frame(z1s = z1thor[1:length(z1thor)-1], z2s = z1thor[2:length(z1thor)], whale = "Thor"    , model = "m1")
z1sthora<- data.frame(z1s = z1thora[1:length(z1thora)-1], z2s = z1thora[2:length(z1thora)], whale = "Thora"   , model = "m1")


z2sasge <- data.frame(z1s = z2asge[1:length(z2asge)-1]  , z2s = z2asge[2:length(z2asge)]  , whale = "Asgeir"  , model = "m2")
z2sbald <- data.frame(z1s = z2bald[1:length(z2bald)-1]  , z2s = z2bald[2:length(z2bald)]  , whale = "Balder"  , model = "m2")
z2seist <- data.frame(z1s = z2eist[1:length(z2eist)-1]  , z2s = z2eist[2:length(z2eist)]  , whale = "Eistla"  , model = "m2")
z2sfred <- data.frame(z1s = z2fred[1:length(z2fred)-1]  , z2s = z2fred[2:length(z2fred)]  , whale = "Frederik", model = "m2")
z2sfrey <- data.frame(z1s = z2frey[1:length(z2frey)-1]  , z2s = z2frey[2:length(z2frey)]  , whale = "Freya"   , model = "m2")
z2sfrid <- data.frame(z1s = z2frid[1:length(z2frid)-1]  , z2s = z2frid[2:length(z2frid)]  , whale = "Frida"   , model = "m2")
z2shelg <- data.frame(z1s = z2helg[1:length(z2helg)-1]  , z2s = z2helg[2:length(z2helg)]  , whale = "Helge"   , model = "m2")
z2she18 <- data.frame(z1s = z2he18[1:length(z2he18)-1]  , z2s = z2he18[2:length(z2he18)]  , whale = "Helge18" , model = "m2")
z2skyrr <- data.frame(z1s = z2kyrr[1:length(z2kyrr)-1]  , z2s = z2kyrr[2:length(z2kyrr)]  , whale = "Kyrri"   , model = "m2")
z2smara <- data.frame(z1s = z2mara[1:length(z2mara)-1]  , z2s = z2mara[2:length(z2mara)]  , whale = "Mara"    , model = "m2")
z2smutt <- data.frame(z1s = z2mutt[1:length(z2mutt)-1]  , z2s = z2mutt[2:length(z2mutt)]  , whale = "Mutti"   , model = "m2")
z2snemo <- data.frame(z1s = z2nemo[1:length(z2nemo)-1]  , z2s = z2nemo[2:length(z2nemo)]  , whale = "Nemo"    , model = "m2")
z2ssigg <- data.frame(z1s = z2sigg[1:length(z2sigg)-1]  , z2s = z2sigg[2:length(z2sigg)]  , whale = "Siggi"   , model = "m2")
z2sthor <- data.frame(z1s = z2thor[1:length(z2thor)-1]  , z2s = z2thor[2:length(z2thor)]  , whale = "Thor"    , model = "m2")
z2sthora<- data.frame(z1s = z2thora[1:length(z2thora)-1], z2s = z2thora[2:length(z2thora)], whale = "Thora"   , model = "m2")

z3sasge <- data.frame(z1s = z3asge[1:length(z3asge)-1]  , z2s = z3asge[2:length(z3asge)]  , whale = "Asgeir"  , model = "m3")
z3sbald <- data.frame(z1s = z3bald[1:length(z3bald)-1]  , z2s = z3bald[2:length(z3bald)]  , whale = "Balder"  , model = "m3")
z3seist <- data.frame(z1s = z3eist[1:length(z3eist)-1]  , z2s = z3eist[2:length(z3eist)]  , whale = "Eistla"  , model = "m3")
z3sfred <- data.frame(z1s = z3fred[1:length(z3fred)-1]  , z2s = z3fred[2:length(z3fred)]  , whale = "Frederik", model = "m3")
z3sfrey <- data.frame(z1s = z3frey[1:length(z3frey)-1]  , z2s = z3frey[2:length(z3frey)]  , whale = "Freya"   , model = "m3")
z3sfrid <- data.frame(z1s = z3frid[1:length(z3frid)-1]  , z2s = z3frid[2:length(z3frid)]  , whale = "Frida"   , model = "m3")
z3shelg <- data.frame(z1s = z3helg[1:length(z3helg)-1]  , z2s = z3helg[2:length(z3helg)]  , whale = "Helge"   , model = "m3")
z3she18 <- data.frame(z1s = z3he18[1:length(z3he18)-1]  , z2s = z3he18[2:length(z3he18)]  , whale = "Helge18" , model = "m3")
z3skyrr <- data.frame(z1s = z3kyrr[1:length(z3kyrr)-1]  , z2s = z3kyrr[2:length(z3kyrr)]  , whale = "Kyrri"   , model = "m3")
z3smara <- data.frame(z1s = z3mara[1:length(z3mara)-1]  , z2s = z3mara[2:length(z3mara)]  , whale = "Mara"    , model = "m3")
z3smutt <- data.frame(z1s = z3mutt[1:length(z3mutt)-1]  , z2s = z3mutt[2:length(z3mutt)]  , whale = "Mutti"   , model = "m3")
z3snemo <- data.frame(z1s = z3nemo[1:length(z3nemo)-1]  , z2s = z3nemo[2:length(z3nemo)]  , whale = "Nemo"    , model = "m3")
z3ssigg <- data.frame(z1s = z3sigg[1:length(z3sigg)-1]  , z2s = z3sigg[2:length(z3sigg)]  , whale = "Siggi"   , model = "m3")
z3sthor <- data.frame(z1s = z3thor[1:length(z3thor)-1]  , z2s = z3thor[2:length(z3thor)]  , whale = "Thor"    , model = "m3")
z3sthora<- data.frame(z1s = z3thora[1:length(z3thora)-1], z2s = z3thora[2:length(z3thora)], whale = "Thora"   , model = "m3")

z4sasge <- data.frame(z1s = z4asge[1:length(z4asge)-1]  , z2s = z4asge[2:length(z4asge)]  , whale = "Asgeir"  , model = "m4")
z4sbald <- data.frame(z1s = z4bald[1:length(z4bald)-1]  , z2s = z4bald[2:length(z4bald)]  , whale = "Balder"  , model = "m4")
z4seist <- data.frame(z1s = z4eist[1:length(z4eist)-1]  , z2s = z4eist[2:length(z4eist)]  , whale = "Eistla"  , model = "m4")
z4sfred <- data.frame(z1s = z4fred[1:length(z4fred)-1]  , z2s = z4fred[2:length(z4fred)]  , whale = "Frederik", model = "m4")
z4sfrey <- data.frame(z1s = z4frey[1:length(z4frey)-1]  , z2s = z4frey[2:length(z4frey)]  , whale = "Freya"   , model = "m4")
z4sfrid <- data.frame(z1s = z4frid[1:length(z4frid)-1]  , z2s = z4frid[2:length(z4frid)]  , whale = "Frida"   , model = "m4")
z4shelg <- data.frame(z1s = z4helg[1:length(z4helg)-1]  , z2s = z4helg[2:length(z4helg)]  , whale = "Helge"   , model = "m4")
z4she18 <- data.frame(z1s = z4he18[1:length(z4he18)-1]  , z2s = z4he18[2:length(z4he18)]  , whale = "Helge18" , model = "m4")
z4skyrr <- data.frame(z1s = z4kyrr[1:length(z4kyrr)-1]  , z2s = z4kyrr[2:length(z4kyrr)]  , whale = "Kyrri"   , model = "m4")
z4smara <- data.frame(z1s = z4mara[1:length(z4mara)-1]  , z2s = z4mara[2:length(z4mara)]  , whale = "Mara"    , model = "m4")
z4smutt <- data.frame(z1s = z4mutt[1:length(z4mutt)-1]  , z2s = z4mutt[2:length(z4mutt)]  , whale = "Mutti"   , model = "m4")
z4snemo <- data.frame(z1s = z4nemo[1:length(z4nemo)-1]  , z2s = z4nemo[2:length(z4nemo)]  , whale = "Nemo"    , model = "m4")
z4ssigg <- data.frame(z1s = z4sigg[1:length(z4sigg)-1]  , z2s = z4sigg[2:length(z4sigg)]  , whale = "Siggi"   , model = "m4")
z4sthor <- data.frame(z1s = z4thor[1:length(z4thor)-1]  , z2s = z4thor[2:length(z4thor)]  , whale = "Thor"    , model = "m4")
z4sthora<- data.frame(z1s = z4thora[1:length(z4thora)-1], z2s = z4thora[2:length(z4thora)], whale = "Thora"   , model = "m4")


z5sasge <- data.frame(z1s = z5asge[1:length(z5asge)-1]  , z2s = z5asge[2:length(z5asge)]  , whale = "Asgeir"  , model = "m5")
z5sbald <- data.frame(z1s = z5bald[1:length(z5bald)-1]  , z2s = z5bald[2:length(z5bald)]  , whale = "Balder"  , model = "m5")
z5seist <- data.frame(z1s = z5eist[1:length(z5eist)-1]  , z2s = z5eist[2:length(z5eist)]  , whale = "Eistla"  , model = "m5")
z5sfred <- data.frame(z1s = z5fred[1:length(z5fred)-1]  , z2s = z5fred[2:length(z5fred)]  , whale = "Frederik", model = "m5")
z5sfrey <- data.frame(z1s = z5frey[1:length(z5frey)-1]  , z2s = z5frey[2:length(z5frey)]  , whale = "Freya"   , model = "m5")
z5sfrid <- data.frame(z1s = z5frid[1:length(z5frid)-1]  , z2s = z5frid[2:length(z5frid)]  , whale = "Frida"   , model = "m5")
z5shelg <- data.frame(z1s = z5helg[1:length(z5helg)-1]  , z2s = z5helg[2:length(z5helg)]  , whale = "Helge"   , model = "m5")
z5she18 <- data.frame(z1s = z5he18[1:length(z5he18)-1]  , z2s = z5he18[2:length(z5he18)]  , whale = "Helge18" , model = "m5")
z5skyrr <- data.frame(z1s = z5kyrr[1:length(z5kyrr)-1]  , z2s = z5kyrr[2:length(z5kyrr)]  , whale = "Kyrri"   , model = "m5")
z5smara <- data.frame(z1s = z5mara[1:length(z5mara)-1]  , z2s = z5mara[2:length(z5mara)]  , whale = "Mara"    , model = "m5")
z5smutt <- data.frame(z1s = z5mutt[1:length(z5mutt)-1]  , z2s = z5mutt[2:length(z5mutt)]  , whale = "Mutti"   , model = "m5")
z5snemo <- data.frame(z1s = z5nemo[1:length(z5nemo)-1]  , z2s = z5nemo[2:length(z5nemo)]  , whale = "Nemo"    , model = "m5")
z5ssigg <- data.frame(z1s = z5sigg[1:length(z5sigg)-1]  , z2s = z5sigg[2:length(z5sigg)]  , whale = "Siggi"   , model = "m5")
z5sthor <- data.frame(z1s = z5thor[1:length(z5thor)-1]  , z2s = z5thor[2:length(z5thor)]  , whale = "Thor"    , model = "m5")
z5sthora<- data.frame(z1s = z5thora[1:length(z5thora)-1], z2s = z5thora[2:length(z5thora)], whale = "Thora"   , model = "m5")

z6sasge <- data.frame(z1s = z6asge[1:length(z6asge)-1]  , z2s = z6asge[2:length(z6asge)]  , whale = "Asgeir"  , model = "m6")
z6sbald <- data.frame(z1s = z6bald[1:length(z6bald)-1]  , z2s = z6bald[2:length(z6bald)]  , whale = "Balder"  , model = "m6")
z6seist <- data.frame(z1s = z6eist[1:length(z6eist)-1]  , z2s = z6eist[2:length(z6eist)]  , whale = "Eistla"  , model = "m6")
z6sfred <- data.frame(z1s = z6fred[1:length(z6fred)-1]  , z2s = z6fred[2:length(z6fred)]  , whale = "Frederik", model = "m6")
z6sfrey <- data.frame(z1s = z6frey[1:length(z6frey)-1]  , z2s = z6frey[2:length(z6frey)]  , whale = "Freya"   , model = "m6")
z6sfrid <- data.frame(z1s = z6frid[1:length(z6frid)-1]  , z2s = z6frid[2:length(z6frid)]  , whale = "Frida"   , model = "m6")
z6shelg <- data.frame(z1s = z6helg[1:length(z6helg)-1]  , z2s = z6helg[2:length(z6helg)]  , whale = "Helge"   , model = "m6")
z6she18 <- data.frame(z1s = z6he18[1:length(z6he18)-1]  , z2s = z6he18[2:length(z6he18)]  , whale = "Helge18" , model = "m6")
z6skyrr <- data.frame(z1s = z6kyrr[1:length(z6kyrr)-1]  , z2s = z6kyrr[2:length(z6kyrr)]  , whale = "Kyrri"   , model = "m6")
z6smara <- data.frame(z1s = z6mara[1:length(z6mara)-1]  , z2s = z6mara[2:length(z6mara)]  , whale = "Mara"    , model = "m6")
z6smutt <- data.frame(z1s = z6mutt[1:length(z6mutt)-1]  , z2s = z6mutt[2:length(z6mutt)]  , whale = "Mutti"   , model = "m6")
z6snemo <- data.frame(z1s = z6nemo[1:length(z6nemo)-1]  , z2s = z6nemo[2:length(z6nemo)]  , whale = "Nemo"    , model = "m6")
z6ssigg <- data.frame(z1s = z6sigg[1:length(z6sigg)-1]  , z2s = z6sigg[2:length(z6sigg)]  , whale = "Siggi"   , model = "m6")
z6sthor <- data.frame(z1s = z6thor[1:length(z6thor)-1]  , z2s = z6thor[2:length(z6thor)]  , whale = "Thor"    , model = "m6")
z6sthora<- data.frame(z1s = z6thora[1:length(z6thora)-1], z2s = z6thora[2:length(z6thora)], whale = "Thora"   , model = "m6")

z8sasge <- data.frame(z1s = z8asge[1:length(z8asge)-1]  , z2s = z8asge[2:length(z8asge)]  , whale = "Asgeir"  , model = "m8")
z8sbald <- data.frame(z1s = z8bald[1:length(z8bald)-1]  , z2s = z8bald[2:length(z8bald)]  , whale = "Balder"  , model = "m8")
z8seist <- data.frame(z1s = z8eist[1:length(z8eist)-1]  , z2s = z8eist[2:length(z8eist)]  , whale = "Eistla"  , model = "m8")
z8sfred <- data.frame(z1s = z8fred[1:length(z8fred)-1]  , z2s = z8fred[2:length(z8fred)]  , whale = "Frederik", model = "m8")
z8sfrey <- data.frame(z1s = z8frey[1:length(z8frey)-1]  , z2s = z8frey[2:length(z8frey)]  , whale = "Freya"   , model = "m8")
z8sfrid <- data.frame(z1s = z8frid[1:length(z8frid)-1]  , z2s = z8frid[2:length(z8frid)]  , whale = "Frida"   , model = "m8")
z8shelg <- data.frame(z1s = z8helg[1:length(z8helg)-1]  , z2s = z8helg[2:length(z8helg)]  , whale = "Helge"   , model = "m8")
z8she18 <- data.frame(z1s = z8he18[1:length(z8he18)-1]  , z2s = z8he18[2:length(z8he18)]  , whale = "Helge18" , model = "m8")
z8skyrr <- data.frame(z1s = z8kyrr[1:length(z8kyrr)-1]  , z2s = z8kyrr[2:length(z8kyrr)]  , whale = "Kyrri"   , model = "m8")
z8smara <- data.frame(z1s = z8mara[1:length(z8mara)-1]  , z2s = z8mara[2:length(z8mara)]  , whale = "Mara"    , model = "m8")
z8smutt <- data.frame(z1s = z8mutt[1:length(z8mutt)-1]  , z2s = z8mutt[2:length(z8mutt)]  , whale = "Mutti"   , model = "m8")
z8snemo <- data.frame(z1s = z8nemo[1:length(z8nemo)-1]  , z2s = z8nemo[2:length(z8nemo)]  , whale = "Nemo"    , model = "m8")
z8ssigg <- data.frame(z1s = z8sigg[1:length(z8sigg)-1]  , z2s = z8sigg[2:length(z8sigg)]  , whale = "Siggi"   , model = "m8")
z8sthor <- data.frame(z1s = z8thor[1:length(z8thor)-1]  , z2s = z8thor[2:length(z8thor)]  , whale = "Thor"    , model = "m8")
z8sthora<- data.frame(z1s = z8thora[1:length(z8thora)-1], z2s = z8thora[2:length(z8thora)], whale = "Thora"   , model = "m8")








head(z1sasge)



z1all <-rbind.data.frame(z1sasge,
                         z1sbald,
                         z1seist,
                         z1sfred,
                         z1sfrey,
                         z1sfrid,
                         z1shelg,
                         z1she18,
                         z1skyrr,
                         z1smara,
                         z1smutt,
                         z1snemo,
                         z1ssigg,
                         z1sthor,
                         z1sthora)

z2all <-rbind.data.frame(z2sasge,
                         z2sbald,
                         z2seist,
                         z2sfred,
                         z2sfrey,
                         z2sfrid,
                         z2shelg,
                         z2she18,
                         z2skyrr,
                         z2smara,
                         z2smutt,
                         z2snemo,
                         z2ssigg,
                         z2sthor,
                         z2sthora)

z3all <-rbind.data.frame(z3sasge,
                         z3sbald,
                         z3seist,
                         z3sfred,
                         z3sfrey,
                         z3sfrid,
                         z3shelg,
                         z3she18,
                         z3skyrr,
                         z3smara,
                         z3smutt,
                         z3snemo,
                         z3ssigg,
                         z3sthor,
                         z3sthora)

z4all <-rbind.data.frame(z4sasge,
                         z4sbald,
                         z4seist,
                         z4sfred,
                         z4sfrey,
                         z4sfrid,
                         z4shelg,
                         z4she18,
                         z4skyrr,
                         z4smara,
                         z4smutt,
                         z4snemo,
                         z4ssigg,
                         z4sthor,
                         z4sthora)

z5all <-rbind.data.frame(z5sasge,
                         z5sbald,
                         z5seist,
                         z5sfred,
                         z5sfrey,
                         z5sfrid,
                         z5shelg,
                         z5she18,
                         z5skyrr,
                         z5smara,
                         z5smutt,
                         z5snemo,
                         z5ssigg,
                         z5sthor,
                         z5sthora)

z6all <-rbind.data.frame(z6sasge,
                         z6sbald,
                         z6seist,
                         z6sfred,
                         z6sfrey,
                         z6sfrid,
                         z6shelg,
                         z6she18,
                         z6skyrr,
                         z6smara,
                         z6smutt,
                         z6snemo,
                         z6ssigg,
                         z6sthor,
                         z6sthora)

z8all <-rbind.data.frame(z8sasge,
                         z8sbald,
                         z8seist,
                         z8sfred,
                         z8sfrey,
                         z8sfrid,
                         z8shelg,
                         z8she18,
                         z8skyrr,
                         z8smara,
                         z8smutt,
                         z8snemo,
                         z8ssigg,
                         z8sthor,
                         z8sthora)




pz1 <- ggplot(z1all, aes(x = z1s, y = z2s)) +
      geom_point(size = .5) +
      facet_wrap(~whale, ncol = 3)   +
      labs(x = expression(z[j]), y = expression(z[j+1]))

pz2 <- ggplot(z3all, aes(x = z1s, y = z2s)) +
  geom_point(size = .5) +
  facet_wrap(~whale, ncol = 3)   +
  labs(x = expression(z[j]), y = expression(z[j+1]))


pz3 <- ggplot(z4all, aes(x = z1s, y = z2s)) +
  geom_point(size = .5) +
  facet_wrap(~whale, ncol = 3)   +
  labs(x = expression(z[j]), y = expression(z[j+1]))

pz4 <- ggplot(z4all, aes(x = z1s, y = z2s)) +
  geom_point(size = .5) +
  facet_wrap(~whale, ncol = 3)   +
  labs(x = expression(z[j]), y = expression(z[j+1]))

pz5 <- ggplot(z5all, aes(x = z1s, y = z2s)) +
  geom_point(size = .5) +
  facet_wrap(~whale, ncol = 3)   +
  labs(x = expression(z[j]), y = expression(z[j+1]))

pz6 <- ggplot(z6all, aes(x = z1s, y = z2s)) +
  geom_point(size = .5) +
  facet_wrap(~whale, ncol = 3)   +
  labs(x = expression(z[j]), y = expression(z[j+1]))

pz8 <- ggplot(z8all, aes(x = z1s, y = z2s)) +
  geom_point(size = .5) +
  facet_wrap(~whale, ncol = 3)   +
  labs(x = expression(z[j]), y = expression(z[j+1]))
pz1
pdf("pz8.pdf", width = 6, height = 6.5)

pz8
dev.off()












#Every whale for themselves!!!!!

#masge <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = asgedata , family = "poisson")
#mbald <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = balddata , family = "poisson")
#meist <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = eistdata , family = "poisson")
#mfred <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = freddata , family = "poisson")
#mfrey <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = freydata , family = "poisson")
#mfrid <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = friddata , family = "poisson")
#mhelg <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = helgdata , family = "poisson")
#mhe18 <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = he18data , family = "poisson")
#mkyrr <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = kyrrdata , family = "poisson")
#mmara <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = maradata , family = "poisson")
#mmutt <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = muttdata , family = "poisson")
#mnemo <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = nemodata , family = "poisson")
#msigg <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = siggdata , family = "poisson")
#mthor <- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = thordata , family = "poisson")
#mthora<- glm(Buzz ~  I(exp(-time_since_tagging)) +ns(Depth, 3) , data = thoradata, family = "poisson")
#
#summary(masge)
#summary(mbald)
#summary(meist)
#summary(mfred)
#summary(mfrey)
#summary(mfrid)
#summary(mhelg)
#summary(mhe18)
#summary(mkyrr)
#summary(mmara)
#summary(mmutt)
#summary(mnemo)
#summary(msigg)
#summary(mthor)
#summary(mthora)
#
#m1asge <- glm(Buzz ~ I(exp(-time_since_tagging)), data = asgedata , family = "poisson")
#m1bald <- glm(Buzz ~ I(exp(-time_since_tagging)), data = balddata , family = "poisson")
#m1eist <- glm(Buzz ~ I(exp(-time_since_tagging)), data = eistdata , family = "poisson")
#m1fred <- glm(Buzz ~ I(exp(-time_since_tagging)), data = freddata , family = "poisson")
#m1frey <- glm(Buzz ~ I(exp(-time_since_tagging)), data = freydata , family = "poisson")
#m1frid <- glm(Buzz ~ I(exp(-time_since_tagging)), data = friddata , family = "poisson")
#m1helg <- glm(Buzz ~ I(exp(-time_since_tagging)), data = helgdata , family = "poisson")
#m1he18 <- glm(Buzz ~ I(exp(-time_since_tagging)), data = he18data , family = "poisson")
#m1kyrr <- glm(Buzz ~ I(exp(-time_since_tagging)), data = kyrrdata , family = "poisson")
#m1mara <- glm(Buzz ~ I(exp(-time_since_tagging)), data = maradata , family = "poisson")
#m1mutt <- glm(Buzz ~ I(exp(-time_since_tagging)), data = muttdata , family = "poisson")
#m1nemo <- glm(Buzz ~ I(exp(-time_since_tagging)), data = nemodata , family = "poisson")
#m1sigg <- glm(Buzz ~ I(exp(-time_since_tagging)), data = siggdata , family = "poisson")
#m1thor <- glm(Buzz ~ I(exp(-time_since_tagging)), data = thordata , family = "poisson")
#m1thora<- glm(Buzz ~ I(exp(-time_since_tagging)), data = thoradata, family = "poisson")
#
#
#summary(m1asge)
#summary(m1bald)
#summary(m1eist)
#summary(m1fred)
#summary(m1frey)
#summary(m1frid)
#summary(m1helg)
#summary(m1he18)
#summary(m1kyrr)
#summary(m1mara)
#summary(m1mutt)
#summary(m1nemo)
#summary(m1sigg)
#summary(m1thor)
#summary(m1thora)
#
#
#
#
#
#plot(predict(masge , newdata =  data.frame(time_since_tagging = seq(1,max(asgedata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1asge , newdata = data.frame(time_since_tagging = seq(1,max(asgedata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mbald , newdata =  data.frame(time_since_tagging = seq(1,max(balddata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1bald , newdata = data.frame(time_since_tagging = seq(1,max(balddata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(meist , newdata =  data.frame(time_since_tagging = seq(1,max(eistdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1eist , newdata = data.frame(time_since_tagging = seq(1,max(eistdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mfred , newdata =  data.frame(time_since_tagging = seq(1,max(freddata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1fred , newdata = data.frame(time_since_tagging = seq(1,max(freddata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mfrey , newdata =  data.frame(time_since_tagging = seq(1,max(freydata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1frey , newdata = data.frame(time_since_tagging = seq(1,max(freydata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mfrid , newdata =  data.frame(time_since_tagging = seq(1,max(friddata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1frid , newdata = data.frame(time_since_tagging = seq(1,max(friddata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mhelg , newdata =  data.frame(time_since_tagging = seq(1,max(helgdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1helg , newdata = data.frame(time_since_tagging = seq(1,max(helgdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mhe18 , newdata =  data.frame(time_since_tagging = seq(1,max(he18data$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1he18 , newdata = data.frame(time_since_tagging = seq(1,max(he18data$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mkyrr , newdata =  data.frame(time_since_tagging = seq(1,max(kyrrdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1kyrr , newdata = data.frame(time_since_tagging = seq(1,max(kyrrdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mmara  , newdata = data.frame(time_since_tagging = seq(1,max(maradata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1mara  , newdata =data.frame(time_since_tagging = seq(1,max(maradata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mmutt , newdata =  data.frame(time_since_tagging = seq(1,max(muttdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1mutt , newdata = data.frame(time_since_tagging = seq(1,max(muttdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(mnemo , newdata =  data.frame(time_since_tagging = seq(1,max(nemodata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1nemo , newdata = data.frame(time_since_tagging = seq(1,max(nemodata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#plot(predict(msigg  , newdata =  data.frame(time_since_tagging = seq(1,max(siggdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1sigg  , newdata = data.frame(time_since_tagging = seq(1,max(siggdata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#
#plot(predict(mthor , newdata = data.frame(time_since_tagging = seq(1,max(thordata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1thor , newdata =data.frame(time_since_tagging = seq(1,max(thordata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#
#
#plot(predict(mthora,data.frame(time_since_tagging = seq(1,max(thoradata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")
#plot(predict(m1thora, data.frame(time_since_tagging = seq(1,max(thoradata$time_since_tagging), 1), Depth = 500), type = "response")*60, type = "l")

max(thoradata$time_since_tagging)

