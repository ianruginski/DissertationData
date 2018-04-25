# Loading Required Packages -----------------------------------------------
require(ggplot2)
require(psych)
require(car)
require(dplyr)
require(lavaan)
require(semPlot)
require(readr)
# CLEANING EXPERIMENTAL DATA -----------------------------------------------------------

setwd("D:/CNS/Dissertation/Data Analysis")
#read in data
clean <- read.csv("ian_diss_cleaneddata.csv")
pt <- read.csv("PerspectiveTakingDataRaw.csv")

#cleaning PT data 
#NOTE: If a participant leaves an item blank, they should be assigned an angular deviation of 90? for that item (90? represents chance performance, given that the angular deviation can vary from 0 to 180?) (this was done when entering data)

#If the angular deviation on any item exceeds 180?, it should be subtracted from 360? to determine the smallest deviation possible. This is because the angular deviation between two angles does not exceed 180 degrees. 

pt$SOTe1c <- ifelse(pt$SOTe1>=180, 360-pt$SOTe1, pt$SOTe1)
pt$SOTe2c <- ifelse(pt$SOTe2>=180, 360-pt$SOTe2, pt$SOTe2)
pt$SOTe3c <- ifelse(pt$SOTe3>=180, 360-pt$SOTe3, pt$SOTe3)
pt$SOTe4c <- ifelse(pt$SOTe4>=180, 360-pt$SOTe4, pt$SOTe4)
pt$SOTe5c <- ifelse(pt$SOTe5>=180, 360-pt$SOTe5, pt$SOTe5)
pt$SOTe6c <- ifelse(pt$SOTe6>=180, 360-pt$SOTe6, pt$SOTe6)
pt$SOTe7c <- ifelse(pt$SOTe7>=180, 360-pt$SOTe7, pt$SOTe7)
pt$SOTe8c <- ifelse(pt$SOTe8>=180, 360-pt$SOTe8, pt$SOTe8)
pt$SOTe9c <- ifelse(pt$SOTe9>=180, 360-pt$SOTe9, pt$SOTe9)
pt$SOTe10c <- ifelse(pt$SOTe10>=180, 360-pt$SOTe10, pt$SOTe10)
pt$SOTe11c <- ifelse(pt$SOTe11>=180, 360-pt$SOTe11, pt$SOTe11)
pt$SOTe12c <- ifelse(pt$SOTe12>=180, 360-pt$S2Te10, pt$SOTe12)

pt <- select(pt, SOTe1c:SOTe12c) #subset newly coded items
require(matrixStats)
pt$SOTsd <- rowSds(as.matrix(pt)) #calculate within person variability?
pt$SOTmean <- rowMeans(select(pt, SOTe1c:SOTe12c))


#add PT data to full dataframe
clean <- cbind(clean, pt$SOTmean) 
clean <- cbind(clean, pt$SOTsd)
#change names
colnames(clean)[87] <- "SOTmean"
colnames(clean)[88] <- "SOTsd"

write.csv(clean, "ian_diss_cleaneddatawithpt.csv")

#lets get the distributions of our residuals to determine estimator... probably have to use MLR?

#lets recode the mode of travel variables
#Q11hen traveling to places away from your neighborhood, do you travel primarily by:
#Q11 Walking (1), Biking (2), Car driver (3), Car passenger (4), Bus or train (5)
clean$Q11 <- as.factor(car::recode(clean$Q11, "1='Walking'; 2='Biking'; 3='Driver'; 4='Passenger'; 5='Public Transit'"))

#Q12 When traveling by car with other people, are you usually the driver or the passenger?
#Q12 Driver always or nearly always (1), Driver usually (2), Both Equally (3), Passenger usually (4), Passenger always or nearly always (5)
clean$Q12 <- as.factor(car::recode(clean$Q12, "1='Driver nearly always'; 2='Drive usually'; 3='Both equally'; 4='Passenger usually'; 5='Passenger nearly always'"))

#Q13 Do you have GPS Yes (1) No (2)
clean$Q13 <- as.factor(car::recode(clean$Q13, "1='Yes'; 2='No'"))

#Q14 If yes, how often do you use the GPS Never(1), Rarely(2), Sometimes(3), Often(4), Always(5)
clean$Q14 <- as.factor(car::recode(clean$Q14, "1='Never'; 2='Rarely'; 3='Sometimes'; 4='Often'; 5='Always'"))

# Summarizing data ----------------------------------------------------------------
require(lavaan)
#make quick summary variable for mobility and navigation anxiety items
clean$Mobility <- rowSums(select(clean,Q1,Q4,Q5,Q9:Q10), na.rm=TRUE)/5
clean$NavAnx <- rowSums(select(clean,Q16,Q19,Q29,Q27,Q32), na.rm=TRUE)/5
clean$Wanderlust <- rowSums(select(clean,Q45,Q42,Q18,Q20,Q15), na.rm=TRUE)/5
clean$NavAbility <- rowSums(select(clean,Q57,Q49,Q50,Q52, Q54, Q51, Q47), na.rm=TRUE)/7

require(psych)
describe(clean$Age)
table(clean$Ethnicity)
# Visualizing and Examining Data --------------------------------------------------------
#lets get the distributions of our different mobility measures...

require(psych)
describe(clean$Q10)

#another package
require(sjPlot)
require(dplyr)
mobility <- select(clean, Q1, Q4, Q5, Q9, Q10)
sjt.itemanalysis(mobility,  show.kurtosis = TRUE) #open in new window, copy/paste to word
psych::alpha(mobility)


#do same for navigation anxiety.
mobility.navanx <- select(clean, Q16, Q19, Q29, Q27, Q25, Q32)
sjt.itemanalysis(mobility.navanx,  show.kurtosis = TRUE) #open in new window, copy/paste to word
psych::alpha(mobility.navanx)


#do the same for the CFA data (read in data first...)
mturkdata <- read.csv("MobilitySurveyData1.2_CleanwithTraits.csv")
require(sjPlot)
require(dplyr)
mobility.cfa <- select(mturkdata, Q1, Q4, Q5, Q9, Q10)
sjt.itemanalysis(mobility.cfa,  show.kurtosis = TRUE) #open in new window, copy/paste to word

#do same for navanx for MTurk
mobility.navanx.cfa <- select(mturkdata, Q16, Q19, Q29, Q27, Q25, Q32)
sjt.itemanalysis(mobility.navanx.cfa,  show.kurtosis = TRUE) #open in new window, copy/paste to word
psych::alpha(mobility.navanx.cfa)

#now check nav anxiety in a model... 


require(ReporteRs)

options(scipen=1000)
require(ggplot2)
theme_set(theme_classic(base_size=18))
require(scales)
ggplot(clean, aes(UTdist)) + geom_density(fill="blue", alpha=.2) + 
  labs(title="Density plot", 
       subtitle="Distance traveled in Utah",
       x="Distance sum from U of U (miles)") + scale_y_continuous(labels = scales::percent) +
  xlim(0, 5000)
ggsave("mobilitydistancedistribution.png", width=8, height=6)

ggplot(clean, aes(Mobility)) + geom_density(fill="blue", alpha=.2) + 
  labs(title="Density plot", 
       subtitle="Average Mobility (min = 1, max = 6.6)",
       x="Mobility") + scale_y_continuous(labels = scales::percent) +
  xlim(1,6.6)
ggsave("mobilitydistribution.png", width=8, height=6)

ggplot(clean, aes(UTrural)) + geom_density(fill="blue", alpha=.2) + 
  labs(title="Density plot", 
       subtitle="Rural places visited in Utah",
       x="Places Visited") + scale_y_continuous(labels = scales::percent)  +
  xlim(0,25)
ggsave("UTrural_distribution.png", width=8, height=6)

ggplot(clean, aes(UTurban)) + geom_density(fill="blue", alpha=.2) + 
  labs(title="Density plot", 
       subtitle="Urban places visited in Utah",
       x="Places Visited") + scale_y_continuous(labels = scales::percent)  +
  xlim(0,25)
ggsave("UTurban_distribution.png", width=8, height=6)

ggplot(clean, aes(UTtotal)) + geom_density(fill="blue", alpha=.2) + 
  labs(title="Density plot", 
       subtitle="Total places visited in Utah",
       x="Places Visited") + scale_y_continuous(labels = scales::percent)  +
  xlim(0,50)
ggsave("UTurban_distribution.png", width=8, height=6)


#plot GPS counts...
clean$Q14 <- factor(clean$Q14, levels = c("Never", "Rarely", "Sometimes", "Often", "Always"))
ggplot(clean, aes(x=Q14, fill=Q14)) + geom_bar(alpha=.8) +
  labs(title="Count Plot", 
       subtitle="GPS Use",
       x="Self-reported GPS Use") + guides(fill=guide_legend(title="Likert scale")) + 
  scale_y_continuous(breaks=seq(0, 80, 15)) + ylab("Number of participants")
ggsave("GPSuse_distribution.png", width=8, height=6)

#plot UT dist counts...
ggplot(clean, aes(x=UTdist)) + geom_bar(alpha=.8) +
  labs(title="Histogram", 
       subtitle="Total Distance to Places Visited in Utah",
       x="Distance (miles)") +
  scale_y_continuous(breaks=seq(0, 4000, 500)) + ylab("Number of participants")
ggsave("GPSuse_distribution.png", width=8, height=6)
                     


#plot distrib by GPS factor...

ggplot(clean, aes(MRT)) + geom_density(aes(fill=Q14), alpha=.6) + 
  labs(title="Density plot", 
       subtitle="MRT Distributions by self-reported GPS usage",
       x="Mental Rotation Ability") + scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=seq(-80, 80, 20) , limits=c(-80,100)) + guides(fill=guide_legend(title="GPS Use"))
ggsave("MRTbyGPS.png", width=8, height=6)

ggplot(clean, aes(BetweenRtPt)) + geom_density(aes(fill=Q14), alpha=.6) + 
  labs(title="Density plot", 
       subtitle="Between route pointing distributions by self-reported GPS usage",
       x="Between Route Pointing Error") + scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=seq(0, 90, 15) , limits=c(0,90)) + guides(fill=guide_legend(title="GPS Use"))
ggsave("BtwPointingbyGPS.png", width=8, height=6)


ggplot(clean, aes(WithinRtPt)) + geom_density(aes(fill=Q14), alpha=.6) + 
  labs(title="Density plot", 
       subtitle="Within route pointing distributions by self-reported GPS usage",
       x="Within Route Pointing Error") + scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=seq(0, 90, 15) , limits=c(0,90)) + guides(fill=guide_legend(title="GPS Use"))
ggsave("WithinPointingbyGPS.png", width=8, height=6)

ggplot(clean, aes(Map)) + geom_density(aes(fill=Q14), alpha=.6) + 
  labs(title="Density plot", 
       subtitle="Cognitive map distributions by self-reported GPS usage",
       x="Cognitive Map Error (cartesian coordinates)") + scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=seq(0, 600, 50) , limits=c(0,600)) + guides(fill=guide_legend(title="GPS Use"))


#check assumptions 
qqPlot(clean$MRT)
qqPlot(clean$Map)
hist(clean$MRT, breaks=20, col="blue")
hist(clean$BetweenRtPt, breaks=20, col="blue") #skewed
hist(clean$WithinRtPt, breaks=20, col="blue") #skewed
hist(clean$Mobility, breaks=20, col="blue") #skewed
hist(clean$NavAnx, breaks=20, col="blue") #skewed

#nice plots. fix for presentation
theme_set(theme_classic(base_size=18))

ggplot(clean, aes(UTtotal,MRT)) + geom_point(alpha=.2) + geom_smooth(formula = MRT~UTtotal, na.rm=TRUE) +
  labs(title="Density plot", 
       subtitle="Total places visited in Utah",
       x="Total places visited in Utah",
       y="MRT") +
  xlim(0,41)
ggsave("UTurban_distribution.png", width=8, height=6)

ggplot(clean, aes(Mobility,MRT)) + geom_point(alpha=.2) + geom_smooth(method="lm") +
  labs(title="Does mobility predict mental rotation ability?", 
       subtitle="",
       x="Mobility",
       y="MRT") +
  xlim(1,6.6)

ggplot(clean, aes(NavAnx,BetweenRtPt)) + geom_point(alpha=.5) + geom_smooth(method="lm", size=1.3) +
  labs(title="Does navigation anxiety predict between route pointing ability?", 
       subtitle="",
       x="Navigation Anxiety",
       y="Between Route Pointing Ability") +
  xlim(1,5)

#plot by factor... 


#plot size: 6.79 x 8.49
#labels each datapoint id.method = "mahal", labels=c(1:33), id.n = length(1:33)
#note: min possible average on mobility would be 1, max possible average on mobility would be 6.6...
7+8+8+5+5 
require(ggplot2)
theme_set(theme_classic(base_size = 24))
ggplot(clean, aes(x=Mobility)) + geom_density(alpha=.3, fill="blue") + scale_x_continuous(limits = c(1, 6.6))



scatterplot(SOTmean ~ as.numeric(Q14), data=clean,  xlab="Mobility", ylab="Perspective Taking Angular Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(WithinRtPt ~ UTdist, data=clean,  xlab="Perspective Taking Angular Error", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(samediff.between.abs.error ~ SOTmean, data=clean,  xlab="Perspective Taking Angular Error", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(map.error ~ SOTmean, data=clean,  xlab="Perspective Taking Angular Error", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) 



scatterplot(MRT.SDT.style ~ Mobility, data=clean,  xlab="Mobility", ylab="Mental Rotation Ability",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(map.error ~ Mobility, data=clean, id.method = "mahal", labels=c(1:33), id.n = length(1:33)) #labels each datapoint

scatterplot(samediff.within.abs.error ~ Mobility, data=clean, xlab="Mobility", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ Mobility, data=clean, xlab="Mobility", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ Mobility, data=clean, xlab="Mobility", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ samediff.within.abs.error, data=clean, xlab="Within Route Pointing Error", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(Map ~ UTrural, data=clean, xlab="Mobility", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(BetweenRtPt ~ NavAnx, clean, xlab="Navigation Anxiety", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(Map ~ NavAnx, clean, xlab="Navigation Anxiety", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint


scatterplot(Map ~ MRT, clean, xlab="Mental Rotation Ability", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint

scatterplot(BetweenRtPt ~ MRT, clean, xlab="Mental Rotation Ability", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint

png("MrtPT.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(SOTmean ~ MRT, clean, xlab="Mental Rotation Ability", ylab="Perspective Taking Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

scatterplot(samediff.within.abs.error ~ MRT.SDT.style, clean, xlab="Mental Rotation Ability", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint

png("PTmobility.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(SOTmean ~ Mobility, clean, xlab="Mobility", ylab="Perspective Taking Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("MRTmobility.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(MRT ~ Mobility, clean, xlab="Mobility", ylab="Mentral Rotation Ability",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("BtwPtmobility.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(BetweenRtPt ~ Mobility, clean, xlab="Mobility", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("WithinPtmobility.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(WithinRtPt ~ Mobility, clean, xlab="Mobility", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("Mapmobility.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(Map ~ Mobility, clean, xlab="Mobility", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("BtwPtAnxiety.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(BetweenRtPt ~ NavAnx, clean, xlab="Navigation Anxiety", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()


png("WithinPtPT.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(WithinRtPt ~ SOTmean, clean, xlab="Perspective Taking Error", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("WithinPtMRT.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(WithinRtPt ~ MRT, clean, xlab="Mental Rotation Ability", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

png("UTdistMobility.png", width = 8, height = 6, units = 'in', res = 300)
scatterplot(UTdist ~ Mobility, clean, xlab="Mobility", ylab="Total distance (miles) to places visited in Utah",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint
dev.off()

scatterplot(BetweenRtPt ~ MRT, clean, xlab="Mental Rotation Ability", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint

# Latent Profile Analysis and Plotting ------------------------------------
#latent profile analysis
#https://cran.r-project.org/web/packages/mclust/mclust.pdf
require(mclust)
clean_ss <-subset(clean,select=c(BetweenRtPt,WithinRtPt))
clean_ss <- na.omit(clean_ss)
mod1 = Mclust(clean_ss, 3)
summary(mod1)
png("mclust3groups.png", width = 8, height = 6, units = 'in', res = 300)
plot(mod1, xlab="Between Route Pointing Error", ylab="Within Route Pointing Error")
dev.off()
0
cl

#k means clustering
clean_ss <-subset(clean,select=c(samediff.within.abs.error, samediff.between.abs.error))
k.means.fit <- kmeans(clean_ss, 3) # k = 3
attributes(k.means.fit)
k.means.fit$cluster
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(clean_ss, nc=6) 

require(cluster)
clusplot(clean_ss, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# Modeling ----------------------------------------------------------------

#first lets redo the CFA....
require(lavaan)
mobility.model.full <- 'Mobility  =~ NA*Q1 + Q4 + Q5 + Q9 + Q10
Mobility ~~ 1*Mobility
Q9 ~~ Q10' 
mobility.fit.full <- cfa(mobility.model.full, estimator = "MLR", data = clean, missing="FIML")
summary(mobility.fit.full, fit.measures = TRUE,  rsquare=TRUE, standardized=TRUE)

#removing Q1..  
mobility.model.full <- 'Mobility  =~ NA*Q4 + Q5 + Q9 + Q10
Mobility ~~ 1*Mobility
Q9 ~~ Q10' 
mobility.fit.full <- cfa(mobility.model.full, estimator = "MLR", data = clean, missing="FIML")
summary(mobility.fit.full, fit.measures = TRUE,  rsquare=TRUE, standardized=TRUE)

#calculate cronbach's alpha...
require(dplyr)
require(psych)
alphatravel <- select(clean, Q4, Q5, Q9, Q10)
psych::alpha(alphatravel)

#check item distibutions...


model <- lm(SOTmean ~ as.numeric(Q14), clean)
model <- lm(MRT ~ Q11, clean)

model <- lm(MRT.SDT.style ~ Mobility, clean)
model <- lm(samediff.within.abs.error ~ Mobility, clean)
model <- lm(samediff.between.abs.error ~ Mobility, clean)

model <- lm(SOTmean ~ Mobility, clean)
model <- lm(map.error ~ Mobility, clean)
model <- lm(samediff.between.abs.error ~ NavAnx, clean)
model <- summary(lm(MRT ~ as.numeric(Q14), clean))
summary(lm(MRT ~ as.numeric(Q14), clean))
model.2 <- lm(samediff.between.abs.error ~ NavAnx + Mobility, clean)
anova(model, model.2) #model comparison

model <- lm(UTdist ~ Mobilityc, clean)


summary(model)

scatterplot(Residuals ~ T1, data=clean)

require(lavaan)

#other measures of mobility..

model.mobility.pt<- '
SOTmean~UTrural'
fit.mobility.pt <- cfa(model.mobility.pt, data = clean)
summary(fit.mobility.pt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.mrt<- '
MRT~UTrural'
fit.mobility.mrt <- cfa(model.mobility.mrt, data = clean)
summary(fit.mobility.mrt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.btw<- '
BetweenRtPt~UTrural'
fit.mobility.btw <- cfa(model.mobility.btw, data = clean)
summary(fit.mobility.btw, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.with<- '
WithinRtPt~UTrural'
fit.mobility.with <- cfa(model.mobility.with, data = clean)
summary(fit.mobility.with, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.dist<- '
Distance~UTrural'
fit.mobility.dist <- cfa(model.mobility.dist, data = clean)
summary(fit.mobility.dist, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.map<- '
Map~UTrural'
fit.mobility.map <- cfa(model.mobility.map, data = clean)
summary(fit.mobility.map, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

#do same for urban...
model.mobility.pt<- '
SOTmean~UTurban'
fit.mobility.pt <- cfa(model.mobility.pt, data = clean)
summary(fit.mobility.pt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.mrt<- '
MRT~UTurban'
fit.mobility.mrt <- cfa(model.mobility.mrt, data = clean)
summary(fit.mobility.mrt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.btw<- '
BetweenRtPt~UTurban'
fit.mobility.btw <- cfa(model.mobility.btw, data = clean)
summary(fit.mobility.btw, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.with<- '
WithinRtPt~UTurban'
fit.mobility.with <- cfa(model.mobility.with, data = clean)
summary(fit.mobility.with, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.dist<- '
Distance~UTurban'
fit.mobility.dist <- cfa(model.mobility.dist, data = clean)
summary(fit.mobility.dist, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.map<- '
Map~UTurban'
fit.mobility.map <- cfa(model.mobility.map, data = clean)
summary(fit.mobility.map, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

#do same for distances....
model.mobility.pt<- '
SOTmean~UTdist'
fit.mobility.pt <- cfa(model.mobility.pt, data = clean)
summary(fit.mobility.pt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.mrt<- '
MRT~UTdist'
fit.mobility.mrt <- cfa(model.mobility.mrt, data = clean)
summary(fit.mobility.mrt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.btw<- '
BetweenRtPt~UTdist'
fit.mobility.btw <- cfa(model.mobility.btw, data = clean)
summary(fit.mobility.btw, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.with<- '
WithinRtPt~UTdist'
fit.mobility.with <- cfa(model.mobility.with, data = clean)
summary(fit.mobility.with, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.dist<- '
Distance~UTdist'
fit.mobility.dist <- cfa(model.mobility.dist, data = clean)
summary(fit.mobility.dist, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mobility.map<- '
Map~UTdist'
fit.mobility.map <- sem(model.mobility.map, data = clean)
summary(fit.mobility.map, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.new <- '
MRT ~GPSuse + SBtotal'
fit.new<- sem(model.new, data = clean, estimator="MLR")
summary(fit.new, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)



#make aggregate distance/pointing measures
clean$Pointing <- (clean$BetweenRtPt + clean$WithinRtPt) / 2
clean$Distance <- (clean$BetweenDist + clean$WithinDist) / 2
model <- 'Navigation Ability =~ NA*Pointing + Distance + Map
Navigation Ability ~~ 1*Navigation Ability'
fit <- sem(model, data = clean, estimator="MLR")
summary(fit, fit.measures = TRUE, rsquare=TRUE)
semPaths(fit, "std",intercepts = TRUE)

#make new GPS var 
clean$GPSuse <- as.numeric(clean$Q14)

#navigation anxiety measurement model
model.navanx <- 'Navigation Anxiety =~ Q16 + Q19 + Q29 + Q27 + Q32 + Q25'
fit.navanx <- sem(model.navanx, data = clean, missing="FIML")
summary(fit.navanx, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)


#try making an observed mobility and binary....
clean$Mobility <- rowSums(select(clean,Q4,Q5,Q9:Q10), na.rm=TRUE)/4
clean$MobilityD <- ifelse(clean$Mobility>=1.5, 1, 0)#dummy code mobility

#mobility validation models
model.pt <- 'Mobility =~  Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
SOTmean ~ Mobility'
fit.pt <- sem(model.pt, data = clean, missing="FIML", estimator="MLR")
summary(fit.pt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.mrt <- 'Mobility =~ Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
MRT ~ Mobility'
fit.mrt <- sem(model.mrt, data = clean, missing="FIML", estimator="MLR")
summary(fit.mrt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)


model.map <- 'Mobility =~ Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
Map ~ Mobility'
fit.map <- sem(model.map, data = clean, estimator="MLR")
summary(fit.spatial, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.btwpt <- 'Mobility =~ Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
BetweenRtPt ~ Mobility'
fit.btwpt <- sem(model.btwpt, data = clean, estimator="MLR")
summary(fit.btwpt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.withinpt <- 'Mobility =~ Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
WithinRtPt ~ Mobility'
fit.withinpt <- sem(model.withinpt, data = clean, estimator="MLR")
summary(fit.withinpt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.dist <- 'Mobility =~ Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
Distance ~ Mobility'
fit.dist <- sem(model.dist, data = clean, estimator="MLR")
summary(fit.dist, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

#test multicollinearity..
cor(clean$MRT,clean$SOTmean, use='complete.obs')
btwmodel <- lm(WithinRtPt ~ MRT + SOTmean, clean)
vif(btwmodel)


#GPS preliminary models
model.gps.spatial <- 'SpatialLearn =~ NA*BetweenRtPt + WithinRtPt + Distance
SpatialLearn ~~ 1*SpatialLearn
SpatialLearn~GPSuse'
fit.gps.spatial <- cfa(model.gps.spatial, data = clean,missing="FIML", estimator="MLR")
summary(fit.gps.spatial, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.gps.mrt <- '
MRT~GPSuse'
fit.gps.mrt <- cfa(model.gps.mrt, data = clean,missing="FIML", estimator='MLR')
summary(fit.gps.mrt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.gps.pt <- '
SOTmean~GPSuse'
fit.gps.pt <- cfa(model.gps.pt, data = clean,missing="FIML", estimator='MLR')
summary(fit.gps.pt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)


#alt model w gps
model.alt <- 'Spatial Learning Ability =~ NA*BetweenRtPt + WithinRtPt + Distance
MRT ~ a*GPSuse
SOTmean ~ c*GPSuse
Map ~ e*MRT + f*SOTmean + GPSuse
Spatial Learning Ability ~ b*MRT + d*SOTmean + GPSuse
#calculate indirect effect. a* is where our predictor predicts the mediating variable.
#b* is where our mediator predicts the final dependent variable.
ab := a*b #Indirect effect of gps on Spatial Learning through MRT
cd := c*d #Ind of gps on SL through PT
ae := a*e #Ind of gps on Map through MRT
bf := c*f" #Ind of gps on Map through PT
Spatial Learning Ability ~~ 1*Spatial Learning Ability'
fit.alt <- sem(model.alt, data = clean, estimator="MLR", missing="FIML")
summary(fit.alt, fit.measures=TRUE, standardized=TRUE)
require(semPlot) #produce a path model output 
png("MoreSatMapModel.png", width = 8, height = 6, units = 'in', res = 300)
semPaths(fit.alt, "par", edge.label.cex = 1.2, fade = FALSE) #plot our CFA
dev.off()

#doublecheck VIF
require(car)
point.btw <- lm(BetweenRtPt ~ MRT + SOTmean, clean)
point.within <- lm(WithinRtPt ~ MRT + SOTmean, clean)
distmodel <- lm(Distance ~ MRT + SOTmean, clean)

vif(distmodel)

model.alt.lesssat <- 'Spatial Learning Ability =~ NA*BetweenRtPt + WithinRtPt + Distance
SOTmean ~ c*GPSuse
MRT ~ a*GPSuse
Map ~ 0*MRT + 0*SOTmean + 0*GPSuse
Map ~~ 0*Spatial Learning Ability
Spatial Learning Ability ~ b*MRT + d*SOfTmean + GPSuse
Spatial Learning Ability ~~ 1*Spatial Learning Ability'
fit.alt.lesssat <- sem(model.alt.lesssat, data = clean, estimator="MLR", missing="FIML")
summary(fit.alt.lesssat, fit.measures=TRUE, standardized=TRUE)

anova(fit.alt, fit.alt.lesssat) #if p > .05, model not different from data. so we want to keep less saturated model with less parameters estimated

model.alt.final <- 'Spatial Learning Ability =~ NA*BetweenRtPt + WithinRtPt + Distance
MRT ~ a*GPSuse
SOTmean ~ c*GPSuse
Map ~ 0*MRT + 0*SOTmean + 0*GPSuse
Map ~~ 0*Spatial Learning Ability
Spatial Learning Ability ~ b*MRT + d*SOTmean + GPSuse
#calculate indirect effect. a* is where our predictor predicts the mediating variable.
#b* is where our mediator predicts the final dependent variable.
ab := a*b #Indirect effect of gps on Spatial Learning through MRT
cd := c*d #Ind of gps on SL through Perspective Taking
Spatial Learning Ability ~~ 1*Spatial Learning Ability'
fit.alt.final <- sem(model.alt.final, data = clean, se="bootstrap", test="bootstrap")
summary(fit.alt.final, fit.measures=TRUE, standardized=TRUE)


#now do the alt odel with PT mediating MRT on spatial learn. 
model.alt.moresat <- 'Spatial Learning Ability =~ NA*BetweenRtPt + WithinRtPt + Distance
SOTmean ~ c*GPSuse + MRT
MRT ~ a*GPSuse
Map ~ 0*MRT + 0*SOTmean + 0*GPSuse
Map ~~ 0*Spatial Learning Ability
Spatial Learning Ability ~ b*MRT + d*SOTmean + GPSuse
Spatial Learning Ability ~~ 1*Spatial Learning Ability'
fit.alt.moresat <- sem(model.alt.moresat, data = clean, estimator="MLR", missing="FIML")
summary(fit.alt.moresat, fit.measures=TRUE)

anova(fit.alt.moresat, fit.alt.lesssat) #if p > .05, model not different from data. so we want to keep less saturated model with less parameters estimated


model.alt.posthoc <- 'Spatial Learning Ability =~ NA*BetweenRtPt + WithinRtPt + Distance
SOTmean ~ c*GPSuse + e*MRT
MRT ~ a*GPSuse
Map ~ 0*MRT + 0*SOTmean + 0*GPSuse
Map ~~ 0*Spatial Learning Ability
Spatial Learning Ability ~ b*MRT + d*SOTmean + GPSuse
Spatial Learning Ability ~~ 1*Spatial Learning Ability
#calculate indirect effect. a* is where our predictor predicts the mediating variable.
#b* is where our mediator predicts the final dependent variable.
ab := a*b #Indirect effect of GPS on Spatial Learning through MRT
cd := c*d #Ind of GPS on SL through Perspective Taking
de := d*e #Ind of MRT on SL through Perspective Taking
ae := a*e #Ind of GPS on PT through MRT
ade := a*d*e #Ind of GPS on SL through MRT and PT'
fit.alt.posthoc <- sem(model.alt.posthoc, data = clean, se="bootstrap", test="bootstrap", missing="FIML")
summary(fit.alt.posthoc, fit.measures=TRUE, rsq=TRUE)



model.alt.posthoc.abil <- 'Spatial Learning Ability =~ NA*BetweenRtPt + WithinRtPt + Distance
SOTmean ~ c*GPSuse + e*MRT + NavAbility
MRT ~ a*GPSuse + NavAbility
Map ~ 0*MRT + 0*SOTmean + 0*GPSuse
Map ~~ 0*Spatial Learning Ability
Spatial Learning Ability ~ b*MRT + d*SOTmean + GPSuse + NavAbility
Spatial Learning Ability ~~ 1*Spatial Learning Ability
#calculate indirect effect. a* is where our predictor predicts the mediating variable.
#b* is where our mediator predicts the final dependent variable.
ab := a*b #Indirect effect of GPS on Spatial Learning through MRT
cd := c*d #Ind of GPS on SL through Perspective Taking
de := d*e #Ind of MRT on SL through Perspective Taking
ae := a*e #Ind of GPS on PT through MRT
ade := a*d*e #Ind of GPS on SL through MRT and PT'
fit.alt.posthoc.abil <- sem(model.alt.posthoc.abil, data = clean, estimator="MLR", missing="FIML")
summary(fit.alt.posthoc.abil, fit.measures=TRUE, rsq=TRUE)
clean$Pointing <- clean$BetweenRtPt + clean$WithinRtPt / 2
clean$Distance <- (clean$BetweenDist + clean$WithinDist) / 2

model.spatial <- 'SpatialLearn =~ NA*BetweenRtPt + WithinRtPt + Map + Distance
SpatialLearn ~~ 1*SpatialLearn'
fit.spatial <- cfa(model.spatial, data = clean,missing="FIML")
summary(fit.spatial, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.spatial.mod <- 'SpatialLearn =~ NA*BetweenRtPt + WithinRtPt + Distance
SpatialLearn ~~ 1*SpatialLearn'
fit.spatial.mod <- sem(model.spatial.mod, data = clean, estimator="MLR")
summary(fit.spatial.mod, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)


#nav ability/strategy and gps #,Q57,Q49,Q50,Q52, Q54, Q51, Q47
require(lavaan)
model.new <- '
MRT ~ GPSuse + NavAbility + Orient
'
fit.new<- sem(model.new, data = clean, estimator="MLR", missing="FIML")
summary(fit.new, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)
#Navigation Ability =~ Q57 + Q49 + Q50 + Q52 + Q54 + Q51 + Q47

#same for perspective taking..
model.new.pt <- '
SOTmean ~ GPSuse + NavAbility + Orient
'
fit.new.pt<- sem(model.new.pt, data = clean, estimator="MLR", missing="FIML")
summary(fit.new.pt, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)

model.new.vislearn <- '
SpatialLearn ~ GPSuse + NavAbility + Orient
SpatialLearn =~ NA*BetweenRtPt + WithinRtPt + Distance
SpatialLearn ~~ 1*SpatialLearn
'
fit.new.vislearn <- sem(model.new.vislearn, data = clean, estimator="MLR", missing="FIML")
summary(fit.new.vislearn, fit.measures = TRUE, rsquare=TRUE, standardized=TRUE)


#missing at random tests. 
summary(output, fit.measures=TRUE)
coverage <- inspect(fit.alt.moresat, 'coverage')
patterns <- inspect(fit.alt.moresat, 'pattern')

PTmissing <- is.na(clean$SOTmean)
Navmissing <- is.na(clean$BetweenRtPt)
MRTmissing <- is.na(clean$MRT)
GroupMis <- clean$MRT[PTmissing==TRUE]
GroupNMis <- clean$MRT[PTmissing==FALSE]
t.test(GroupMis, GroupNMis)

#what about correlations
variables <- cbind(PTmissing,Navmissing,MRTmissing,clean$SOTmean, clean$GPSuse, clean$BetweenRtPt, clean$Distance, clean$MRT)
colnames(variables) <- c("PTmissing","Navmissing","MRTmissing","SOT","GPS", "BtwRt", "Distance", "MRT")
require(Hmisc)

output <- rcorr(variables)
output$P
require(dplyr)

#could also do logistic regression
model <- glm(PTmissing~clean$GPSuse, family="binomial")
summary(model)

#make the corrplots...

require(dplyr)
combined.sub <- select(clean, UTtotal:UTdist, Map, WithinRtPt:BetweenRtPt, MRT, SOTmean, Mobility:Wanderlust, Distance, GPSuse, NavAbility)
colnames(combined.sub) <- c("UT Places", "Rural Places", "Urban Places", "UT Distance", "Cognitive Map", "Within Pointing", "Between Pointing", "Mental Rotation", "Perspective Taking Error", "Mobility", "Navigation Anxiety", "Wanderlust", "Distance", "GPS Use", "Navigation Ability") #rename cols for graph
combined.sub <- combined.sub[, c(1,2,3,4,10,11,12,15,14,8,9,6,7,13,5)] #reorder...


require(Hmisc) #for rcorr function
cor_5 <- rcorr(as.matrix(combined.sub))
M <- cor_5$r
p_mat <- cor_5$P


require(corrplot)
png("Correlationsp01.png", width = 8, height = 6, units = 'in', res = 300)
corrplot(M, method="circle", type = "lower", order = "original", p.mat = p_mat, sig.level = 0.01, insig="blank")
dev.off()

png("Correlationsp05.png", width = 8, height = 6, units = 'in', res = 300)
corrplot(M, method="circle", type = "lower", order = "original", p.mat = p_mat, sig.level = 0.05, insig="blank")
dev.off()
 
#diff type of corrplot..
require(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

#correlation table for paper .. 
require(xtable)
require(Hmisc)
corstars(combined.sub, result="html") #first load corstars funct below.. makes a nice corr table in hmtl
# Corstars function -------------------------------------------------------

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

testcor <- cor(combined.sub, use="complete.obs")
