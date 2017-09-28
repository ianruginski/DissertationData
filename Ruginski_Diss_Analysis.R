# Loading Required Packages -----------------------------------------------
require(ggplot2)
require(psych)
require(car)
require(dplyr)
require(lavaan)
require(semPlot)
require(readr)
# CLEANING EXPERIMENTAL DATA -----------------------------------------------------------

setwd("D:/CNS/Dissertation/data pilot analysis")
#read in data
clean <- read.csv("cleanedpilot.csv")
pt <- read_csv("cleanedpt.csv")

#cleaning PT data 
#NOTE: If a participant leaves an item blank, they should be assigned an angular deviation of 90° for that item (90° represents chance performance, given that the angular deviation can vary from 0 to 180°) (this was done when entering data)

#If the angular deviation on any item exceeds 180°, it should be subtracted from 360° to determine the smallest deviation possible. This is because the angular deviation between two angles does not exceed 180 degrees. 

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
colnames(clean)[82] <- "SOTmean"
colnames(clean)[83] <- "SOTsd"


#note: Two Utah_129's?

#make quick summary variable for mobility and navigation anxiety items
clean$Mobility <- rowSums(select(clean,Q1,Q4,Q5,Q9:Q10), na.rm=TRUE)/5
clean$NavAnx <- rowSums(select(clean,Q16,Q19,Q29,Q27,Q32), na.rm=TRUE)/5
clean$Wanderlust <- rowSums(select(clean,Q45,Q42,Q18,Q20,Q15), na.rm=TRUE)/5

require(psych)
describe(clean$Age)

# Visualizing and Examining Data --------------------------------------------------------
#plot size: 6.79 x 8.49
#labels each datapoint id.method = "mahal", labels=c(1:33), id.n = length(1:33)
#note: min possible average on mobility would be 1, max possible average on mobility would be 6.6...
7+8+8+5+5 
require(ggplot2)
theme_set(theme_bw(base_size = 24))
ggplot(clean, aes(x=Mobility)) + geom_density(alpha=.3, fill="blue") + scale_x_continuous(limits = c(1, 6.6))

scatterplot(SOTmean ~ Mobility, data=clean,  xlab="Mobility", ylab="Perspective Taking Angular Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(samediff.within.abs.error ~ SOTmean, data=clean,  xlab="Perspective Taking Angular Error", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(samediff.between.abs.error ~ SOTmean, data=clean,  xlab="Perspective Taking Angular Error", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(map.error ~ SOTmean, data=clean,  xlab="Perspective Taking Angular Error", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) 



scatterplot(MRT.SDT.style ~ Mobility, data=clean,  xlab="Mobility", ylab="Mental Rotation Ability",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(map.error ~ Mobility, data=clean, id.method = "mahal", labels=c(1:33), id.n = length(1:33)) #labels each datapoint

scatterplot(samediff.within.abs.error ~ Mobility, data=clean, xlab="Mobility", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ Mobility, data=clean, xlab="Mobility", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ Mobility, data=clean, xlab="Mobility", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ samediff.within.abs.error, data=clean, xlab="Within Route Pointing Error", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(map.error ~ Mobility, data=clean, xlab="Mobility", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6)

scatterplot(samediff.between.abs.error ~ NavAnx, clean, xlab="Navigation Anxiety", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) 

scatterplot(map.error ~ NavAnx, clean, xlab="Navigation Anxiety", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint


scatterplot(map.error ~ MRT.SDT.style, clean, xlab="Mental Rotation Ability", ylab="Cognitive Map Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint

scatterplot(samediff.between.abs.error ~ MRT.SDT.style, clean, xlab="Mental Rotation Ability", ylab="Between Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint

scatterplot(samediff.within.abs.error ~ MRT.SDT.style, clean, xlab="Mental Rotation Ability", ylab="Within Route Pointing Error",  cex.lab = 1.6, cex.axis=1.6) #labels each datapoint


# Latent Profile Analysis and Plotting ------------------------------------
#latent profile analysis
#https://cran.r-project.org/web/packages/mclust/mclust.pdf
require(mclust)
clean_ss <-subset(clean,select=c(samediff.between.abs.error,samediff.within.abs.error))
mod1 = Mclust(clean_ss)
summary(mod1)
plot(mod1, xlab="Between Route Pointing Error", ylab="Within Route Pointing Error")
cl

#k means clustering
clean_ss <-subset(clean,select=c(samediff.within.abs.error, samediff.between.abs.error))
k.means.fit <- kmeans(clean_ss, 2) # k = 3
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


model <- lm(MRT.SDT.style ~ Mobility, clean)
model <- lm(samediff.within.abs.error ~ Mobility, clean)
model <- lm(samediff.between.abs.error ~ Mobility, clean)

model <- lm(SOTmean ~ Mobility, clean)
model <- lm(map.error ~ Mobility, clean)
model <- lm(samediff.between.abs.error ~ NavAnx, clean)

model.2 <- lm(samediff.between.abs.error ~ NavAnx + Mobility, clean)
anova(model, model.2) #model comparison


summary(model)

scatterplot(Residuals ~ T1, data=clean)

require(lavaan)
#probably shouldn't fit SEMs?
model <- 'Mobility =~ Q1 + Q4 + Q5 + Q9 + Q10
Q9 ~~ Q10
Navigation Ability =~ samediff.between.abs.error + samediff.within.abs.error + map.error
MRT.SDT.style~Mobility
SOTmean~Mobility
Navigation Ability~MRT.SDT.style + SOTmean + Mobility'
fit <- sem(model, data = clean)
summary(fit, fit.measures = TRUE, rsquare=TRUE)
semPaths(fit, "std",intercepts = TRUE)


