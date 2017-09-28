#Script for cleaning data from Qualtrics for SCAN group Mobility Survey final version (1.2) for CFA (U of Utah)
# Loading Required Packages -----------------------------------------------
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(car)
library(readr)
#note: use install.packages('packagename') if the package is not installed on your current computer
#note: needs to be downloaded using "Legacy format" from Qualtrics
#note: delete 2nd row in raw data in excel before running script to get rid of string-based issues
# Cleaning Data-----------------------------------------------------------

#read in data (note: change directory to location where datafile is located on your cpu)
rawdata <- read.csv("Mobility_Survey_Version_1.3_IanDiss.csv")
#note this has been changed to extract participant ID's for Ian's dissertation
#rawdata$ResponseID <- as.numeric(rawdata$ResponseID) #change responseID's from long text strings to numbers
traitdata <- rawdata %>% select(id, LO1:FavActivity, -Map) #delete unnecessary columns of demographic data, store in a separate dataframe
traitdata <- traitdata[order(traitdata$ID),] #sorts dataset by participant ID in ascending order
#recode ethnicity
traitdata$Ethnicity <- as.numeric(traitdata$Ethnicity)
traitdata$Ethnicity[traitdata$Ethnicity == '1'] <- 'African-American'
traitdata$Ethnicity[traitdata$Ethnicity == '2'] <- 'Asian/Pacific-Islanders'
traitdata$Ethnicity[traitdata$Ethnicity == '3'] <- 'Caucasian'
traitdata$Ethnicity[traitdata$Ethnicity == '4'] <- 'Latino or Hispanic'
traitdata$Ethnicity[traitdata$Ethnicity == '5'] <- 'Native American or Aleut'
traitdata$Ethnicity[traitdata$Ethnicity == '6'] <- 'Other'

#recode gender
traitdata$Gender <- as.numeric(traitdata$Gender)
traitdata$Gender[traitdata$Gender == '1'] <- 'Male'
traitdata$Gender[traitdata$Gender == '2'] <- 'Female'
traitdata$Gender[traitdata$Gender == '3'] <- 'Androgynous'
traitdata$Gender[traitdata$Gender == '4'] <- 'Transgender'
traitdata$Gender[traitdata$Gender == '5'] <- 'Transsexual'
traitdata$Gender[traitdata$Gender == '6'] <- 'Other'

#summarize mobility data into a new column
traitdata$UStotal <- rowSums(traitdata[,79:128], na.rm=TRUE)
traitdata <- traitdata %>% select(id:KidsHome_1, Fav:UStotal) #subset out individual states columns
#fix mobility data so that 0 places is missing data
traitdata$UStotal <- car::recode(traitdata$UStotal, c("0=NA"))

#reverse code SBSOD data using car package, then summarize into single variable
#these actually have to be the reverse of the suggested recodings by the scale creators since we made 7 strongly agree.
traitdata$SB2r <- car::recode(traitdata$SB2, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB6r <- car::recode(traitdata$SB6, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB8r <- car::recode(traitdata$SB8, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB10r <- car::recode(traitdata$SB10, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB11r <- car::recode(traitdata$SB11, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB12r <- car::recode(traitdata$SB12, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB13r <- car::recode(traitdata$SB13, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SBtotal <- rowSums(select(traitdata,("SB2r"),("SB6r"),("SB8r"),("SB10r"),("SB11r"), ("SB12r"), ("SB13r"),("SB1"),("SB3"),("SB4"),("SB5"),("SB7"),("SB9"),("SB14")), na.rm=TRUE)
traitdata$SBtotal <- traitdata$SBtotal/15

#summarize the lawton strategy questions
traitdata$Orient <- rowSums(select(traitdata,c(LO1:LO11)), na.rm=TRUE)
traitdata$Orient <- traitdata$Orient/11
traitdata$Route <- rowSums(select(traitdata,c(LR1:LR6)), na.rm=TRUE)
traitdata$Route <- traitdata$Route/6

#summarize the lawton spatial anxiety questions
traitdata$SA <- rowSums(select(traitdata,c(SA1:SA8)), na.rm=TRUE)
traitdata$SA <- traitdata$SA/8

#big five personality questions - recode and summarize
#Gosling, S. D., Rentfrow, P. J., & Swann, W. B., Jr. (2003). A Very Brief Measure of the Big Five Personality Domains. Journal of Research in Personality, 37, 504-528.
#Extraversion: 1, 6R; Agreeableness: 2R, 7; Conscientiousness; 3, 8R; Emotional Stability: 4R, 9; Openness to Experiences: 5, 10R.

traitdata$BF2r <- car::recode(traitdata$BF2, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF4r <- car::recode(traitdata$BF4, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF6r <- car::recode(traitdata$BF6, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF8r <- car::recode(traitdata$BF8, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF10r <- car::recode(traitdata$BF10, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))

traitdata$BFExtraverted <- (rowSums(select(traitdata,('BF1'),('BF6r')), na.rm=TRUE))/2
traitdata$BFAgreeableness <- (rowSums(select(traitdata,('BF2r'),('BF7')), na.rm=TRUE))/2
traitdata$BFConscient <- (rowSums(select(traitdata,('BF3'),('BF8r')), na.rm=TRUE))/2
traitdata$BFEmoStabil <- (rowSums(select(traitdata,('BF4r'),('BF9')), na.rm=TRUE))/2
traitdata$BFOpennness <- (rowSums(select(traitdata,('BF5'),('BF10r')), na.rm=TRUE))/2

#next: SOI Sociosexual orientation index
#Items 1-3 should be coded as 0 = 1, 1 = 2, 2 = 3, 3 = 4, 4 = 5, 5-6 = 6, 7-9 = 7, 10-19 = 8, 20
#or more = 9; they can then be aggregated (i.e., summed or averaged) to form the Behavior
#facet ( = .85). After reverse-coding item 6, items 4-6 can be aggregated to form the Attitude
#facet ( = .87). Aggregating items 7-9 results in the Desire facet ( = .86). Finally, all nine
#items can be aggregated to a total score of global sociosexual orientation ( = .83). 
#http://www.larspenke.eu/pdfs/SOI-R%20Manual.pdf

#recode those who indicated prefer not to answer to missing data
traitdata$SOI1 <- car::recode(traitdata$SOI1, c("10=NA"))
traitdata$SOI2 <- car::recode(traitdata$SOI2, c("10=NA"))
traitdata$SOI3 <- car::recode(traitdata$SOI3, c("10=NA"))
traitdata$SOI4 <- car::recode(traitdata$SOI4, c("10=NA"))
traitdata$SOI5 <- car::recode(traitdata$SOI5, c("10=NA"))
traitdata$SOI6 <- car::recode(traitdata$SOI6, c("10=NA"))
traitdata$SOI7 <- car::recode(traitdata$SOI7, c("10=NA"))
traitdata$SOI8 <- car::recode(traitdata$SOI8, c("10=NA"))
traitdata$SOI9 <- car::recode(traitdata$SOI9, c("10=NA"))

#reverse code item 6
traitdata$SOI6r <- car::recode(traitdata$SOI6, c("1=9; 2=8; 3=7; 4=6; 5=5; 6=4; 7=3; 8=2; 9=1"))

#aggregate into SOI scores
traitdata$SOIbehavior <- (rowSums(select(traitdata,c(SOI1:SOI3)), na.rm=TRUE))/3
traitdata$SOIattitude <- (rowSums(select(traitdata,c(SOI4:SOI5),('SOI6r')), na.rm=TRUE))/3
traitdata$SOIdesire <- (rowSums(select(traitdata,c(SOI7:SOI9)), na.rm=TRUE))/3
traitdata$SOItotal <- (rowSums(select(traitdata,c(SOI1:SOI5),c(SOI7:SOI9), ('SOI6r')), na.rm=TRUE))/9

#ATOT questions
#This comprises 10 items relating to two factors: (i) pleasure in exploring places (items: 1, 2, 5, 6, 9);
#and (ii) no pleasure in exploring places (3, 4, 7, 8, 10).
#Judgments are expressed using a Likert scale from 1 (not at all) to 6 (very much). The sum is
#calculated for each factor.
#The factor analysis confirmed two-factors: the first “pleasure in exploring places” accounted for
#the 40% of variance; the second “the no pleasure in exploring places” accounted for the 15% of
#variance (loads from.40 to 82); the internal consistency is .78 for “pleasure in exploring places”
#and .83 for “no pleasure in exploring places” (De Beni et al., 2014).
traitdata$ATOTpleasure <- (rowSums(select(traitdata,c(ATOT1:ATOT2),c(ATOT5:ATOT6),('ATOT9')), na.rm=TRUE))/5
traitdata$ATOTnopleasure <- (rowSums(select(traitdata,c(ATOT3:ATOT4), c(ATOT7:ATOT8), ('ATOT10')), na.rm=TRUE))/5

#remove original trait variables from dataset now that they are summarized #could we learn something from within-person response variability, though? or does this just tell us about each test?
traitdata <- select(traitdata, -c(LO1:ATOT10), -c(SB2r:SB13r), -c(BF2r:BF10r), -c(SOI6r))

#delete unnecessary columns for factor analysis correlation matrix file
cleandata <- select(rawdata, -c(V1:V10), -c(intro:text1), -(text2), -(text3),
                    -(text4), -(text5),
                    -c(LO1:LocationAccuracy))
totaldata <- merge(cleandata, traitdata, by="id")

#quick histograms
library(ggplot2)
ggplot(traitdata, aes(x=Age)) + geom_bar()
ggplot(traitdata, aes(x=UStotal)) + geom_bar()
ggplot(traitdata, aes(x=UStotal)) + geom_bar()
mean(traitdata$Age, na.rm=TRUE)
sd(traitdata$Age, na.rm=TRUE)
table(traitdata$Gender)

#quick distribution plots
library(yarrr)
pirateplot(SBtotal~Gender, totaldata)
pirateplot(Q14~Gender, totaldata)


#demographics for manuscript
table(traitdata$Gender)
table(traitdata$Ethnicity)

nrow(traitdata)
f
#check quick GPS predictions q13 us yes no, q14 is gps use
summary(lm(Route~Q14, totaldata))
cor.test(totaldata$Q14,totaldata$ATOTnopleasure) 
#extremely small effect of route

#writes cleaned files to local disk, change directory and name as needed
write.csv(totaldata, "MobilitySurveyData1.2_CleanwithTraits.csv")
write.csv(cleandata, "MobilitySurveyData1.2_CFAmatrixclean.csv")
