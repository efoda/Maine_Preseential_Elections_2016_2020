#Disclaimer: this code is following tutorials from www.lynda.com
#groups <- c(rep("Obama",   56.27), rep("Romney",    40.98),rep("Johnson", 1.31),            rep("Stein",  1.14),            rep("Paul",  0.29),            rep("Anderson",  0.01),            rep("Reed",  0)            )
#Q1 to make histogram for candidates & their percentages and bins on the candidates
groups <- c(rep(1,   56.27),
            rep(2,    40.98),
            rep(3, 1.31),
            rep(4,  1.14),
            rep(5,  0.29),
            rep(6,  0.01),
            rep(7,  0)
)
hist(groups)
h <- hist(groups,  # Save histogram as object
          breaks = seq(0, 7, by = 1),
          col = "thistle1", # Or use: col = colors() [626]
          main = "Histogram of Candidates and their winning percentages in 2012",
          xlab = "The candidates, 1: Obama 2:Romney 3:Johnson, 4:Stein 5:Paul 6:Anderson 7:Reed")
#-------------------------------------------------------
#Q2 boxplot
boxplot(groups,horizontal = TRUE,ylim = c(0, 7),whisklty = 1)

#scatter plot
plot(groups)
#--------------------------------------------------------
#Q3 overlay plot
h <- hist(groups,  # Save histogram as object
          #breaks to 7 bins according to the numberof candidates as the question requests          #
          prob = TRUE,
          breaks = seq(0, 7, by = 1),
          col = "thistle1", # Or use: col = colors() [626]
          main = "Histogram of Candidates and their winning percentages in 2012",
          xlab = "The candidates, 1: Obama 2:Romney 3:Johnson, 4:Stein 5:Paul 6:Anderson 7:Reed")
curve(dnorm(x, mean = mean(groups), sd = sd(groups)), 
      col = "red", 
      lwd = 3,
      add = TRUE)
# Plot 3 & 4: Kernel density lines (if prob = TRUE)
lines(density(groups), col = "blue")
lines(density(groups, adjust = 3), col = "darkgreen")
#----------------------
#Scatter with color:
plot(groups, col = ifelse(groups==1,"blue",ifelse(groups==2, "red", "purple")))
#------------------------
#Histogram with color:
h <- hist(groups,  # Save histogram as object
            #breaks to 7 bins according to the numberof candidates as the question requests          #
            breaks = seq(0, 7, by = 1),
            col = c("blue","red","gray","green","yellow","yellow","yellow"),
            main = "Histogram of Candidates and their winning percentages in 2012",
            xlab = "The candidates, 1: Obama 2:Romney 3:Johnson, 4:Stein 5:Paul 6:Anderson 7:Reed")

#-----------------------------------------------------------------------------
#Exercise 4 Frequency
#Democratic won the following 27 states:
#CA,CO,CT,DE,DC,FL,HI,IL,IA,ME,MD,MA,MI,MN,NV,NH,NJ,NM,NY,OH,PA,RI,VT,VA,WA,AI
#Republican won the following 24 states:
#AL,AK,AZ,AR,GA,ID,IN,KS,KY,LA,MS,MO,MT,NE,NC,ND,OK,SC,SD,TN, TX,UT,WV,WY

groups <- c(rep("Democratic",   27),
              rep("Republic",    24))
# CREATE FREQUENCY TABLES
groups.t1 <- table(groups)  # Creates frequency table 
groups.t1  # Print table

# MODIFY FREQUENCY TABLES
groups.t2 <- sort(groups.t1, decreasing = TRUE)  # Sorts by frequency, saves table
groups.t2  # Print table
# PROPORTIONS AND PERCENTAGES
prop.table(groups.t2)  # Give proportions of total
round(prop.table(groups.t2), 2)  # Give proportions w/2 decimal places
round(prop.table(groups.t2), 2) * 100  # Give percentages w/o decimal places
#-----------------------------------------------------------------------------
#Q5 Descriptive statistics

#q5<-read.csv("C:/Users/Engy/Downloads/Harvard/5th semester ISA/parctical approach for data science/hw 2/q5.csv",header=TRUE);
#str(q5);
#summary(q5);
#class(q5);

PARTY=c("R","D","IND","W","IND","IND","","R","D","LIB","GRE","W","","R","D","LIB","GRE","W","W","W","W","W","W","","R","D","LIB","GRE","SLP","","D","R","LIB","GRE","PFP","AIP","W","W","W","W","W","W","W","W","","D","R","LIB","GRE","AMC","PFP","UN","JUS","WTP","AMP","SLP","SUS","ATP","OBJ","SWP","SEP","W","","D","R","LIB","IP","W","W","W","W","W","W","","D","R","LIB","GRE","W","W","W","","D","R","DCG","LIB","W","","D","R","LBF","GPF","PFF","OBF","CPF","JPF","APF","REF","SFL","SLF","W","W","W","W","W","","R","D","LIB","W","W","W","W","W","W","W","W","","D","R","LIB","GRE","","R","D","LIB","IND","IND","CON","","D","R","LIB","GRE","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","","R","D","LIB","W","W","W","W","W","W","W","W","W","","D","R","LIB","W","IG","CON","NP","SWP","PSL","","R","D","LIB","REF","W","W","W","W","W","W","W","W","W","","R","D","LIB","IND","GRE","W","W","W","W","W","W","W","W","","R","D","LIB","GRE","CON","WTP","JUS","SLP","P","SWP","SEP","","D","R","LIB","GI","W","W","W","","D","R","LIB","GRE","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","W","","D","R","LIB","GR","W","","D","R","GRE","UST","W","NLP","W","W","W","","DFL","R","LIB","GRE","W","CON","GRT","JUS","CG","SWP","SLP","W","W","W","W","W","W","","R","D","LIB","CON","GRE","REF","","R","D","LIB","CON","","R","D","LIB","W","W","W","W","W","W","W","","R","D","LIB","W","BP","","D","R","LIB","","IAP","","D","R","LIB","W","W","CON","W","","D","R","LIB","GRE","CON","NJJ","NSA","SWP","ATP","SLP","","D","R","LIB","GRE","NMI","CON","","Combined Parties:","D","WF","Combined Parties:","R","CRV","LIB","GRE","W","CON","SLP","W","W","W","W","W","W","W","","R","D","LIB","W","W","","R","DNL","LIB","W","GRE","CON","","D","R","LIB","GRE","IND","CON","SUS","W","W","W","W","W","","R","D","","D","R","LIB","PG","W","CON","PRO","","D","R","LIB","GRE","W","W","W","","D","R","LIB","GRE","W","W","CON","JUS","SLP","W","W","","R","D","LIB","GRE","CON","","R","D","LIB","CON","","R","D","IND","GRE","CON","IND","IND","","R","D","LIB","GRE","W","W","W","W","W","W","W","","R","D","LIB","JUS","GRE","CON","UN","W","W","W","W","W","W","W","W","","D","R","LIB","JUS","W","W","SLP","W","W","W","","D","R","LIB","CON","GRE","W","W","W","W","W","W","","D","R","LIB","GRE","CON","JUS","SLP","SWP","","R","D","LIB","MTP","NPA","","D","R","IND","IND","W","CON","IND","IND","W","W","","R","D","LIB","W","CON")
GENERAL.RESULTS=c(1255925,795696,12328,4011,3397,2981,2074338,164676,122640,7392,2917,2870,300495,1233654,1025232,32100,7816,289,119,17,14,7,6,2299254,647744,394409,16276,9305,1734,1069468,7854285,4839958,143221,85638,53824,38372,21461,992,503,82,79,72,54,6,13038547,1323102,1185243,35545,7508,6234,5059,2589,1260,792,679,317,308,266,235,192,189,4,2569522,905083,634892,12580,5487,863,25,19,5,5,1,1558960,242584,165484,3882,1940,23,7,1,413921,267070,21381,2458,2083,772,293764,4237756,4163447,44726,8947,8154,3856,2607,1754,946,820,799,322,36,3,3,2,1,8474179,2078688,1773827,45324,1516,432,154,55,30,21,2,1,3900050,306658,121015,3840,3184,434697,420911,212787,9453,4402,2499,2222,652274,3019512,2135216,56229,30222,233,185,182,121,22,16,14,10,8,5,5,4,4,4,3,3,3,2,2,2,2,2,1,1,1,5242014,1420543,1152887,50111,625,290,35,17,8,8,7,2,1,2624534,822544,730617,12926,7442,3769,3038,1027,445,372,1582180,692634,440726,20456,5017,714,187,95,58,48,19,12,4,1,1159971,1087190,679370,17063,6872,6337,245,60,37,30,4,2,1,1,1797212,1152262,809141,18157,6978,2508,1767,1368,622,518,389,355,1994065,401306,292276,9352,8119,2035,62,30,713180,1677844,971869,30195,17110,8788,625,418,204,64,35,26,19,19,18,18,15,13,8,7,7,7,5,3,2,2,2,1,1,1,1,2707327,1921290,1188314,30920,20691,6552,3167767,2564569,2115256,21897,16119,7774,5147,89,68,42,4730961,1546167,1320225,35098,13023,10533,3722,3149,1996,1092,1051,397,46,35,14,8,4,1,2936561,710746,562949,6676,2609,1588,1016,1285584,1482440,1223796,43151,7936,2757323,267928,201839,14165,59,39,6,5,4,2,1,484048,475064,302081,11109,3717,2408,794379,531373,463567,10968,5770,3240,1014918,369561,329918,8212,1374,875,708,324,710972,2125101,1477568,21045,9888,2064,1724,1007,710,664,521,3640292,415335,335788,27788,2691,1174,982,783758,4485741,4337622,148119,2490431,2228060,262371,47256,39982,9076,6274,2050,217,34,34,27,19,12,6,7081159,2270395,2178391,44515,11537,534,4505372,188163,124827,5231,1860,1361,1185,322627,2827709,2661437,49493,18573,12502,8152,2944,14,13,9,1,0,5580847,891325,443547,1334872,970488,754175,24089,19427,13275,4432,3384,1789270,2990274,2680434,49991,21341,11219,383,28,5753670,279677,157204,4388,2421,686,617,430,416,132,64,14,446049,1071645,865941,16321,5446,4765,1964118,210610,145039,5795,2371,363815,1462330,960709,18623,6515,6022,2639,1739,2458577,4569843,3308124,88580,24657,1287,426,374,209,162,102,87,7993851,740600,251813,12572,5335,3817,2871,393,18,6,5,5,2,1,1,1,1017440,199239,92698,3487,1128,717,710,695,594,13,9,299290,1971820,1822522,31216,13058,8627,7151,76,14,3,1,1,3854489,1755396,1290670,42202,20928,8851,4946,1318,1205,3125516,417655,238269,6302,4406,3806,670438,1620985,1407966,20439,7665,5170,4930,553,526,112,88,3068434,170962,69286,5326,2035,1452)

q5=data.frame(PARTY,GENERAL.RESULTS)
str(q5);

RepVoters<- subset(q5, q5$PARTY=="R");
sumRepVoters=sum(RepVoters$GENERAL.RESULTS)

DemVoters<- subset(q5, q5$PARTY=="D");
sumDemVoters=sum(DemVoters$GENERAL.RESULTS)

GreenVoters<- subset(q5, q5$PARTY=="GRE");
sumGreenVoters=sum(GreenVoters$GENERAL.RESULTS)

LibVoters<- subset(q5, q5$PARTY=="LIB");
sumLibVoters=sum(LibVoters$GENERAL.RESULTS)

sumvoter=c(sumRepVoters,sumDemVoters,sumGreenVoters,sumLibVoters);
party=c("R","D","GRE","LIB");
tableVoterParty=data.frame(sumvoter,party);
#plot(sumvoter ~ party, data = tableVoterpopYears,xlab = "party",ylab = "voters population",main = "voters")


ranking <- tableVoterParty[order(-sumvoter),]
ranking  # Print table

# PROPORTIONS AND PERCENTAGES
prop.table(ranking$sumvoter)  # Give proportions of total
round(prop.table(ranking$sumvoter), 2)  # Give proportions w/2 decimal places
percentages<-round(prop.table(ranking$sumvoter), 2) * 100  # Give percentages w/o decimal places
tablePartyPercentage=data.frame(ranking$party,percentages)
tablePartyPercentage


# CALCULATE DESCRIPTIVES
summary(tableVoterParty$sumvoter)  # Summary for one variable
summary(tableVoterParty)  # Summary for entire table

# Tukey's five-number summary: minimum, lower-hinge,
# median, upper-hinge, maximum. No labels.
fivenum(tableVoterParty$sumvoter)

# Boxplot stats: hinges, n, CI, outliers
boxplot.stats(tableVoterParty$sumvoter)

# ALTERNATIVE DESCRIPTIVES
# From the package "psych"
#help(package = "psych")
#install.packages("psych")
require("psych")
describe(tableVoterParty)


#---------------------------------------------------------------
#Q6 Single proportion hypothesis
# Based on the historical data of Maine: http://www.270towin.com/states/Maine
#Since 1960, Republicans won 6 times out of 14 times, and Democratic won 8 times
#As I expect that the Democratic will win in Maine because Maine is a solid Democratic state
#My Null Hypothesis will be Republicans win in 2016
#It is a 2-tailed Hypothesis
#H0: R wins 
#Ha: R loses

# PROP TEST
prop.test(6, 14)
#-----------------------------------------------------------------
#Q7 Single mean hypothesis
#Based upon the values of the votes from the following sources
#https://ballotpedia.org/Presidential_election_in_Maine,_2016
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_2004
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_2000
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1996
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1992
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1988
#https://en.wikipedia.org/wiki/United_States_presidential_election,_1984
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1984
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1980
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1976
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1972
#https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_1968
#https://en.wikipedia.org/wiki/United_States_presidential_election,_1964
#https://en.wikipedia.org/wiki/United_States_presidential_election,_1960

#Again running the Null Hypothesis as Republican will win by
#collecting the Republican votes in the elections from 2016 till 1988
RepVote=c(335593,292276,295273,330201,286616,186378,206504,307131,336500,238522,236320,256458,169254,118701,240608)
t.test(RepVote)

# Two-sided t-test
t.test(RepVote, alternative = "two.sided")
#--------------------------------------------------------------------------------------
#Q8  chi-square test

  #http://www.maine.gov/sos/cec/elec/data/index.html
  #http://www.maine.gov/sos/cec/elec/data/r-e-active.pdf
  #I followed the instruction here to create the table:
  #http://www.cyclismo.org/tutorial/R/tables.html
  
  MaineVoters2020=matrix(c(335593,357735,38105),ncol=3)
colnames(MaineVoters2020) <-c("R","D","others")
MaineVoters2020 <- as.table(MaineVoters2020)
round(prop.table(MaineVoters2020), 2)  # Show as proportions w/2 digits

chi1 <- chisq.test(MaineVoters2020)  # Save tests as object "chi1"
chi1  # Check results

#compare it with 2016 percentages
chi2 <- chisq.test(MaineVoters2020, p = c(0.449,0.478,0.073))
chi2
#------------------------------------------------------------------------------
#chi-square test with green party details

#http://www.maine.gov/sos/cec/elec/data/index.html
  #http://www.maine.gov/sos/cec/elec/data/r-e-active.pdf
  #I followed the instruction here to create the table:
  #http://www.cyclismo.org/tutorial/R/tables.html
  
  MaineVoters2016=matrix(c(264673,319679,39133,359047),ncol=4)
colnames(MaineVoters2016) <-c("R","D","GRE","Unenrolled")
MaineVoters2016 <- as.table(MaineVoters2016)
round(prop.table(MaineVoters2016), 2)  # Show as proportions w/2 digits

chi1 <- chisq.test(MaineVoters2016)  # Save tests as object "chi1"
chi1  # Check results

#compare it with 2012 percentages
#The source of 2012 percentages is: http://www.270towin.com/states/Maine
#and from here https://en.wikipedia.org/wiki/United_States_presidential_election_in_Maine,_2012
chi2 <- chisq.test(MaineVoters2016, p = c(.41, 0.563, .011, 0.016))
chi2
#-----------------------------------------------------
#Q9Exercise 9 Decision Tree

D<-c(401306)
R<-c(292276)
other<-c(19598)
winparty<-c("D")
partyTree <- data.frame(D,R,other,winparty)
str(partyTree)
fit <- rpart( winparty ~ D + R + other, method="class", data.frame(partyTree))
printcp(fit)
summary(fit)

tr <- tree( winparty ~ D + R + other, method="class", data.frame(partyTree))
# see summary
summary(tr)
#plot tree
plot(tr)

require("party")
# grow tree
ct <- ctree( winparty ~ D + R + other, method="class", data.frame(partyTree))
# see summary
summary(ct)
#plot tree
plot(ct)  
#-----------------------------------------------------------------------------------
#Q10 Exercise 10 Scatter plot of the counties and parties
#http://www.politico.com/2016-election/results/map/president/maine/



counties=c("Androscoggin","Aroostook","Cumberland","Franklin","Hancock","Kennebec","Knox","Lincoln","Oxford","Penobscot","Piscataquis","Sagadahoc","Somerset","Waldo","Washington","York");
Democrate=c(22975,13377,102935,7001,16107,31753,12440,10241,16214,32832,3098,10679,9092,10442,6358,55828)
Republican=c(28189,19419,57697,7900,13682,29296,9148,9727,12172,41601,5403,9304,14998,10378,9037,50388)
MaineCounties=data.frame(Democrate,Republican,count = c(1:16));
counties1=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
plot(counties1,Democrate,col="blue",xlab = "counties, 1:Androscoggin 2:Aroostook 3:Cumberland 4:Franklin 5:Hancock 6:Kennebec 7:Knox 8:Lincoln \n 9:Oxford 10:Penobscot 11:Piscataquis 12:Sagadahoc 13:Somerset 14:Waldo 15:Washington 16:York", ylab="votes Democrate:Blue Republican: Red Others: Green",xaxt="n",ylim=c(1417,1e+05))
axis(1, at = seq(1, 16, by = 1), las=2)
par(new=TRUE)
plot(counties1,Republican,col="red",ylim=c(2e+04,1e+05),axes=FALSE,ann=FALSE)
par(new=TRUE)
plot(counties1,others,col="green",ylim=c(1417,1e+05),axes=FALSE,ann=FALSE)


library(ggplot2)
library(reshape2)


countychoice=c("D","D","D","D","D","D","D","D","D","D","R","D","D","D","D","D");
#countychoice=c(1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1)
MaineCounties=data.frame(counties,Democrate,Republican);
plot(Democrate~Republican,col=c("blue","red"))

vote.mod1 = lm(Democrate~Republican, data = MaineCounties)
abline(lm(Democrate~Republican))

#------------------------------------------
#Scatter plot as in Lynda

MaineCounties=data.frame(Republican,Democrate,counties);
MaineCounties[1:3]

# Modified scatterplot matrices

# Create palette with RColorBrewer
require("RColorBrewer")
display.brewer.pal(3, "Pastel2")

# Put histograms on the diagonal (from "pairs" help)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,  ...)
  # Removed "col = "cyan" from code block; original below
  # rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...) 
}

pairs(MaineCounties[1:3], 
      panel = panel.smooth,  # Optional smoother
      main = "Scatterplot Maine Counties",
      diag.panel = panel.hist, 
      pch = 16, 
      col = brewer.pal(3, "Pastel2"))

# Similar with "car" package
# Gives kernal density and rugplot for each variable
library(car)
scatterplotMatrix(~Democrate+Republican | counties,
                  data = MaineCounties,
                  col = brewer.pal(3, "Dark2"),
                  main="Scatterplot Matrix for Maine Counties Data Using \"car\" Package")






