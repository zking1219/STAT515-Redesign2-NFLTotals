#==================================================
# Script to explore, model, and visualize 
# NFL game results from 2016-2019
#==================================================

setwd("/Users/zackkingbackup/Documents/Grad Schools/GMU_Masters/STAT515/RedesignProject2/Rscripts")

library(tidyverse)
library(randomForest)
library(ggplot2)

# Read in processed data originally obtained from SportRadar NFL API
#nfldata <- as_tibble(read_csv("../refined_api_data/NFLregression-dataset.csv"))
nfldata <- read_csv("../refined_api_data/NFLregression-dataset.csv")

# Filter out games yet to be played
nfldataP <- nfldata[nfldata$played == TRUE, ]

# Lets do some EDA
names(nfldataP)

# I want a splom showing distributions of my variables
offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
                   trans=function(x)x^.5)
  panel.loess(x , y, ..., lwd=2,col='red')
}
onDiag <- 
  function(x, ...){
    yrng <- current.panel.limits()$ylim
    d <- density(x, na.rm=TRUE)
    d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
    panel.lines(d,col=rgb(.83,.66,1),lwd=2)
    diag.panel.splom(x, ...)
  }

library(lattice)
library(hexbin)
quartz(width=9,height=9) 
splom(nfldataP[,c(1:14,23)], type=c("g","p"),as.matrix=TRUE,
      xlab='',
      pscale=3, varname.cex=0.8,axis.text.cex=0.65,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5, pch=21,
      col="black",lwd=3
)

#quartz(width=12,height=12) 
png(filename='TotalPts-splom.png')
splom(nfldataP[,c(1:14,23)], as.matrix = TRUE,
      xlab = '',main = "NFL Data 2016-2019 ",
      varnames=c("HOPYPG","HDPYPG","HORYPG","HDRYPG",
                 "HOPPG","HDPPG","HW%","AOPYPG","ADPYPG","AORYPG",
                 "ADRYPG","AOPPG","ADPPG","AW%","TPts"),
      pscale = 0, varname.col = "red",
      varname.cex = 0.56, varname.font = 2,
      axis.text.cex = 0.2, axis.text.col = "red",
      axis.text.font = 0.6, axis.line.tck = .5,
      panel = function(x,y,...) {
        panel.grid(h = -1,v = -1,...)
        panel.hexbinplot(x,y,xbins = 12,...,
                         border = gray(.7),
                         trans = function(x)x^1)
        panel.loess(x , y, ...,
                    lwd = 2,col = 'purple')
      },
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm = TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d,col = gray(.8),lwd = 2)
        diag.panel.splom(x, ...)
      }
)
dev.off()

png(filename='HomePts-splom.png')
splom(nfldataP[,c(1:14,20)], as.matrix = TRUE,
      xlab = '',main = "NFL Data 2016-2019 ",
      varnames=c("HOPYPG","HDPYPG","HORYPG","HDRYPG",
                 "HOPPG","HDPPG","HW%","AOPYPG","ADPYPG","AORYPG",
                 "ADRYPG","AOPPG","ADPPG","AW%","HPts"),
      pscale = 0, varname.col = "red",
      varname.cex = 0.56, varname.font = 2,
      axis.text.cex = 0.2, axis.text.col = "red",
      axis.text.font = 0.6, axis.line.tck = .5,
      panel = function(x,y,...) {
        panel.grid(h = -1,v = -1,...)
        panel.hexbinplot(x,y,xbins = 12,...,
                         border = gray(.7),
                         trans = function(x)x^1)
        panel.loess(x , y, ...,
                    lwd = 2,col = 'purple')
      },
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm = TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d,col = gray(.8),lwd = 2)
        diag.panel.splom(x, ...)
      }
)
dev.off()

# We can see from the sploms above that nothing correlates with 
# TotalPts which is insane because people bet on over/unders all the time
# based on two offenses scoring a combined XX pts per game play each other
# and its assumed each offense will output their season average. This is simply
# not the case at all. 
#
# Lets use a few regression techniques to show how you can't get something from
# nothing - none of these methods will yield a model explaining more than 3% of
# the variance.
#
# Use Random Forest Regression to estimate total points scored in the game
# based on only parameters that would be known pre-kickoff
set.seed(7)
nflRf <- randomForest(x = nfldataP[ , 1:14], y=nfldataP$totalPts,
                        importance=TRUE,  proximity=FALSE, ntree=200,
                        keepForest=TRUE)

nflRf

varImpPlot(nflRf)

plot(nflRf)
# looks like ~200-300 trees will do as well as 500

png(filename='TotalPts6vars-splom.png')
splom(nfldataP[,c("away_off_ppg","home_off_ppg",
                  "away_def_pass_ypg","home_off_pass_ypg",
                  "away_off_pass_ypg","away_def_rush_ypg","totalPts")], as.matrix = TRUE,
      xlab = '',main = "NFL Data 2016-2019 ",
      varnames=c("Away Off. \nPPG","Home Off. \nPPG",
                 "Away Def. \nPass YPG","Home Off. \nPass YPG",
"Away Off. \nPass YPG","Away Def. \nRush YPG","HPts"),
      pscale = 0, varname.col = "red",
      varname.cex = 0.56, varname.font = 2,
      axis.text.cex = 0.2, axis.text.col = "red",
      axis.text.font = 0.6, axis.line.tck = .5,
      panel = function(x,y,...) {
        panel.grid(h = -1,v = -1,...)
        panel.hexbinplot(x,y,xbins = 12,...,
                         border = gray(.7),
                         trans = function(x)x^1)
        panel.loess(x , y, ...,
                    lwd = 2,col = 'purple')
      },
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm = TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d,col = gray(.8),lwd = 2)
        diag.panel.splom(x, ...)
      }
)
dev.off()

# Use a smaller model
varNamesRf <- c("away_off_ppg","home_off_ppg",
                "away_def_pass_ypg","home_off_pass_ypg",
                "away_off_pass_ypg","away_def_rush_ypg")

nflRf6 <- randomForest(x = nfldataP[ , varNamesRf], y=nfldataP$totalPts,
                       importance=TRUE,  proximity=FALSE, ntree=200,
                       keepForest=TRUE, mtry=2)

nflRf6

varImpPlot(nflRf6)

# Well this didn't work at all... getting exclusively negative variances explained.
# I have a suspiciion this is due to early season games where the points per game,
# yards per game statistics are only representative of a tiny sample size.
# Lets assume that teams need until week 9 before their statistics actually
# correlate with points scored. 
#
# Note to self - write some code to look at this... how does, for example,
# home_off_ppg corelate with totalPts by week? I'd guess horribly for weeks 2-4
# and probably pretty well for weeks 9-16 (probably need to remove week 17s
# since many teams have nothing to play for and rest starters)
source("hw.R")
library(ggpubr)
# ggplot(nfldataP, aes(x=home_off_ppg,y=totalPts)) + 
#   geom_point() +
#   hw + 
#   facet_wrap( ~ week, ncol=4) + 
#   stat_smooth(method="lm")
#   #coord_cartesian(xlim=c(0, 4)) +
#   labs(x="Offensive PPG (home team)",y="Total Points") +
#   ggtitle("PPG v. Total Points correlations by week") +
#   stat_cor(label.x = 35, label.y = 80)

#nfldataP$fseason = factor(nfldataP$season) 
ggscatter(
    nfldataP, x = "home_off_ppg", y = "totalPts",
    add = "reg.line", add.params = list(color = "blue", fill = "lightgray")
  ) +
    facet_wrap(~week, ncol=4) +
    stat_cor(label.x = 0, label.y = 100) + hw +
    ggtitle("Off. PPG v. Total Points correlations by week") +
   labs(x="Home Offensive Points per Game", y="Total Points Scored")

# Wow the correlations are generally awful throughout except for week 11 and 12
# only which I expect is a fluke

# Filter out games from before week 9 and try RF regression again
# Show that using week 9+ data doesn't help anything
nfldata9plus <- nfldataP[nfldataP$week > 8,]

# Once again lets use all variables to get a sense for importance
nflRf9plus <- randomForest(x = nfldata9plus[ , 1:14], y=nfldata9plus$totalPts,
                      importance=TRUE,  proximity=FALSE, ntree=500,
                      keepForest=TRUE)

nflRf9plus

varImpPlot(nflRf9plus)

# And again with 6 meaningful variables
nflRf6_9plus <- randomForest(x = nfldata9plus[ , varNamesRf], y=nfldata9plus$totalPts,
                       importance=TRUE,  proximity=FALSE, ntree=500,
                       keepForest=TRUE, mtry=2)

nflRf6_9plus

varImpPlot(nflRf6_9plus)

# Using a subset of data was hardly more successful, we're back to 
# predicting as well as a monkey (explaining none of the variance)
#
# Now lets move on to trying out a vanilla linear model
nfllm_data <- nfldataP[,c(1:14,23)]
nfl_lm <- lm(totalPts ~ ., data=nfllm_data)

nfl_lm
summary(nfl_lm)

# Try a two variable model (away_win_pct, away_off_ppg)
nfllm_data2 <- nfldataP[,c("away_win_pct","away_off_ppg","totalPts")]
nfl_lm2 <- lm(totalPts ~ ., data=nfllm_data2)

nfl_lm2
summary(nfl_lm2)

# That was horrible as suspected
# Maybe try standardizing the predictors and introducing some interactions

# try variable selection techniques; plot R^2 over num. variables


# Perhaps we can treat this as a classification problem instead, lets simply
# try to predict winners via
# 
# 1. Random Forest Classification
# 2. Logistic Regression
#
# Do a little EDA first with boxplots (x-axis has is_winner = away , home,
# y-axis is the variable (like home_off_ppg))

# boxplot(nfldataP$home_off_ppg)
# 
boxplot(home_off_ppg~winner,
        data=nfldataP,
        main="Home Off. PPG in wins & losses",
        xlab="Winning Team",
        ylab="Off. PPG",
        col="orange",
        border="brown"
)

boxplot(away_off_ppg~winner,
        data=nfldataP,
        main="Away Off. PPG in wins & losses",
        xlab="Winning Team",
        ylab="Off. PPG",
        col="orange",
        border="brown"
)

# Lets examine a few more variables common to my models with boxplots
#
#
#

# Begin using RF classification

set.seed(7)
nflRf <- randomForest(x = nfldataP[ , 1:14], y=factor(nfldataP$winner),
                      importance=TRUE,  proximity=FALSE, ntree=200,
                      keepForest=TRUE)

nflRf

varImpPlot(nflRf)

# One more Random Forest only using data post week 9
nflRf9 <- randomForest(x = nfldata9plus[ , 1:14], y=factor(nfldata9plus$winner),
                       importance=TRUE,  proximity=FALSE, ntree=200,
                       keepForest=TRUE)

nflRf9

# That didn't help even slightly
# Lets use a subset of variables
varNamesRFC <- c("home_off_ppg","home_win_pct",
                 "away_off_ppg","away_win_pct",
                 "home_def_ppg","away_def_ppg",
                 "away_off_pass_ypg")

# png(filename='Winner6vars-splom.png')
# splom(nfldataP[,c("home_off_ppg","home_win_pct",
#                   "away_off_ppg","away_win_pct",
#                   "home_def_ppg","away_def_ppg",
#                   "away_off_pass_ypg")], as.matrix = TRUE,
#       xlab = '',main = "NFL Data 2016-2019 ",
#       varnames=c("Home Off. \nPPG","Home Win %",
#                  "Away Off. \nPPG","Away Win %",
#                  "Home Def. \nPPG","Away Def. \nPPG",
#                  "Away Off. \nPass YPG"),
#       pscale = 0, varname.col = "red",
#       varname.cex = 0.56, varname.font = 2,
#       axis.text.cex = 0.2, axis.text.col = "red",
#       axis.text.font = 0.6, axis.line.tck = .5,
#       panel = function(x,y,...) {
#         panel.grid(h = -1,v = -1,...)
#         panel.hexbinplot(x,y,xbins = 12,...,
#                          border = gray(.7),
#                          trans = function(x)x^1)
#         panel.loess(x , y, ...,
#                     lwd = 2,col = 'purple')
#       },
#       diag.panel = function(x, ...){
#         yrng <- current.panel.limits()$ylim
#         d <- density(x, na.rm = TRUE)
#         d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
#         panel.lines(d,col = gray(.8),lwd = 2)
#         diag.panel.splom(x, ...)
#       }
# )
# dev.off()

nflRf7 <- randomForest(x = nfldata9plus[ , varNamesRFC], y=factor(nfldata9plus$winner),
                       importance=TRUE,  proximity=FALSE, ntree=200,
                       keepForest=TRUE)

nflRf7

# This is an improvement! But not by much.
varImpPlot(nflRf7)

# Somehow away_def_ppg is hurting us, remove it
varNamesRFC6 <- c("home_off_ppg","home_win_pct",
                 "away_off_ppg","away_win_pct",
                 "home_def_ppg",
                 "away_off_pass_ypg")

nflRf6a <- randomForest(x = nfldata9plus[ , varNamesRFC6], y=factor(nfldata9plus$winner),
                       importance=TRUE,  proximity=FALSE, ntree=500,
                       keepForest=TRUE)

nflRf6a

varImpPlot(nflRf6a)

# We lost a tiny bit of accuracy but all the variables
# seem to matter now. This is probably about as good as
# we'll do with a Random Forest and this dataset
# Error Rate = 38.78%

# Predict Sunday's games with this classifier
# Extract from nfldata the week 13 season 2019 games
# use nflRf6a.predict(?) on that data 
nfldataUnplayed = nfldata[nfldata$played == FALSE,]
nfldataUnplayed$predicted_winner = predict(nflRf6a, nfldataUnplayed)
nfldataUnplayed[,c("predicted_winner","home_team","away_team","week","season")]

nfl201913 <- nfldataUnplayed[nfldataUnplayed$week == 13,]

# Now use a logistic regression to predict the week13 games
model <- glm(factor(winner) ~ home_off_ppg + home_win_pct +
             away_off_ppg + away_win_pct +
             home_def_ppg + away_off_pass_ypg,family=binomial(link='logit'),
             data=nfldataP)

nfldataUnplayed$home_win_P <- predict(model, nfldataUnplayed, type="response")
nfldataUnplayed[,c("predicted_winner","home_win_P","home_team","away_team")]


# TODO: 1. fix data -> put all week 12 results into nfldata
#       1a. call API a few more times for Sunday/Monday/Thurs games
#           ***
#           ***ran out of API calls, will have to analyze retroactively***
#           *** 
#       2. Report a line (ex: +7.5), O/U
#       2a. convert line to moneyline
#       3. Report the winner picked by this RF model
#       4. Report vegas line, O/U
#       5. Deltas

# https://www.boydsbets.com/nfl-spread-to-moneyline-conversion/
# https://www.trendmut.com/how-to-convert-odds/

# Lets make some point total predictions with our terrible
# models above
nfldataUnplayed$predicted_total = predict(nfl_lm2, nfldataUnplayed)

# Lets do a model for point spread
nfldataP$spread <- nfldataP$homePts - nfldataP$awayPts

# Take a peak of this new feature in a splom to see if
# any data we have affects it
#quartz(10,10)
png(filename='spread-splom.png')
splom(nfldataP[,c(1:14,31)], as.matrix = TRUE,
      xlab = '',main = "NFL Data 2016-2019 ",
      varnames=c("HOPYPG","HDPYPG","HORYPG","HDRYPG",
                 "HOPPG","HDPPG","HW%","AOPYPG","ADPYPG","AORYPG",
                 "ADRYPG","AOPPG","ADPPG","AW%","Spr"),
      pscale = 0, varname.col = "red",
      varname.cex = 0.56, varname.font = 2,
      axis.text.cex = 0.2, axis.text.col = "red",
      axis.text.font = 0.6, axis.line.tck = .5,
      panel = function(x,y,...) {
        panel.grid(h = -1,v = -1,...)
        panel.hexbinplot(x,y,xbins = 12,...,
                         border = gray(.7),
                         trans = function(x)x^1)
        panel.loess(x , y, ...,
                    lwd = 2,col = 'purple')
      },
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm = TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d,col = gray(.8),lwd = 2)
        diag.panel.splom(x, ...)
      }
)
dev.off()

nfllm_spread_data <- nfldataP[,c(1:14,31)]
nfl_lm_spread <- lm(spread ~ ., data=nfllm_spread_data)

nfl_lm_spread
summary(nfl_lm_spread)

# try 4 variable model: (away_off_ppg, away_off_pass_ypg, 
# home_win_pct, home_off_rush_ypg)
varNamesSpread <- c("away_off_ppg", "away_off_pass_ypg", 
                     "home_win_pct", "home_off_rush_ypg","spread")

nfllm_spread_data4 <- nfldataP[,varNamesSpread]
nfl_lm_spread4 <- lm(spread ~ ., data=nfllm_spread_data4)

nfl_lm_spread4
summary(nfl_lm_spread4)

# Add spread predictions into nfldataUnplayed
nfldataUnplayed$predicted_spread <- predict(nfl_lm_spread4, nfldataUnplayed)
table <- nfldataUnplayed[,c("predicted_winner","home_win_P","predicted_spread","predicted_total","home_team","away_team","week","season")]

# Make another predicted_winner column based off the spread
# label each predicted winner column as 'linear regression', or 'random forest',
# or 'logistic regression'
nfldataUnplayed$home_predicted_winner_lm <- nfldataUnplayed$predicted_spread>0

tf2ha <- function(x) {
  if (x == TRUE) {
    return("home")
  } else if (x == FALSE) {
    return("away")
  }
  return("error")
} 

nfldataUnplayed$predicted_winner_lm <- lapply(nfldataUnplayed$home_predicted_winner_lm, tf2ha)
