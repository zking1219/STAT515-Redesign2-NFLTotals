#==================================================
#
# Purpose: Predict winners of nfl games using
# the NFLregression-dataset.csv dataset extracted
# from the SportRadar NFL API
#
# Build an ensemble classifier out of
# 1. Random Forest Classification
# 2. Logistic Regression
# 3. Linear Regression (predict spread; if spread > 0,
#                       classify as victory for the home team)
# 
# Majority rules; if two weak learners pick the away team
# then so does the ensemble classifier
#
# Use a 10-fold Cross Validation to report an accuracy
# with a 95% CI for the ensemble classifier
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

# 1. Random Forest Classification
# 2. Logistic Regression
# 3. Linear Regression for spreads; spreads to pick winners

# See AnalyzeNFL.R for how I came to this model. After running the full model,
# I selected 7 variables that were important according to the varImpPlot function
# but then noticed away_def_ppg was hurting performance; removed it to get this.
varNamesRFC6 <- c("home_off_ppg","home_win_pct",
                  "away_off_ppg","away_win_pct",
                  "home_def_ppg",
                  "away_off_pass_ypg")

set.seed(7)
nflRf6a <- randomForest(x = nfldataP[ , varNamesRFC6], y=factor(nfldataP$winner),
                        importance=TRUE,  proximity=FALSE, ntree=200,
                        keepForest=TRUE)

# Predict unplayed games
# use nflRf6a
nfldataUnplayed = nfldata[nfldata$played == FALSE,]
nfldataUnplayed$predicted_winner_rf = predict(nflRf6a, nfldataUnplayed)

# Now use a logistic regression to predict the week13 games
# First try the variables that worked nicely for RF
model <- glm(factor(winner) ~ home_off_ppg + home_win_pct +
               away_off_ppg + away_win_pct +
               home_def_ppg + away_off_pass_ypg,family=binomial(link='logit'),
             data=nfldataP)
summary(model)

# Now try to stepwise select variables
# http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/
library(MASS)
model1 <- glm(factor(winner) ~ .,family=binomial(link='logit'),
             data=nfldataP[ , c(1:14,22)]) %>% stepAIC(trace = FALSE, direction="both")
summary(model1)

# Again, predict the unplayed games' results
nfldataUnplayed$home_win_P <- predict(model1, nfldataUnplayed, type="response")

# Lets do a model for point spread
nfldataP$spread <- nfldataP$homePts - nfldataP$awayPts

nfllm_spread_data <- nfldataP[,c(1:14,31)]
nfl_lm_spread <- lm(spread ~ ., data=nfllm_spread_data)

# try stepwise selection
nfl_lm_spreadS <- lm(spread ~ ., data=nfllm_spread_data) %>% stepAIC(trace = FALSE, direction="both")

# Add spread predictions into nfldataUnplayed
nfldataUnplayed$predicted_spread <- predict(nfl_lm_spreadS, nfldataUnplayed)

# Make another predicted_winner column based off the spread
# and then do so again based off the probabilities coming out of logistic regression
nfldataUnplayed$predicted_winner_lm <- ifelse(nfldataUnplayed$predicted_spread > 0, "home", "away")
nfldataUnplayed$predicted_winner_logR <- ifelse(nfldataUnplayed$home_win_P > .5, "home", "away")

betting_table <- nfldataUnplayed[,c("week","home_team","away_team","predicted_winner_lm",
                                    "predicted_winner_rf","predicted_winner_logR",
                                    "predicted_spread")]

# Our 3 classification models are:
# 1. RF
#   - nflRf6a <- randomForest(x = nfldataP[ , varNamesRFC6], y=factor(nfldataP$winner),
#                             importance=TRUE,  proximity=FALSE, ntree=200,
#                             keepForest=TRUE)
# 2. LM
#   - nfl_lm_spreadS <- lm(spread ~ ., data=nfllm_spread_data) %>% stepAIC(trace = FALSE, direction="both")
#
# 3. Log. Reg.
#   - glm(factor(winner) ~ .,family=binomial(link='logit'),
#         data=nfldataP[ , c(1:14,22)]) %>% stepAIC(trace = FALSE, direction="both")
#
# Re-run all these models through cross validation. Compare accuracies on
# their own and as a voting ensemble (also look at accuracy when all models agree)

# Implement 10-fold Cross Validation
# 
# Note: you shouldn't re-do variable selection for each execution during
# cross validation, just re-train on the given training set -  
# you're trying to evaluate how well a PARTICULAR model does.
# Therefore, for both logistic and linear regressions, we'll use the variables
# picked by the stepAIC function earlier using the whole dataset
vars_lm <- c("home_off_pass_ypg","home_def_pass_ypg",
             "home_off_rush_ypg","home_def_rush_ypg",
             "home_win_pct","away_off_pass_ypg",
             "away_def_pass_ypg","away_off_ppg",
             "spread") # need the dependent variable

vars_logR <- c("home_off_pass_ypg","home_off_rush_ypg",
               "home_def_ppg","home_win_pct","away_off_pass_ypg",
               "away_off_ppg","away_win_pct",
               "winner") # need the dependent variable

## set the seed to make cross validation reproducible
set.seed(2)

# Randomly assign the 10 folds
nfldataP$samp_num <- sample(1:10,size=nrow(nfldataP),
                            replace=TRUE,prob=c(.1,.1,.1,.1,.1,
                                                .1,.1,.1,.1,.1))

# Initialize an empty data.frame to contain the results
cv_df <- data.frame(samp_num=integer(),
                    rf_acc=double(),
                    lm_acc=double(),
                    logR_acc=double(),
                    lm_spread_bias=double(),
                    lm_spread_var=double(),
                    maj_acc=double(),
                    agree_acc=double(),
                    agree_rate=double())

for (samp in c(1:10)) {
  test <- nfldataP[nfldataP$samp_num==samp,]
  train <- nfldataP[nfldataP$samp_num!=samp,]
  
  rf_model <- randomForest(x = train[ , varNamesRFC6], y=factor(train$winner),
                           importance=TRUE,  proximity=FALSE, ntree=200,
                           keepForest=TRUE)
  
  lm_model <-  lm(spread ~ ., data=train[, vars_lm])
  
  logr_model <- glm(factor(winner) ~ .,family=binomial(link='logit'),
                    data=train[ , vars_logR])
  
  # Apply our trained models to the test sets and see how we did
  test$rf_winner <- rf_model %>% predict(test, type = "response")
  
  test$lm_spread <- lm_model %>% predict(test)
  # convert spread predictions to predictions of which team won (home | away)
  test$lm_winner <- ifelse(test$lm_spread > 0, "home", "away")
  
  test$logR_home_win_P <- logr_model %>% predict(test)
  # convert spread predictions to predictions of which team won (home | away)
  test$logR_winner <- ifelse(test$logR_home_win_P > .5, "home", "away")
  
  # Model accuracies
  rf_acc <- mean(test$winner==test$rf_winner)
  lm_acc <- mean(test$winner==test$lm_winner)
  logR_acc <- mean(test$winner==test$logR_winner)
  # Note: from experimentation it looks like the random seed 
  # really matters and all these models are viable
  
  # How good are the spreads I created?
  residuals <- test$spread - test$lm_spread
  mean_res <- mean(residuals) # is the model biased?
  var_res <- var(residuals) # how much variance?
  
  # Lets see how a voting ensemble would do; majority wins
  # https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r/
  test$pred_majority<-as.factor(ifelse(test$rf_winner=='home' & test$lm_winner=='home','home',
                                       ifelse(test$rf_winner=='home' & test$logR_winner=='home','home',
                                              ifelse(test$lm_winner=='Y' & test$logR_winner=='home','home',
                                                     'away'))))
  
  # Accuracy
  maj_acc <- mean(test$winner==test$pred_majority)
  
  # How does our ensemble do when all 3 weak learners agree?
  test_agree <- subset(test, (rf_winner == 'home' & logR_winner == 'home' & lm_winner == 'home') |
                         (rf_winner == 'away' & logR_winner == 'away' & lm_winner == 'away'))
  
  agree_acc <- mean(test_agree$winner==test_agree$rf_winner) # doesn't matter which 'winner' I specify since they're equal here
  
  # how often do all three agree?
  agree_rate <- dim(test_agree)[1] / dim(test)[1]
  # Load up cv_df
  cv_df[nrow(cv_df)+1, ] <- c(samp, rf_acc, lm_acc, logR_acc,
                              mean_res, var_res, maj_acc, agree_acc,
                              agree_rate)
  
}

means <- colMeans(cv_df)
variances <- apply(cv_df, 2, var)
# 1. for 56% of the games, I can get 2/3 of them right (when all weak learners agree)
# 2. lm_acc is consistently as good if not better than maj_acc
# 3. 95% CI for ensemble accuracy via 10-fold CV is: [.45, .71]

# Calculate 95% CIs for rf, lm, logR, majority, all_agree
getCI <- function(mu, var) {
  low <- mu - 2*sqrt(var)
  high <- mu + 2*sqrt(var)
  return (unname(c(low, high)))
}

rf_95ci <- getCI(means[2], variances[2])
lm_95ci <- getCI(means[3], variances[3])
logR_95ci <- getCI(means[4], variances[4])
maj_95ci <- getCI(means[7], variances[7])
agree_95ci <- getCI(means[8], variances[8])

# Create a basic bar graph with the 95% CIs
bar_df <- data.frame(method=character(),
                     mean_acc=double(),
                     ci_lower=double(),
                     ci_upper=double(),
                     stringsAsFactors = FALSE)

bar_df[1,] <- c(as.character("Random Forest"), unname(means[2]), rf_95ci[1], rf_95ci[2])
bar_df[2,] <- c(as.character("Linear Regression"), unname(means[3]), lm_95ci[1], lm_95ci[2])
bar_df[3,] <- c(as.character("Logistic Regression"), unname(means[4]), logR_95ci[1], logR_95ci[2])
bar_df[4,] <- c(as.character("Majority Vote"), unname(means[7]), maj_95ci[1], maj_95ci[2])
bar_df[5,] <- c(as.character("All Agree"), unname(means[8]), agree_95ci[1], agree_95ci[2])

bar_df$mean_acc <- as.numeric(bar_df$mean_acc) * 100
bar_df$ci_lower <- as.numeric(bar_df$ci_lower) * 100
bar_df$ci_upper <- as.numeric(bar_df$ci_upper) * 100

detach("package:randomForest", unload=TRUE) # is masking margin function
source("hw.R")
ggplot(bar_df, aes(x=method, y=mean_acc, fill=method)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ci_lower, ymax=ci_upper)) +
  ggtitle("Individual Model and Ensembles: CV Performance") +
  labs(x="Model",y="Accuracy (% correct)") +
  coord_cartesian(ylim=c(0,100)) + 
  hw + theme(legend.title = element_blank())

ggplot(bar_df[1:4,], aes(x=method, y=mean_acc, fill=method)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ci_lower, ymax=ci_upper)) +
  ggtitle("Individual Model and Ensemble: CV Performance") +
  labs(x="Model",y="Accuracy (% correct)") +
  coord_cartesian(ylim=c(0,100)) + 
  hw + theme(legend.title = element_blank())

# Look at week 13 completed games
# for which we make a prediction
#
# Get betting lines + results from:
# https://www.teamrankings.com/nfl-odds-week-13
nfl13_odds_results <- c(c("Lions","Bears",-5.5,20,24,"away"),
                c("Cowboys","Bills",6.5,15,26,"away"),
                c("Falcons","Saints",-7,18,26,"away"),
                c("Ravens","49ers",5.5,20,17,"home"),
                c("Panthers","Redskins",10.5,21,29,"away"),
                c("Giants","Packers",-6.5,13,31,"away"),
                c("Steelers","Browns",-1,20,13,"home"),
                c("Jaguars","Buccaneers",-3,11,28,"away"),
                c("Dolphins","Eagles",-10.5,37,31,"home"),
                c("Bengals","Jets",-2.5, 22, 6, "home"),
                c("Colts","Titans",-1, 17, 31, "away"),
                c("Cardinals","Rams",-2.5,7,34,"away"),
                c("Broncos","Chargers",-4.5,23,20,"home"),
                c("Chiefs","Raiders",+11.5,40,9,"home"),
                c("Texans","Patriots",-3.5,28,22,"home"),
                c("Seahawks","Vikings",2.5, 37, 30, "home"))

mat13 <- matrix(nfl13_odds_results, nrow=16, ncol=6, byrow=TRUE)
df13 <- as.data.frame(mat13)
colnames(df13) <- c("home_team","away_team","vegas_spread",
                    "homePts_act","awayPts_act","winner_act")

# merge df13 and nfldataUnplayed to evaluate our predictions
df13m <- merge(nfldataUnplayed[nfldataUnplayed$week==13,], df13, by="home_team")

# need to make predicted_winner_maj_vote column
df13m$predicted_winner_maj_vote <- as.factor(ifelse(df13m$predicted_winner_rf=='home' & df13m$predicted_winner_lm=='home','home',
                                             ifelse(df13m$predicted_winner_rf=='home' & df13m$predicted_winner_logR=='home','home',
                                             ifelse(df13m$predicted_winner_lm=='Y' & df13m$predicted_winner_logR=='home','home',
                                            'away'))))

# This can go in an appendix, maybe a subset can be in the paper
table13 <- df13m[c("home_team","away_team.x","vegas_spread",
                   "homePts_act","awayPts_act","winner_act",
                   "home_win_P","predicted_spread","predicted_winner_rf",
                   "predicted_winner_lm","predicted_winner_logR",
                   "predicted_winner_maj_vote")]

# Make a row plot showing difference between my line and the vegas line
# colored by winning team (home | away)
df13m$vegas_spread <- as.numeric(as.character(df13m$vegas_spread))
get_matchup <- function(home, away) {
  match <- paste(home, away, sep="/")
  return(match)
}
get_abs_spread <- function(pred, act) {
  return(abs(pred - act))
}
df13m$matchup <- apply(df13m[,c('home_team','away_team.x')], 1, function(x) get_matchup(x[1],x[2]))
df13m$abs_spread <- apply(df13m[,c('predicted_spread','vegas_spread')], 1, function(x) get_abs_spread(x[1],x[2]))
df13m <- df13m[order(df13m$abs_spread),]
ggplot(df13m, aes(matchup, predicted_spread)) + 
  geom_linerange(aes(x=matchup, ymin=predicted_spread, ymax=vegas_spread)) +
  geom_point(aes(color=factor(winner_act)), size=5) +
  coord_flip() +
  labs(y="Predicted HomePts - AwayPts (from Vegas line)",x="",col="Won Game") +
  ggtitle("Predicted and Actual Spreads 2019 Week 13") +
  hw

write.csv(df13m,"ModelOutputsWeek13.csv", row.names = FALSE)

# Make sploms for our chosen logistic regression and linear regression models
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
png("LM_spread_vars.png")
splom(nfldataP[,vars_lm], as.matrix = TRUE,
      xlab = '',main = "Linear Regression: Selected Variables (Step-wise/AIC)",
      varnames=c("Home Off.\nPass YPG","Home Def.\nPass YPG",
                 "Home Off.\nRush YPG","Home Def.\n Rush YPG",
                 "Home Win %","Away Off.\nPass YPG",
                 "Away Def.\nPass YPG", "Away Off. PPG", "Spread"),
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

# Future work: look at how the models do when the logistic regression
# win probability is high/low, or the spread is large





