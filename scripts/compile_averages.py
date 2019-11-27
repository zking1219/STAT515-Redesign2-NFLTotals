#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 26 18:07:39 2019

@author: zackkingbackup
"""

import pandas as pd
from create_predictors import get_team_stats

dfALL = pd.read_csv("NFL_game_dataset_2016-19.csv")

# Derive all of the following:
# e. home record (going in)
# f. away record (going in)
# g. home passing yards per game
# h. home rushing yards per game
# i. home pasing defense yards per game
# j. home rushing defense yards per game
# k. home points scored per game
# m. home points allowed per game
# n. away passing yards per game
# o. away rushing yards per game
# p. away pasing defense yards per game
# q. away rushing defense yards per game
# r. away points scored per game
# s. away points allowed per game

# do this for every record that isn't a week1 (need separate model/metrics, 
# maybe use the previous year's stats instead of ytd stats)
#
# order of both home_stats and away_stats is:
# passing_yards_off, passing_yards_def, rushing_yards_off, rushing_yards_def,
# offensive_ppg, defensive_ppg, win_pct

column_names = ["home_off_pass_ypg", "home_def_pass_ypg",
                "home_off_rush_ypg", "home_def_rush_ypg",
                "home_off_ppg", "home_def_ppg", "home_win_pct",
                "away_off_pass_ypg", "away_def_pass_ypg",
                "away_off_rush_ypg", "away_def_rush_ypg",
                "away_off_ppg", "away_def_ppg", "away_win_pct"]

rec_dict = {}

#rec1 = dfALL.iloc[150]
#home_stats = get_team_stats(dfALL, rec1.season, rec1.week, rec1.home_team)
#away_stats = get_team_stats(dfALL, rec1.season, rec1.week, rec1.away_team)
#new_rec = home_stats + away_stats
#rec_dict['{}_{}_{}_{}'.format(rec1.home_team, rec1.away_team, rec1.week, rec1.season)] = new_rec

for idx in dfALL.index:
    rec = dfALL.loc[idx]
    if rec.week == 1:
        continue
    print("{} v. {}".format(rec.home_team, rec.away_team))
    home_stats = get_team_stats(dfALL, rec.season, rec.week, rec.home_team)
    away_stats = get_team_stats(dfALL, rec.season, rec.week, rec.away_team)
    new_rec = home_stats + away_stats
    rec_dict['{}_{}_{}_{}'.format(rec.home_team, rec.away_team, rec.week, rec.season)] = new_rec
    
new_df = pd.DataFrame.from_dict(rec_dict, orient='index', columns=column_names)

new_df.to_csv("WeeklyAverages.csv")
# Add various choices for dependent variable back into new_rec such as:
# winner, totalPts, homePts, awayPts
# AND obvious requirements for the models like who the teams were
#
# NO - SCREW ALL OF THAT
# Just put season, week, and the teams back in as a composite key and 
# join the new dataframe back to the original

