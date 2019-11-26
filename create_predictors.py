#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 25 18:33:43 2019

@author: zackkingbackup
"""

import pandas as pd

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

# First figure out a way to get the home teams's stats for the year to date
# start with just passing_yards_per_game for grins (don't include the current week!!!)
# Define a function that takes 'season', 'week', 'home_team', and of course dfALL
#
# TODO: refactor this function to collect ALL STATS for every record of dfALL
def get_passing_ypg(dfALL, season, week, home_team):
    '''
    this is super slow, get smarter about how you implement this idea
    
    the answer is correct though
    '''
    
    # start by getting a subset only containing games played
    # by home team ytd going into the current game
    dfHT = dfALL[(dfALL['home_team'] == home_team) | (dfALL['away_team'] == home_team)]
    dfHTseasonTD = dfHT[(dfHT['season'] == season) & (dfHT['week'] < week)]
    
    # how many games so far?
    games_played = len(dfHTseasonTD)
    
    # compute average passing yards per game, trick is to figure out if the 'home_team'
    # is home or away in previous games
    # probably a pandas.DataFrame.apply kind of situation
    dfHTseasonTD['passing_yards'] = dfHTseasonTD.apply(lambda row: get_off_stat(row, 'passing_yards', 
                home_team), axis=1)
    
    # try average passing yards ALLOWED
    dfHTseasonTD['def_passing_yards'] = dfHTseasonTD.apply(lambda row: get_def_stat(
            row, 'passing_yards',home_team), axis=1)
        
    return sum(dfHTseasonTD['passing_yards']) / games_played, sum(dfHTseasonTD['def_passing_yards']) / games_played  
    
# function that takes in a team name, a stat (like passing yards), and a row of dfALL
def get_off_stat(row, stat, team):
    if row['home_team'] == team:
        statistic = 'home_'+stat
        return row[statistic]
    elif row['away_team'] == team:
        statistic = 'away_'+stat
        return row[statistic]
    else:
        print ("ERROR")
        return
    
# function that takes in a team name, a stat (like passing yards), and a row of dfALL
def get_def_stat(row, stat, team):
    if row['home_team'] == team:
        statistic = 'away_'+stat
        return row[statistic]
    elif row['away_team'] == team:
        statistic = 'home_'+stat
        return row[statistic]
    else:
        print ("ERROR")
        return