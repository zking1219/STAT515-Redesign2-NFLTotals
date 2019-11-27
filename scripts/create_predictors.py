#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 25 18:33:43 2019

@author: zackkingbackup
"""

import pandas as pd

#dfALL = pd.read_csv("NFL_game_dataset_2016-19.csv")

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


def get_team_stats(dfALL, season, week, team):
    '''
    improve get_passing_ypg by collecting all stats for one team
    
    this function doesn't work for week 1 matchups
    '''
    # start by getting a subset only containing games played
    # by home team ytd going into the current game
    df = dfALL[((dfALL['home_team'] == team) | (dfALL['away_team'] == team)) & 
                (dfALL['season'] == season) & (dfALL['week'] < week) &
                (dfALL['played'] == True)]
    
    games_played = len(df)
    
    # stats to derive rushing/passing (offense and defense)
    stats_rp = ["passing_yards","rushing_yards"]
    
    # order of stat_list will be passing_yards_off, passing_yards_def, 
    # rushing_yards_off, rushing_yards_def, offensive_ppg, defensive_ppg
    stat_list = []
    
    # compute averages for passing/rushing yards gained/allowed
    try:
        for stat in stats_rp:
            df[stat+"_off"] = df.apply(lambda row: get_off_stat(row, stat, team), axis=1)
            df[stat+"_def"] = df.apply(lambda row: get_def_stat(row, stat, team), axis=1)
            stat_list.append(sum(df[stat+"_off"]) / games_played)
            stat_list.append(sum(df[stat+"_def"]) / games_played)
        
        # compute averages for off/def ppg
        df["Pts_off"] = df.apply(lambda row: get_points_scored(row, team), axis=1)
        df["Pts_def"] = df.apply(lambda row: get_points_allowed(row, team), axis=1)
        stat_list.append(sum(df['Pts_off']) / games_played)
        stat_list.append(sum(df['Pts_def']) / games_played)
        
        # compute win_pct
        df["wonlosstie"] = df.apply(lambda row: get_winloss(row, team), axis=1)
        wins = len(df[df["wonlosstie"] == 'won'])
        ties = len(df[df["wonlosstie"] == 'tie'])
        stat_list.append((wins + 0.5*ties) / games_played)
    except:
        return ['failure']*7
    
    return stat_list

    
# function that takes a team name, row of dfALL, and returns offensive points scored
def get_points_scored(row, team):
    if row['home_team'] == team:
        return row['homePts']
    elif row['away_team'] == team:
        return row['awayPts']
    else:
        print ("ERROR")
        return
    
def get_points_allowed(row, team):
    if row['home_team'] == team:
        return row['awayPts']
    elif row['away_team'] == team:
        return row['homePts']
    else:
        print ("ERROR")
        return

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
    
# function to return whether or not the input team won, lost, or tied
def get_winloss(row, team):
    if row['home_team'] == team:
        if row['winner'] == 'home':
            return 'won'
        if row['winner'] == 'away':
            return 'lost'
    if row['away_team'] == team:
        if row['winner'] == 'home':
            return 'lost'
        if row['winner'] == 'away':
            return 'won'
    if row['winner'] == 'tie':
        return 'tie'