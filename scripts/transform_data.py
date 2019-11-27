#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 24 16:02:29 2019

@author: zackkingbackup
"""

import json
import pandas as pd

#total_points = allgames[0]['summary']['home']['points'] + \
#               allgames[0]['summary']['away']['points']
season_dict = {}
for seasonfile in ['season16_allgames.json','season17_allgames.json',
                   'season18_allgames.json','season19_allgames.json']:
    f = open(seasonfile,'r')
    season_json = json.load(f)
    f.close()
    season_dict[seasonfile.split('_')[0]] = season_json
    
# what tables do we want??
# 1 table per season that includes a record for each game containing fields:
#
# a. total points
# b. home points
# c. away points
# d. home team && away team
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
# t. has been played yet?
#
# May need intermediate tables with whatever raw stats can be found from the extract
    
# Define a function here to create the record per season

def get_record(game):
    '''
    
    Params
    ------
    game : dict
        dictionary of statistics for one game
        
    Returns
    -------
    one table record
    '''
    week = game['summary']['week']['sequence']
    
    played = True
    if 'statistics' not in game.keys():
        played = False
    
    # d - d1
    home_team = game['summary']['home']['name']
    away_team = game['summary']['away']['name']
    home_alias = game['summary']['home']['alias']
    away_alias = game['summary']['away']['alias']
    
    if not played:
        return (played, home_team, away_team, home_alias, away_alias,
                0,0,0,0,0,0,0,0,week)
    
    # a-c
    homePts = game['summary']['home']['points']
    awayPts = game['summary']['away']['points']
    winner = get_winner(homePts, awayPts)
    totalPts = homePts + awayPts
    
    # e-f: records will have to be derived later
    # by subsetting on team and parsing on winner/home/away team
    
    # g-s will have to be derived from the statistics below
    home_passing_yards = game['statistics']['home']['passing']['totals']['net_yards']
    away_passing_yards = game['statistics']['away']['passing']['totals']['net_yards']
    
    home_rushing_yards = game['statistics']['home']['rushing']['totals']['yards']
    away_rushing_yards = game['statistics']['away']['rushing']['totals']['yards']
    
    return (played, home_team, away_team, home_alias, away_alias,
            homePts, awayPts, winner, totalPts, home_passing_yards,
            away_passing_yards, home_rushing_yards, away_rushing_yards, week)
    
    
def get_winner(homePts, awayPts):
    if homePts > awayPts:
        return 'home'
    elif awayPts > homePts:
        return 'away'
    elif awayPts == homePts:
        return 'tie'
    else:
        print("error determining winner")
        exit(1)
        
field_names = ["played","home_team",'away_team','home_alias','away_alias',
               'homePts','awayPts','winner','totalPts','home_passing_yards',
               'away_passing_yards','home_rushing_yards','away_rushing_yards','week']

# pd.DataFrame.from_dict(dictionary,orient='index')
for key in season_dict.keys():
    game_dict = {}
    
    for idx, game in enumerate(season_dict[key]):
        record = get_record(game)
        game_dict[idx] = record
    
    df = pd.DataFrame.from_dict(game_dict, columns=field_names, orient='index')
    df.to_csv("{}_game_records.csv".format(key),index=False)

# read all 4 csvs back in, append a season column with the year, write out to one csv
df16 = pd.read_csv("season16_game_records.csv")
df17 = pd.read_csv("season17_game_records.csv")
df18 = pd.read_csv("season18_game_records.csv")
df19 = pd.read_csv("season19_game_records.csv")

df16['season'] = 2016
df17['season'] = 2017
df18['season'] = 2018
df19['season'] = 2019

dfALL = pd.concat([df16, df17, df18, df19])
    