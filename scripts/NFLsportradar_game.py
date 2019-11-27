#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 23:44:08 2019

@author: zackkingbackup
"""

###################################
# Simple Program to make a call to
# SportsRadar's NFL API v5
###################################

import requests
import json
import numpy
import pandas as pd
import time

f = open('my_api_key.json','r')
my_key = json.load(f)['api_key']
f.close()

week_num = 0
game_num = 0

#game_id = season19['weeks'][week_num]['games'][game_num]['id']
for season_year in [18,17,16]:
    f = open('nfl20{}season.json'.format(season_year),'r')
    season = json.load(f)
    f.close()
    # try to get all game_ids for a particular season
    sXX_games = {}
    for week in range(len(season['weeks'])):
    #for week in range(14):
        for game in range(len(season['weeks'][week]['games'])):
            sXX_games['s{}-w{}-g{}'.format(season_year,week,game)] = season['weeks'][week]['games'][game]['id']
    
    # extract the full game data for each game_id and save it off to folder: games
    # don't extract beyond week 14 so save on API requests
    sXXgame_list = []
    for key in sXX_games.keys():
        print("Making call for {}".format(key))
        # make the API call
        game_id = sXX_games[key]
        game_response = requests.get(\
                                     "https://api.sportradar.us/nfl/official/trial/v5/en/games/{}/statistics.json".format(str(game_id)),
                                     params={'api_key':my_key,'format':'json'})
        gjson = game_response.json()
        print("Parsing return values for {}".format(key))
        f = open('games/{}.json'.format(key), 'w')
        json.dump(gjson, f)
        f.close()
        sXXgame_list.append(gjson)
        time.sleep(1.1)
    
    f = open('season{}_allgames.json'.format(season_year),'w')
    json.dump(sXXgame_list, f)
    f.close()
#game_response = requests.get(\
#"https://api.sportradar.us/nfl/official/trial/v5/en/games/{}/statistics.json".format(str(game_id)),
#                               params={'api_key':my_key,'format':'json'})



#sjson = season_response.json()
#f = open("nfl{}season.json".format(str(year)),'w')
#json.dump(sjson, f)
#f.close()
#time.sleep(1.1)