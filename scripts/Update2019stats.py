#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 29 14:34:26 2019

@author: zackkingbackup
"""

import requests
import json
import time

f = open('/Users/zackkingbackup/Documents/Grad Schools/GMU_Masters/STAT515/RedesignProject2/keys/my_api_key.json','r')
my_key = json.load(f)['api_key']
f.close()

season_year = 19

f = open('../raw_api_data/seasons/nfl2019season.json','r')
season = json.load(f)
f.close()
# try to get all game_ids for a particular season
s19_games = {}
for week in [12,13]:
#for week in range(14):
    for game in range(len(season['weeks'][week]['games'])):
        s19_games['s{}-w{}-g{}'.format(season_year,week,game)] = season['weeks'][week]['games'][game]['id']

# extract the full game data for each game_id and save it off to folder: games
# don't extract beyond week 14 so save on API requests
s19game_list = []
for key in s19_games.keys():
    print("Making call for {}".format(key))
    # make the API call
    game_id = s19_games[key]
    game_response = requests.get(\
                                 "https://api.sportradar.us/nfl/official/trial/v5/en/games/{}/statistics.json".format(str(game_id)),
                                 params={'api_key':my_key,'format':'json'})
    gjson = game_response.json()
    print("Parsing return values for {}".format(key))
    f = open('../raw_api_data/games/{}-update.json'.format(key), 'w')
    json.dump(gjson, f)
    f.close()
    s19game_list.append(gjson)
    time.sleep(1.1)

f = open('season19_week12_13.json'.format(season_year),'w')
json.dump(s19game_list, f)
f.close()


