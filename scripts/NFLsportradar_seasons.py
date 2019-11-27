#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 23:12:32 2019

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

for year in [2016, 2017, 2018]:

    season_response = requests.get("https://api.sportradar.us/nfl/official/trial/v5/en/games/{}/reg/schedule.json".format(str(year)),
                                   params={'api_key':my_key,'format':'json'})
    sjson = season_response.json()
    f = open("nfl{}season.json".format(str(year)),'w')
    json.dump(sjson, f)
    f.close()
    time.sleep(1.1)
