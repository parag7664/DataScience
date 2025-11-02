# -*- coding: utf-8 -*-
"""


"""
from math import log
from bs4 import BeautifulSoup
import string
import re

def load_tweets(fileName):
    text =  open(fileName, "r") # encoding="ISO-8859-1" can be added
    line_count = sum(1 for line in open(fileName, "r"))     # counts how many tweets there are in file
    text = text.read().lower()

    if "train" in fileName: 
        text = text.read()
        checking = BeautifulSoup(text, 'lxml')  # HTML decoding
        text = checking.get_text()
    print (type(text))
    return text, line_count

# --------------------------------------------------