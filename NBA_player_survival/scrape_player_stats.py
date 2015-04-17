def get_page_content(url):
  '''
  extract HTML content of user-defined URL
  '''
  try:
    response = urllib2.urlopen(url)
  except urllib2.HTTPError, e:
    print e.code
  except urllib2.URLError, e:
    print e.args
  page_source = response.read()
  soup = BeautifulSoup(page_source, "html5lib")
  return soup


def scrape_player_stats(player_name, player_url, header):
  '''
  extract and store player seasonal per game performance in 
  pandas dataframe
  '''
  url = 'http://www.basketball-reference.com' + player_url
  soup_player = get_page_content(url)
  player_season = soup_player.findAll( "tr", {"id":re.compile("^per_game")})
  if len(player_season) > 1:
    df = pd.DataFrame(columns=header)
    for season in player_season:
      try:
        test = [i.text for i in season.findAll("td")]
        test.append(player_name)
        df = df.append(pd.Series(test, index=header), ignore_index=True)
      except:
        continue
    return df
  else:
    return None


def scrape_player_data(letter):
  '''
  scrapes each page in alphabetic order to extract target URL
  of all NBA players that played after 1980
  '''
  # extract content of web page for draft data of input year
  url = 'http://www.basketball-reference.com/players/' + str(letter)
  soup = get_page_content(url)
  df = pd.DataFrame(columns=header)
  # extract number in primary table of web page
  players = soup.findAll( "tr", {"class":""})
  for player in players:
    try:
      player_info = player.findAll( "td")
      player_stats = [el.text for el in player_info]
      player_name = player_stats[0]
      if player_stats[1] > 1980 and player_stats[1] != '':
        print player_name
        player_url = str(player_info[0].findAll('a')[0].get('href'))
        player_data = scrape_player_stats(player_name, player_url, header)
        if player_data is not None:
          df = df.append(player_data, ignore_index=True)
    except:
      continue
  return df




if __name__ == '__main__':
	'''
	Ran on january 27th 2015
	'''
import urllib2
from bs4 import BeautifulSoup
import pandas as pd
import re
#	import nltk
#	from nltk.tokenize import RegexpTokenizer
#	from nltk.corpus import stopwords
#	stop = stopwords.words('english')

import string
alphabet = list(string.ascii_lowercase)

header = ["Season","Age","Tm","Lg","Pos","G",
          "GS","MP","FG","FGA","FG%","3P","3PA",
          "3P%","2P","2PA","2P%","FT","FTA","FT%",
          "ORB","DRB","TRB","AST","STL","BLK",
          "TOV","PF","PTS","Name"]

df = pd.DataFrame(columns=header)
for letter in alphabet:
  all_players = scrape_player_data(letter)
  df = df.append(all_players, ignore_index=True)

df.to_csv('nba_player_stats.csv', encoding='utf-8')