def rating_crawler():
  ####################################################################
  # This function downloads all NBA game by game data for seasons
  # between 1964 and 2014. All information is stored in a SQL database
  # called game_data.db
  ####################################################################
  import urllib2
  from bs4 import BeautifulSoup
  import re
  import math
  import json
  import sqlite3

  db = sqlite3.connect('game_data.db')
  print "Opened database successfully";
  
  cur = db.cursor()
  cur.execute("SELECT * FROM NBA LIMIT 10")
  for game in cur.fetchall():
    print game

    db.execute('''CREATE TABLE NBA
      (ID INTEGER PRIMARY KEY,
      YEAR INTEGER,
      MONTH REAL,
      DAY INTEGER,
      TEAM1 TEXT,
      TEAM1_SCORE REAL,
      TEAM2 TEXT,
      TEAM2_SCORE REAL,
      HOME_TEAM TEXT);''')
    print "Table created successfully";

    year = range(1946, 2013)
    league_differential = dict()
    for y in year:
      print y
    
      # extract html for specified URL
      season_start = y
      season_end = y + 1
      url = 'http://www.landofbasketball.com/results/%s_%s_scores_full.htm' % (str(season_start), str(season_end))
      try:
        response = urllib2.urlopen(url)
        page_source = response.read()
        soup = BeautifulSoup(page_source, 'html.parser')
      except IOError:
        sys.stderr.write('Problem reading getting data for year ' + y)


      table = soup.findAll('table', {'class': 'c0s5'})
      table = table[1]
      # create multiple empty lists
      year, month, day, team1, team1_score, team2, team2_score, home_team = ([] for i in range(8))
      count = 0
      for row in table.findAll("tr", valign="top"):
          cells = row.findAll("td")
          #For each "tr", assign each "td" to a variable.
          if len(cells) == 7:
            count += 1
            # extract and record date of game
            mystr = cells[0].find(text=True)
            match = re.search(r'(\w+)\s*(\d+),\s*(\d+)', mystr)
            month.append(match.group(1))
            day.append(match.group(2))
            year.append(match.group(3))
    
            # record name and score of team 1
            team1.append(cells[1].findAll(text=True)[0].strip())
            team1_score.append(cells[2].findAll(text=True)[0].strip())
    
            # record name and score of team 2
            mystr = cells[3].find(text=True)
            mystr = re.sub(r'[\n\t]', '', mystr).strip()
            team2.append(mystr)
            team2_score.append(cells[4].findAll(text=True)[0].strip())
    
            # record name of home team
            mystr = cells[6].find(text=True)
            mystr = re.sub(r'[\n\t]', '', mystr)
            mystr = re.sub(r'\s+at\s+', '', mystr)
            home_team.append(mystr)
        else:
            print 'skip!'

        for index in range(0, count):
          insertstmt=("INSERT INTO NBA (YEAR, MONTH, DAY, TEAM1, TEAM1_SCORE, TEAM2, TEAM2_SCORE, HOME_TEAM) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')" % (str(year[index]), str(month[index]), str(day[index]), str(team1[index]), str(team1_score[index]), str(team2[index]), str(team2_score[index]), str(home_team[index])))
        db.execute(insertstmt)



if __name__ == '__main__':
	rating_crawler()


#cur = db.cursor()
#cur.execute("select * from nba")
#for game in cur.fetchall():
#  print game

