def game_pbp_crawler():
	'''
	leverages the basketball-reference.com website to download
	play-by-play data for all games played between 2000 and 2014.
	The only information that is retained is the year, quarter, time
	in quarter, and score. The function can be adapted to incorporate
	more information. All information is stored in a SQL database
	'''

	import urllib2
	from bs4 import BeautifulSoup
	import re
	import sqlite3

	db = sqlite3.connect('game_play_by_play_2000_2015.db')
	print "Opened database successfully";

	db.execute('''CREATE TABLE GAME_PBP
		(ID INTEGER PRIMARY KEY,
		GAME_ID TEXT,
		YEAR INTEGER,
		HOME_TEAM TEXT,
		AWAY_TEAM TEXT,
		QUARTER INTEGER,
		TIME REAL,
		SCORE TEXT);''')
	print "Table created successfully";

	year = range(2001, 2015)
	for y in year:
		print y
		# find all links to play-by-play data fir given season 
		url = 'http://www.basketball-reference.com/leagues/NBA_' + str(y) + '_games.html'
		response = urllib2.urlopen(url)
		page_source = response.read()
		soup = BeautifulSoup(page_source, "html5lib")
		table = soup.find("table", id="games")
		table_subset = table.findAll("tr")

		# go through each game in season
		for row in range(1, len(table_subset)-1):
			cells = table_subset[row].findAll("td")
			# extract play-by-play id and follow URL
			if len(str(cells[1])) > 0:
				searchObj = re.search( r'/(\d+\w{3}).html', str(cells[1]))
			if searchObj:
				game_id = searchObj.group(1)
				print game_id
				pbp_url = 'http://www.basketball-reference.com/boxscores/pbp/' + str(game_id) + '.html'
				pbp_response = urllib2.urlopen(pbp_url)
				pbp_source = pbp_response.read()
				pbp_soup = BeautifulSoup(pbp_source, "html5lib")

				# extract play-by-play information
				pbp_table = pbp_soup.find("table", attrs = {"class": "no_highlight stats_table"})
				if pbp_table is not None:
					pbp_table_subset = pbp_table.findAll("tr")

					# find teams that are playing
					team_info = pbp_table_subset[1].findAll("th")
					away_team = str(team_info[1].find(text=True))
					home_team = str(team_info[3].find(text=True))

					# find score and time at which a play took place
					time = []
					score = []
					for play in range(3, len(pbp_table_subset)-1):
						pbp_cells = pbp_table_subset[play].findAll("td")
						tmp = []
						for c in pbp_cells:
							tmp.append(c.find(text=True))
						if len(tmp) == 6:
							time.append(str(tmp[0]))
							score.append(str(tmp[3]))

					# find quarter in which each play took place
					quarter = []
					current_quarter = 0
					counter = 0
					for t in time:
						if int(t.split(':')[0]) == 11 and counter == 0:
							current_quarter += 1
							counter = 1
						if int(t.split(':')[0]) == 0:
							counter = 0
						quarter.append(current_quarter)

					# initiate game id list
					game_id_list = [game_id for i in range(len(time))]
					# initiate year list
					year_list = [y for i in range(len(time))]
					# initiate home and away team list
					away_team_list = [away_team for i in range(len(time))]
					home_team_list = [home_team for i in range(len(time))]

					# add data in SQL database
					for index in range(len(time)):
						insertstmt=("INSERT INTO GAME_PBP (GAME_ID, YEAR, HOME_TEAM, AWAY_TEAM, QUARTER, TIME, SCORE) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s')" % (str(game_id_list[index]), str(year_list[index]), str(home_team_list[index]), str(away_team_list[index]), str(quarter[index]), str(time[index]), str(score[index])))
						db.execute(insertstmt)

	# close SQL database
	db.commit()
	db.close()

if __name__ == '__main__':
	game_pbp_crawler()
	
