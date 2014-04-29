def rating_crawler():
  import urllib2
  from bs4 import BeautifulSoup
  import re
  import math

  teams = ['ATL', 'BOS', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW', 'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NJN', 'NOH', 'NYK', 'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS']
  
  file_name = 'NBA_trade_all_data.txt'
  f = open(file_name, 'w')
  
  for t in range(0, len(teams)-1):
    for tt in range(t+1, len(teams)):
      print('%s --- %s' % (teams[t], teams[tt]))
      url = 'http://www.basketball-reference.com/friv/trades.cgi?f1=' + str(teams[t]) + '&f2=' + str(teams[tt])
      response = urllib2.urlopen(url)
      page_source = response.read()
      soup = BeautifulSoup(page_source)
      trades = soup.findAll(class_='bullets')
      dates = soup.findAll(class_='bold_text')

      #lis = [li for info in trades for li in info.findAll('li')]
      trade_ws = []
      for info in trades:
        for li in info.findAll('li'):
          trade_ws.append(li)
          #print(li)
    
      year = []
      for ds in dates:
        word = ds.text.encode("utf-8")
        if ',' in word:
          word2 = word.split(', ')
          year.append(word2[1])

      count=0
      for ws in trade_ws:
        year_index = int(math.floor(count / 2))
        word = ws.text.encode("utf-8")
        word2 = word.split(' ')
        ws_past = int([i for i, x in enumerate(word2) if x == 'past'][0]) - 1
        ws_future = int([i for i, x in enumerate(word2) if x == 'future'][0]) - 1
        team_name = str(' ' .join(word2[0:ws_past-1]))
        info = []
        info.extend((team_name, str(word2[ws_past]), str(word2[ws_future]), year[year_index]))
        string = str(','.join(info))
        f.write(string + '\n')
        count += 1

  f.close()

	
if __name__ == '__main__':
	rating_crawler()

