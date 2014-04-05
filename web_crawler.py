def rating_crawler():
  import urllib2
  from bs4 import BeautifulSoup

  year = range(1981, 2014)
  for y in year:
    print y
  
    url = 'http://www.basketball-reference.com/leagues/NBA_' + str(y) + '.html'
    response = urllib2.urlopen(url)
    page_source = response.read()
    soup = BeautifulSoup(page_source)
    table = soup.find("table", id="opponent")
    table_subset = table.findAll("tr")
  
    file_name = 'Opponent_stats_' + str(y) + '.txt'
    f = open(file_name, 'w')
    for row in range(1, len(table_subset)-1):
      cells = table_subset[row].findAll("td")
      table_data = []
      for c in cells:
        table_data.append(c.find(text=True))
      string = str(','.join(table_data))
      f.write(string + '\n')
    f.close()
	
if __name__ == '__main__':
	rating_crawler()
	
