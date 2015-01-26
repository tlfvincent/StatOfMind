def scrape_draft_data(year):
	'''
	leverages the basketball-reference.com website to download
	draft data for user-defined year
	'''
	# extract content of web page for draft data of input year
	url = 'http://www.basketball-reference.com/draft/NBA_' + str(year) + '.html'
	try:
		response = urllib2.urlopen(url)
	except urllib2.HTTPError, e:
		print e.code
	except urllib2.URLError, e:
		print e.args
	page_source = response.read()
	soup = BeautifulSoup(page_source, "html5lib")
	# extract number in primary table of web page
	table = soup.find( "table", {"id":"stats"} )
	table_tr = table.findAll("tr")
	# parse through rows of table and compute statistics
	df = []
	for row in table_tr:
		cells = row.findAll("td")
		entry = [c.find(text=True) for c in cells]
		if len(entry) > 0:
			df.append({'draft_year':year, 'rank':entry[0], 'college': entry[4], \
								'team':entry[2], 'player':entry[3], 'gp': entry[6], \
								'mp':entry[7], 'mpg':entry[-6], 'ppg':entry[-5], \
								'rbg':entry[-4], 'apg':entry[-3], 'yrs': entry[5], \
								'ws':entry[-2], 'ws_48':entry[-1]})
	draft_data = pd.DataFrame(df)
	return draft_data


if __name__ == '__main__':
	import urllib2
	from bs4 import BeautifulSoup
	import pandas as pd
	#import numpy as np

	year = range(1980, 2014)
	draft_data = []
	for y in year:
		print 'Retrieving draft data for ' + str(y)
		draft_year = scrape_draft_data(y)
		draft_data.append(draft_year)

	df = pd.concat(draft_data)
	df.to_csv('nba_draft_data.txt', sep=',', index=False, encoding='utf-8')



