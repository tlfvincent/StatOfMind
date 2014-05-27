def multipleReplace(text, wordDict):
  for word in wordDict:
    text = text.replace(word, '')
  return text

from HTMLParser import HTMLParser
class MLStripper(HTMLParser):
  def __init__(self):
    self.reset()
    self.fed = []
  def handle_data(self, d):
    self.fed.append(d)
  def get_data(self):
    return ''.join(self.fed)

def strip_tags(html):
  s = MLStripper()
  s.feed(html)
  return s.get_data()


def rating_crawler():
  import re
  import string
  import urllib2
  from bs4 import BeautifulSoup
  from nltk import FreqDist
  from nltk import word_tokenize
  from nltk.corpus import stopwords
  
  wordDict = ['/n', '?', '!', '.', ',', '\'', '-', '\"']
  stop = stopwords.words('english') + word_tokenize(string.punctuation)
  
  count = 0
  file_name = 'character_lines.txt'
  f = open(file_name, 'w')
  for season in range(1, 8):
    url = 'http://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=big-bang-theory&season=' + str(season)
    response = urllib2.urlopen(url)
    page_source = response.read()
    soup = BeautifulSoup(page_source)
    
    # find all links to original version of episode script
    sAll = soup.findAll('a')
    episode_url = []
    for a in sAll:
      if "view_episode_scripts" in str(a):
        episode_url.append(a['href'])
    
    #episode_url = set(episode_url)
    #episode_url = list(episode_url)
    
    # parse through all links
    if len(episode_url) > 0:
      episode_count = 0
      for vo_url in episode_url:
        episode_count += 1
        print(episode_count)
        
        temp_url = 'http://www.springfieldspringfield.co.uk/' + str(vo_url)
        response = urllib2.urlopen(temp_url)
        page_source = response.read()
        soup1 = BeautifulSoup(page_source)
        
        # extract text lines from html
        div = soup1.find("div", attrs={'class':'episode_script'})
        div_subset = div.findAll("div")
        
        # remove punctuation and html br tags
        lines = div_subset[1].contents
        episode_script = []
        for line in lines:
          if line.find('br') == -1:
            new_line = line.strip()
            #new_line=[i for i in word_tokenize(line.lower()) if i not in stop]
            new_line = multipleReplace(new_line, wordDict)
            episode_script.append(new_line)
        
        # create list of all words in episode
        episode_words = []
        for words in episode_script:
          words_filtered = [w.lower() for w in words.split() if len(w) >= 3]
          episode_words.extend(words_filtered)
        
        # find frequency of words in episode and print to file
        word_distribution = FreqDist(episode_words)
        #print word_distribution.N() # print number of samples
        #print word_distribution.B() # print number of bins
        file_name = 'word_freq_' + str(season) + '_' + str(episode_count) + '.txt'
        f = open(file_name, 'w')
        for word in word_distribution.keys():
          #print word, word_distribution[word]
          data = [word.encode("utf-8"), str(word_distribution[word]).encode("utf-8")]
          string = str(','.join(data))
          f.write(string + '\n')
        
        f.close()

if __name__ == '__main__':
  rating_crawler()
