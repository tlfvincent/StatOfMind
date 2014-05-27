'scrape_ratings' <- function(url)
{  
  # get HTML of url
  doc <- htmlParse(url)

  # find all tables in webpage
  tables <- readHTMLTable(doc)
  
  # find largest table and return as dataframe
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(nrows)]]
  
  return(df)
}

'clean_dataframe' <- function(df)
{
  # remove html tags
  df[, 1] <- gsub('Â', '', as.vector(df[, 1]))
  
  # remove commas in numeric columns (messes up R)
  df$UserVotes <- gsub(',', '', as.vector(df$UserVotes))
  
  # remove non-annotated seasons and episode
  missing.rows <- which(df[, 1]=='-')
  if(length(missing.rows) > 0 )
  {
    df <- df[-missing.rows, ]
  }
  
  #extract season and episode numbers
  series.episode <- do.call(rbind, strsplit(df[, 1], split='.', fixed=TRUE))
  
  # store and return clean dataframe of series ratings
  clean.df <-  data.frame(season = as.numeric(series.episode[, 1]),
                          episode = as.numeric(series.episode[, 2]),
                          title = df$Episode,
                          rating = as.numeric(as.vector(df$UserRating)),
                          VoteCount = as.numeric(as.vector(df$UserVotes)))
  
  return(clean.df)
}

'find_series_sentiment' <- function(affin, stop.words, clean.df)
{
  # read word frequency for each episode
  #files <- list.files(pattern = "txt")
  episode.sentiment <- mat.or.vec(nrow(clean.df), 3)
  colnames(episode.sentiment) <- c('season', 'episode', 'score')
  word.frequency <- vector('list', nrow(clean.df))
  for(f in 1:nrow(clean.df))
  {
    input.file <- sprintf('word_freq_%d_%d.txt', clean.df$season[f], clean.df$episode[f])
    if(file.exists(input.file))
    {
      #file.info <- strsplit(input.file, split='_', fixed=TRUE)[[1]]
      
      episode.sentiment[f, 1] <- clean.df$season[f]
      episode.sentiment[f, 2] <- clean.df$episode[f]
      
      # read in word distribution
      freq <- read.csv(input.file, header=FALSE)
      
      # remove common English words
      #index.common <- match(stop.words, freq[, 1])
      #index.common <- index.common[!is.na(index.common)]
      #if(length(index.common) > 0)
      #{
      #  freq <- freq[-index.common, ]
      #}
      
      # store in list for future use
      word.frequency[[f]] <- freq
      
      # find global sentiment score of episode
      sentiment.words <- intersect(freq[, 1], affin[, 1])
      sentiment.words.index <- match(sentiment.words, affin[, 1])
      if(length(sentiment.words.index) > 0 )
      {
        sentiment.words.score <- affin[sentiment.words.index, 2]
        sentiment.words.frequency <- freq[match(sentiment.words, freq[, 1]), 2]
        episode.sentiment[f, 3] <- sum(sentiment.words.score * sentiment.words.frequency)
      }
    }
  }
  
  return(episode.sentiment)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
'multiplot' <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
      layout.pos.col = matchidx$col))
    }
  }
}
