library(XML)
require(ggplot2)
require(grid)
setwd("~/Sentiment_Analysis_of_TV_shows")
parent.dir <- getwd()
source('misc.R')

tv.series <- c('BBT', 'Breaking Bad', 'Family Guy' ,'Friends',
          'Glee', 'GoT', 'Greys Anatomy', 'HIMYM', 'Mad Men',
          'Sex in the City', 'Simpsons', 'South Park', 'West Wing')
id <- c('tt0898266', 'tt0903747', 'tt0182576', 'tt0108778',
        'tt1327801', 'tt0944947', 'tt0413573', 'tt0460649' ,
        'tt0804503' ,'tt0159206', 'tt0096697', 'tt0121955',
        'tt0200276')

## ONLY NEEDS TO BE RAN ONCE ##
#imdb.id <- cbind(tv.series, id)
#series.ratings <- vector('list', nrow(imdb.id))
#names(series.ratings) <- series
#for(i in 1:nrow(imdb.id))
#{
#  cat('Processing series', imdb.id[i, 1], '...\n')
#  url <- paste('http://www.imdb.com/title/', imdb.id[i, 2], '/epdate', sep='')
#  series.ratings[[i]] <- scrape_ratings(url)
#}
#save(series.ratings, file='series_ratings.Rdata')
################################


# define stop words in English dictionnary
stop.words <- as.vector(scan(file=sprintf('%s/english_stop_words.txt', parent.dir), sep=',', what='raw()'))

# read AFINN list of sentiment words
affin <- read.csv(file=sprintf('%s/AFINN/AFINN-111.txt', parent.dir), header=FALSE)

# find all tv series in directory
tv.series.dir <- list.dirs()[-c(1:2)]
tv.series.dir <- gsub('./', '', tv.series.dir)

load(file='series_ratings.Rdata')
imdb.ratings <- episode.sentiment <- vector('list', length(series.ratings))
for(s in 1:length(series.ratings))
{
  cat('Analyzing', tv.series[s], '...\n')
  ratings <- series.ratings[[s]]
  
  # clean up series ratings dataframe
  imdb.ratings[[s]] <- clean_dataframe(ratings)
  
  # get sentiment for each episode in series
  work.dir <- sprintf('%s/%s', parent.dir, tv.series.dir[s])
  setwd(work.dir)
  episode.sentiment[[s]] <- find_series_sentiment(affin, stop.words, imdb.ratings[[s]])
  
  # remove rows with null data
  null.rows <- which(apply(episode.sentiment[[s]], 1, sd)==0)
  if(length(null.rows) > 0){
    episode.sentiment[[s]] <- episode.sentiment[[s]][-null.rows, ]
    imdb.ratings[[s]] <- imdb.ratings[[s]][-null.rows, ]
  }
  
  # plot imdb ratings for each episode
  p1 <- ggplot(imdb.ratings[[s]], aes(x=episode, y=rating, color=factor(season))) +
              theme_bw() +
              facet_grid(. ~ season, scales = "free") +
              geom_point(shape=20, size=5) +
              geom_smooth(method = "lm", size=1.5) +
              scale_colour_discrete(name="Season") +
              theme(axis.text.y  = element_text(size=20),
              axis.title.y  = element_text(size=28),
              axis.text.x  = element_text(size=20, angle=45, hjust=1),
              axis.title.x  = element_text(size=26)) +
              theme(legend.title=element_text(size=26)) +
              theme(legend.title=element_text(size=26),
              legend.text = element_text(size = 24)) +
              theme(legend.key = element_rect(colour = "black"), legend.key.size = unit(1, "cm")) +
              theme(strip.text.x = element_text(size = 26)) +
              ylab('IMDB rating') +
              ggtitle("IMDB user ratings") +
              theme(plot.title = element_text(lineheight=.8, face="bold", size=32))
  
  # plot sentiment score for each episod
  df <- episode.sentiment[[s]]
  df <- as.data.frame(apply(df, 2, as.numeric))
  p2 <- ggplot(df, aes(x=episode, y=score, color=factor(season))) +
              theme_bw() +
              facet_grid(. ~ season, scales = "free") +
              geom_point(shape=20, size=5) +
              geom_smooth(method = "lm", size=1.5) +
              scale_colour_discrete(name="Season") +
              theme(axis.text.y  = element_text(size=20),
                    axis.title.y  = element_text(size=28),
                    axis.text.x  = element_text(size=20,  angle=45, hjust=1),
                    axis.title.x  = element_text(size=26)) +
              theme(legend.title=element_text(size=26),
                    legend.text = element_text(size = 24)) +
              theme(legend.key = element_rect(colour = "black"), legend.key.size = unit(1, "cm")) +
              theme(strip.text.x = element_text(size = 26)) +
              ylab('Sentiment Score') +
              ggtitle("Sentiment Score") +
              theme(plot.title = element_text(lineheight=.8, face="bold", size=32))

  # plot all together
  setwd(parent.dir)
  png(file=sprintf('%s.png', tv.series[s]), width=1800, height=1100)
  multiplot(p1, p2, cols=1)
  dev.off()
}

# concatenate all data into a single dataframe
series <- rep(tv.series, times=unlist(lapply(episode.sentiment, nrow)))
episode.index <- unlist(sapply(unlist(lapply(episode.sentiment, nrow)), function(x) 1:x))
episode.sentiment <- do.call(rbind, episode.sentiment)
imdb.ratings <- do.call(rbind, imdb.ratings)
dat <- data.frame(episode.sentiment, imdb.ratings, series=series, index=episode.index)

# plot imdb ratings for all tv series
png(file='rating_all_series.png', width=1800, height=500)
ggplot(dat, aes(x=index, y=rating, colour=factor(series))) +
      theme_bw() +
      facet_grid(. ~ series, scales = "free") +
      geom_point(shape=20, size=2) +
      geom_smooth(method = "loess", size=1) +
      scale_colour_discrete(name="Series") +
      theme(axis.text.y  = element_text(size=20),
            axis.title.y  = element_text(size=28),
            axis.text.x  = element_text(size=14,  angle=45, hjust=1),
            axis.title.x  = element_text(size=26)) +
      theme(legend.title=element_text(size=26),
      legend.text = element_text(size = 24)) +
      theme(legend.key = element_rect(colour = "black"), legend.key.size = unit(1, "cm")) +
      theme(strip.text.x = element_text(size = 18)) +
      ylab('IMDB rating') + xlab('Episode') +
      ggtitle("IMDB user rating") +
      theme(plot.title = element_text(lineheight=.8, face="bold", size=32))
dev.off()

# plot sentiment score for all tv series
png(file='sentiment_all_series.png', width=1800, height=500)
ggplot(dat, aes(x=index, y=score, colour=factor(series))) +
      theme_bw() +
      facet_grid(. ~ series, scales = "free") +
      geom_point(shape=20, size=2) +
      geom_smooth(method = "loess", size=1) +
      scale_colour_discrete(name="Series") +
      theme(axis.text.y  = element_text(size=20),
            axis.title.y  = element_text(size=28),
            axis.text.x  = element_text(size=14,  angle=45, hjust=1),
            axis.title.x  = element_text(size=26)) +
      theme(legend.title=element_text(size=26),
      legend.text = element_text(size = 24)) +
      theme(legend.key = element_rect(colour = "black"), legend.key.size = unit(1, "cm")) +
      theme(strip.text.x = element_text(size = 18)) +
      ylab('Sentiment Score') + xlab('Episode') +
      ggtitle("Sentiment Score") +
      theme(plot.title = element_text(lineheight=.8, face="bold", size=32))
dev.off()


# perform 10-fold CV using linear regression
require(earth)
require(cvTools)
K <- 10 #the number of folds
folds <- split(sample(nrow(dat)),rep(1:K,length=nrow(dat)))
#folds <- cvFolds(nrow(dat), K=K)
RMSE.lm <- mat.or.vec(K, 1)
RMSE.mars <- mat.or.vec(K, 1)
y <- y.hat <- series <- c()
for(k in 1:K)
{
  #index.test <- which(folds$which == k)
  index.test <- folds[[k]]
  train <- dat[-index.test, ]
  test <- dat[index.test, ]
  
  # run multiple regression
  fit <- lm(rating ~ score + VoteCount +series, data=train)
  y.pred <- predict(fit, test)
  RMSE.lm[k] <- sqrt(mean((test$rating - y.pred)^2))
  
  # run MARS
  fit <- earth(rating ~ ., data = train[, c(3, 7, 8, 9)])
  y.pred <- predict(fit, test[, c(3, 7, 8, 9)])
  RMSE.mars[k] <- sqrt(mean((test$rating - y.pred)^2))
  
  y <- c(y, test$rating)
  y.hat <- c(y.hat, y.pred)
  series <- c(series, test$series)
}

# plot RMSE obtained by multiple regression and MARS at each fold  
df <- data.frame(RMSE=c(RMSE.lm, RMSE.mars), Model=c(rep('lm', 10), rep('MARS', 10)), fold=rep(1:10, times=2))
ggplot(df, aes(x=factor(fold), y=RMSE, fill=Model)) +
      theme_bw() +
      geom_bar(colour="black", stat="identity", position=position_dodge(), size=.1) +
      scale_fill_manual(values=c("#999999", "#E69F00")) +
      theme(axis.text.y  = element_text(size=20),
            axis.title.y  = element_text(size=28),
            axis.text.x  = element_text(size=20),
            axis.title.x  = element_text(size=26)) +
      theme(legend.title=element_text(size=26),
            legend.text = element_text(size = 24)) +
      ylab('RMSE') + xlab('Fold')


# plot average error of MARS
hist(y - y.hat, breaks=100,
                xlim=c(-2, 2),
                col='royalblue3',
                xlab='True rating - Predicted rating',
                main='Predicted IMDB user ratings by MARS')

