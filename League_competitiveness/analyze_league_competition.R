# define gobal variables
setwd('~/League_competitiveness')
require(RSQLite)
require(ggplot2)
require(reshape2)
require(bear)

###################################################################
# Find odds of observing distribution of NBA champions obtained
# from 1980 to 2014
home.dir <- getwd()
setwd(home.dir)
dat <- read.table(file='NBA_finals_data.txt', sep='\t', header=TRUE)
head(dat)

champions <- as.vector(dat[which(dat$Year >= 1980), 'Champion'])
champ.freq <- table(champions)
obs <- c(champ.freq, rep(0, (30-length(champ.freq))))

# plot data for number of championships won per team
df <- as.data.frame(champ.freq)
df$champions <- factor(df$champions, levels=names(sort(champ.freq)))
ggplot(df, aes(x=champions, y=Freq)) +
      geom_bar(color='black', fill='royalblue2') +
      theme_classic() +
      ylab('Number of championships') + xlab('') +
      theme(axis.text.x=element_text(size=18, angle=45, hjust=1),
            axis.title.x=element_text(size=22),
            axis.text.y=element_text(size=18),
            axis.title.y=element_text(size=22))


# probability mass function of multinomial distribution
nom <- factorial(sum(champ.freq))
denom <- prod(sapply(obs, factorial))
prob <- (nom / denom) * prod(sapply(obs, function(x) (1/30)^x))
prob
###################################################################


###################################################################
##### THE SECTION IS NECESSARY TO RUN ALL OTHER SCRIPTS BELOW #####
###################################################################
# read in data from SQL database
drv <- dbDriver('SQLite')
con <- dbConnect(drv, 'game_data.db')

# list all the tables in the database
dbListTables(con)
# list all the columns in a given table
dbListFields(con, 'NBA')

# define year and month variables
year <- 1960:2013
month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
###################################################################

###################################################################
### All plots featured in blog post can be reproduce with the
### code below. Not that I decided to treat each plot separately,
### although all of teh data could also have been parsed through a
### single SQL search
###################################################################

###################################################################
# Analyze data at year level
points.by.year <- diff.by.year <- vector('list', length(year))
names(points.by.year) <- names(diff.by.year) <- year
for(y in 1:length(year))
{
  cat('Processing year', year[y], '...\n')
  query <- sprintf('SELECT *  FROM NBA WHERE YEAR=%s', year[y])
  game.data = dbGetQuery(con, query)
  # remove commas from column
  game.data$TEAM1_SCORE <- as.numeric(gsub(',', '', game.data$TEAM1_SCORE))
  points.by.year[[y]] <- game.data$TEAM1_SCORE + game.data$TEAM2_SCORE
  diff.by.year[[y]] <- abs(game.data$TEAM1_SCORE - game.data$TEAM2_SCORE)
}

# plot total number of points scored per game
df <- melt(points.by.year)
colnames(df) <- c('points', 'year')
ggplot(df, aes(x=year, y=points)) +
      geom_boxplot(outlier.size = 0, color='royalblue4') +
      geom_jitter(size=1, alpha=1/4, position = position_jitter(width = 0.15), color='royalblue4') +
      geom_smooth(aes(group=1), size=2, color='royalblue4') +
      theme_classic() +
      ylab('Total points scored per game') + xlab('') +
      theme(axis.text.x=element_text(size=18, angle=45, hjust=1),
      axis.title.x=element_text(size=22),
      axis.text.y=element_text(size=18),
      axis.title.y=element_text(size=22))

# plot point differential between each game
df <- melt(diff.by.year)
colnames(df) <- c('points', 'year')
ggplot(df, aes(x=year, y=points)) +
      geom_boxplot(outlier.size = 0, color='royalblue4') +
      geom_jitter(size=1, alpha=1/4, position = position_jitter(width = 0.15), color='royalblue4') +
      ylim(0, 40) +
      theme_classic() +
      ylab('Point differential per year') + xlab('') +
      theme(axis.text.x=element_text(size=18, angle=45, hjust=1),
            axis.title.x=element_text(size=22),
            axis.text.y=element_text(size=18),
            axis.title.y=element_text(size=22))
###################################################################


###################################################################
# Analyze data at month level
month <- c('Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug')
points.by.month <- diff.by.month <- vector('list', length(year)*length(month))
names(points.by.month) <- names(diff.by.month) <- as.vector(sapply(year, function(x) paste(month, x, sep='-')))
count <- 0
for(y in 1:length(year))
{
  cat('Processing year', year[y], '...\n')
  for(m in 1:length(month))
  {
    count <- count + 1
    query <- sprintf('SELECT * FROM NBA WHERE YEAR=%s AND MONTH="%s"', year[y], month[m])
    game.data = dbGetQuery(con, query)
    if(nrow(game.data) > 0)
    {
      # remove commas from column
      game.data$TEAM1_SCORE <- as.numeric(gsub(',', '', game.data$TEAM1_SCORE))
      points.by.month[[count]] <- game.data$TEAM1_SCORE + game.data$TEAM2_SCORE
      diff.by.month[[count]] <- abs(game.data$TEAM1_SCORE - game.data$TEAM2_SCORE)
    }
    else
    {
      points.by.month[[count]] <- rep(0, 100)
      diff.by.month[[count]] <- rep(0, 100)
    }
  }
}
# plot data showing total number of points scores per month
timestamp <- as.vector(sapply(year, function(x) paste(month, x, sep='-')))
df <- data.frame(time=timestamp,
                diff=unlist(lapply(points.by.month, mean)),
                se=unlist(lapply(points.by.month, sd)),
                year=rep(year, each=12))
df$time <- factor(df$time, levels=timestamp)

# plot data
ggplot(df, aes(x=time, y=diff, color=year)) +
      geom_point() +
      geom_errorbar(aes(ymin=diff-se, ymax=diff+se), width=.2, position=position_dodge(.9)) +
      theme_classic() +
      xlab('') + ylab('Total points scored') +
      ylim(150, 250) +
      theme(axis.text.x=element_text(size=0, angle=45),
            axis.title.x=element_text(size=22),
            axis.text.y=element_text(size=18),
            axis.title.y=element_text(size=22))

# plot data showing point differential per game per month
timestamp <- as.vector(sapply(year, function(x) paste(month, x, sep='-')))
df <- data.frame(time=timestamp,
diff=unlist(lapply(diff.by.month, mean)),
            se=unlist(lapply(diff.by.month, sd)),
            year=rep(year, each=12))
df$time <- factor(df$time, levels=timestamp)

# plot data
ggplot(df, aes(x=time, y=diff, color=year)) +
      geom_point() +
      geom_errorbar(aes(ymin=diff-se, ymax=diff+se), width=.2, position=position_dodge(.9)) +
      theme_classic() +
      ylab('Point differential per month') + xlab('') +
      ylim(1, 20) +
      theme(axis.text.x=element_text(size=0, angle=45),
            axis.title.x=element_text(size=22),
            axis.text.y=element_text(size=18),
            axis.title.y=element_text(size=22))
###################################################################


###################################################################
# collect data to find teams scored for home and away teams
year <- 1980:2013
month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
score <- vector('list', length(year))
names(score) <- year
for(y in 1:length(year))
{
  cat('Processing year', year[y], '...\n')
  query <- sprintf('SELECT *  FROM NBA WHERE YEAR=%s', year[y])
  game.data = dbGetQuery(con, query)
  # remove commas from column
  game.data$TEAM1_SCORE <- as.numeric(gsub(',', '', game.data$TEAM1_SCORE))
  game.data.month <- unique(game.data$MONTH)
  score[[y]] <- vector('list', length(game.data.month))
  names(score[[y]]) <- game.data.month
  for(m in 1:length(game.data.month))
  {
    index <- which(game.data$MONTH == game.data.month[m])
    
    # define date in POSIX format
    index.month <- match(game.data.month[m], month)
    date.stamp <- paste(game.data$YEAR[index], index.month, game.data$DAY[index], sep='-')
    date.stamp <- as.POSIXlt(date.stamp)
    
    # determine home-team and compute game score accordingly
    temp.score <- mat.or.vec(length(index), 3)
    for(i in 1:length(index))
    {
      #temp.score[i, 1] <- game.data$HOME_TEAM[index[i]]
      if(grepl(game.data$HOME_TEAM[index[i]], game.data$TEAM1[index[i]]))
      {
        temp.score[i, 1] <- game.data$TEAM1[index[i]]
        temp.score[i, 2] <- game.data$TEAM2[index[i]]
        temp.score[i, 3] <- game.data$TEAM1_SCORE[index[i]] - game.data$TEAM2_SCORE[index[i]]
      }
      else
      {
        temp.score[i, 1] <- game.data$TEAM2[index[i]]
        temp.score[i, 2] <- game.data$TEAM1[index[i]]
        temp.score[i, 3] <- game.data$TEAM2_SCORE[index[i]] - game.data$TEAM1_SCORE[index[i]]
      }
    }
    # store year information into dataframe
score[[y]][[m]] <- data.frame(home_team=temp.score[, 1],
                              away_team=temp.score[, 2],
                              score_diff=temp.score[, 3])
  }
}

# find best home team
year.list <- lapply(score, function(x) do.call(rbind, x))
teams <- as.vector(unique(do.call(rbind, year.list)[, 1]))
home.team <- mat.or.vec(length(teams), length(year))
row.names(home.team) <- teams
colnames(home.team) <- year
for(y in 1:length(year))
{
  for(t in 1:length(teams))
  {
    index <- which(year.list[[y]][, 1] ==  teams[t])
    if(length(index) > 0)
    {
      home.team[t, y] <- mean(as.numeric(as.vector(year.list[[y]][index, 3])), na.rm=TRUE)
    }
  }
}

home.team <- home.team[order(apply(home.team, 1, function(x) length(which(x==0))), decreasing=FALSE), ]
pval <- mat.or.vec(length(teams), length(year))
pval[which(home.team==0)] <- 1
require(corrplot)
corrplot(home.team,
         method='circle',
         is.corr = FALSE,
         tl.col='black', tl.cex=0.75,
         p.mat = pval,
         sig.level = 0.05,
         pch.cex=0.5)

# find best away team
year.list <- lapply(score, function(x) do.call(rbind, x))
teams <- as.vector(unique(do.call(rbind, year.list)[, 2]))
away.team <- mat.or.vec(length(teams), length(year))
row.names(away.team) <- teams
colnames(away.team) <- year
for(y in 1:length(year))
{
  for(t in 1:length(teams))
  {
    index <- which(year.list[[y]][, 2] ==  teams[t])
    if(length(index) > 0)
    {
      away.team[t, y] <- mean(-as.numeric(as.vector(year.list[[y]][index, 3])), na.rm=TRUE)
    }
  }
}

away.team <- away.team[order(apply(away.team, 1, function(x) length(which(x==0))), decreasing=FALSE), ]
pval <- mat.or.vec(length(teams), length(year))
pval[which(away.team==0)] <- 1
require(corrplot)
corrplot(away.team,
        method='circle',
        is.corr = FALSE,
        tl.col='black', tl.cex=0.75,
        p.mat = pval,
        sig.level = 0.05,
        pch.cex=0.5)
###################################################################


###################################################################
# find highest scoring teams from 1960 to 2013
year <- 1980:2013
query <- 'SELECT * FROM NBA WHERE YEAR BETWEEN 1980 AND 2013'
game.data = dbGetQuery(con, query)
# remove commas from column 6
game.data$TEAM1_SCORE <- as.numeric(gsub(',', '', game.data$TEAM1_SCORE))
teams <- unique(c(game.data$TEAM1, game.data$TEAM2))
points.matrix <- mat.or.vec(length(teams), length(year))
row.names(points.matrix) <- teams
colnames(points.matrix) <- year
for(y in 1:length(year))
{
  index.year <- which(game.data$YEAR==year[y])
  for(t in 1:length(teams))
  {
    index1 <- intersect(which(game.data$TEAM1==teams[t]), index.year)
    index2 <- intersect(which(game.data$TEAM2==teams[t]), index.year)
    points.matrix[t, y] <- mean(c(game.data$TEAM1_SCORE[index1], game.data$TEAM2_SCORE[index2]), na.rm=TRUE)
  }
}

pval <- mat.or.vec(length(teams), length(year))
pval[is.na(points.matrix)] <- 1
points.matrix[is.na(points.matrix)] <- 80
require(corrplot)
corrplot(points.matrix,
         method='circle',
         is.corr = FALSE,
         tl.col='black', tl.cex=0.75,
         p.mat = pval,
         sig.level = 0.05,
         pch.cex=0.5)
###################################################################
         
