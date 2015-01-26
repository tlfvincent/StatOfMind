# load required libraries
library(rCharts)
library(ggplot2)
library(reshape2)
library(pheatmap)
library(stargazer)
library(survival)

# read in draft data
df <- read.csv(file='nba_draft_data.txt', header=TRUE)
str(df)

# only keep players that have been in league for over a year
df <- df[which(df$gp > 82), ]
year <- sort(unique(df$draft_year))
teams <- sort(unique(df$team))

##################################################################
############## find most (in)valuable draft picks ################
##################################################################
# for each draft pick, compute average stats
stats <- c('mpg', 'ppg', 'rbg', 'apg', 'ws_48')
av.stats <- mat.or.vec(60, length(stats))
row.names(av.stats) <- 1:60
colnames(av.stats) <- stats
for(i in 1:60)
{
  index.rank <- which(df$rank == i)
  av.stats[i, ] <- apply(df[index.rank, stats], 2, 
      function(x) median(x, na.rm=TRUE))
}

av.stats.melt <- melt(df[, c('draft_year', 'rank', stats)], 
              id=c('draft_year', 'rank'))
colnames(av.stats.melt) <- c('draft_year', 'rank', 'stat', 'value')

#ggplot(av.stats.melt, aes(x=reorder(draft_year), y=value, group=rank)) + 
#    geom_boxplot(varwidth=FALSE, position_dodge(width = 0.2)) +
#    facet_grid(stat ~ .)

# normalize each column
av.stats.norm <- apply(av.stats, 2, function(x) x / max(x, na.rm=TRUE))
#pheatmap(t(av.stats.norm), cluster_cols=FALSE, fontsize=15)
heatmap(av.stats.norm, 
        Rowv=NA, 
        Colv=NA, 
        col = brewer.pal(9, 'PuBu'), 
        scale="column", margins=c(5,10))

##################################################################
############## find best player per pick position ################
##################################################################
# add differences between player stats and average stats for 
# his pick position
player.performance <- vector('list', 60)
for(i in 1:60)
{
  index.rank <- which(df$rank == i)
  #tmp <- mat.or.vec(length(index.rank), 1)
  tmp <- t(apply(df[index.rank, stats], 1, 
                function(x) x / av.stats[i, ]))
  row.names(tmp) <- df$player[index.rank]
  player.performance[[i]] <- sort(apply(tmp, 1, sum), decreasing=TRUE)
}

# retain only top 3 players at each pick position
player.performance <- lapply(player.performance, 
                function(x) names(x[1:3]))
df.player.performance <- do.call(rbind, player.performance)
colnames(df.player.performance) <- c('Best pick', '2nd best pick', '3rd best pick')
row.names(df.player.performance) <- 1:60

# print out dataframe as html table
stargazer(df.player.performance, type='html')


##################################################################
################# find best year for draft crop ##################
##################################################################
year.performance <- vector('list', length(year))
names(year.performance) <- year
for(i in 1:length(year))
{
  index.year <- which(df$draft_year == year[i])
  tmp <- c()
  for(j in 1:length(index.year))
  {
    index.rank <- df[index.year[j], 'rank']
    if(index.rank > 60){index.rank <- 60}
    tmp <- rbind(tmp, df[index.year[j], stats] / av.stats[index.rank, ])
  }
  year.performance[[i]] <- tmp
}
draft.stats <- do.call(rbind, lapply(year.performance, function(x) apply(x, 2, mean)))
# normalize each column
draft.stats.norm <- apply(draft.stats, 2, function(x) x/max(x, na.rm=TRUE))

# plot results as heatmap
heatmap(draft.stats.norm, 
        Rowv=NA, 
        Colv=NA, 
        col = brewer.pal(9, 'PuBu'), 
        scale="column", margins=c(5,10))

##################################################################
############## find teams that made the best picks ###############
##################################################################
# for each team, sum player performance compared to average at his
# pick position
perf <- unlist(player.performance)
teams.rank <- vector('list', length(teams))
names(teams.rank) <- teams
for(i in 1:length(teams))
{
  index.team <- which(df$team == teams[i])
  index.player <- match(df$player[index.team], names(perf))
  index.na <- which(is.na(index.player) == 'TRUE')
  if(length(index.na) > 0)
  {
    index.player <- index.player[-index.na]
  }
  teams.rank[[i]] <- perf[index.player]
}
teams.rank.df <- melt(teams.rank)
colnames(teams.rank.df) <- c('value', 'team')
teams.rank.df$team <- factor(teams.rank.df$team, 
                levels = names(sort(unlist(lapply(teams.rank, median)))))

ggplot(data=teams.rank.df, aes(x=team, y=value, color=team)) + 
      geom_boxplot(size=1.4) +
      theme_bw() +
      xlab('Teams') +
      ylab('Rating') +
      #guides(col = guide_legend(ncol=3)) +
      theme(axis.text.x = element_text(angle=90, size=12),
            axis.title.x = element_text(size=15),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=15),
            legend.position = 'none')


##################################################################
########### get survival curve for each pick position ############
##################################################################
pick.survival <- vector('list', 60)
names(pick.survival) <- 1:60
for(i in 1:60)
{
  index.rank <- which(df$rank == i)
  surv <- mat.or.vec(length(index.rank), 2)
  colnames(surv) <- c('time', 'cens')
  surv[, 1] <- as.vector(df$yrs[index.rank])
  surv[which(df$draft_year[index.rank] + df$yrs[index.rank] != 2015), 2] <- 1
  if(nrow(surv) != 0)
  {
    fit <- survfit(Surv(surv[, 1], surv[, 2]) ~ 1)
    pick.survival[[i]] <- data.frame(time=fit$time, 
                                      surv=fit$surv,
                                      pick=rep(i, length(fit$time)))
  }
}

surv.df <- do.call(rbind, pick.survival)

# plot survival curves
ggplot(data = surv.df, aes(x=time, y=surv, group=pick, colour=pick)) + 
        geom_line(size=1.2) +
        xlab('Number of Years') +
        ylab('Probability of Survival') +
        theme_bw() +
        theme(axis.text.x = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.text.y = element_text(size=14),
            axis.title.y = element_text(size=16))


