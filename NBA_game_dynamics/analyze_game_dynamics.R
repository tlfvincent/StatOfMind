######################################################################
# (1) load required libraries
######################################################################
setwd('/Users/ThomasVincent/Documents/GitHub/NBA/Game_Dynamics')
library(RSQLite)
#library(rCharts)
library(data.table)
library(ggplot2)
library(reshape2)
library(stringr)

library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)
library(rjson)
library(d3heatmap)
# if(packageVersion("data.table") < "2.15.0") {
#     stop("Need to wait until package:stats 2.15 is released!")
# }
suppressPackageStartupMessages(library(googleVis))


######################################################################
# (2) define functions
######################################################################
'find_longest_streak' <- function(vec) {
  streaks <- c()
  current <- 0
  for(i in 2:length(vec)) {
    if(abs(vec[i]) >= abs(vec[i-1])) {
      current <- current + (abs(vec[i]) - abs(vec[i-1]))
    }
    else{
      streaks <- c(streaks, current)
      current <- 0
    }
  }
  return(max(streaks))
}


'get_dt_calc' <- function(dt, expr, gby, col1, col2, gby2) {
  tmp <- game.data[, eval(expr), eval(gby)]
  if(gby2 == 'HOME_TEAM') {
    df <- tmp[, mean(V1), by=HOME_TEAM]
  }
  else if(gby2 == 'AWAY_TEAM') {
    df <- tmp[, mean(V1), by=AWAY_TEAM]
  }
  setnames(df, c(col1, col2))
  return(df)
}


######################################################################
# (3) read in data
######################################################################

# read in data from SQL database
drv <- dbDriver('SQLite')
#con <- dbConnect(drv, 'game_play_by_play_2010_2014.db')
con <- dbConnect(drv, 'game_play_by_play_2000_2015.db')
# list all the tables in the database
dbListTables(con)
# list all the columns in a given table
dbListFields(con, 'GAME_PBP')


######################################################################
# (4) data processing
######################################################################
year <- 2001:2014
year.stats <- vector('list', length(year))
names(year.stats) <- year
home.perf <- away.perf <- vector('list', length(year))
for(i in 1:length(year))
{
  print(year[i])
  # extract data for given 
  query <- sprintf('SELECT *  FROM GAME_PBP WHERE YEAR=%s', year[i])
  game.data = dbGetQuery(con, query)
  game.data <- data.table(game.data)
  # remove id columns and only retain unique rows
  game.data[, ID:=NULL]
  game.data <- unique(game.data)

    # convert time into seconds
  quarter.info <- as.vector(game.data$QUARTER)
  time.in.secs <- sapply(as.vector(game.data$TIME), function(x) strsplit(x, split='[:.]'))
  game.time <- mat.or.vec(length(time.in.secs), 1)
  for(t in 1:length(time.in.secs))
  {
    game.time[t] <- (as.numeric(time.in.secs[[t]][1])*60) + as.numeric(time.in.secs[[t]][2])
  }
  game.time <- 720 - game.time
  game.time <- game.time + ((quarter.info-1)*720)
  game.data[, 'seconds' := game.time]

  # track score differential across time
  game.data[, c('away_score', "home_score") := do.call(Map, c(f = c, strsplit(SCORE, '-', fixed=TRUE))) ]

  # compute score differential
  game.data[, 'score_diff' := as.numeric(home_score) - as.numeric(away_score)]

  # find average lead for home/away teams
  tmp <- game.data[, mean(score_diff), by=HOME_TEAM]
  tmp <- tmp[, 'year' := rep(year[i], nrow(tmp))]
  home.perf[[i]] <- tmp
  tmp <- game.data[, mean(score_diff), by=AWAY_TEAM]
  tmp <- tmp[, 'year' := rep(year[i], nrow(tmp))]
  away.perf[[i]] <- tmp

  # find proportion in lead for teams playing at home
  expr <- quote(round(length(which(score_diff >= 0)) / length(score_diff), 2) * 100)
  gby <- 'GAME_ID,HOME_TEAM'
  home.lead.prop <- get_dt_calc(game.data, expr, gby, 'team', 'home_lead_prop', gby2='HOME_TEAM')
  # find proportion in lead for teams playing away
  expr <- quote(round(length(which(score_diff <= 0))/ length(score_diff), 2) * 100)
  gby <- 'GAME_ID,AWAY_TEAM'
  away.lead.prop <- get_dt_calc(game.data, expr, gby, 'team', 'away_lead_prop', gby2='AWAY_TEAM')
  # merge lead datatsets
  dt.lead <- merge(home.lead.prop, away.lead.prop, by='team')

  # find longest streak for teams playing at home
  expr <- quote(find_longest_streak(score_diff))
  gby <- 'GAME_ID,HOME_TEAM'
  home.streak <-  get_dt_calc(game.data, expr, gby, 'team', 'home_streak', gby2='HOME_TEAM')
  # find longest streak for teams playing at home
  expr <- quote(find_longest_streak(score_diff))
  gby <- 'GAME_ID,AWAY_TEAM'
  away.streak <- get_dt_calc(game.data, expr, gby, 'team', 'away_streak', gby2='AWAY_TEAM')
  # merge streak datatsets
  dt.streak <- merge(home.streak, away.streak, by='team')

  #find largest lead for teams playing at home
  expr <- quote(ifelse(max(score_diff)<0, 0, max(score_diff)))
  gby <- 'GAME_ID,HOME_TEAM'
  home.largest.lead <- get_dt_calc(game.data, expr, gby, 'team', 'home_largest_lead', gby2='HOME_TEAM')
  #find largest lead for teams playing away
  expr <- quote(ifelse(min(score_diff)>0, 0, min(score_diff)))
  gby <- 'GAME_ID,AWAY_TEAM'
  away.largest.lead <- get_dt_calc(game.data, expr, gby, 'team', 'away_largest_lead', gby2='AWAY_TEAM')
  # merge largest lead datatsets
  dt.largest.lead <-merge(home.largest.lead, away.largest.lead, by='team')

  dt <- merge(dt.streak, dt.lead, by='team')
  dt <- merge(dt, dt.largest.lead, by='team')
  # dt <- merge(game.diff, game.streak, by='GAME_ID')
  # dt <- merge(dt, lead.change, by='GAME_ID')
  # setnames(dt, c('GAME_ID', 'game_diff', 'game_streak', 'lead_change'))
  dt[, 'year' := rep(year[i], nrow(dt))]
  year.stats[[i]] <- dt
}


######################################################################
# (5) format score differential data in JSON format suitable for D3
######################################################################
# output home differential stats to JSON format
home.perf <- rbindlist(home.perf)
mat <- acast(home.perf, HOME_TEAM ~ year, value.var='V1')
mat[is.na(mat)] <- 0
test <- apply(mat,
      MARGIN=1,
      FUN=function(r) list(r))
cat(toJSON(test))

# output home differential stats to JSON format
away.perf <- rbindlist(away.perf)
mat <- acast(away.perf, AWAY_TEAM ~ year, value.var='V1')
mat[is.na(mat)] <- 0
test <- apply(mat,
      MARGIN=1,
      FUN=function(r) list(r))
cat(toJSON(test))


########################################################################
# (6) format proportion in lead for teams in JSON format suitable for D3
########################################################################
df <- rbindlist(year.stats)

# generate D3 heatmap for lead propensity at home
mat <- acast(df, team ~ year, value.var='home_lead_prop')
mat[is.na(mat)] <- 0
p <- d3heatmap(mat,
               dendrogram = "row",
               scale = "none",
               k_row=3,
               color = scales::col_quantile("Blues", NULL, 5))
saveWidget(p, file='home_lead_prop.html')

# generate D3 heatmap for lead propensity away
mat <- acast(df, team ~ year, value.var='away_lead_prop')
mat[is.na(mat)] <- 0
p <- d3heatmap(mat,
               dendrogram = "row",
               scale = "none",
               k_row=3,
               color = scales::col_quantile("Blues", NULL, 5))
saveWidget(p, file='away_lead_prop.html')

# generate D3 heatmap for lead differential propensity
df[, 'lead_prop_diff' := home_lead_prop - away_lead_prop]
mat <- acast(df, team ~ year, value.var='lead_prop_diff')
mat[is.na(mat)] <- 0
p <- d3heatmap(mat,
               dendrogram = "row",
               scale = "none", k_row = 3,
               color = "RdBu")
saveWidget(p, file='lead_prop_diff.html')


########################################################################
# (7) format scoring streaks for teams in JSON format suitable for D3
########################################################################

# generate D3 heatmap for scoring streaks at home
mat <- acast(df, team ~ year, value.var='home_streak')
mat[is.na(mat)] <- 0
p <- d3heatmap(mat,
               dendrogram = "row",
               scale = "none",
               k_row=3,
               color = scales::col_quantile("Blues", NULL, 5))
saveWidget(p, file='home_scoring_streak.html')

# generate D3 heatmap for scoring streaks at away
mat <- acast(df, team ~ year, value.var='away_streak')
mat[is.na(mat)] <- 0
p <- d3heatmap(mat,
               dendrogram = "row",
               scale = "none",
               k_row=3,
               color = scales::col_quantile("Blues", NULL, 5))
saveWidget(p, file='away_scoring_streak.html')

# generate D3 heatmap for lead differential propensity
df[, 'scoring_streak_diff' := home_streak - away_streak]
mat <- acast(df, team ~ year, value.var='scoring_streak_diff')
mat[is.na(mat)] <- 0
p <- d3heatmap(mat,
               dendrogram = "row",
               scale = "none", k_row = 3,
               color = "RdBu")
saveWidget(p, file='scoring_streak_diff.html')


########################################################################
# (8) find number of wins per team away and at home
########################################################################
year <- 2001:2014
win.share <- vector('list', length(year))
names(win.share) <- year
for(i in 1:length(year))
{
  print(year[i])
  # extract data for given 
  query <- sprintf('SELECT *  FROM GAME_PBP WHERE YEAR=%s', year[i])
  game.data = dbGetQuery(con, query)
  game.data <- data.table(game.data)
  # remove id columns and only retain unique rows
  game.data[, ID:=NULL]
  game.data <- unique(game.data)

  # select last scoring event of game
  game.data <- game.data[ ,.SD[.N], by=GAME_ID]
  # track score differential across time
  game.data[, c('away_score', "home_score") := do.call(Map, c(f = c, strsplit(SCORE, '-', fixed=TRUE))) ]
  # compute score differential
  game.data[, 'score_diff' := as.numeric(home_score) - as.numeric(away_score)]

  tmp <- game.data[,sum(ifelse(home_score > away_score, 1, 0)) / .N, by=HOME_TEAM]
  setnames(tmp, c('team', 'win_share'))
  tmp[, 'year' := rep(year[i], nrow(tmp))]
  win.share[[i]] <- tmp
}

win.share <- rbindlist(win.share)
win.share[, 'home_perf' := home.perf$V1]
win.share[, 'away_perf' := away.perf$V1]

# plot htmlwidget for home score differential correlation with win shares
tmp <- as.data.frame(win.share)
tmp$win_share <- tmp$win_share*100
colnames(tmp) <- c('team',
                   'Win Share',
                   'year',
                   'Home Score Diff',
                   'Away Score Diff')