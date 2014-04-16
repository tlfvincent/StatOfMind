setwd("~/NBA_League_attendance")

library(reshape2)
library(beeswarm)
library(ggplot2)
library(grid)
library(gridExtra)

# read team abbreviations
team.abb <- read.csv(file='Team_abbreviation.txt', header=FALSE)

# read win share data
franchise.wins <- read.csv(file='franchise_wins.txt', header=TRUE)

# define franchises that changed names at some point between 1981 and 2013
# could do this via shell scripting but defining this here for transparency
# (should also consider the fact that they changed arenas!)
old.franchise <- c('Washington Bullets', 'Kansas City Kings', 'San Diego Clippers', 'Seattle SuperSonics', 'New Jersey Nets', 'Charlotte Hornets', 'New Orleans Hornets', 'Vancouver Grizzlies')
new.franchise <- c('Washington Wizards', 'Sacramento Kings', 'Los Angeles Clippers', 'Oklahoma City Thunder', 'Brooklyn Nets', 'Charlotte Bobcats', 'New Orleans Pelicans', 'Memphis Grizzlies')

# read league attendance for every season betweem 1981 and 2013
year <- 1981:2013
league.attendance <- offense <- defense <- vector('list', length(year))
names(league.attendance) <- names(offense) <- names(defense) <- year
for(y in 1:length(year))
{
  # read in file with league attendance records
  input.file <- sprintf('League_attendance_%s.txt', year[y])
  if(file.info(input.file)$size > 0) # data fo year 2002-2006 was not available
  {
    dat <- scan(file=sprintf('League_attendance_%s.txt', year[y]), what="raw()", sep='\n')
    attendance <- mat.or.vec(length(dat), 1)
    teams.file <- mat.or.vec(length(dat), 1)
    for(i in 1:length(dat))
    {
      str <- strsplit(dat[i], split=' ')[[1]]
      last.item <- str[length(str)]
      last.item <- strsplit(last.item, split=',')[[1]][-1]
      attendance[i] <- as.numeric(paste(last.item, collapse=""))
      names(attendance)[i] <- strsplit(dat[i], split=',')[[1]][2]
    }
    
    # manually change franchise names to most recent
    for(o in 1:length(old.franchise))
    {
      index <- match(old.franchise[o], names(attendance))
      if(!is.na(index))
      {
        names(attendance)[index] <- new.franchise[o]
      }
    }
    
    # change franchise names to team abbreviations
    index <- match(names(attendance), team.abb[,2])
    names(attendance) <- team.abb[index, 1]
    
    league.attendance[[y]] <- attendance
  }
  else
  {
    league.attendance[[y]] <- rep(0, 20)
  }
  
  # read in file with franchise offense statistics
  offense.file <- sprintf('Team_stats_%s.txt', year[y])
  if(file.info(offense.file)$size > 0)
  {
    dat <- read.csv(file=offense.file, header=FALSE)
    offense[[y]] <- as.numeric(dat[, ncol(dat)])
    names(offense[[y]]) <- dat[, 2]
    
    # manually change franchise names to most recent
    for(o in 1:length(old.franchise))
    {
      index <- match(old.franchise[o], names(offense[[y]]))
      if(!is.na(index))
      {
        names(offense[[y]])[index] <- new.franchise[o]
      }
    }
    
    # change franchise names to team abbreviations
    index <- match(names(offense[[y]]), team.abb[,2])
    names(offense[[y]]) <- team.abb[index, 1]
  }
  else
  {
    offense[[y]] <- rep(0, 20)
  }
  
  # read in file with franchise defense statistics
  defense.file <- sprintf('Opponent_stats_%s.txt', year[y])
  if(file.info(defense.file)$size > 0)
  {
    dat <- read.csv(file=defense.file, header=FALSE)
    defense[[y]] <- as.numeric(dat[, ncol(dat)])
    names(defense[[y]]) <- dat[, 2]
    
    # manually change franchise names to most recent
    for(o in 1:length(old.franchise))
    {
      index <- match(old.franchise[o], names(defense[[y]]))
      if(!is.na(index))
      {
        names(defense[[y]])[index] <- new.franchise[o]
      }
    }
    
    # change franchise names to team abbreviations
    index <- match(names(defense[[y]]), team.abb[,2])
    names(defense[[y]]) <- team.abb[index, 1]
  }
  else
  {
    defense[[y]] <- rep(0, 20)
  }
}

####################################################
# plot distribution of NBA league attendance by year
par(mar=c(6,7,4,2))
stripchart(league.attendance,
    cex=0.75,
    col='blue',
    pch=20,
    las=2,
    vertical = TRUE,
    cex.axis=1.5)
boxplot(league.attendance,
    las=2,
    outcex=0,
    boxlwd=3,
    col="#0000ff22",
    cex.axis=0.01,
    cex.lab=2,
    ylab='',
    add=T,
    names = rep('', length(year)))
text(c(19, 32), c(520000, 760000), labels=c('*', '*'), col='red', cex=2)
mtext(side=2, 'NBA League Attendance', line=5, cex=2)


########################################################
# analyze relationship between points scored for/against each team and how that
# impacts fan attendance
attendance.per.offense <- vector('list', length(year))
attendance.per.defense <- vector('list', length(year))
for(y in 1:length(year))
{
  # find correlation between number of points scored by team and attendance
  index <- match(names(league.attendance[[y]]), names(offense[[y]]))
  index <- index[!is.na(index)]
  if(length(index) > 0)
  {
    attendance.per.offense[[y]] <- cbind(offense[[y]][index], league.attendance[[y]])
    # account for high/low-scoring seasons
    attendance.per.offense[[y]][,1] <- attendance.per.offense[[y]][,1] - mean(attendance.per.offense[[y]][,1], na.rm=TRUE)
  }
  # find correlation between number of points scored against team and attendance
  index <- match(names(league.attendance[[y]]), names(defense[[y]]))
  index <- index[!is.na(index)]
  if(length(index) > 0)
  {
    attendance.per.defense[[y]] <- cbind(defense[[y]][index], league.attendance[[y]])
    # account for high/low-scoring seasons
    attendance.per.defense[[y]][,1] <- attendance.per.defense[[y]][,1] - mean(attendance.per.defense[[y]][,1], na.rm=TRUE)
  }
}

# transform lists into dataframes compatible for plotting in ggplot
# remove item 22 to 26 (missing data for these years)
df.offense <- as.data.frame(do.call(rbind, attendance.per.offense[-c(22:26)]))
colnames(df.offense) <- c('points', 'attendance')
df.defense <- as.data.frame(do.call(rbind, attendance.per.defense[-c(22:26)]))
colnames(df.defense) <- c('points', 'attendance')

# plot correlation between fan attendance and points scored/against
# use grid setup and ggplot
p1 = ggplot(df.offense, aes(x=points-1, y=attendance)) +
  geom_point(size=4, colour='royalblue3') +
  geom_smooth(method=lm, size=2.5, colour='black') +
  theme_classic() +
xlim(-10,10) +
  ylab('Fan Attendance') + xlab('Points scored by home team compared to league average') +
  theme(axis.text.x=element_text(size=22), axis.text.y=element_text(size=22)) +
  theme(axis.title.x=element_text(size=26), axis.title.y=element_text(size=26)) +
  theme(legend.title = element_text(size=14)) +
 theme(legend.text = element_text(size = 13))

p2 = ggplot(df.defense, aes(x=points-1, y=attendance)) +
geom_point(size=3.5, colour='indianred3') +
geom_smooth(method=lm, size=2.5, colour='black') +
theme_classic() +
xlim(-10,10) +
ylab('Fan Attendance') + xlab('Points scored against home team compared to league average') +
theme(axis.text.x=element_text(size=22), axis.text.y=element_text(size=22)) +
theme(axis.title.x=element_text(size=26), axis.title.y=element_text(size=26)) +
theme(legend.title = element_text(size=14)) +
theme(legend.text = element_text(size = 13))

grid.arrange(p1, p2, ncol = 2, main = "")

########################################################

# analyze the relationship between number of wins by a team and its
# impact on fan attendance
count <- length(year) + 1
win.attendance <- vector('list', length(year))
for(y in 1:length(year))
{
  print(y)
  count <- count - 1
  win.ratio <- franchise.wins[count, -(1:3)]
  index <- match(names(league.attendance[[y]]), names(win.ratio))
  index <- index[!is.na(index)]
  win.attendance[[y]] <- cbind(as.vector(as.matrix(win.ratio[index])), league.attendance[[y]])
}

# create dataframe across all time and also assign conference membership of each team
all.dat <- do.call(rbind, win.attendance[-(22:26)])

# find number of fans attending per wins
unique.teams <- sort(unique(row.names(all.dat)))
attendance.per.win <- vector('list', length(unique.teams))
names(attendance.per.win) <- unique.teams
for(u in 1:length(unique.teams))
{
  index <- which(row.names(all.dat) == unique.teams[u])
  attendance.per.win[[u]] <- apply(all.dat[index, ], 1, function(x) x[2] / x[1])
}

# sort teams by median number of fans attending per games won
index <- order(unlist(lapply(attendance.per.win, function(x) median(x, na.rm=TRUE) )), decreasing=FALSE)
attendance.per.win <- attendance.per.win[index]

# plot violin plots, where each violin represents the distribution of fan attendance
# for a given team during the period 181-2013
df <- melt(attendance.per.win)
df$L1 <- factor(df$L1, levels=names(attendance.per.win))
ggplot(df, aes(y=value, x=L1)) +
    geom_violin(colour='royalblue3', size=1.4, scale='width') +
    theme_bw() +
    geom_boxplot(width=.4, size=1.2) +
    ylab('Attendance per winning game\n between 1981-2013') +
    xlab('') +
    theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5), axis.text.y=element_text(size=18)) +
    theme(axis.title.y=element_text(size=22)) +
    geom_hline(aes(yintercept=median(df[,1], na.rm=TRUE)), colour='indianred3', size=2)
