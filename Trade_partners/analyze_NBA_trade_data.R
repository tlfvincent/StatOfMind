setwd('~/Documents/GitHub/NBA/Trade_partners')

# read in count matrix of number of trades completed between each
# pair of NBA teams
trade.matrix <- read.csv(file='Historical_trades_NBA.txt', header=TRUE)

# plot heatmap of trade matrix
require(pheatmap)
pheatmap(trade.matrix)

# plot correlation plot of NBA trade matrix
require(corrplot)
trade.matrix <- as.data.frame(trade.matrix)
png(file='NBA_trade_partners_corrplot.png', width=900, height=900)
corrplot.mixed(as.matrix(trade.matrix),
  lower = "number",
  upper = "circle",
  is.corr=FALSE,
  tl.pos='lt',
  rect.lwd=1.5,
  cl.cex=1.6,
  order="AOE",
  tl.col="black",
  tl.cex=1.3)
dev.off()

# read trade history and team colors of NBA teams
trade.data <- read.csv(file='NBA_trade_all_data.txt', header=FALSE)
team.colors <- read.csv(file='NBA_team_colors.csv', header=FALSE) # team colors were scraped from http://teamcolors.arc90.com/

# aggregate trades by year in list
years <- sort(unique(trade.data[, 4]))
trades.by.year <- vector('list', length(years))
for(i in 1:length(years))
{
  trades.by.year[[i]] <- trade.data[which(trade.data[, 4] == years[i]), ]
}

# plot number of trades by year
require(ggplot2)
df <- data.frame(trades=unlist(lapply(trades.by.year, nrow)), year=years)
ggplot(df, aes(x=year, y=trades)) + 
  geom_line(colour="dodgerblue4", size=1.5) +
  geom_point(colour="dodgerblue4", size=4, shape=21, fill="white") +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
    axis.text.x  = element_text(angle=45, vjust=0.5, size=16)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
    axis.text.y  = element_text(vjust=0.5, size=16)) +
  scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  ylab('Number of trades')
  

# find the teams that operate the best trades
teams <- as.vector(sort(unique(unique(trade.data[, 1]))))
trade.ws <- vector('list', length(teams))
for(i in 1:length(teams))
{
  index <- which(trade.data[, 1] == teams[i])
  trade.ws[[i]] <- apply(trade.data[index, 2:3], 1, diff)
  
}
trades.mean <- order(unlist(lapply(trade.ws, mean)))

# rename teams and assign colors
team.col <- mat.or.vec(length(teams), 1)
for(t in 1:length(teams))
{
  index <- grep(teams[t], as.vector(team.colors[, 1]))
  if(length(index) > 0)
  {
    teams[t] <- as.character(team.colors[index, 1])
    team.col[t] <- as.vector(team.colors[index, 2])
  }
  else
  {
    teams[t] <- paste(teams[t], '*', sep='')
    team.col[t] <- '#C1CDCD'
  }
}
names(trade.ws) <- teams

# plot distribution of trade scores for each team in NBA
require(beeswarm)
png(file='win_shares_per_trade.png', width=1200, height=800)
par(mar=c(14, 6, 4,4))
beeswarm(trade.ws[trades.mean],
    pch = 21,
    bg = "#00000050",
    corral = 'gutter',
    vertical=TRUE,
    cex=0.8,
    las=1,
    col=team.col[trades.mean],
    las=2,
    cex.lab=1.5,
    yaxt='n', xaxt='n')
axis(1, at=1:length(trade.ws),
    labels=names(trade.ws[trades.mean]),
    las=2,
    cex.axis=1.3,
    tick=TRUE)
axis(2, at=seq(-300, 300, 100),
    labels=seq(-300, 300, 100),
    las=2, cex.axis=1.5)
mtext('Win share per trade', side=2, line=3, cex=2)
dev.off()

