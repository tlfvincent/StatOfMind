setwd("~/Documents/GitHub/NBA/NBA_carbon_footprint")

# read NBA team schedule for season 2013-2014
schedule <- read.table(file='NBA_schedule.txt', header=FALSE, sep='\t')

# filter out date entries
game.index <- grep('@', schedule[, 1], fixed=TRUE)
schedule <- schedule[game.index, ]

# find all homes arenas of each NBA team
away.team <- as.vector(sapply(as.vector(schedule[, 1]),
          function(x) strsplit(x, split=' @ ', fixed=TRUE)[[1]][1]))
home.team <- as.vector(sapply(as.vector(schedule[, 1]),
          function(x) strsplit(x, split=' @ ', fixed=TRUE)[[1]][2]))
games.played <- cbind(away.team, home.team)
home.arenas <- cbind(as.vector(home.team), as.vector(schedule[, 3]))
home.arenas <- unique(home.arenas)

####! Only needs to be ran once !#
# source('find_arena_location.R')
####! Only needs to be ran once !#

load(file='NBA_arena_coordinates.Rdata')

# Function to calculate distance in kilometers between two points
# reference: http://andrew.hedges.name/experiments/haversine/
earth.dist <- function (lon1, lat1, lon2, lat2, R)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  real.d <- min(abs((R*2) - d), d)
  return(real.d)
}

# compute distance between each NBA arena
dist.mat <- mat.or.vec(nrow(arena.geo.tag), nrow(arena.geo.tag))
colnames(dist.mat) <- row.names(dist.mat) <- arena.geo.tag[, 1]
R <- 6378.145 # define radius of earth in km
for(i in 1:nrow(home.arenas))
{
  for(j in 1:nrow(home.arenas))
  {
    lon1 <- arena.geo.tag$lon[i]
    lat1 <- arena.geo.tag$lat[i]
    lon2 <- arena.geo.tag$lon[j]
    lat2 <- arena.geo.tag$lat[j]
    dist.mat[i, j] <- earth.dist(lon1, lat1, lon2, lat2, R)
  }
}

# find miles travelled by each team
all.teams <- sort(as.vector(arena.geo.tag[, 1]))
team.travel <- mat.or.vec(length(all.teams), length(all.teams))
colnames(team.travel) <- row.names(team.travel) <- all.teams
#cum.miles <- mat.or.vec(nrow(games.played), 1)
for(i in 1:length(all.teams))
{
  # find all instances where team travelled
  index.away <- which(games.played[, 1] == all.teams[i])
  for(j in 1:length(all.teams))
  {
    # find all instance of host team
    index.home <- which(games.played[, 2] == all.teams[j])
    number.of.game <- length(intersect(index.away, index.home))
    # find distance between both teams and multiply by number of times they travelled
    if(number.of.game > 0)
    {
      distance <- dist.mat[which(row.names(dist.mat) == all.teams[i]), which(colnames(dist.mat) == all.teams[j])]
      team.travel[i, j] <- distance * number.of.game
      #cum.miles[intersect(index.away, index.home)] <- distance
    }
  }
}

# Total miles travelled by all NBA teams
cat('Teams travelled a total of ', sum(team.travel), '\n')

# sort teams according to km travelled
team.distance <- sort(apply(team.travel, 1, sum), decreasing=TRUE)
team.distance <- round(team.distance, 0)
#html.code <- sapply(team.distance, function(x) paste(names(x), round(x, 0), sep='</td><td>'))
#html.code <- paste(names(team.distance), html.code, sep='')
#sapply(html.code, function(x) cat('<tr><td>', x, '</td></tr>\n'))


# plot interactive barchart of distance travelled by NBA teams
require(reshape2)
require(googleVis)
df <- melt(as.matrix(team.distance))
df <- df[, -2]
colnames(df) <- c('team', 'distance')
df$Mean=mean(df$distance)
distance.barchart <- gvisComboChart(df, xvar='team',
  yvar=c('distance', 'Mean'),
  options=list(seriesType=c('line'), width=1000, height=500,
  title='Total distance (in km) travelled by NBA teams during the 2013-2014 season',
  series='{0: {type:"bars"}}',
  hAxis='{slantedTextAngle:90}'))
plot(distance.barchart)
print(distance.barchart,"chart", file="distance_barchart")



# compute total CO2 emission
# Airbus 319: 9.92 kg/km
total.emission <- sum(team.travel) * 9.92
team.emission <- sort(apply(team.travel, 1, sum) * 9.92)

# plot interactive barchart of CO2 emission by NBA teams
df <- melt(as.matrix(team.emission))
df <- df[, -2]
colnames(df) <- c('team', 'co2_emission')
df$Mean=mean(df$co2_emission)
co2.barchart <- gvisComboChart(df, xvar='team',
  yvar=c('co2_emission', 'Mean'),
  options=list(seriesType=c('line'), width=1000, height=500,
  title='Total CO2(in kg) emitted by NBA teams during the 2013-2014 season',
  series='{0: {type:"bars"}}',
  hAxis='{slantedTextAngle:90}'))
plot(co2.barchart)
print(co2.barchart ,"chart", file="co2_barchart ")

# plot USA map and NBA tavels
library(maps)
library(geosphere)
require(ggplot2)
load(file='NBA_arena_coordinates.Rdata')


png(file='team_travels.png', width=1100, height=900)
map("state", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.1, mar=c(0.1, 0.1, 0.1, 0.1))
max.point.size <- 4
for(i in 1:nrow(arena.geo.tag))
{
  point.cex <- (team.distance[which(names(team.distance) == as.vector(arena.geo.tag[i, 1]))] * max.point.size) / team.distance[1]
  points(arena.geo.tag[i, 3:4], col='black', cex=point.cex, pch=21, bg='royalblue3')
}
for (i in 1:nrow(games.played))
{
  index.home <- which(as.vector(arena.geo.tag[, 1]) == games.played[i, 2])
  index.away <- which(as.vector(arena.geo.tag[, 1]) == games.played[i, 1])
  inter <- gcIntermediate(arena.geo.tag[index.away, 3:4],
  arena.geo.tag[index.home, 3:4],
  n=100,
  addStartEnd=TRUE)
  lines(inter, lwd=0.7, col="#0000ff30")
  #lines(inter, lwd=0.2, col="white")
}
dev.off()


