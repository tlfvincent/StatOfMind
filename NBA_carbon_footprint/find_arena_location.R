####! Only needs to be ran once !####
# find geocode location of each arena
library(maps)
library(mapdata)
library(ggmap)
lon <- lat <- mat.or.vec(nrow(home.arenas), 1)
for(p in 1:nrow(home.arenas))
{
  geo.tag <- geocode(paste(home.arenas[p, 2], home.arenas[p, 1], sep=' '))
  lon[p] <- geo.tag$lon
  lat[p] <- geo.tag$lat
  #points(geo.tag$lon, geo.tag$lat, col=2, pch=20)
}
arena.geo.tag <- data.frame(team=home.arenas[, 1], arena=home.arenas[, 2], lon=lon, lat=lat)

# Google API fails for a few arenas so fill in manually
# not pretty but fast given the numbers
arena.geo.tag[3, 3:4] <- c(118.2672, 34.0431)
arena.geo.tag[10, 3:4] <- c(90.0819, 29.9489)
arena.geo.tag[11, 3:4] <- c(119.2176, 46.2198)
arena.geo.tag[12, 3:4] <- c(96.8103, 32.7906)
arena.geo.tag[13, 3:4] <- c(98.4375, 29.4269)
arena.geo.tag[15, 3:4] <- c(112.0714, 33.4458)
arena.geo.tag[19, 3:4] <- c(118.2672, 34.0431)
arena.geo.tag[21, 3:4] <- c(81.3836, 28.5392)
arena.geo.tag[24, 3:4] <- c(71.0622, 42.3663)
arena.geo.tag[29, 3:4] <- c(122.67621, 45.52345)
arena.geo.tag[30, 3:4] <- c(97.5150, 35.4633)
arena.geo.tag[ ,3] <- abs(arena.geo.tag[, 3])
save(arena.geo.tag, file='NBA_arena_coordinates.Rdata')
####! Only needs to be ran once !####
