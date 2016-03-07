library(dplyr)
library(reshape)
library(plyr)
setwd("C:/Users/Meyappan/Documents/GitHub/ncaab")

## Save agg.team
agg.team <- readRDS(file="agg_team.RDS")
## Save agg.player
agg.player<- readRDS(file="agg_player.RDS")
## Save ind.team
ind.team <- readRDS(file="ind_team.RDS")

# Create a vector with all of the 64 teams in the NCAA Tournament
# Need to edit this on Selection Sunday 
tourneyteams <- c("457", "23", "29", "28", "51", "77", "90", "140", "149", "157", "164", "169", "175", "180", "193", "202", "235", "249", "260", "275", "312", "311", "328", "327", "334", "671", "367", "381", "400", "404", "406", "418", "416", "797", "450", "463", "473", "472", "457", "489", "490", "493", "518", "522", "521", "529", "545", "556", "606", "609", "626", "674", "676", "688", "694", "703", "699", "719", "110", "740", "739", "746", "758", "774", "782", "796", "2915", "812")

tourney_team_agg_stats <- agg.team[tourneyteams, ]
tourney_team_ind_stats <- subset(ind.team, thisteam_team_id %in% tourneyteams)
tourney_player_ind_stats <- subset(ind.player, team_id %in% tourneyteams)

# Melt the data for percentile rankings
tourney_team_agg_stats.m <- melt(tourney_team_agg_stats[c("team_name", "team_fgpct", "team_three_fgpct", "team_ptsavg", "team_offreb", "team_rebavg", "team_ast", "team_to", "team_stl", "team_blk", "opp_team_fgpct", "opp_team_three_fgpct", "opp_team_ptsavg", "opp_team_offreb", "opp_team_rebavg", "opp_team_ast", "opp_team_to", "opp_team_stl", "opp_team_blk", "team_avg_ptsdiff", "opp_team_avg_ptsdiff")])
tourney_team_agg_stats.m[is.na(tourney_team_agg_stats.m)] <- 0
tourney_team_agg_stats.rescale <- ddply(tourney_team_agg_stats.m, .(variable), transform, rescale = scale(value))
tourney_team_agg_stats.percentile <-  ddply(tourney_team_agg_stats.m, .(variable), transform, percentile=ecdf(value)(value))

# Create lists of variables we want to plot, by dataframe
ind.player.plot <- c("three_fga", "three_fgm", "ast", "blk", "fga", "fgm", "fouls", "fta", "ft", "pts", "defreb", "offreb", "totreb", "stl", "to")
ind.team.plot <- c("three_fgpct", "three_fga", "three_fgm", "ast", "blk", "fgpct", "fga", "fgm", "fouls", "ftpct", "fta", "ft", "pts", "defreb", "offreb", "stl", "totreb", "to")
agg.team.plot <- c("three_fgpct", "three_fga", "three_fgm", "ast", "forward_avg_height", "guard_avg_height", "avg_ptsdiff", "away_avg_ptsdiff", "home_avg_ptsdiff", "ptsavg", "rebavg", "blk", "fgpct", "fga", "fgm", "fouls", "ftpct", "fta", "ft", "losses", "away_losses", "home_losses", "forward_points", "guard_points", "pts", "defreb", "offreb", "totreb", "stl", "to", "winpct", "away_wins", "home_wins", "wins")
