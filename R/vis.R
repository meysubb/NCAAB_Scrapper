library("Cairo")
library("ggplot2")
library("gridExtra")
library("psych")
library("reshape")
library("scales")
library("sm")
getwd()
setwd("C:/Users/Meyappan/Documents/GitHub/ncaab")
################# LOAD DATA
start.time <- Sys.time()
# Only Two digits
options(digits=8)
# Load aggregate player data
agg.player <- read.csv(file="data/summary_player_data.tsv", sep="\t", header=TRUE, row.names=1, na.strings="?")
agg.player$player_name <- as.character(agg.player$player_name)
agg.player$team_name <- as.character(agg.player$team_name)
agg.player$pos <- factor(agg.player$pos, c("G", "F", "C"))
agg.player$year <- factor(agg.player$year, c("Fr", "So", "Jr", "Sr"))
agg.player$height <- sapply(strsplit(as.character(agg.player$height), "-"), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
agg.player$minutes <- sapply(strsplit(as.character(agg.player$minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
agg.player$astavg <- agg.player$ast/agg.player$played
agg.player$ast_to <- ifelse(agg.player$to==0,0,agg.player$ast/agg.player$to)
#levels(agg.player$pos)[1] <- "N/A"
# replace's NA with 0, except for factor columns i.e Pos
agg.player[is.na(agg.player)] <- 0 
# rownames
#rownames(agg.player) <- seq(1:nrow(agg.player))

# Load aggregate team data
# Remove strings as factors
agg.team <- read.csv(file="data/summary_team_data.tsv", sep="\t", header=TRUE, row.names=1, na.strings="?",stringsAsFactors = FALSE)
agg.team$team_name <- as.character(agg.team$team_name)
agg.team$team_minutes = sapply(strsplit(as.character(agg.team$team_minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
agg.team$opp_team_minutes = sapply(strsplit(as.character(agg.team$opp_team_minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
agg.team$team_pts <- gsub(",","",agg.team$team_pts)
agg.team$opp_team_pts <- gsub(",","",agg.team$opp_team_pts)
agg.team$team_fga <- gsub(",","",agg.team$team_fga)
agg.team$opp_team_fga <- gsub(",","",agg.team$opp_team_fga)
agg.team$team_totreb <- gsub(",","",agg.team$team_totreb)
agg.team$opp_team_totreb <- gsub(",","",agg.team$opp_team_totreb)
agg.team$team_ast_to <- agg.team$team_ast/agg.team$team_to
agg.team$opp_team_fgpct <- gsub(",","",agg.team$opp_team_fgpct)
agg.team$opp_team_fgpct <- as.numeric(agg.team$opp_team_fgpct)
agg.team$team_fgm <- as.numeric(agg.team$team_fgm)
agg.team$team_fga <- as.numeric(agg.team$team_fga)
agg.team$team_totreb <- as.numeric(agg.team$team_totreb)
agg.team[is.na(agg.team)] <- 0 
#rownames(agg.team) <- seq(1:nrow(agg.team))

# Load Individual Game Data
ind.game <- read.csv(file="data/game_data.tsv", sep="\t", header=TRUE, row.names=1, na.strings="?")
ind.game$game_date <- as.Date(ind.game$game_date, format='%m/%d/%Y')
ind.game$home_team_name <- as.character(ind.game$home_team_name)
ind.game$away_team_name <- as.character(ind.game$away_team_name)
ind.game$home_team_minutes = sapply(strsplit(as.character(ind.game$home_team_minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
ind.game$away_team_minutes = sapply(strsplit(as.character(ind.game$away_team_minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
ind.game$home_team_id <- sapply(seq(1:length(ind.game$home_team_id)),function(x){
  gsub("12260","",gsub("\\D","",ind.game$home_team_id[x]))
})
ind.game$away_team_id <- sapply(seq(1:length(ind.game$away_team_id)),function(x){
  gsub("12260","",gsub("\\D","",ind.game$away_team_id[x]))
})

# rownames
#rownames(ind.game) <- seq(1:nrow(ind.game))

# Load Individual Player Data
ind.player <- read.csv(file="data/player_data.tsv", sep="\t", header=TRUE, row.names=NULL, na.strings="?")
ind.player$player_name <- as.character(ind.player$player_name)
ind.player$team_name <- as.character(ind.player$team_name)
ind.player$pos <- factor(ind.player$pos, c("G", "F", "C"))
ind.player$minutes = sapply(strsplit(as.character(ind.player$minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
ind.player$game_date <- as.Date(ind.player$game_date, format='%m/%d/%Y')
# rownames
#rownames(ind.player) <- seq(1:nrow(ind.player))

# Load Individual Team Data
ind.team <- read.csv(file="data/team_data.tsv", sep="\t", header=TRUE, row.names=NULL, na.strings="?")
ind.team$team_name <- as.character(ind.team$team_name)
ind.team$team_minutes = sapply(strsplit(as.character(ind.team$team_minutes), ":"), function(x){as.numeric(x[1]) + as.numeric(x[2])/60})
ind.team$game_date <- as.Date(ind.team$game_date, format='%m/%d/%Y')
# rownames
#rownames(ind.team) <- seq(1:nrow(ind.team))


### Fix teams with problems
# Create a couple of functions to help with that
find_errors <- function(agg_df, statistic, nhead=6) {
  head(agg_df[rev(order(agg_df[,statistic])),c("team_name",statistic)], nhead)
}
recalc_opp_stat <- function(t_id, agg_df, ind_df, var1, var2=NULL, fun=c("sum","avg","createavg")) {
  fun <- match.arg(fun)
  t_id  <- paste("/team/",t_id,"/12260",sep="")
  game_subset <- subset(ind_df, team_id == t_id, select = game_id)
  if(fun == "createavg") {
    opp_game_subset <- ind_df[ind_df$game_id %in% game_subset$game_id & ind_df$team_id != t_id, c(var1,var2)]
  } else {
    opp_game_subset <- ind_df[ind_df$game_id %in% game_subset$game_id & ind_df$team_id != t_id, var1]
  }
  if(fun == "sum") {
    ret <- sum(opp_game_subset)
  } else if(fun == "avg") {
    ret <- mean(opp_game_subset)
  } else if(fun == "createavg") {
    x <- sum(opp_game_subset[,1])
    y <- sum(opp_game_subset[,2])
    if(x <= y) {
      ret <- mean(x/y)
    } else {
      ret <- mean(y/x)
    }
  }
  return(ret)
}

# Check the min/max in the summaries for obvious errors
summary(agg.player)
summary(agg.team)
summary(ind.game)
summary(ind.player)
summary(ind.team)

find_errors(agg.team, "opp_team_fgpct", 10)
# Vanderbilt/Buffalo/LSU - 736/86/365 
find_errors(agg.team, "opp_team_three_fgpct")
# Buffalo/LSU/Vanderbilt - 86/365/736 (124/223/253)
find_errors(agg.team, "opp_team_ftpct")
find_errors(agg.team, "opp_team_ptsavg")
find_errors(agg.team, "opp_team_rebavg")
#Vanderbilt

agg.team["736","opp_team_fgpct"] <- recalc_opp_stat(736,agg.team,ind.team,var1="team_fgm",var2="team_fga",fun="createavg") * 100
agg.team["86","opp_team_fgpct"] <- recalc_opp_stat(86,agg.team,ind.team,var1="team_fgm",var2="team_fga",fun="createavg") * 100
agg.team["365","opp_team_fgpct"] <- recalc_opp_stat(365,agg.team,ind.team,var1="team_fgm",var2="team_fga",fun="createavg") * 100
agg.team["86","opp_team_three_fgpct"] <- recalc_opp_stat(86,agg.team,ind.team,var1="team_three_fgm",var2="team_three_fga",fun="createavg") * 100
agg.team["365","opp_team_three_fgpct"] <- recalc_opp_stat(365,agg.team,ind.team,var1="team_three_fgm",var2="team_three_fga",fun="createavg") * 100
agg.team["736","opp_team_three_fgpct"] <- recalc_opp_stat(736,agg.team,ind.team,var1="team_three_fgm",var2="team_three_fga",fun="createavg") * 100

################# TAKE OUR EXISTING DATA, ADD TO IT, AND CREATE NEW DATAFRAMES
##### Create a list with our basic stats
basicgamestats <- c("fgm", "fga", "three_fgm", "three_fga", "ft", "fta", "pts", "ptsavg", "offreb", "defreb", "totreb", "rebavg", "ast", "to", "stl", "blk", "fouls", "dbldbl", "trpdbl")
basicgamestats_team <- basicgamestats

##### Add a few pieces of data to each game
# Point differential for each game
ind.game$ptsdiff <- ind.game$home_team_pts - ind.game$away_team_pts
#agg.team$ptsdiff <- agg.team$team_pts - agg.team$opp_team_pts
# Field goal percentage
ind.game$away_team_fgpct <- ind.game$away_team_fgm/ind.game$away_team_fga
ind.game$home_team_fgpct <- ind.game$home_team_fgm/ind.game$home_team_fga
# Three-point percentage
ind.game$away_team_three_fgpct <- ind.game$away_team_three_fgm/ind.game$away_team_three_fga
ind.game$home_team_three_fgpct <- ind.game$home_team_three_fgm/ind.game$home_team_three_fga
# Free-throw percentage
ind.game$away_team_ftpct <- ind.game$away_team_ft/ind.game$away_team_fta
ind.game$home_team_ftpct <- ind.game$home_team_ft/ind.game$home_team_fta

# Add our changes to our list of variables
basicgamestatsaddpct <- c("fgpct", "three_fgpct", "ftpct")
basicgamestats_team <- append(basicgamestats_team, basicgamestatsaddpct)
basicgamestatsaddunique <- c("ptsdiff")

#### Calculate additional player data (PER)
# Calculate PER 
# unadjusted PER
#lgAST <- sum(agg.team$team_ast,na.rm=TRUE)
#lgFG <- sum(as.numeric(agg.team$team_fgm),na.rm=TRUE)
#lgFT <- sum(agg.team$team_ft,na.rm=TRUE)
#lgPTS <- sum(as.numeric(agg.team$team_pts),na.rm=TRUE)
#lgFGA <- sum(as.numeric(agg.team$team_fga),na.rm=TRUE)
#lgORB <- sum(as.numeric(agg.team$team_offreb),na.rm=TRUE)
#lgTO <- sum(as.numeric(agg.team$team_to),na.rm=TRUE)
#lgFTA <- sum(as.numeric(agg.team$team_fta),na.rm=TRUE)
#lgTRB <- sum(as.numeric(agg.team$team_totreb),na.rm=TRUE)
#factor_PER <- 2/3 - ((0.5 * (lgAST/lgFG)) / (2 * (lgFG/lgFT)))
#VOP_PER <- lgPTS/(lgFGA-lgORB+lgTO+0.44*lgFTA)
#DRBP_PER <- (lgTRB - lgORB)/lgTRB

agg.player$PF <- sapply(seq(1:nrow(agg.player)),function(i){
  player_subset <- subset(ind.player,ind.player$player_name==agg.player$player_name[i])
  return(sum(player_subset$fouls,na.rm=TRUE))
})
#lgPF <- sum(agg.player$PF)

#cols <- colnames(agg.player)
#agg.player$uPER <- sapply(seq(1:nrow(agg.player)),function(x){
#  team_n <- agg.player$team_name[x]
#  tm_subset <- agg.player[agg.player$team_name==team_n,]
#  tmAST <- sum(tm_subset$ast)
#  tmFG <- sum(tm_subset$fgm)
#  block1 <- ((agg.player$PF[x] * lgFT)/lgPF) + 
#    (agg.player$ft[x]/2 * (2-(tmAST/(3*tmFG))))
#  block2 <- (agg.player$fgm[x]*(2-((factor_PER*tmAST)/tmFG)))
#  constant3 <- (2*agg.player$ast[x])/3 + VOP_PER
#  block3 <- DRBP_PER * (2*agg.player$offreb[x]+agg.player$blk[x]-0.2464*(agg.player$fta[x]-agg.player$ft[x])
#                        - (agg.player$fga[x]-agg.player$fgm[x])-agg.player$totreb[x]) 
#  + ((0.44*lgFTA*agg.player$PF[x])/lgPF) - (agg.player$to[x] + agg.player$offreb[x]) 
#  + agg.player$stl[x] + agg.player$totreb[x] - 0.1936*(agg.player$fta[x]-agg.player$ft[x])
#  uPER <- (1/agg.player$minutes[x]) * (((agg.player$three_fgm[x] - block1)) + block2 + constant3 * block3)
#  return(uPER)
#})

#rm(list = ls(pattern = "\\blg"))


# adjusted PER
# No Possession Data, how do we do adjust PER now?


##### Calculate additional team data
# Create a vector with each team
agg.team.teams <- row.names(agg.team)

agg.team$team_home_avg_ptsdiff <- sapply(seq(along=agg.team.teams), function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  gen_home_ptsdiff <- ind.game[ind.game$home_team_id == gen_team_id, ]$ptsdiff
  gen_home_avg_ptsdiff <- mean(gen_home_ptsdiff)
})

agg.team$team_away_avg_ptsdiff <- sapply(seq(along=agg.team.teams), function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  # Multiply by -1 (pts diff = home pts - away pts)
  gen_away_ptsdiff <- ind.game[ind.game$away_team_id == gen_team_id, ]$ptsdiff * -1
  gen_away_avg_ptsdiff <- mean(gen_away_ptsdiff)
})

agg.team$team_avg_ptsdiff <- sapply(seq(1:length(agg.team$team_home_avg_ptsdiff)),function(x){
  mean(c(agg.team$team_home_avg_ptsdiff[x],agg.team$team_away_avg_ptsdiff[x]))
})

agg.team$team_home_wins <- sapply(seq(1:length(agg.team$team_home_avg_ptsdiff)),function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  team_subset <- subset(ind.game,ind.game$home_team_id == gen_team_id)
  gen_home_wins <- length(which(team_subset$ptsdiff > 0))
})

agg.team$team_away_wins <- sapply(seq(1:length(agg.team$team_away_avg_ptsdiff)),function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  team_subset <- subset(ind.game,ind.game$away_team_id == gen_team_id)
  gen_home_wins <- length(which(team_subset$ptsdiff > 0))
})

agg.team$team_wins <- agg.team$team_home_wins + agg.team$team_away_wins

agg.team$team_home_losses <- sapply(seq(1:length(agg.team$team_home_avg_ptsdiff)),function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  team_subset <- subset(ind.game,ind.game$home_team_id == gen_team_id)
  gen_home_wins <- length(which(team_subset$ptsdiff < 0))
})

agg.team$team_away_losses <- sapply(seq(1:length(agg.team$team_away_avg_ptsdiff)),function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  team_subset <- subset(ind.game,ind.game$away_team_id == gen_team_id)
  gen_home_wins <- length(which(team_subset$ptsdiff < 0))
})

agg.team$team_losses <- agg.team$team_home_losses + agg.team$team_away_losses

agg.team$winpct <- agg.team$team_wins / (agg.team$team_wins + agg.team$team_losses)

# Guard Points vs. Forwards etc 

agg.team$team_guard_points <- sapply(seq(1:length(agg.team$team_ft)),function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  gen_agg_team_stats <- agg.team[gen_team_id, ]
  gen_agg_player_stats <- agg.player[agg.player$team_id == gen_team_id, ]
  gen_guards <- gen_agg_player_stats[gen_agg_player_stats$pos == "G", ]
  gen_guard_points <- sum(gen_guards$pts, na.rm=TRUE)/sum(as.numeric(gen_agg_team_stats$team_pts), na.rm=TRUE)
})

agg.team$team_forward_points <- sapply(seq(1:length(agg.team$team_ft)), function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  gen_agg_team_stats <- agg.team[gen_team_id, ]
  gen_agg_player_stats <- agg.player[agg.player$team_id == gen_team_id, ]
  gen_forwards <- gen_agg_player_stats[grep("F|C", gen_agg_player_stats$pos), ]
  gen_forward_points <- sum(gen_forwards$pts, na.rm=TRUE)/sum(as.numeric(gen_agg_team_stats$team_pts), na.rm=TRUE)
})

agg.team$team_forward_avg_height <- sapply(seq(1:length(agg.team$team_ft)), function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  gen_agg_team_stats <- agg.team[gen_team_id, ]
  gen_agg_player_stats <- agg.player[agg.player$team_id == gen_team_id, ]
  gen_forwards <- gen_agg_player_stats[grep("F|C", gen_agg_player_stats$pos), ]
  gen_forward_height <- mean(gen_forwards$height,na.rm=TRUE)
})

agg.team$team_guard_avg_height <- sapply(seq(1:length(agg.team$team_ft)), function(x){
  gen_team_id <- as.character(agg.team.teams[x])
  gen_agg_team_stats <- agg.team[gen_team_id, ]
  gen_agg_player_stats <- agg.player[agg.player$team_id == gen_team_id, ]
  gen_guards <- gen_agg_player_stats[gen_agg_player_stats$pos == "G", ]
  gen_guard_height <- mean(gen_guards$height,na.rm=TRUE)
})
# Add a few more elements to our vector of variables
basicgamestatsaddsingle <- c("home_avg_ptsdiff","away_avg_ptsdiff","avg_ptsdiff","home_wins","away_wins","wins","home_losses","away_losses","losses")
basicgamestats_team <- append(basicgamestats_team, basicgamestatsaddsingle)

##### Append new team data to each game
# Create our list of games
ind.game.games <- row.names(ind.game)

home_away_vars_to_populate <- sapply(seq(1:31), function (x){
  gen_var_name <- basicgamestats_team[x]
  paste0("team_", gen_var_name)
})

for (variable in seq(along=home_away_vars_to_populate)) {
  gen_var_name <- home_away_vars_to_populate[variable]
  away_command <- paste0("away_season_", gen_var_name)
  home_command <- paste0("home_season_", gen_var_name)
  assign(away_command, sapply(seq(1:length(ind.game$game_date)),function(x){
    gen_game_id <- ind.game.games[x]
    gen_away_team_id <- as.character(ind.game[gen_game_id, ]$away_team_id)
    tm_sub <- agg.team[gen_away_team_id,]
    tm_sub[,gen_var_name]
  }))
  assign(home_command, sapply(seq(1:length(ind.game$game_date)),function(x){
    gen_game_id <- ind.game.games[x]
    gen_home_team_id <- as.character(ind.game[gen_game_id, ]$home_team_id)
    tm_sub <- agg.team[gen_home_team_id,]
    tm_sub[,gen_var_name]
  }))
}

ind.game$home_season_team_fgm <-         home_season_team_fgm
ind.game$home_season_team_fga <-         home_season_team_fga
ind.game$home_season_team_fgpct <-       home_season_team_fgpct
ind.game$home_season_team_three_fgm <-   home_season_team_three_fgm
ind.game$home_season_team_three_fg <-    home_season_team_three_fg
ind.game$home_season_team_three_fgpct <- home_season_team_three_fgpct
ind.game$home_season_team_ftm <-         home_season_team_ftm
ind.game$home_season_team_fta <-         home_season_team_fta
ind.game$home_season_team_ftpct <-       home_season_team_ftpct
ind.game$home_season_team_pts <-         home_season_team_pts
ind.game$home_season_team_ptsavg <-      home_season_team_ptsavg
ind.game$home_season_team_offreb <-      home_season_team_offreb
ind.game$home_season_team_defreb <-      home_season_team_defreb
ind.game$home_season_team_totreb <-      home_season_team_totreb
ind.game$home_season_team_rebavg <-      home_season_team_rebavg
ind.game$home_season_team_ast <-         home_season_team_ast
ind.game$home_season_team_to <-          home_season_team_to
ind.game$home_season_team_stl <-         home_season_team_stl
ind.game$home_season_team_blk <-         home_season_team_blk
ind.game$home_season_team_fouls <-       home_season_team_fouls
ind.game$home_season_team_dbldbl <-      home_season_team_dbldbl
ind.game$home_season_team_trpldbl <-     home_season_team_trpdbl

ind.game$away_season_team_fgm <-         away_season_team_fgm
ind.game$away_season_team_fga <-         away_season_team_fga
ind.game$away_season_team_fgpct <-       away_season_team_fgpct
ind.game$away_season_team_three_fgm <-   away_season_team_three_fgm
ind.game$away_season_team_three_fg <-    away_season_team_three_fg
ind.game$away_season_team_three_fgpct <- away_season_team_three_fgpct
ind.game$away_season_team_ftm <-         away_season_team_ftm
ind.game$away_season_team_fta <-         away_season_team_fta
ind.game$away_season_team_ftpct <-       away_season_team_ftpct
ind.game$away_season_team_pts <-         away_season_team_pts
ind.game$away_season_team_ptsavg <-      away_season_team_ptsavg
ind.game$away_season_team_offreb <-      away_season_team_offreb
ind.game$away_season_team_defreb <-      away_season_team_defreb
ind.game$away_season_team_totreb <-      away_season_team_totreb
ind.game$away_season_team_rebavg <-      away_season_team_rebavg
ind.game$away_season_team_ast <-         away_season_team_ast
ind.game$away_season_team_to <-          away_season_team_to
ind.game$away_season_team_stl <-         away_season_team_stl
ind.game$away_season_team_blk <-         away_season_team_blk
ind.game$away_season_team_fouls <-       away_season_team_fouls
ind.game$away_season_team_dbldbl <-      away_season_team_dbldbl
ind.game$away_season_team_trpldbl <-     away_season_team_trpdbl

rm(list = ls(pattern = "\\baway_season_"))
rm(list = ls(pattern = "\\bhome_season_"))

#### Haven't edited code after this. 


##### Create a new team-level data frame that has individual game stats and looks like: Team Stats, Opponent Stats (Note: Each game will thus have two entries!)
# First, the away team
vec_team_away <- data.frame(thisteam_team_id = ind.game$away_team_id, thisteam_team_name = ind.game$away_team_name, game_id = row.names(ind.game), game_date = ind.game$game_date, neutral_site = ind.game$neutral_site, home = 0, opp_team_id = ind.game$home_team_id, opp_team_name = ind.game$home_team_name)
for (variable in seq(along=basicgamestats_team)) {
  gen_var_name <- basicgamestats_team[variable]
  command <- paste0("vec_team_away$thisteam_", gen_var_name, " = ind.game$away_team_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_away$opp_", gen_var_name, " = ind.game$home_team_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_away$thisteam_season_", gen_var_name, " = ind.game$away_season_team_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_away$opp_season_", gen_var_name, " = ind.game$home_season_team_", gen_var_name)
  eval(parse(text=command))
}
for (variable in seq(along=basicgamestatsaddunique)) {
  gen_var_name <- basicgamestatsaddunique[variable]
  command <- paste0("vec_team_away$", gen_var_name, " = ind.game$", gen_var_name)
  eval(parse(text=command))
}
vec_team_away$ptsdiff <- vec_team_away$ptsdiff * -1 # The way we calculate ptsdiff is Home Team Score - Away Team Score, so we need to take the inverse here.

# Then, the home team
vec_team_home <- data.frame(thisteam_team_id = ind.game$home_team_id, thisteam_team_name = ind.game$home_team_name, game_id = row.names(ind.game), game_date = ind.game$game_date, neutral_site = ind.game$neutral_site, home = 1, opp_team_id = ind.game$away_team_id, opp_team_name = ind.game$away_team_name)
for (variable in seq(along=basicgamestats_team)) {
  gen_var_name <- basicgamestats_team[variable]
  command <- paste0("vec_team_home$thisteam_", gen_var_name, " = ind.game$home_team_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_home$opp_", gen_var_name, " = ind.game$away_team_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_home$thisteam_season_", gen_var_name, " = ind.game$home_season_team_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_home$opp_season_", gen_var_name, " = ind.game$away_season_team_", gen_var_name)
  eval(parse(text=command))
}
for (variable in seq(along=basicgamestatsaddunique)) {
  gen_var_name <- basicgamestatsaddunique[variable]
  command <- paste0("vec_team_home$", gen_var_name, " = ind.game$", gen_var_name)
  eval(parse(text=command))
}

# Bind the two dataframes into ind.team, which will have team-level data for each game
ind.team <- rbind(vec_team_away, vec_team_home)

# Erase any existing data frame-populating vectors
rm(list = ls(pattern = "\\bvec_"))
rm(list = ls(pattern = "\\bgen_"))


##### Add in opponents' winning percentage, etc. (all stats from basicgamestatsaddsingle, but for opponents)
# Create a vector with each team
agg.team.teams <- row.names(agg.team)

# Add each variable in the column of overall data frame
for(var in seq(along=basicgamestatsaddsingle)){
  var_name = basicgamestatsaddsingle[var]
  command = paste0("agg.team$opp_team_",var_name," <-","rep(0,length(agg.team$team_ft))")
  eval(parse(text=command))
}
# Make Calculations 
for (team in seq(along=agg.team.teams)) {
  home_avg_ptsdiff <- NULL
  away_avg_ptsdiff <- NULL
  avg_ptsdiff <- NULL
  home_wins <- NULL
  away_wins <- NULL
  wins <- NULL 
  home_losses <- NULL
  away_losses <- NULL
  losses <- NULL 
  gen_team_id <- as.character(agg.team.teams[team])
  opp_subset <- ind.team[ind.team$thisteam_team_id == gen_team_id, ]$opp_team_id
  for(i in seq(along=opp_subset)){
    tm_subset <- ind.team[ind.team$thisteam_team_id == opp_subset[i],]
    home_subset <- subset(tm_subset,tm_subset$home==1)
    away_subset <- subset(tm_subset,tm_subset$home == 0)
    # Populate Data
    home_avg_ptsdiff <- append(home_avg_ptsdiff, ifelse(nrow(home_subset)==0,0,mean(home_subset$ptsdiff)))
    away_avg_ptsdiff <- append(away_avg_ptsdiff, ifelse(nrow(away_subset)==0,0,mean(away_subset$ptsdiff)))
    avg_ptsdiff <- append(avg_ptsdiff,mean(tm_subset$ptsdiff))
    home_wins <- length(which(home_subset$ptsdiff>0))
    away_wins <- length(which(away_subset$ptsdiff>0))
    wins <- length(which(tm_subset$ptsdiff>0))
    home_losses <- length(which(home_subset$ptsdiff<0))
    away_losses <- length(which(away_subset$ptsdiff<0))
    losses <- length(which(tm_subset$ptsdiff<0))
  }
  agg.team[gen_team_id,]$opp_team_home_avg_ptsdiff <- mean(home_avg_ptsdiff)
  agg.team[gen_team_id,]$opp_team_away_avg_ptsdiff <- mean(away_avg_ptsdiff)
  agg.team[gen_team_id,]$opp_team_avg_ptsdiff <- mean(avg_ptsdiff)
  agg.team[gen_team_id,]$opp_team_home_wins <- home_wins
  agg.team[gen_team_id,]$opp_team_away_wins <- away_wins
  agg.team[gen_team_id,]$opp_team_wins <- wins
  agg.team[gen_team_id,]$opp_team_home_losses <- home_losses
  agg.team[gen_team_id,]$opp_team_away_losses <- away_losses
  agg.team[gen_team_id,]$opp_team_losses <- losses
  rm(home_avg_ptsdiff)
  rm(away_avg_ptsdiff)
  rm(avg_ptsdiff)
  rm(home_wins)
  rm(away_wins)
  rm(wins)
  rm(home_losses)
  rm(away_losses)
  rm(losses)
}


##### Now, we append all of that opponent data to each game
# Create our list of games
ind.game.games <- row.names(ind.game)

# Determine what variables need to be populated
opp_vars_to_populate <- NULL
for (variable in seq(along=basicgamestatsaddsingle)) {
  gen_var_name <- basicgamestatsaddsingle[variable]
  opp_vars_to_populate <- append(opp_vars_to_populate, paste0("opp_", gen_var_name))
}

# Create empty vectors for the information to be added
for (variable in seq(along=basicgamestatsaddsingle)) {
  gen_var_name <- basicgamestatsaddsingle[variable]
  command <- paste0("gen_var_away_vec_", gen_var_name, " <- NULL")
  eval(parse(text=command))
  command <- paste0("gen_var_home_vec_", gen_var_name, " <- NULL")
  eval(parse(text=command))
}

# Parse each game and generate statistics
for (game in seq(along=ind.game.games)) {
  gen_game_id <- ind.game.games[game]
  gen_away_team_id <- as.character(ind.game[gen_game_id, ]$away_team_id)
  gen_home_team_id <- as.character(ind.game[gen_game_id, ]$home_team_id)
  
  # For each one of our variables
  for (variable in seq(along=basicgamestatsaddsingle)) {
    gen_var_name <- basicgamestatsaddsingle[variable]
    
    # Get values from team data frame
    command <- paste0("gen_values_away_", gen_var_name, " <- agg.team['", gen_away_team_id, "', ]$opp_team_", gen_var_name)
    eval(parse(text=command))
    command <- paste0("gen_values_home_", gen_var_name, " <- agg.team['", gen_home_team_id, "', ]$opp_team_", gen_var_name)
    eval(parse(text=command))
    
    # Append it to our earlier vector
    command <- paste0("gen_var_away_vec_", gen_var_name, " <- append(gen_var_away_vec_", gen_var_name, ", gen_values_away_", gen_var_name, ")")
    eval(parse(text=command))
    command <- paste0("gen_var_home_vec_", gen_var_name, " <- append(gen_var_home_vec_", gen_var_name, ", gen_values_home_", gen_var_name, ")")
    eval(parse(text=command))
  }
}

# Add the data to the data frame
for (variable in seq(along=basicgamestatsaddsingle)) {
  gen_var_name <- basicgamestatsaddsingle[variable]
  command <- paste0("ind.game$away_season_opp_", gen_var_name, " <- gen_var_away_vec_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("ind.game$home_season_opp_", gen_var_name, " <- gen_var_home_vec_", gen_var_name)
  eval(parse(text=command))
}


# Erase any existing data frame-populating vectors
rm(list = ls(pattern = "\\bvec_"))
rm(list = ls(pattern = "\\bgen_"))


#### And now we take that opponent data and feed it back into our aggregate team dataframe
# First, the away team
vec_team_away <- data.frame(row.names=row.names(ind.team))
for (variable in seq(along=basicgamestatsaddsingle)) {
  gen_var_name <- basicgamestatsaddsingle[variable]
  command <- paste0("vec_team_away$thisteam_season_opp_", gen_var_name, " = ind.game$away_season_opp_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_away$opp_season_opp_", gen_var_name, " = ind.game$home_season_opp_", gen_var_name)
  eval(parse(text=command))
}

# Then, the home team
vec_team_home <- data.frame(row.names=row.names(ind.team))
for (variable in seq(along=basicgamestatsaddsingle)) {
  gen_var_name <- basicgamestatsaddsingle[variable]
  command <- paste0("vec_team_home$thisteam_season_opp_", gen_var_name, " = ind.game$home_season_opp_", gen_var_name)
  eval(parse(text=command))
  command <- paste0("vec_team_home$opp_season_opp_", gen_var_name, " = ind.game$away_season_opp_", gen_var_name)
  eval(parse(text=command))
}

vec_ind.team <- rbind(vec_team_away, vec_team_home)
ind.team <- cbind(ind.team, vec_team_away, vec_team_home)

# Erase any existing data frame-populating vectors
rm(list = ls(pattern = "\\bvec_"))
rm(list = ls(pattern = "\\bgen_"))

## Save agg.team
saveRDS(agg.team,file="agg_team.RDS")
## Save agg.player
saveRDS(agg.player,file="agg_player.RDS")
## Save ind.team
saveRDS(ind.team,file="ind_team.RDS")
## Save ind.player 
saveRDS(ind.player,file="ind_player.RDS")
  

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken