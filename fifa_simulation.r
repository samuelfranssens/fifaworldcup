# read data ---------------------------------------------------------------
load("games.rdata")
load("odds.list.rdata")
load("teams_groups.rdata")

games <- games %>% 
  select(-link.winner, -link.correct_score) %>% 
  mutate(homepoints = 0, awaypoints = 0, homegoals = 0, awaygoals = 0)

groups <- unique(teams_groups$group)

getal <- function(x){as.numeric(as.character(x))}

# parameters and data frame for simulation
simulations <- 10000
result_simulations <- tibble(simulation = rep(seq(simulations),each = length(groups)), 
                             group = rep(groups,simulations), 
                             winner = "", 
                             runnerup= "")

knockout <- tibble(simulation = rep(seq(simulations), each = 8),
                   nr = rep(c(49:56),simulations), 
                   home = "", away = "")

quarters <- tibble(simulation = rep(seq(simulations), each = 4),
                   nr = rep(c(57:60),simulations), 
                   home = "", away = "")

semis <- tibble(simulation = rep(seq(simulations), each = 2),
                nr = rep(c(61:62),simulations), 
                home = "", away = "")

final <- tibble(simulation = rep(seq(simulations), each = 1),
                nr = rep(c(63),simulations), 
                home = "", away = "")

# function that calculates probability of team proceeding -----------------------------------------------------------------------
get_group <- function(grp, dataset = result_simulations){
  teams <- teams_groups$team[teams_groups$group==grp] # get all the teams in a group
  x <- tibble(team=NA, winner = NA, runnerup = NA, gothrough=NA)
  
  for (i in seq(length(teams))){
    team <- teams[i]
    y <- dataset %>% 
      filter(group == grp) %>% 
      mutate(winner = as.numeric(grepl(team,winner)),
             runnerup = as.numeric(grepl(team,runnerup)),
             gothrough = winner + runnerup) %>% 
      summarize(team = first(team), count = n(), winner = sum(winner)/count, runnerup = sum(runnerup)/count, gothrough = sum(gothrough)/count) %>% 
      select(-count)
    x <- rbind(x,y)
  }
  x <- filter(x, !is.na(team)) %>% arrange(desc(winner))
  
  return(x)  
}

# function that calculates percentage on getting to a certain round -------
get2round <- function(round,team){
  opponents <- c(round$away[round$home == team],round$home[round$away == team])
  x <- tibble(team = names(table(opponents)), count = as.numeric(table(opponents))) %>% 
    mutate(unconditional = count / simulations, conditional = count / sum(count)) %>% 
    arrange(desc(unconditional))
  
  return(list(paste0(team," has ",sum(x$unconditional)*100," percent chance of reaching this round"),x))
}

# simulate group games based on CORRECT SCORES ----------------------------
for (j in seq(simulations)){
  if(j %% 100 == 0){print(paste0("progress: ",j/simulations*100,"%"))}
  
  # simulate the games:
  for (i in seq(nrow(games))){
    possibilities <- odds.list[[i]]
    probabilities <- possibilities$mean_odds
    outcome <- rmultinom(1,1,probabilities) # column is observation; row is category (win, draw, lose)
    games$homegoals[i] <- possibilities[which(outcome==1),c("homegoals")]
    games$awaygoals[i] <- possibilities[which(outcome==1),c("awaygoals")]
  }
  
  games$homegoals <- getal(games$homegoals)
  games$awaygoals <- getal(games$awaygoals)
  
  # calculate points
  games$homepoints <- case_when(games$homegoals>games$awaygoals ~ 3,
                                    games$homegoals==games$awaygoals ~ 1,
                                    games$homegoals<games$awaygoals ~ 0)
  games$awaypoints <- case_when(games$homegoals>games$awaygoals ~ 0,
                                    games$homegoals==games$awaygoals ~ 1,
                                    games$homegoals<games$awaygoals ~ 3)
  
  # calculate group standing
  ho <- select(games,group, home, homepoints, homegoals, awaygoals) %>% rename(team = home, points = homepoints, scored = homegoals, against = awaygoals)
  aw <- select(games,group, away, awaypoints, homegoals, awaygoals) %>% rename(team = away, points = awaypoints, scored = awaygoals, against = homegoals)
  standing <- rbind(ho,aw) %>% 
    mutate(scored = getal(scored), against=getal(against)) %>% 
    group_by(team) %>% 
    summarize(group = first(group), points = sum(points), scored = sum(scored), against = sum(against), goaldifference = scored-against) %>% 
    arrange(group,desc(points),desc(goaldifference),desc(scored))
  
  # convert group standing to winner & runner up
  for (k in seq(length(groups))){
    groupi <- groups[k]
    
    group.table <- standing %>% 
      filter(group == groupi) %>% 
      mutate(rank.points.goals = rank(points) + rank(goaldifference)/5, # add fraction of rank goal difference to split ties
             rank.total = rank(rank.points.goals)) %>% 
      select(-rank.points.goals) %>% 
      arrange(desc(rank.total))
    
    rank1 <- group.table$rank.total[1]
    rank2 <- group.table$rank.total[2]
    rank3 <- group.table$rank.total[3]
    rank4 <- group.table$rank.total[4]
    t1 <- group.table$team[1]
    t2 <- group.table$team[2]
    t3 <- group.table$team[3]
    t4 <- group.table$team[4]
    
    # 1 > 2 > 3
    if(rank1>rank2 & rank2>rank3){
      winner <- t1
      runnerup <- t2
    }
    
    # 1 = 2 > 3
    if(rank1==rank2 & rank2>rank3){
      # winner: get the game between 1 and 2
      ties <- games[(games$home == t1 & games$away == t2) | (games$home == t2 & games$away == t1),]
      winner <- case_when(ties$homepoints == 3 ~ ties$home,
                          ties$homepoints == 0 ~ ties$away,
                          ties$homepoints == 1 ~ ties$away) # away team scores more away goals
      runnerup <- c(t1,t2)[c(t1,t2) %nin% winner]
    }
    
    # 1 > 2 = 3
    if(rank1>rank2 & rank2==rank3){
      winner <- t1
      # runner up: get the game between 2 and 3:
      ties <- games[(games$home == t2 & games$away == t3) | (games$home == t3 & games$away == t2),]
      runnerup <- case_when(ties$homepoints == 3 ~ ties$home,
                            ties$homepoints == 0 ~ ties$away,
                            ties$homepoints == 1 ~ ties$away)
    }
    
    # 1 = 2 = 3
    if(rank1 == rank3){
      ties1 <- games[(games$home == t1 & games$away == t2) | (games$home == t2 & games$away == t1),]
      ties2 <- games[(games$home == t1 & games$away == t3) | (games$home == t3 & games$away == t1),]
      ties3 <- games[(games$home == t3 & games$away == t2) | (games$home == t2 & games$away == t3),]
      ties <- rbind(ties1,ties2,ties3)
      
      ho <- select(ties,group, home, homepoints, homegoals, awaygoals) %>% rename(team = home, points = homepoints, scored = homegoals, against = awaygoals)
      aw <- select(ties,group, away, awaypoints, homegoals, awaygoals) %>% rename(team = away, points = awaypoints, scored = awaygoals, against = homegoals)
      new.standing <- rbind(ho,aw) %>% 
        mutate(scored = getal(scored), against=getal(against)) %>% 
        group_by(team) %>% 
        summarize(group = first(group), points = sum(points), scored = sum(scored), against = sum(against), goaldifference = scored-against) %>% 
        arrange(group,desc(points),desc(goaldifference),desc(scored)) %>% 
        mutate(rank.points.goals = rank(points) + rank(goaldifference)/5, 
               rank.total = rank(rank.points.goals)) %>% 
        select(-rank.points.goals) %>% 
        arrange(desc(rank.total))
      
      newrank1 <- new.standing$rank.total[1]
      newrank2 <- new.standing$rank.total[2]
      newrank3 <- new.standing$rank.total[3]
      newt1 <- new.standing$team[1]
      newt2 <- new.standing$team[2]
      newt3 <- new.standing$team[3]
      
      if(newrank1 > newrank2 & newrank2 > newrank3){ # now 1 > 2 > 3
        winner <- newt1
        runnerup <- newt2
      }
      
      if(newrank1 > newrank2 & newrank2 == newrank3){ # now 1 > 2 = 3
        winner <- newt1
        runnerup <- sample(c(newt2,newt3),1)
      }
      
      if(newrank1 == newrank2 & newrank2 > newrank3){ # now 1 = 2 > 3
        d <- sample(c(newt1,newt2),2)
        winner <- d[1]
        runnerup <- d[2]
      }
      
      if(newrank1 == newrank3){ # still 1 = 2 = 3
        d <- sample(c(newt1,newt2,newt3),2)
        winner <- d[1]
        runnerup <- d[2]
      }
    }
    
    # 1 = 2 = 3 = 4
    if(rank1 == rank4){
      d <- sample(c(t1,t2,t3,t4),2)
      winner <- d[1]
      runnerup <- d[2]
      }
    
    # done calculating ranking, store results
    result_simulations$winner[result_simulations$group == groupi & result_simulations$simulation == j] <- winner
    result_simulations$runnerup[result_simulations$group == groupi & result_simulations$simulation == j] <- runnerup
  }
  
  ds <- result_simulations[result_simulations$simulation == j,]
  
  # calculate next round results
  knockout$home[knockout$simulation==j & knockout$nr == 49] <- ds$winner[ds$group=="A"]
  knockout$away[knockout$simulation==j & knockout$nr == 49] <- ds$runnerup[ds$group=="B"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 50] <- ds$winner[ds$group=="C"]
  knockout$away[knockout$simulation==j & knockout$nr == 50] <- ds$runnerup[ds$group=="D"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 51] <- ds$winner[ds$group=="B"]
  knockout$away[knockout$simulation==j & knockout$nr == 51] <- ds$runnerup[ds$group=="A"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 52] <- ds$winner[ds$group=="D"]
  knockout$away[knockout$simulation==j & knockout$nr == 52] <- ds$runnerup[ds$group=="C"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 53] <- ds$winner[ds$group=="E"]
  knockout$away[knockout$simulation==j & knockout$nr == 53] <- ds$runnerup[ds$group=="F"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 54] <- ds$winner[ds$group=="G"]
  knockout$away[knockout$simulation==j & knockout$nr == 54] <- ds$runnerup[ds$group=="H"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 55] <- ds$winner[ds$group=="F"]
  knockout$away[knockout$simulation==j & knockout$nr == 55] <- ds$runnerup[ds$group=="E"]
  
  knockout$home[knockout$simulation==j & knockout$nr == 56] <- ds$winner[ds$group=="H"]
  knockout$away[knockout$simulation==j & knockout$nr == 56] <- ds$runnerup[ds$group=="G"]
  
} # end simulation

# save(result_simulations,file="results_simulations.rdata")
# save(knockout,file="knockout.rdata")

# SIMULATE NEXT ROUNDS ----------------------------------------------------
# function to link teams with probability to win the tournament
link <- function(dataset, dataset2=teams_groups){
  left_join(dataset, dataset2, by = c("home" = "team")) %>% 
    select(names(dataset), probability) %>% 
    rename(home.ability = probability) %>% 
    left_join(dataset2, by = c("away" = "team")) %>% 
    select(names(dataset), home.ability, probability) %>% 
    rename(away.ability = probability) %>% 
    mutate(home.pct.winning = 1-log(home.ability) / (log(home.ability) + log(away.ability)),
           winner = "")
}

# function to simulate based on win percentage
sim <- function(dataset){
  for (i in seq(nrow(dataset))){
    hteam <- dataset$home[i]
    ateam <- dataset$away[i]
    hpct  <- round(dataset$home.pct.winning[i]*simulations,0)
    dataset$winner[i] <- sample(c(rep(hteam, hpct),rep(ateam,simulations-hpct)),1)
  }
  return(dataset)
}

# simulate the knockout phase ----------------------------------------
knockout <- knockout %>% link %>% sim

# carry on with the quarters ----------------------------------------------
for (i in seq(simulations)){
  ds <- knockout[knockout$simulation==i,]
  
  quarters$home[quarters$simulation==i & quarters$nr == 57] <- ds$winner[ds$nr == 49]
  quarters$away[quarters$simulation==i & quarters$nr == 57] <- ds$winner[ds$nr == 50]
  
  quarters$home[quarters$simulation==i & quarters$nr == 58] <- ds$winner[ds$nr == 53]
  quarters$away[quarters$simulation==i & quarters$nr == 58] <- ds$winner[ds$nr == 54]
  
  quarters$home[quarters$simulation==i & quarters$nr == 59] <- ds$winner[ds$nr == 51]
  quarters$away[quarters$simulation==i & quarters$nr == 59] <- ds$winner[ds$nr == 52]
  
  quarters$home[quarters$simulation==i & quarters$nr == 60] <- ds$winner[ds$nr == 55]
  quarters$away[quarters$simulation==i & quarters$nr == 60] <- ds$winner[ds$nr == 56]
}

quarters <- quarters %>% link %>% sim

# carry on with semis -----------------------------------------------------
for (i in seq(simulations)){
  ds <- quarters[quarters$simulation==i,]
  
  semis$home[semis$simulation==i & semis$nr == 61] <- ds$winner[ds$nr == 57]
  semis$away[semis$simulation==i & semis$nr == 61] <- ds$winner[ds$nr == 58]
  
  semis$home[semis$simulation==i & semis$nr == 62] <- ds$winner[ds$nr == 59]
  semis$away[semis$simulation==i & semis$nr == 62] <- ds$winner[ds$nr == 60]
}

semis <- semis %>% link %>% sim

# final -------------------------------------------------------------------
for (i in seq(simulations)){
  ds <- semis[semis$simulation==i,]
  
  final$home[final$simulation==i & final$nr == 63] <- ds$winner[ds$nr == 61]
  final$away[final$simulation==i & final$nr == 63] <- ds$winner[ds$nr == 62]
}

final <- final %>% link %>% sim 

# who wins it all ---------------------------------------------------------
final %>% 
  group_by(winner) %>% 
  summarize(count = n(), percent = count/simulations) %>% 
  arrange(desc(percent))

# get probabilities -------------------------------------------------------
get_group("G")
get2round(final,"Belgium")
