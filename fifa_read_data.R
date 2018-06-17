# preparation -------------------------------------------------------------
getal <- function(x){as.numeric(as.character(x))}

# create the games
games <- tribble(~home, ~away, ~group,
                 "Russia","Saudi Arabia","A", 
                 "Egypt","Uruguay","A",
                 "Morocco","Iran","B",
                 "Portugal","Spain","B",
                 "France","Australia","C",
                 "Argentina","Iceland","D",
                 "Peru","Denmark","C",
                 "Croatia","Nigeria","D",
                 "Costa Rica","Serbia","E",
                 "Germany","Mexico","F",
                 "Brazil","Switzerland","E",
                 "Sweden","South Korea","F",
                 "Belgium","Panama","G",
                 "Tunisia","England","G",
                 "Colombia","Japan","H",
                 "Poland","Senegal","H",
                 "Russia","Egypt","A",
                 "Portugal","Morocco","B",
                 "Uruguay","Saudi Arabia","A",
                 "Iran","Spain","B",
                 "Denmark","Australia","C",
                 "France","Peru","C",
                 "Argentina","Croatia","D",
                 "Brazil","Costa Rica","E",
                 "Nigeria","Iceland","D",
                 "Serbia","Switzerland","E",
                 "Belgium","Tunisia","G",
                 "South Korea","Mexico","F",
                 "Germany","Sweden","F",
                 "England","Panama","G",
                 "Japan","Senegal","H",
                 "Poland","Colombia","H",
                 "Uruguay","Russia","A",
                 "Saudi Arabia","Egypt","A",
                 "Spain","Morocco","B",
                 "Iran","Portugal","B",
                 "Denmark","France","C",
                 "Australia","Peru","C",
                 "Nigeria","Argentina","D",
                 "Iceland","Croatia","D",
                 "South Korea","Germany","F",
                 "Mexico","Sweden","F",
                 "Serbia","Brazil","E",
                 "Switzerland","Costa Rica","E",
                 "Japan","Poland","H",
                 "Senegal","Colombia","H",
                 "England","Belgium","G",
                 "Panama","Tunisia","G")

# groups & betting companies
groups <- unique(games$group)
betting_companies <- c("bet365","skybet","ladbrokes","williamhill","marathonbet","betfair","sunbets","paddypower","unibet",
                  "coral","betfred","boylesports","blacktype","betstars","betway","betbright","10bet","sportingbet",
                  "188bet","888","betvictor","sportpesa","spreadex","royalpanda","betfairexchange","betdaq","matchbook","smarkets")

link.pre <- "https://www.oddschecker.com/football/world-cup/"

# links to oddschecker.com
link.winner <- "/winner" # winner per game, not outright winner
link.correct_score <- "/correct-score"

games$link0 <- gsub(" ","-",paste0(tolower(games$home),"-v-",tolower(games$away))) # lower case everything and replace spaces with -
games$link.winner <- paste0(link.pre,games$link0,link.winner) # add pre and suffix
games$link.correct_score <- paste0(link.pre,games$link0,link.correct_score) # add pre and suffix
games <- select(games, -link0) # at this point it's good to check the links to see if they work (copy all of the above in an rmd file; see links.rmd)

# initialize some columns
games$home.pct <- -1 # percentage chance of home team winning 
games$draw.pct <- -1
games$away.pct <- -1

# initiate a list for odds on correct scores
odds.list <- list()

# scrape WINNER odds (who wins each game) -------------------------------------------------------------
for (i in seq(nrow(games))){
  
  hometeam <- games$home[i] # which team is the official home team
  awayteam <- games$away[i] # which team is the official away team
  grp <- games$group[i] # which group
  print(paste(hometeam,awayteam,grp)) # keep track
  
  # Read the data from the webpages
  page.winner <- xml2::read_html(games$link.winner[i])
  
  # WINNER: Check whether official home team = home team on oddschecker
  oc.order <- strwrap(rvest::html_nodes(page.winner,".selTxt")[1]) 
  oc.order <- oc.order[grepl("data-name",oc.order)]
  if(grepl(hometeam,oc.order)){ # oddschecker has official home team first
    OC.same <- 1
    teams <- c(hometeam,"draw",awayteam)}
  
  if(grepl(hometeam,oc.order)==FALSE & grepl(awayteam,oc.order)==TRUE){ # oddschecker has official away team first
    OC.same <- 0
    teams <- c(awayteam,"draw",hometeam)} 
  
  if(grepl(hometeam,oc.order)==FALSE & grepl(awayteam,oc.order)==FALSE){
    print(paste("error with game",i,"; double check"))
    teams <- c(hometeam,"draw",awayteam)} # but continue as if oddschecker has official home team first
  
  # WINNER: Extract odds
  odds.winner1 <- strwrap(rvest::html_nodes(page.winner,"#oddsTableContainer")[1])
  odds.winner1 <- odds.winner1[grepl("data-o=\"", odds.winner1)]
  odds.winner1 <- unlist(strsplit(odds.winner1," "))
  odds.winner1 <- odds.winner1[grepl("data-o=",odds.winner1)]
  odds.winner1 <- gsub("data-o=\"","",odds.winner1)
  odds.winner1 <- gsub("\"","",odds.winner1)
  
  # WINNER: Tidy odds
  L <- length(odds.winner1)/3
  odds.winner <- tibble(team = rep(teams,each = L), odds = odds.winner1, bc = rep(betting_companies,3)) %>% # teams = teams as in the order on Oddschecker
    separate(odds, c("voor","tegen"), sep = "/", fill="right") %>% # separate the odds in for and against; if only one number, put in the voor column and leave tegen = NA
    mutate(tegen=replace(tegen, is.na(tegen), 1), original.odds = getal(tegen)/(getal(voor)+getal(tegen))) %>% # replace NA with 1
    arrange(bc) %>% 
    filter(!is.na(original.odds)) # sometimes there are no odds for certain betting companies
  
  # Overround:
  # quoted odds = odds * delta + 1
  # 1 = stake = to be paid back to the gambler in case s/he wins
  # delta < 1 is proportion of bets that is actually paid out by bookmakers
  # 1 - delta = overround = profit margin of the bookmaker
  overround.winner <- summarize(group_by(odds.winner,bc), count = n(), OR = (sum(original.odds)-1)/count) # overround per game
  
  # WINNER: Remove overround from odds
  odds.winner.final <- odds.winner %>% 
    join(overround.winner) %>% 
    mutate(true.odds = original.odds - OR) %>% 
    select(team, bc, true.odds) %>% 
    reshape::cast(bc ~ team) %>% 
    select(teams) %>%
    summarize_all(funs(mean)) %>% 
    unlist
  
  # WINNER: Gather odds in the games data frame
  games$home.pct[i] <- odds.winner.final[hometeam]
  games$draw.pct[i] <- odds.winner.final["draw"]
  games$away.pct[i] <- odds.winner.final[awayteam]
  print(c(odds.winner.final[hometeam],odds.winner.final["draw"],odds.winner.final[awayteam]))
  
}

games <- games %>% 
  select(-link.winner) %>% 
  mutate(homepoints = 0, awaypoints = 0, homegoals = 0, awaygoals = 0)

save(games, file = "games.rdata")

# scrape CORRECT SCORE data -----------------------------------------------
for (i in seq(nrow(games))){
  
  hometeam <- games$home[i] # which team is the official home team
  awayteam <- games$away[i] # which team is the official away team
  grp <- games$group[i] # which group
  print(paste(hometeam,awayteam,grp)) # keep track
  
  # Read the data from the webpages
  page.correct_score <- xml2::read_html(games$link.correct_score[i])
  
  # CORRECT SCORE: Get the possible scores
  possible.scores <- strwrap(rvest::html_nodes(page.correct_score,".selTxt"))
  possible.scores <- possible.scores[grepl("data-name",possible.scores)]
  possible.scores <- gsub("data-name=\"","",possible.scores)
  possible.scores <- gsub(">.*","",possible.scores)
  possible.scores <- gsub("\".*","",possible.scores)
  
  # CORRECT SCORE: Extract odds
  odds.cs1 <- strwrap(rvest::html_nodes(page.correct_score,"#oddsTableContainer")[1])
  odds.cs1 <- odds.cs1[grepl("data-o=\"", odds.cs1)]
  odds.cs1 <- unlist(strsplit(odds.cs1," "))
  odds.cs1 <- odds.cs1[grepl("data-o=",odds.cs1)]
  odds.cs1 <- gsub("data-o=\"","",odds.cs1)
  odds.cs1 <- gsub("\"","",odds.cs1)
  
  # CORRECT SCORE: Tidy odds
  L <- length(odds.cs1)/length(possible.scores)
  odds.cs <- tibble(score = rep(possible.scores,each = L), odds = odds.cs1, bc = rep(betting_companies,length(possible.scores))) %>% 
    filter(score!="Any Other Score") %>% 
    separate(odds, c("voor","tegen"), sep = "/", fill="right") %>% 
    mutate(tegen=replace(tegen, is.na(tegen), 1), original.odds = getal(tegen)/(getal(voor)+getal(tegen))) %>% 
    filter(voor!="" & tegen!="") %>% 
    arrange(bc) %>% 
    mutate(ochome = case_when(grepl(awayteam,score) ~ awayteam, # oc has listed away team first
                              TRUE ~ hometeam),
           ocaway = case_when(grepl(awayteam,score) ~ hometeam, # oc has listed away team first
                              TRUE ~ awayteam)) %>% 
    mutate(score = gsub(paste0(hometeam," "),"",score)) %>% # clean up score variable
    mutate(score = gsub(paste0(awayteam," "),"",score)) %>% # clean up score variable
    mutate(score = gsub(paste0("Draw"," "),"",score)) %>%   # clean up score variable
    separate(score, c("oc.homegoals", "oc.awaygoals"), sep="-") %>% 
    mutate(group = grp, hometeam = hometeam, awayteam = awayteam,
           homegoals = case_when(ochome == hometeam ~ oc.homegoals,
                                 ochome == awayteam ~ oc.awaygoals),
           awaygoals = case_when(ochome == hometeam ~ oc.awaygoals,
                                 ochome == awayteam ~ oc.homegoals)) %>%  # use the official home & away teams, not the OC ones
    select(-oc.homegoals, -oc.awaygoals, -ochome, - ocaway) %>% 
    mutate(score = paste0(homegoals,"-",awaygoals)) %>% 
    filter(original.odds<.50) # sometimes there are mistakes
  
  # Overround:
  # quoted odds = odds * delta + 1
  # 1 = stake = to be paid back to the gambler in case s/he wins
  # delta < 1 is proportion of bets that is actually paid out by bookmakers
  # 1 - delta = overround = profit margin of the bookmaker
  overround.cs <- summarize(group_by(odds.cs,bc), count = n(), OR = (sum(original.odds)-1)/count) %>% 
    mutate(OR=replace(OR, OR<0, 0))
  
  # CORRECT SCORE: Remove overround from odds & remove extreme low probability scores
  odds.cs.final <- odds.cs %>% 
    join(overround.cs) %>% 
    mutate(true.odds = original.odds - OR) %>% 
    # select(score, bc, true.odds) %>% 
    filter(true.odds>0) %>% # Sometimes there are negative odds to due overround removal. Any other score isn't very useful.
    group_by(score) %>% 
    summarize(group = first(group), hometeam = first(hometeam), awayteam = first(awayteam),
              homegoals = first(homegoals), awaygoals = first(awaygoals),
              mean_odds = mean(true.odds)) %>% # averaged across betting companies
    arrange(desc(mean_odds)) %>% 
    mutate(cumulative = cumsum(mean_odds)) %>% # Cumulative is needed to make sure the odds add to 1
    filter(cumulative<=1) # Make sure that the probabilities don't add to more than 1
  
  # CORRECT SCORE: Make sure the percentages sum to one
  odds.cs.final$mean_odds[1] <- odds.cs.final$mean_odds[1] + (1-odds.cs.final$cumulative[nrow(odds.cs.final)]) # add a tiny bit to the most likely score
  
  # CORRECT SCORE: Store data in a list
  odds.cs.final <- odds.cs.final %>% select(-cumulative)
  odds.list[[i]] <- odds.cs.final
}

names(odds.list) <- paste(games$home,games$away,sep=" - ") # name the list with odds on correct scores
save(odds.list, file = "odds.list.rdata")
games <- select(games, - link.correct_score)

# data frame with most likely score per game ------------------------------
most.likely.scores <- odds.list[[1]][1,]
for (i in 2:length(odds.list)){most.likely.scores <- rbind(most.likely.scores,odds.list[[i]][1,])}
most.likely.scores <- arrange(most.likely.scores, group)

# scrape OUTRIGHT WINNER data ---------------------------------------------
teams_groups <- unique(games[c("home","group")]) %>% 
  arrange(group) %>% 
  rename(team = home)
  
# Read the data from the webpages
page.outright <- xml2::read_html("https://www.oddschecker.com/football/world-cup/winner")

# Get oddschecker's listing:
oc.listing <- strwrap(rvest::html_nodes(page.outright,".selTxt"))
oc.listing <- oc.listing[grepl("data-name",oc.listing)]
oc.listing <- gsub("data-name=\"","",oc.listing)
oc.listing <- gsub(">.*","",oc.listing)
oc.listing <- gsub("\".*","",oc.listing)

# outright: Extract odds
odds.outright1 <- strwrap(rvest::html_nodes(page.outright,"#oddsTableContainer")[1])
odds.outright1 <- odds.outright1[grepl("data-o=\"", odds.outright1)]
odds.outright1 <- unlist(strsplit(odds.outright1," "))
odds.outright1 <- odds.outright1[grepl("data-o=",odds.outright1)]
odds.outright1 <- gsub("data-o=\"","",odds.outright1)
odds.outright1 <- gsub("\"","",odds.outright1)

# outright: Tidy odds
L <- length(odds.outright1)/length(oc.listing)
odds.outright <- tibble(team = rep(oc.listing,each = L), odds = odds.outright1, bc = rep(betting_companies,length(oc.listing))) %>% # teams = teams as in the order on Oddschecker
  separate(odds, c("voor","tegen"), sep = "/", fill="right") %>% # separate the odds in for and against; if only one number, put in the voor column and leave tegen = NA
  mutate(tegen=replace(tegen, is.na(tegen), 1), original.odds = getal(tegen)/(getal(voor)+getal(tegen))) %>% # replace NA with 1
  arrange(bc) %>% 
  filter(!is.na(original.odds)) # sometimes there are no odds for certain betting companies

# Overround:
# quoted odds = odds * delta + 1
# 1 = stake = to be paid back to the gambler in case s/he wins
# delta < 1 is proportion of bets that is actually paid out by bookmakers
# 1 - delta = overround = profit margin of the bookmaker
overround.outright <- summarize(group_by(odds.outright,bc), count = n(), OR = (sum(original.odds)-1)/count) # overround per game

# outright: Remove overround from odds
odds.outright.final <- odds.outright %>% 
  join(overround.outright) %>% 
  mutate(true.odds = original.odds - OR) %>% 
  select(team, bc, true.odds) %>% 
  mutate(true.odds=replace(true.odds, true.odds<0, 0)) %>% # Sometimes there are negative odds to due overround removal. Any other score isn't very useful.
  group_by(team) %>% 
  summarize(mean_odds = mean(true.odds)) %>% # averaged across betting companies
  arrange(desc(mean_odds)) %>% 
  mutate(cumulative = cumsum(mean_odds), # Cumulative is needed to make sure the odds add to 1
         cum.diff = 1-cumulative) # cum.diff is useful to find the cumulative closest to one (see next line)
  
# outright: Make sure the percentages sum to one
min.chance <- min(odds.outright.final$cum.diff[odds.outright.final$cum.diff>0])

odds.outright.final1 <- odds.outright.final %>% 
  filter(cumulative<1)
odds.outright.final2 <- odds.outright.final %>% 
  filter(cumulative>=1)
odds.outright.final2$mean_odds <- min.chance/nrow(odds.outright.final2)

odds.outright.final <- rbind(odds.outright.final1,odds.outright.final2) %>% 
  select(-cumulative, -cum.diff) %>% 
  rename(probability = mean_odds)

teams_groups <- join(teams_groups,odds.outright.final)

save(teams_groups, file = "teams_groups.rdata")
