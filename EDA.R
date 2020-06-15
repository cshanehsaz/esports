#__init__
library(pacman)
p_load(tidyr, dplyr, ggplot2, randomForest, pROC)
theme_set(theme_bw())



bans_raw <- read.csv('data/bans.csv')
gold_raw <- read.csv('data/gold.csv')
kills_raw <- read.csv('data/kills.csv')
lol_raw <- read.csv('data/LeagueofLegends.csv')
matchinfo_raw <- read.csv('data/matchinfo.csv')
monsters_raw <- read.csv('data/monsters.csv')
structures_raw <- read.csv('data/structures.csv')

lol_demo <- head(lol_raw)
gold_demo <- head(gold_raw)


lol_red <- lol_raw %>%
  filter(Year == 2017) %>%
  group_by(blueTeamTag) %>%
  summarise(blue=n()) %>%
  arrange(desc(blue)) %>%
  rename(team = blueTeamTag)

lol_blue <- lol_raw %>%
  filter(Year == 2017) %>%
  group_by(redTeamTag) %>%
  summarise(red=n()) %>%
  arrange(desc(red)) %>%
  rename(team = redTeamTag)

lol <- merge(lol_red, lol_blue, by = "team") %>%
  mutate(total = blue + red, diff_redblue = abs(blue - red)) %>%
  filter(total > 20) %>%
  arrange(desc(total))


lol_red_all <- lol_raw %>%
  group_by(blueTeamTag) %>%
  summarise(blue=n()) %>%
  arrange(desc(blue)) %>%
  rename(team = blueTeamTag)

lol_blue_all <- lol_raw %>%
  group_by(redTeamTag) %>%
  summarise(red=n()) %>%
  arrange(desc(red)) %>%
  rename(team = redTeamTag)

lol_all <- merge(lol_red_all, lol_blue_all, by = "team") %>%
  mutate(total = blue + red, diff_redblue = abs(blue - red)) %>%
  arrange(desc(total))

head(lol)

#------------

selectTeamData <- function(team) {
  return(
    lol_raw %>%
      filter(Year==2017, blueTeamTag == team | redTeamTag == team) %>%
      mutate(blueTeam = ifelse(blueTeamTag==team, 1, 0), redTeam = ifelse(redTeamTag==team, 1, 0)) %>%
      mutate(winloss = ifelse(blueTeam*bResult==1 | redTeam*rResult==1, 1, 0))
  )
}

analyzeTeamWinPercentage <- function(team) {
  teamData <- lol_raw %>%
    filter(Year==2017, blueTeamTag == team | redTeamTag == team) %>%
    mutate(blueTeam = ifelse(blueTeamTag==team, 1, 0), redTeam = ifelse(redTeamTag==team, 1, 0)) %>%
    mutate(winloss = ifelse(blueTeam*bResult==1 | redTeam*rResult==1, 1, 0))
  
  winrate <- sum(teamData$winloss) / nrow(teamData)
  return(winrate)
}

#returns w/l of every pro team from 2017
winPercents <- data.frame(winPercent = sapply(as.character(lol$team), analyzeTeamWinPercentage))


hist(winPercents$winPercent)

ggplot(winPercents, aes(winPercent)) + geom_histogram(bins = 20)

#------------------


#only selects data for the chosen team

skt_raw <- selectTeamData('SKT') %>%
  mutate(redTeam=redTeam*-1)

skt_blue <- skt_raw %>%
  filter(blueTeam==1) %>%
  select(League, Year, Season, Type, gamelength, golddiff, gold=goldblue, kills=bKills, 
        towers=bTowers, inhibs=bInhibs, dragons=bDragons, barons=bBarons, heralds=bHeralds, 
        topChamp=blueTopChamp, jungleChamp=blueJungleChamp, midChamp=blueMiddleChamp, 
        adcChamp=blueADCChamp, supportChamp=blueSupportChamp, address=Address, winloss, team=blueTeam)

skt_red <- skt_raw %>%
  filter(blueTeam==0) %>%
  select(League, Year, Season, Type, gamelength, golddiff, gold=goldred, kills=rKills, 
        towers=rTowers, inhibs=rInhibs, dragons=rDragons, barons=rBarons, heralds=rHeralds, 
        topChamp=redTopChamp, jungleChamp=redJungleChamp, midChamp=redMiddleChamp, 
        adcChamp=redADCChamp, supportChamp=redSupportChamp, address=Address, winloss, team=redTeam)
 
skt <- rbind(skt_blue, skt_red)

skt_keys <- skt$address


#try to isolate the gold for each game separately and merge it
gold <- gold_raw %>%
  select(address=Address, type=Type, min_5, min_10, min_15, min_20, min_25, min_30, min_35, min_40, min_45)

skt_gold <- gold %>%
  filter(address %in% skt_keys) %>%
  arrange(address)


#make this one operation rather than merging massive then filtering (find how to merge better)
skt_com <- merge(skt_gold, skt) %>%
  select(-gold, -golddiff)

skt_com2 <- skt_com %>%
  filter(type=='golddiff') %>%
  mutate(min_5=min_5*team, min_10=min_10*team, min_15=min_15*team, min_20=min_20*team, 
         min_25=min_25*team, min_30=min_30*team, min_35=min_35*team, 
         min_40=min_40*team, min_45=min_45*team)

skt_com3 <- skt_com %>%
  filter(type!='golddiff')
  

skt_combined <- rbind(skt_com2, skt_com3) %>%
  mutate(firstTower=towers[1]) %>%
  arrange(address)

#-----------------------

skt_golddiff <- skt_combined %>%
  filter(type=='golddiff')

logit <- glm(winloss~min_10, family='binomial', skt_golddiff)
summary(logit)
predict(logit, skt_golddiff[1,], type='response')
predict(logit, skt_golddiff[2,], type='response')

ggplot(skt_combined %>% filter(type=='golddiff'), aes(min_15)) + geom_histogram(bins=20) +
  facet_grid(vars(winloss))
     
#---------------------

#doing basic golddiff against win or lose

bWins <- matchinfo_raw %>%
  select(address=Address, bResult)

golddiff <- gold %>%
  filter(type=='golddiff') %>%
  select(address, type, min_5, min_10)

goldbywin <- merge(golddiff, bWins)

#predicting all matches by golddiff against win/loss
#general accuracy at min_10: 68%

set.seed(2)
train_index <- sort(sample(nrow(goldbywin), nrow(goldbywin)*.8))
train_data <- goldbywin[train_index,]
test_data <- goldbywin[-train_index,]

logit2 <- glm(bResult~min_10, family='binomial', train_data)
summary(logit2)

predicts <- predict(logit2, test_data, type='response')
result <- cbind(test_data, predicts) %>%
  mutate(predicted = ifelse(predicts > .5, 1, 0))
result_table <- table(result$predicted, result$bResult)
(result_table[1,1] + result_table[2,2])/nrow(test_data)


#predicting only matches with large golddiffs
#accuracy for +-500 at min_10: 74%
goldbywinX <- test_data %>%
  filter(min_10 > 500 | min_10 < -500)
head(goldbywinX)
predicts <- predict(logit2, goldbywinX, type='response')
result <- cbind(goldbywinX, predicts) %>%
  mutate(predicted = ifelse(predicts > .5, 1, 0))
result_table <- table(result$predicted, result$bResult)
(result_table[1,1] + result_table[2,2])/nrow(goldbywinX)


#----------------------------------
# add monster kills in first 10 min to dataset

monstIndex <-
  monsters_raw %>%
  filter(Time <= 10) %>%
  rename(address=Address, team=Team, monsterTime=Time, monsterType=Type)

gamesWithMonsterKill <- goldbywin[which(goldbywin$address %in% monstIndex$address),]$address

monsterKillsAgg <- data.frame(address=c('a'), rDragon=c(0), bDragon=c(0), rHerald=c(0), bHerald=c(0))

gamesWithMonsterKillMini <- gamesWithMonsterKill[1]
monstIndex[560,]

#reduces monster kills into one-hot by match
for (match in 1:length(gamesWithMonsterKill)) {
  monsterKills <- monstIndex[which(monstIndex$address == gamesWithMonsterKill[match]),]
  data <- data.frame(
    address = gamesWithMonsterKill[match],
    rDragon = 0,
    bDragon = 0,
    rHerald = 0,
    bHerald = 0
  )
  for (kill in 1:nrow(monsterKills)){
    if(monsterKills$team[kill]=='rDragons') { data$rDragon[1] = 1}
    if(monsterKills$team[kill]=='bDragons') { data$bDragon[1] = 1}
    if(monsterKills$team[kill]=='rHeralds') { data$rHerald[1] = 1}
    if(monsterKills$team[kill]=='bHeralds') { data$bHerald[1] = 1}
  }
  monsterKillsAgg <- rbind(monsterKillsAgg, data)
}
monsterKillsAgg <- monsterKillsAgg[-1,]

(!(goldbywin$address %in% monsterKillsAgg$address))

gamesNoMonsterKills <- goldbywin[(!(goldbywin$address %in% monsterKillsAgg$address)),]
gamesNoMonsterKillsAgg <- data.frame(
  address=gamesNoMonsterKills$address,
  rDragon=rep(0,nrow(gamesNoMonsterKills)),
  bDragon=rep(0,nrow(gamesNoMonsterKills)),
  rHerald=rep(0,nrow(gamesNoMonsterKills)),
  bHerald=rep(0,nrow(gamesNoMonsterKills))
)

gameMonsterDataAgg <- rbind(monsterKillsAgg, gamesNoMonsterKillsAgg)

superData <- merge(goldbywin, gameMonsterDataAgg)

#---------------------
#analysis on superdata (golddiff10 and monsters)


train_index <- sort(sample(nrow(superData), nrow(superData)*.8))
train_data <- superData[train_index,]
test_data <- superData[-train_index,]
test_data_biggold <- test_data %>%
  filter(min_10 > 500 | min_10 < -500)


superLogit <- glm(bResult~min_10+rDragon+bDragon, family='binomial', train_data)
summary(superLogit)



superPredict <- ifelse(predict(superLogit, test_data, type='response')>.5,1,0)
result_table <- table(superPredict, test_data$bResult)
(result_table[1,1] + result_table[2,2])/nrow(test_data)

randForest1 <- randomForest(bResult~min_10+rDragon+bDragon+rHerald+bHerald, data=train_data,
                            mtry=3, ntree=500)
plot(randForest1)


superPredictRaw <- predict(randForest1, test_data_biggold, type='response')
superPredict <- ifelse(predict(randForest1, test_data, type='response')>.5,1,0)
result_table <- table(superPredict, test_data$bResult)
(result_table[1,1] + result_table[2,2])/nrow(test_data)

rf1.roc <- roc(superPredictRaw, test_data_biggold$bResult, plot=T)


hist(predict(randForest1, test_data, type='response'))


#-----------------------
#ELO Time

#train based on who plays who and whether win or lose
#update each team's elo based on scores going in
#continue through games

#1 if a wins, -1 if b wins
eloMatchup <- function(aTeam, bTeam, result, eloData) {
  elodiff <- aTeam$elo - bTeam$elo
  aElo <- 0
  bElo <- 0
  #upset
  if (elodiff < 0 && result == 1 || elodiff > 0 && result == -1) {
    aElo <- aTeam$elo + round(result * 25 * sqrt(log(abs(elodiff))))
    bElo <- bTeam$elo - round(result * 25 * sqrt(log(abs(elodiff))))
    print('a')
  }
  # #expected
  if (elodiff > 0 && result == 1 || elodiff < 0 && result == -1) {
    aElo <- aTeam$elo + round(result * 25 / (1 + log(elodiff)))
    bElo <- bTeam$elo - round(result * 25 / (1 + log(elodiff)))
    print('b')
  }
  # #same elo
  if (elodiff == 0) {
    aElo <- aTeam$elo + result * 25
    bElo <- bTeam$elo - result * 25
    print('c')
  }
  
  eloData[which(eloData$team==aTeam$team),]$elo <- aElo
  eloData[which(eloData$team==bTeam$team),]$elo <- bElo
  return(eloData)
}
# eloMatchup(elo[1,],elo[2,],1, elo)

#initialize elo
elo <- data.frame(team=lol_all$team, elo=rep(1000, length(lol_all$team)))

#loop through each game
matchinfo <- matchinfo_raw %>% 
  select(year=Year, blueTeamTag, redTeamTag, bResult, address=Address)

for (i in 1:nrow(matchinfo)) {
  current <- matchinfo[i,]
  print(current$blueTeamTag %in% elo$team)
  print(current$blueTeamTag)
  # print(as.character(current$redTeamTag))
  # print(which(as.character(elo$team)==as.character(current$redTeamTag)))
  #print(elo[which(as.character(elo$team)==as.character(current$blueTeamTag)),])
  elo <- eloMatchup(
              aTeam=elo[which(as.character(elo$team)==as.character(current$blueTeamTag)),],
              bTeam=elo[which(as.character(elo$team)==as.character(current$redTeamTag)),],
              result=ifelse(current$bResult==1,1,-1),
              elo
         )
}
View(elo)
     