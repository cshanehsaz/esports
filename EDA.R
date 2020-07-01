#__init__
library(pacman)
p_load(tidyr, dplyr, ggplot2, randomForest, pROC)
theme_set(theme_bw())
load("C:/Users/cshan/Box Sync/RProjects/League/workspace.RData")



#start
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
  if(!(aTeam$team %in% allTeams2$team) || !(bTeam$team %in% allTeams2$team)) {return(eloData)}
  elodiff <- aTeam$elo - bTeam$elo
  # print(elodiff)
  # cat('A Team Elo: ', aTeam$elo)
  # print('')
  # cat('B Team Elo: ', bTeam$elo)
  # print('')
  # cat('DIFF: ', elodiff)
  aElo <- 0
  bElo <- 0
  #upset
  if ((elodiff < 0 && result == 1) || (elodiff > 0 && result == -1)) {
    aElo <- aTeam$elo + round(result * 25 * sqrt(log(abs(elodiff))))
    bElo <- bTeam$elo - round(result * 25 * sqrt(log(abs(elodiff))))
    # print('--Route a--')
  }
  # #expected
  if ((elodiff > 0 && result == 1) || (elodiff < 0 && result == (-1))) {
    aElo <- aTeam$elo + round(result * 25 / (1 + log(abs(elodiff))))
    bElo <- bTeam$elo - round(result * 25 / (1 + log(abs(elodiff))))
    # print('--Route b--')
  }
  # #same elo
  if (elodiff == 0) {
    aElo <- aTeam$elo + result * 25
    bElo <- bTeam$elo - result * 25
    # print('--Route c--')
  }
  
  eloData[which(eloData$team==aTeam$team),]$elo <- aElo
  eloData[which(eloData$team==bTeam$team),]$elo <- bElo
  return(eloData)
}
# eloMatchup(elo[1,],elo[2,],1, elo)

#sets up matches to be looped over
matchinfo <- matchinfo_raw %>% 
  select(year=Year, blueTeamTag, redTeamTag, bResult, address=Address)
matchinfo <- matchinfo[c(-3427,-7240:-7300),]

allTeams <- data.frame(team=matchinfo_raw$redTeamTag, team2=matchinfo_raw$blueTeamTag)
allTeams2 <- data.frame(team=c(as.character(allTeams$team), as.character(allTeams$team2))) %>%
  group_by(team) %>%
  summarise(count=n())

#initialize elo
elo <- data.frame(team=allTeams2$team, elo=rep(1000, length(allTeams2$team)))


for (i in 1:nrow(matchinfo)) {
  current <- matchinfo[i,]
  elo <- eloMatchup(
              aTeam=elo[which(as.character(elo$team)==as.character(current$blueTeamTag)),],
              bTeam=elo[which(as.character(elo$team)==as.character(current$redTeamTag)),],
              result=ifelse(current$bResult==1,1,-1),
              elo
         )
}
View(elo)
ggplot(elo, aes(x=elo)) + geom_histogram(bins=20)


#----------------------
# combine superData with elo data
tempElo <- data.frame(address='', bElo=0, rElo=0)
for(i in 1:nrow(matchinfo)) {
  bElo <- elo[which(as.character(elo$team) == as.character(matchinfo[i,]$blueTeamTag)),]$elo
  rElo <- elo[which(as.character(elo$team) == as.character(matchinfo[i,]$redTeamTag)),]$elo
  # tempElo <- rbind(tempElo, data.frame(as.character(matchinfo[i,]$address), bElo, rElo))
  tempElo <- rbind(tempElo, data.frame(address=matchinfo[i,]$address, bElo=bElo, rElo=rElo))
}

superEloData <- merge(superData, tempElo) %>%
  mutate(eloDiff = bElo - rElo)

train_index <- sort(sample(nrow(superEloData), nrow(superEloData)*.8))
train_data <- superEloData[train_index,]
test_data <- superEloData[-train_index,]

eloFit <- glm(bResult ~ min_10+rDragon+bDragon+eloDiff, train_data, family='binomial')  
summary(eloFit)

superEloPredict <- ifelse(predict(eloFit, test_data, type='response')>.5,1,0)
result_table <- table(superEloPredict, test_data$bResult)
(result_table[1,1] + result_table[2,2])/nrow(test_data)

#------------------------
#Adding golddiff by lane to see which role is most important to get gold for
goldByLane <- gold_raw %>%
  select(address=Address, type=Type, min_5, min_10)

#top--------
goldBlueTop <- goldByLane %>%
  filter(type=='goldblueTop') %>%
  select(address, type, bmin_5 = min_5, bmin_10 = min_10)
goldRedTop <- goldByLane %>%
  filter(type=='goldredTop') %>%
  select(address, type, rmin_5 = min_5, rmin_10 = min_10)

goldTop <- merge(goldBlueTop, goldRedTop, by='address') %>%
  mutate(
    min_5_top = bmin_5 - rmin_5,
    min_10_top= bmin_10 - rmin_10
  ) %>%
  select(address, min_5_top, min_10_top)

#mid----------
goldBlueMid <- goldByLane %>%
  filter(type=='goldblueMiddle') %>%
  select(address, type, bmin_5 = min_5, bmin_10 = min_10)
goldRedMid <- goldByLane %>%
  filter(type=='goldredMiddle') %>%
  select(address, type, rmin_5 = min_5, rmin_10 = min_10)

goldMid <- merge(goldBlueMid, goldRedMid, by='address') %>%
  mutate(
    min_5_mid = bmin_5 - rmin_5,
    min_10_mid= bmin_10 - rmin_10
  ) %>%
  select(address, min_5_mid, min_10_mid)

#adc-----------
goldBlueADC <- goldByLane %>%
  filter(type=='goldblueADC') %>%
  select(address, type, bmin_5 = min_5, bmin_10 = min_10)
goldRedADC <- goldByLane %>%
  filter(type=='goldredADC') %>%
  select(address, type, rmin_5 = min_5, rmin_10 = min_10)

goldADC <- merge(goldBlueADC, goldRedADC, by='address') %>%
  mutate(
    min_5_ADC = bmin_5 - rmin_5,
    min_10_ADC= bmin_10 - rmin_10
  ) %>%
  select(address, min_5_ADC, min_10_ADC)

#supp----------
goldBlueSupp <- goldByLane %>%
  filter(type=='goldblueSupport') %>%
  select(address, type, bmin_5 = min_5, bmin_10 = min_10)
goldRedSupp <- goldByLane %>%
  filter(type=='goldredSupport') %>%
  select(address, type, rmin_5 = min_5, rmin_10 = min_10)

goldSupp <- merge(goldBlueSupp, goldRedSupp, by='address') %>%
  mutate(
    min_5_supp = bmin_5 - rmin_5,
    min_10_supp= bmin_10 - rmin_10
  ) %>%
  select(address, min_5_supp, min_10_supp)

#jung-----------
goldBlueJung <- goldByLane %>%
  filter(type=='goldblueJungle') %>%
  select(address, type, bmin_5 = min_5, bmin_10 = min_10)
goldRedJung <- goldByLane %>%
  filter(type=='goldredJungle') %>%
  select(address, type, rmin_5 = min_5, rmin_10 = min_10)

goldJung <- merge(goldBlueJung, goldRedJung, by='address') %>%
  mutate(
    min_5_jung = bmin_5 - rmin_5,
    min_10_jung= bmin_10 - rmin_10
  ) %>%
  select(address, min_5_jung, min_10_jung)

allLanes <- merge(goldTop, merge(goldMid, merge(goldADC, merge(goldSupp, goldJung))))

superMegaData <- merge(superEloData, allLanes)
  
#-----------------------
#analyzing superMegaData (by lanes)
train_index <- sort(sample(nrow(superMegaData), nrow(superMegaData)*.8))
train_data <- superMegaData[train_index,]
test_data <- superMegaData[-train_index,]

megaLogit <- glm(as.factor(bResult) ~ rDragon+bDragon+min_10_top+min_10_mid+
                              min_10_ADC+min_10_supp+min_10_jung+eloDiff,
                 family='binomial',
                 train_data
                 )
summary(megaLogit)  


superEloPredict <- ifelse(predict(megaLogit, test_data, type='response')>.5,1,0)
result_table <- table(superEloPredict, test_data$bResult)
(result_table[1,1] + result_table[2,2])/nrow(test_data)


rf2 <- randomForest(as.factor(bResult)~.-address-type-bElo-rElo, train_data, mtry=5, ntree=1000)
result_table <- table(predict(rf2, test_data), test_data$bResult)
(result_table[1,1] + result_table[2,2])/nrow(test_data)


#-----------------
#STRUCTURES


View(structures_raw)
structures <- structures_raw %>%
  mutate(Team = ifelse(Team=="bTowers", "b", "r")) %>%
  filter(Time <= 10)
  # mutate(struc = strsplit(as.character(Lane), "_")[1]) %>%
  # mutate(struc2 = struc[])
  # mutate(Struc = paste( unlist( strsplit(as.character(Lane), "_" )), 
  #                       "_", 
  #                       unlist(strsplit(as.character(Type),"_")), 
  #                       sep=""))
View(structures)

summary(structures)

strucs <- data.frame(address="", team="", strucs = as.character(""))
for(i in 1:nrow(structures)) {
  s <- data.frame(
                  address = structures$Address[i],
                  team = structures$Team[i],
                  strucs=
                     as.character(paste(
                       as.character(structures$Team[i]), 
                       "_",
                       strsplit(as.character(structures$Lane[i]), "_" )[[1]][1], 
                       "_",
                       strsplit(as.character(structures$Type[i]), "_")[[1]][1],
                     sep=""))
                 )
  strucs <- rbind(strucs, s)
}

struc.mat <- as.data.frame(model.matrix(team~strucs, strucs)[,-1])
struc.fin <- cbind(strucs$address, strucs$team, struc.mat)[-1,]

fin <- struc.fin[1,]
for(i in unique(struc.fin$`strucs$address`)) {
  
  curr <- struc.fin[which(struc.fin$`strucs$address` == i),]
  combined <- struc.fin[1,]
  combined[1,1] <- i
  
  for(col in 3:ncol(curr)) {
    combined[1,col] <- ifelse(sum(as.numeric(curr[,col])) >= 1, 1, 0)
  }
  
  fin <- rbind(fin, combined)
}
fin <- fin[-1,]

notower <- superMegaData$address[which(!(superMegaData$address %in% fin$`strucs$address`))]
View(notower)

length(which(!(fin$`strucs$address` %in% superMegaData$address)))

blank <- fin[1,]
blank$strucsb_BOT_OUTER <- 0
blank2 <- blank

for(add in unique(notower)) {
  blank2$`strucs$address`[1] <- add
  blank <- rbind(blank, blank2)
}

fin.fin <- rbind(fin, blank[-1,]) %>%
  mutate(address = `strucs$address`) %>%
  select(-`strucs$address`, -`strucs$team`) %>%
  mutate_if(is.numeric, as.factor)

superUltraData <- merge(superMegaData, fin.fin)
superUltraData2 <- superUltraData %>%
  select(-type,-min_5,-min_10,-bElo,-rElo,
         -min_5_top,-min_5_mid,-min_5_ADC,-min_5_supp,-min_5_jung,
         -strucsb_BOT_BASE,-strucsb_BOT_FOUNTAIN,-strucsb_TOP_BASE,
         -strucsr_TOP_BASE,-strucsr_BOT_BASE,-strucsr_BOT_INHIBITOR,
         -strucsr_TOP_INHIBITOR) %>%
  mutate(strucsb_BOT_OUTER = as.factor(strucsb_BOT_OUTER))

summary(superUltraData)
summary(superUltraData2)
names(superUltraData2)

#---------------------------
#predicting using structures

superUltraLogit <- glm(bResult~
                         rDragon+bDragon+rHerald+bHerald+eloDiff+
                         min_10_top+min_10_mid+min_10_ADC+min_10_supp+min_10_jung+
                         strucsb_BOT_OUTER+strucsb_TOP_OUTER+strucsb_MID_OUTER+
                         strucsr_BOT_OUTER+strucsr_TOP_OUTER+strucsr_MID_OUTER+
                         strucsb_BOT_INNER+strucsb_TOP_INNER+
                         strucsr_BOT_INNER+strucsr_TOP_INNER,
                  family="binomial",
                  superUltraData2)




summary(superUltraLogit)






