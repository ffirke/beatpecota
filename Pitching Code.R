# Marcel
# ZiPS
# Steamer
# PECOTA
# wOBA or OPS
# tAV
# Predicting: Push, High, or Low?
# Age
# Team
# Position
# ABs

rm(list=ls())

wkdir <- "C:/Users/Frank/Documents/Blog/Baseball/BeatPecota/"

files <- list.files(wkdir)

pecota <- files[substr(files,1,3)=="pec"]

require(readr)
require(readxl)

sink("blah.txt")

pecota13hitters <- read_excel(paste0(wkdir,pecota[1]),sheet=2)
pecota14hitters <- read_excel(paste0(wkdir,pecota[2]),sheet=2)
pecota15hitters <- read_excel(paste0(wkdir,pecota[3]),sheet=2)

pecota13pitchers <- read_excel(paste0(wkdir,pecota[1]),sheet=3)
pecota14pitchers <- read_excel(paste0(wkdir,pecota[2]),sheet=3)
pecota15pitchers <- read_excel(paste0(wkdir,pecota[3]),sheet=3,skip=5)
pecota16pitchers <- read_excel(paste0(wkdir,pecota[4]),sheet=3)

sink()

require(dplyr)
require(magrittr)

pitchers14 <- pecota14pitchers %>% select(BPID,LASTNAME,FIRSTNAME,THROWS,HEIGHT,WEIGHT,TEAM,LG,AGE,BB9,SO9,GB_PERCENT,
                            BABIP,ERA,BREAKOUT,IMPROVE,COLLAPSE,ATTRITION,ROOKIE_FL,DC_FL,IP,YEAR,MLBCODE) %>%
  filter(!is.na(BPID))

pitchers15 <- pecota15pitchers %>% select(BPID,LASTNAME,FIRSTNAME,THROWS,HEIGHT,WEIGHT,TEAM,LG,AGE,BB9,SO9,GB_PERCENT,
                                          BABIP,ERA,BREAKOUT,IMPROVE,COLLAPSE,ATTRITION,ROOKIE_FL,DC_FL,IP,YEAR,MLBCODE) %>%
  filter(!is.na(BPID))

pitchers16 <- pecota16pitchers %>% select(BPID,LASTNAME,FIRSTNAME,THROWS,HEIGHT,WEIGHT,TEAM,LG,AGE,BB9,SO9,GB_PERCENT,
                                          BABIP,ERA,BREAKOUT,IMPROVE,COLLAPSE,ATTRITION,ROOKIE_FL,DC_FL,IP,YEAR,MLBCODE) %>%
  filter(!is.na(BPID))

pitchersproj <- rbind_list(pitchers14,pitchers15,pitchers16)

perf <- files[substr(files,1,3)=="bps"]

for (i in 1:3){
  temp <- read_csv(paste0(wkdir,perf[i]))
  temp <- temp %>% select(NAME,YEAR,ERA,DRA,IP)
  if (i == 1){
    perfstack <- temp
  } else {
    perfstack <- rbind_list(perfstack,temp)
  }
}

perfstack %<>% rename(NAMEFULL=NAME,ERA_ACTUAL=ERA,DRA_ACTUAL=DRA,IP_ACTUAL=IP)

for (i in 1:length(perf)){
  temp <- read_csv(paste0(wkdir,perf[i]))
  temp <- temp %>% select(NAME,YEAR,ERA,DRA,IP,cFIP)
  if (i == 1){
    lagperfstack <- temp
  } else {
    lagperfstack <- rbind_list(lagperfstack,temp)
  }
}

pitchersproj %>% mutate(NAMEFULL = paste0(FIRSTNAME," ",LASTNAME)) %>% filter(DC_FL=="T") %>%
  left_join(perfstack,by=c("NAMEFULL","YEAR")) -> pitcherout

pitcherout %<>% left_join(lagperfstack %>% mutate(YEAR=YEAR+1) %>% 
                           rename(NAMEFULL=NAME,ERA_LAG1=ERA,DRA_LAG1=DRA,IP_LAG1=IP,cFIP_LAG1=cFIP),
                         by=c("NAMEFULL","YEAR"))

pitcherout %<>% left_join(lagperfstack %>% mutate(YEAR=YEAR+2) %>% 
                            rename(NAMEFULL=NAME,ERA_LAG2=ERA,DRA_LAG2=DRA,IP_LAG2=IP,cFIP_LAG2=cFIP),
                          by=c("NAMEFULL","YEAR"))

pitcherout %<>% left_join(lagperfstack %>% mutate(YEAR=YEAR+3) %>% 
                            rename(NAMEFULL=NAME,ERA_LAG3=ERA,DRA_LAG3=DRA,IP_LAG3=IP,cFIP_LAG3=cFIP),
                          by=c("NAMEFULL","YEAR"))

pitcherout %<>% left_join(lagperfstack %>% mutate(YEAR=YEAR+4) %>% 
                            rename(NAMEFULL=NAME,ERA_LAG4=ERA,DRA_LAG4=DRA,IP_LAG4=IP,cFIP_LAG4=cFIP),
                          by=c("NAMEFULL","YEAR"))

pitcherout %<>% group_by(NAMEFULL,YEAR) %>% mutate(ct=n()) %>% filter(ct==1) %>% ungroup()

pecota13pitchers %>% select(BPID,YEAR,FAIR_RA)  -> proj13
pecota14pitchers %>% select(BPID,YEAR,FAIR_RA) -> proj14
pecota15pitchers %>% select(BPID,YEAR,FAIR_RA) -> proj15

lagproj <- bind_rows(proj13,proj14,proj15) %>% mutate(YEAR=YEAR+1) %>% rename(LAG_PROJ = FAIR_RA)

pitcherout %<>% left_join(lagproj,by=c("YEAR","BPID"))
pitcherout %>% filter(IP >= 50 & (! is.na(ERA_ACTUAL) | YEAR==2016)) %>% 
  mutate(ProjResult = as.factor(ifelse(YEAR!=2016,ifelse(IP_ACTUAL <= 20,"Push",
                             ifelse(ERA_ACTUAL - ERA >= 0.3,"Over",
                             ifelse(ERA_ACTUAL - ERA <= -0.3,"Under",
                                    "Push")) ),NA)),
         LagProjResult = as.factor(ifelse(is.na(LAG_PROJ) | is.na(IP_LAG1) | IP_LAG1 <= 20,"Push",
                                       ifelse(ERA_LAG1 - LAG_PROJ >= 0.3,"Over",
                                              ifelse(ERA_LAG1 - LAG_PROJ <= -0.3,"Under",
                                                     "Push")) )))  -> eligiblep

lagperfstack %>% group_by(YEAR) %>% summarize(AvgcFIP = weighted.mean(cFIP,IP),
                                              AvgERA = weighted.mean(ERA,IP),
                                              AvgDRA = weighted.mean(DRA,IP)) %>% ungroup() -> yearmeans


meanfill<-function(lag){
  
  tokeep <- names(eligiblep)
  
  yearmeans %>% mutate(YEAR = YEAR + lag) -> temp 
  joined <- eligiblep %>% left_join(temp,by="YEAR")
  
  joined[,paste0("DRA_LAG",lag)] <- ifelse(is.na(joined[[paste0("DRA_LAG",lag)]]),joined$AvgDRA,
                                           joined[[paste0("DRA_LAG",lag)]])
  
  joined[,paste0("cFIP_LAG",lag)] <- ifelse(is.na(joined[[paste0("cFIP_LAG",lag)]]),joined$AvgcFIP,
                                           joined[[paste0("cFIP_LAG",lag)]])
  
  joined[,paste0("ERA_LAG",lag)] <- ifelse(is.na(joined[[paste0("ERA_LAG",lag)]]),joined$AvgERA,
                                           joined[[paste0("ERA_LAG",lag)]])
  
  joined[,tokeep]
}

zerofill<-function(x){
  x[is.na(x)] = 0 #convert the item with NA to median value from the column
  x #display the column
}


zerofillcol <- c("IP_LAG1","IP_LAG2","IP_LAG3","IP_LAG4")

for (k in 1:4) {
  print(k)
  eligiblep <- meanfill(k)
}

for (k in zerofillcol) {
  eligiblep[,k] <- zerofill(eligiblep[,k])
}

zips14p <- read_csv(paste0(wkdir,"zips_pitchers_2014.csv")) %>% select(season,Name,ERA,FIP,`K/9`,`BB/9`,playerid)

zips15p <- read_csv(paste0(wkdir,"zips_pitchers_2015.csv")) %>% select(season,Name,ERA,FIP,`K/9`,`BB/9`,playerid) 

zips16p <- read_csv(paste0(wkdir,"zips_pitchers_2016.csv")) 
names(zips16p)[1] <- "Name"
zips16p %<>% select(Name,ERA,FIP,`K/9`,`BB/9`,playerid) %>% mutate(season=2016)

zipsp <- rbind_list(zips14p,zips15p,zips16p) %>%
  rename(zERA = ERA,zFIP = FIP,zK9 = `K/9`,zBB9 = `BB/9`,FGid=playerid) %>% 
  mutate(Name = ifelse(Name == "Hyun-Jin Ryu","Hyun-jin Ryu",gsub("de la Rosa","De La Rosa",Name)))

eligiblep %<>% inner_join(zipsp,by=c("NAMEFULL"="Name","YEAR"="season"))

steamer14p <- read_csv(paste0(wkdir,"steamer_pitchers_2014.csv")) %>% 
  select(season,Name,ERA,FIP,`K/9`,`BB/9`,playerid,season) %>% 
  rename(sK9 = `K/9`,sBB9 = `BB/9`,combid=playerid) %>%
  mutate(combid = as.numeric(combid))

steamer15p <- read_csv(paste0(wkdir,"steamer_pitchers_2015.csv")) %>% 
  select(firstname,lastname,ERA,FIP,K9,BB9,mlbamid) %>% rename(sK9=K9,sBB9=BB9,combid=mlbamid) %>%
  mutate(season=2015,Name = paste0(firstname," ",lastname)) %>% select(-firstname,-lastname)

steamer16p <- read_csv(paste0(wkdir,"steamer_pitchers_2016.csv")) 
names(steamer16p)[1] <- "Name"
steamer16p %<>% select(Name,ERA,FIP,`K/9`,`BB/9`,playerid) %>% mutate(season=2016) %>%
  rename(sK9 = `K/9`,sBB9 = `BB/9`,combid=playerid) %>%
  mutate(combid = as.numeric(combid))

steamerp <- rbind_list(steamer14p,steamer15p,steamer16p) %>% rename(sERA = ERA,sFIP = FIP) %>%
  filter(Name != "Carlos Martinez" | !is.na(combid)) %>% 
  filter(Name != "Jose Fernandez" | combid != '622774') %>% 
  filter(Name != "Jose Fernandez" | combid != '622774') %>%
  mutate(Name = ifelse(Name == "Hyun-Jin Ryu","Hyun-jin Ryu",gsub("de la Rosa","De La Rosa",Name)))

eligiblep %<>% 
  inner_join(steamerp,by=c("NAMEFULL"="Name","YEAR"="season")) %>% filter(!(combid == 6688 & YEAR==2016)) %>%
  filter(!(BPID == 60812 & YEAR ==20016))

eligiblep %<>% mutate(sERADiff = ERA - sERA,zERADiff = ERA-zERA)

require(caret)
require(randomForest)

AllCurrentBP <- ProjResult ~ THROWS + HEIGHT + WEIGHT + TEAM + AGE + LAG_PROJ + LagProjResult +
  BB9 + SO9 + GB_PERCENT + BABIP + ERA + BREAKOUT + IMPROVE + COLLAPSE + ATTRITION + ROOKIE_FL + IP

CurrentBPNoTeam <- ProjResult ~ THROWS + HEIGHT + WEIGHT +  AGE + LAG_PROJ + LagProjResult +
  BB9 + SO9 + GB_PERCENT + BABIP + ERA + BREAKOUT + IMPROVE + COLLAPSE + ATTRITION + ROOKIE_FL + IP

BPLag2 <- ProjResult ~ THROWS + HEIGHT + WEIGHT +  AGE +LAG_PROJ + LagProjResult + 
  IP_LAG1 + ERA_LAG1 + DRA_LAG1 + cFIP_LAG1 +
  IP_LAG2 + ERA_LAG2 + DRA_LAG2 + cFIP_LAG2 +
  BB9 + SO9 + GB_PERCENT + BABIP + ERA + BREAKOUT + IMPROVE + COLLAPSE + ATTRITION + ROOKIE_FL + IP

NoTeamSZSmall <- ProjResult ~ THROWS + HEIGHT + WEIGHT +  AGE + LAG_PROJ + LagProjResult +
  BB9 + SO9 + GB_PERCENT + BABIP + ERA + BREAKOUT + IMPROVE + COLLAPSE + ATTRITION + ROOKIE_FL + IP +
  zERA + sERA

NoTeamSZSmallDiff <- ProjResult ~ THROWS + HEIGHT + WEIGHT +  AGE + LAG_PROJ + LagProjResult +
  BB9 + SO9 + GB_PERCENT + BABIP + ERA + BREAKOUT + IMPROVE + COLLAPSE + ATTRITION + ROOKIE_FL + IP +
  zERADiff + sERADiff

All <- ProjResult ~ .

formulalist<- list()
formulalist[["BPLag2"]] <- BPLag2
# formulalist[["All"]] <- All
formulalist[["AllCurrentBP"]] <- AllCurrentBP
formulalist[["CurrentBPNoTeam"]] <- CurrentBPNoTeam
formulalist[["NoTeamSZSmall"]] <- CurrentBPNoTeam
formulalist[["NoTeamSZSmallDiff"]] <- CurrentBPNoTeam
names(formulalist)
modellist <- list(length(formulalist))

set.seed(100)

eligiblep %<>% mutate(LAG_PROJ = ifelse(is.na(LAG_PROJ),ERA,LAG_PROJ))

eligiblep %>% filter(YEAR != 2016) -> pre16

inTrain <- createDataPartition(y = pre16$ProjResult,p = .7,list = FALSE)

training1<-pre16[inTrain,]

ctrl <- trainControl(method = "repeatedcv",repeats = 2)

noTrain = pre16[setdiff(1:nrow(pre16),inTrain),]
valid <- createDataPartition(y = noTrain$ProjResult,p = .7,list = FALSE)
validData <- noTrain[valid,]

fit = T

for (i in 1:length(formulalist)) {
  print(names(formulalist)[i])
  a= Sys.time()
  
  if (fit) {
  
    rf_model<-train(formulalist[[i]],data=training1,method="rf",
                    trControl=ctrl,
                    prox=TRUE,allowParallel=TRUE)  
    modellist[[names(formulalist)[i]]] <- rf_model
  }
  
  tmodel <- modellist[[names(formulalist)[i]]]
  
  res_valid <- predict.train(tmodel,newdata=validData)
  prob_valid <- predict.train(tmodel,newdata=validData,type="prob")
  validData %>% cbind(prob_valid) %>% cbind(res_valid) %>% 
    mutate(gameresult = ifelse(ProjResult == "Push",0,
                               ifelse(res_valid==ProjResult,10,-11.5))) -> withresults
  
  withresults %>% filter(Over > 0.5 | Under > 0.5) %>%
    summarize(result = sum(gameresult),ct=n()) -> out1
  
  withresults %>% filter(res_valid != 'Push') %>% 
    summarize(result = sum(gameresult),ct=n()) -> out2
  
  withresults %>% mutate(EV = ifelse(Over > Under,10*Over - 11.5*Under,10*Under-11.5*Over)) %>%
    filter(EV > 0) %>%
    summarize(result = sum(gameresult),ct=n()) -> out3
  
  withresults %>% mutate(EV = ifelse(Over > Under,10*Over - 11.5*Under,10*Under-11.5*Over)) %>%
    filter(EV > 0 & res_valid != 'Push') %>%
    summarize(result = sum(gameresult),ct=n()) -> out4
  
  temp <- data_frame(Model = names(formulalist)[i],
                         likelySum = out1$result[1],likelyCt = out1$ct[1],
                         predSum = out2$result[1],predCt = out2$ct[1],
                     evPlusSum = out3$result[1],evPlusCt = out3$ct[1],
                     evPlusNoPushSum = out4$result[1],evPlusNoPushCt = out4$ct[1])
  
  if (i == 1){
    metricdf <- temp
  } else {
    metricdf <- rbind_list(metricdf,temp)
  }
  print(Sys.time()-a)
}

# eligiblep %<>% mutate(include=runif(nrow(eligiblep)),rk = cume_dist(include),
#                       type = ifelse(rk <= 0.2,"Test",ifelse(rk<=0.4,"Validate","Train")))


for (z in names(modellist)) {
  print(z)
  print(modellist[[z]])
  print(modellist[[z]]$finalModel)
}

print(rf_model$finalModel)

library(doParallel)
require(doRNG)

cl <- makeCluster(3)
registerDoParallel(cl)

set.seed(200)

a = Sys.time()

data04 <- foreach(n = 1:(length(formulalist)),.combine=rbind) %dorng% {
  library(dplyr)
  library(magrittr)
  library(caret)
  tmodel <- modellist[[names(formulalist)[n]]]
  stable = validData

  for (z in 1:500) {
    
    validData = stable %>% sample_frac(replace=T)
  
    res_valid <- predict.train(tmodel,newdata=validData)
    prob_valid <- predict.train(tmodel,newdata=validData,type="prob")
    validData %>% cbind(prob_valid) %>% cbind(res_valid) %>% 
      mutate(gameresult = ifelse(ProjResult == "Push",0,
                                 ifelse(res_valid==ProjResult,10,-11.5))) -> withresults
    
    withresults %>% filter(Over > 0.5 | Under > 0.5) %>%
      summarize(result = sum(gameresult),ct=n()) -> out1
    
    withresults %>% filter(res_valid != 'Push') %>% 
      summarize(result = sum(gameresult),ct=n()) -> out2
    
    withresults %>% mutate(EV = ifelse(Over > Under,10*Over - 11.5*Under,10*Under-11.5*Over)) %>%
      filter(EV > 0) %>%
      summarize(result = sum(gameresult),ct=n()) -> out3
    
    withresults %>% mutate(EV = ifelse(Over > Under,10*Over - 11.5*Under,10*Under-11.5*Over)) %>%
      filter(EV > 0 & res_valid != 'Push') %>%
      summarize(result = sum(gameresult),ct=n()) -> out4
    
    temp <- data_frame(Model = names(formulalist)[n],
                       likelySum = out1$result[1],likelyCt = out1$ct[1],
                       predSum = out2$result[1],predCt = out2$ct[1],
                       evPlusSum = out3$result[1],evPlusCt = out3$ct[1],
                       evPlusNoPushSum = out4$result[1],evPlusNoPushCt = out4$ct[1],
                       iter = z)
    
    if (z == 1) {
      results <- temp
    } else {
      results <- bind_rows(results,temp)
    }

  }
  results
}

Sys.time() - a

require(ggplot2)

data04 %>% ggplot(aes(x=evPlusNoPushSum,color=Model,group=Model)) + geom_density()
data04 %>% ggplot(aes(x=evPlusSum,color=Model,group=Model)) + geom_density()

final_model<-train(formulalist[["NoTeamSZSmallDiff"]],data=pre16,method="rf",
                trControl=ctrl,
                prox=TRUE,allowParallel=TRUE)

eligiblep %>% filter(YEAR == 2016) -> pitchers16

res_valid <- predict.train(final_model,newdata=pitchers16)
prob_valid <- predict.train(final_model,newdata=pitchers16,type="prob")
pitchers16 %>% cbind(prob_valid) %>% cbind(res_valid) -> results

results %>% filter(res_valid != 'Push') %>% arrange(res_valid,LASTNAME) %>%
  select(BPID,res_valid,NAMEFULL) -> output

output %>% filter(res_valid == "Over") -> shellfeed

for (i in 104:nrow(shellfeed)) {
  shell.exec(paste0(
    "http://www.baseballprospectus.com/fantasy/extras/beatpecota/beatpecota.php?PO3=",
    shellfeed$BPID[i]))
  Sys.sleep(20)
}

shell.exec("")

asd <- c("THROWS","HEIGHT","WEIGHT","AGE" ,"LAG_PROJ" ,"LagProjResult","BB9" ,"SO9" ,"GB_PERCENT" ,"BABIP",
  "ERA","BREAKOUT","IMPROVE","COLLAPSE","ATTRITION","ROOKIE_FL","IP","zERADiff","sERADiff")

for (k in asd) {
  print(paste(k,sum(is.na(pitchers16[,k]))))
}

output %>% filter(res_valid == "Under") -> shellfeed2

shellfeed2$picked <- FALSE
shellfeed2$picked[1:2] <- TRUE

i <- i + 20
n <- 19
while (n < 25 & i <= nrow(shellfeed2)){
  if (shellfeed2$picked[i] == F) { 
    shell.exec(paste0(
    "http://www.baseballprospectus.com/fantasy/extras/beatpecota/beatpecota.php?PU3=",
    shellfeed2$BPID[i]))
    Sys.sleep(10)
    n <- n + 1
  }
  i <- i + 1
  print (paste(i,n))
}