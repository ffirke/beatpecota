wkdir <- "C:/Users/Frank/Documents/Blog/Baseball/BeatPecota/"

files <- list.files(wkdir)

pecota <- files[substr(files,1,3)=="pec"]

require(readr)
require(readxl)

sink("blah.txt")

pecota13hitters <- read_excel(paste0(wkdir,pecota[1]),sheet=2)
pecota14hitters <- read_excel(paste0(wkdir,pecota[2]),sheet=2)
pecota15hitters <- read_excel(paste0(wkdir,pecota[3]),sheet=2,skip=5)
pecota16hitters <- read_excel(paste0(wkdir,pecota[4]),sheet=2)

sink()

require(dplyr)
require(magrittr)

hitters14 <- pecota14hitters %>% 
  select(BPID,LASTNAME,FIRSTNAME,POS,BATS,HEIGHT,WEIGHT,LG,AGE,HR,BB,SO,AVG,OBP,SLG,TAv,BABIP,
        BREAKOUT,IMPROVE,COLLAPSE,ATTRITION,ROOKIE_FL,DC_FL,PA,YEAR,MLBCODE)

hitters15 <- pecota15hitters %>% 
  select(BPID,LASTNAME,FIRSTNAME,POS,BATS,HEIGHT,WEIGHT,LG,AGE,HR,BB,SO,AVG,OBP,SLG,TAv,BABIP,
         BREAKOUT,IMPROVE,COLLAPSE,ATTRITION,ROOKIE_FL,DC_FL,PA,YEAR,MLBCODE)

hitters16 <- pecota16hitters %>% 
  select(BPID,LASTNAME,FIRSTNAME,POS,BATS,HEIGHT,WEIGHT,LG,AGE,HR,BB,SO,AVG,OBP,SLG,TAv,BABIP,
         BREAKOUT,IMPROVE,COLLAPSE,ATTRITION,ROOKIE_FL,DC_FL,PA,YEAR,MLBCODE)

hittersproj <- rbind_list(hitters14,hitters15,hitters16)

perf <- files[substr(files,1,3)=="tav"]

for (i in 1:3){
  temp <- read_csv(paste0(wkdir,perf[i]))
  temp <- temp %>% select(NAME,YEAR,TAv,PA)
  if (i == 1){
    perfstack <- temp
  } else {
    perfstack <- rbind_list(perfstack,temp)
  }
}

perfstack %<>% rename(NAMEFULL=NAME,TAv_ACTUAL=TAv,PA_ACTUAL=PA)

for (i in 1:length(perf)){
  temp <- read_csv(paste0(wkdir,perf[i]))
  temp <- temp %>% select(NAME,YEAR,TAv,PA)
  if (i == 1){
    lagperfstack <- temp
  } else {
    lagperfstack <- rbind_list(lagperfstack,temp)
  }
}



hittersproj %>% mutate(NAMEFULL = paste0(FIRSTNAME," ",LASTNAME)) %>% filter(DC_FL=="T") %>%
  left_join(perfstack,by=c("NAMEFULL","YEAR")) -> hitterout

hitterout %<>% left_join(lagperfstack %>% mutate(YEAR=YEAR+1) %>% 
                           rename(NAMEFULL=NAME,TAv_LAG1=TAv,PA_LAG1 = PA),
                         by=c("NAMEFULL","YEAR"))

hitterout %<>% group_by(NAMEFULL,YEAR) %>% mutate(ct=n()) %>% filter(ct==1) %>% ungroup()

pecota13hitters %>% select(BPID,YEAR,TAv)  -> proj13
pecota14hitters %>% select(BPID,YEAR,TAv) -> proj14
pecota15hitters %>% select(BPID,YEAR,TAv) -> proj15

lagproj <- bind_rows(proj13,proj14,proj15) %>% mutate(YEAR=YEAR+1) %>% rename(LAG_PROJ = TAv)

hitterout %<>% left_join(lagproj,by=c("YEAR","BPID"))
hitterout %>% filter(PA >= 251) %>%   mutate(ProjResult = 
           as.factor(ifelse(YEAR!=2016 & !(is.na(PA_ACTUAL)),
                            ifelse(PA_ACTUAL <= 80,"Push",
                                   ifelse(TAv_ACTUAL - TAv >= 0.007,"Over",
                                          ifelse(TAv_ACTUAL - TAv <= -0.007,"Under",
                                                                       "Push")) ),
                            ifelse(YEAR=='2016',NA,"Push"))),
         LagProjResult = as.factor(ifelse(is.na(LAG_PROJ) | is.na(PA_LAG1) | PA_LAG1 <= 20,"Push",
                                          ifelse(TAv_LAG1 - LAG_PROJ >= 0.007,"Over",
                                                 ifelse(TAv_LAG1 - LAG_PROJ <= -0.007,"Under",
                                                        "Push")) )))  -> eligibleh

lagperfstack %>% group_by(YEAR) %>% 
  summarize(AvgTAv = weighted.mean(TAv,PA)) %>% ungroup() -> yearmeans

yearmeans %>% mutate(YEAR = YEAR + 1) -> temp 
eligibleh <- eligibleh %>% left_join(temp,by="YEAR") %>% 
  mutate(TAv_LAG1 = ifelse(is.na(TAv_LAG1),AvgTAv,
                           TAv_LAG1),
         PA_LAG1 = ifelse(is.na(PA_LAG1),0,
                          PA_LAG1))

eligibleh %<>% mutate(LAG_PROJ = ifelse(is.na(LAG_PROJ),TAv,LAG_PROJ))

eligibleh %>% filter(YEAR != 2016) -> pre16

inTrain <- createDataPartition(y = pre16$ProjResult,p = .7,list = FALSE)

training1<-pre16[inTrain,]

ctrl <- trainControl(method = "repeatedcv",repeats = 2)

validData = pre16[setdiff(1:nrow(pre16),inTrain),]

BasicForm <- ProjResult ~ POS + BATS + HEIGHT + WEIGHT + LG + AGE + HR + BB + SO +
  AVG + OBP + SLG + TAv + BABIP + BREAKOUT + IMPROVE + COLLAPSE + ATTRITION + ROOKIE_FL + PA +
  LAG_PROJ + LagProjResult + TAv_LAG1 + PA_LAG1

rf_model<-train(BasicForm,data=training1,method="rf",
                trControl=ctrl,
                prox=TRUE,allowParallel=TRUE) 

tmodel <- rf_model

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

final_modelh<-train(BasicForm,data=pre16,method="rf",
                   trControl=ctrl,
                   prox=TRUE,allowParallel=TRUE)

eligibleh %>% filter(YEAR == 2016) %>% mutate(POS=ifelse(POS=="DH","1B",POS)) -> hitterspred

res_valid <- predict.train(final_modelh,newdata=hitterspred)
prob_valid <- predict.train(final_modelh,newdata=hitterspred,type="prob")
hitterspred %>% cbind(prob_valid) %>% cbind(res_valid) -> resultsh

resultsh %>% filter(res_valid != 'Push') %>% arrange(res_valid,LASTNAME) %>%
  select(BPID,res_valid,NAMEFULL) -> output_h

output_h %>% filter(res_valid == "Over") -> shellfeed

shellfeed$picked <- FALSE
shellfeed$picked[1] <- T

i <- 60
n <- 0
while (n < 30 & i <= nrow(shellfeed)){
  if (shellfeed$picked[i] == F) { 
    shell.exec(paste0(
      "http://www.baseballprospectus.com/fantasy/extras/beatpecota/beatpecota.php?BO3=",
      shellfeed$BPID[i]))
    Sys.sleep(10)
    n <- n + 1
    shellfeed$picked[i] == T
  }
  i <- i + 1
  print (paste(i,n))
}

  output_h %>% filter(res_valid == "Under") -> shellfeed
  
  shellfeed$picked <- FALSE
  shellfeed$picked[1] <- T
  
  i <- 60
  n <- 0
  while (n < 50 & i <= nrow(shellfeed)){
    if (shellfeed$picked[i] == F) { 
      shell.exec(paste0(
        "http://www.baseballprospectus.com/fantasy/extras/beatpecota/beatpecota.php?BU3=",
        shellfeed$BPID[i]))
      Sys.sleep(10)
      n <- n + 1
      shellfeed$picked[i] == T
      print (paste(i,n,shellfeed$NAMEFULL[i]))
    }
    i <- i + 1
    
  }
  
  
  output_h %>% filter(res_valid == "Over") -> blah
  
for (i in 1:5) {
      shell.exec(paste0(
        "http://www.baseballprospectus.com/fantasy/extras/beatpecota/beatpecota.php?BO3=",
        blah$BPID[i+54]))
      Sys.sleep(10)
    }