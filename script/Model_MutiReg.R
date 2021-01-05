library(dplyr)
library(Hmisc)
# 1. Read data ####
house <- read.csv("/Users/yunmo/Documents/iii/FinalReport/House/data/FinalMerge/HouseVar.csv", row.names = 1L)

# 2. Check data ####
str(house)

# 2.1 Check na ####
house %>% is.na() %>% sum()

# 2.2 Descrive Statistic ####
# 2.2.1 Fliter Vars ####
HouseRe <- house[house$建物型態 %in% c("住宅大樓", "公寓", "套房", "華廈"), ] 
HouseRe <- HouseRe %>% filter(建物現況格局.房<11, 建物現況格局.衛<10, 車位數<5,
                         總坪數 < 75 & 總坪數 > 10 & 總價元 > 7000000 &
                         總價元 < 30000000 & 單價元坪 < 1000000 &
                         單價元坪 > 150000 & year > 2012) 
# 2.2.2 Plot ####
# par(mfrow=c(1,1))
# plot(HouseRe$Area,main="Area",family="黑體-繁 中黑")
# hist(HouseRe$總價元, main="總價",family="黑體-繁 中黑")
# plot(HouseRe$建物型態, main="建物型態",family="黑體-繁 中黑")
# hist(HouseRe$建物現況格局.廳,main="廳數量",family="黑體-繁 中黑")
# hist(HouseRe$建物現況格局.房,main="房數量",family="黑體-繁 中黑")
# hist(HouseRe$建物現況格局.衛,main="衛數量",family="黑體-繁 中黑")
# plot(HouseRe$建物現況格局.隔間,main="隔間",family="黑體-繁 中黑")
# plot(HouseRe$有無管理組織,main="管理組織",family="黑體-繁 中黑")
# hist(HouseRe$總坪數,main="總坪數",family="黑體-繁 中黑")
# hist(HouseRe$單價元坪,main="單價元坪",family="黑體-繁 中黑")
# hist(HouseRe$車位數,main="車位數",family="黑體-繁 中黑")
# hist(HouseRe$floor,main="樓層",family="黑體-繁 中黑")

# 2.2.3 Select Continuous vars ####
HouseRe$男女比 <- HouseRe$女性人數/HouseRe$男性人數
HouseRe2 <- HouseRe[c("Area", "Address","建物現況格局.廳",
                      "建物現況格局.房","建物現況格局.衛",
                      "車位數","floor", "ParkCount","GasCount",
                      "govCount", "hospitalCount", "martCount",
                      "firewayCount","每戶人數","所得總額","單價元坪")]
HouseCon <- HouseRe[c("建物現況格局.廳","建物現況格局.房","建物現況格局.衛",
                    "車位數","floor", "ParkCount","GasCount",
                    "govCount", "hospitalCount", "martCount",
                    "firewayCount","每戶人數","所得總額","單價元坪")]
                    # delect variables below
                    # "EightCount","FuneralCount","PoliceCount",
                    # "總坪數","總價元",
                    # "subwayCount","busCount", "CrimeCount", 
                    # "clinicCount","pharmacyCount","fireareaCount",
                    # "mallCount","cinemaCount","總人口數",
                    # "男性人數","女性人數","男女比","土地面積",
                    # "每戶成年人數",
                    # "所得收入總計","可支配所得","消費支出",
                    # "人口密度","儲蓄"
# # HouseCon[1:13] <- lapply(HouseCon[1:13], as.numeric)
str(HouseCon)
summary(HouseCon)

# 3. Modeling ####

## 3.1 Cor ####
cor_ <- cor(HouseCon, method = 'pearson') %>% round(3)
View(cor_)

# 3.2 Normalization ####
# HouseConSca <- scale(HouseCon) %>% as.data.frame()
# HouseConNor <- normalize(HouseCon) %>% as.data.frame()
HouseConSca <- HouseCon %>% mutate_at(scale, .vars = vars(-單價元坪))

## 3.3 Split data ####
# train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(HouseConSca),
                      size=ceiling(0.8*nrow(HouseCon)))
#ceiling():無條件進位
train <- HouseConSca[train.index, ]
test <- HouseConSca[-train.index, ]

## 3.2 Mutiple Regression ####
mOri <- lm(單價元坪 ~ ., data = train)
summary(mOri)
par(mfrow=c(2,2))
plot(mOri)

## 3.3 Check Multicollinearity ####
# library(car)
vif(mOri) #VIF > 10 --> remove

# 3.4 Make predictions ####
distPred <- predict(mOri, test)  # predict distance
actuals_preds <- data.frame(cbind(actuals=test$dist, predicteds=distPred))  
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy

# 3.5 Final model
# library(dataPreparation)
mFinal <- lm(單價元坪 ~ ., data = HouseConSca)
summary(mFinal)

# 3.6 Predict     
HouseRe2$pred <- fitted(mFinal)
HouseRe2$漲跌 <- HouseRe2$pred-HouseRe2$單價元坪

# 4. Summarize
updown <- HouseRe2 %>% group_by(Area) %>% summarise(count = n(),
                                          avg_單價元坪 = mean(單價元坪), 
                                          avg_漲跌 = mean(漲跌)) 
updown$pre_單價元坪 <- updown$avg_單價元坪 + updown$avg_漲跌

# options(scipen=999)
