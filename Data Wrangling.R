## Data Wrangling file 
# Paul Harmon
# March 2, 2022



#### Info on Script ####
## combines a bunch of the files into a single training dataset that we can use to assess the model. 

library(tibble)
library(readr)
library(ggplot2)
library(dplyr)

#### Read in data 

mteams <- read_csv("Data/MTeams.csv")
mres <- read_csv("Data/MRegularSeasonCompactResults.csv")
mresfull <- read_csv("Data/MRegularSeasonDetailedResults.csv")


#### Filter: 

#filters to only the last 5 seasons (2018:2022)
mr20 <- mresfull %>% filter(Season %in% c(2018:2022))


#adds team information
mr20_full <- left_join(mr20, mteams, by = c("WTeamID" = "TeamID"))
findat <- left_join(mr20_full, mteams, by = c("LTeamID" = "TeamID"))



## Look at a single university: 

CSU <- findat %>% dplyr::filter(TeamName.x %in% c("Colorado St") | TeamName.y %in% c("Colorado St"))

table(CSU$Season)


#### If we care about overall quality of a team - and not the comparison between winning and losing - we'd need to duplicate the dataset

names(findat)

#winning teams
windat <- findat %>% select(TeamName.x, Season, NumOT, starts_with("W"))
windat <- windat %>% select(-WLoc)

#losing teams
lossdat <- findat %>% select(TeamName.y, Season, NumOT, starts_with("L"))
lossdat <- lossdat %>% select(-c("LastD1Season.x", "LastD1Season.y"))

names(windat) <- names(lossdat) <- c("TeamName", "Season", "NumOT", "TeamID", "Score", 
                                     "FGM", "FGA", "FGM3", "FGA3", "WFTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "Pf")

final_data <- bind_rows(windat, lossdat)
final_data$Won <- factor(rep(c(1,0), each = nrow(windat)))

### Engineer some features
final_data <- final_data %>% mutate(FGPCT = FGM/FGA, FG3PCT = FGM3/FGA3, FTP = WFTM/FTA)

write.csv(final_data, "NCAABasketballResults.csv")

test_data <- final_data %>% filter(Season >= 2022)
write.csv(test_data, "NCAABasketballTest22.csv")

### Test a GLM

out1 <- glm(Won ~ Season + FGPCT + FG3PCT + FTP + OR + DR + Ast + TO + Stl + Blk + Pf, 
            data = final_data, family = binomial(link = "logit"))


### Generate some Effects Plots
library(effects)
plot(allEffects(out1))

## Pseudo R-squared
library(DescTools)
PseudoR2(out1, which = "Nagelkerke")
PseudoR2(out1, which = "McFaddenAdj")















