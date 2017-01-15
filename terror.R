library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(htmltools)
library(reshape)
library(caret)
library(randomForest)
library(leaflet)

# Read the file
data <- read.csv("globalterrorismdb_0616dist.csv")

# Remove incidents where there is doubt as to whether the incident is an act of terrorism.
#data1 <- data[data$doubtterr == 0,]

# Cleaning data file:
data1 <- subset(data, select = -c(approxdate,resolution,country,region,specificity,vicinity,location,summary,
                                  alternative,alternative_txt,attacktype1,attacktype2,attacktype3,attacktype2_txt,
                                  attacktype3_txt,targtype1,targsubtype1,target1,natlty1,targtype2,targtype2_txt,targsubtype2,
                                  targsubtype2_txt,corp2,target2,natlty2,natlty2_txt,targtype3,targtype3_txt,targsubtype3,
                                  corp3,target3,natlty3,natlty3_txt,gsubname,gname2,gsubname2,gname3,ingroup2,ingroup3,gsubname3,
                                  motive,guncertain2,guncertain3,claimed,claimmode,claimmode_txt,claimmode2,claimmode2_txt,
                                  claimmode3,claimmode3_txt,compclaim,weaptype1,weapsubtype1,weaptype2,weapsubtype2,weaptype2_txt,
                                  weaptype3,weapsubtype3,weaptype3_txt,weapsubtype2_txt,weapsubtype3_txt,weaptype4,weapsubtype4,weaptype4_txt,
                                  weapsubtype4_txt,provstate, doubtterr,targsubtype1,targsubtype3_txt,claim2,claim3,targsubtype1_txt,
                                  corp1,ingroup,guncertain1,weapsubtype1_txt,weapdetail,propcomment,propextent,ransomnote,addnotes,
                                  scite2,scite1,scite3,dbsource,INT_LOG,INT_IDEO,INT_MISC,INT_ANY,related))

# round of nkill 
data1$nkill <- round(data1$nkill, digits = 0)  

# Convert month, day and year into dates
data1$date <- paste(data1$iyear,"-",data1$imonth,"-",data1$iday, sep="")
data1$date <- as.Date(data1$date, format = "%Y-%m-%d")
data1 <- subset(data1, select = -c(imonth,iday))

# change the column names to more userfriendly
colnames(data1) <- gsub("country_txt","country", colnames(data1))
colnames(data1) <- gsub("region_txt","region", colnames(data1))
colnames(data1) <- gsub("attacktype1_txt","attack_type", colnames(data1))
colnames(data1) <- gsub("targtype1_txt","target_type", colnames(data1))
colnames(data1) <- gsub("natlty1_txt","target_nationality", colnames(data1))
colnames(data1) <- gsub("gname","group_name", colnames(data1))
colnames(data1) <- gsub("weaptype1_txt","weapon_type", colnames(data1))
is.na(data1$propvalue) = data1$propvalue < 0

# remove row no 73488 (all values are NAs)
data1 <- data1[-c(73488),]

# some priperty values are missing. impute "NA" by average values from each categories
test <- aggregate(propvalue ~ propextent_txt, data = data1, FUN = mean,na.action = na.omit)
test2 <- subset(data1,select = c(eventid,propextent_txt, propvalue))
test2 <- test2[is.na(test2$propvalue),]
test2 <- merge(test2, test, by = "propextent_txt")
test2 <- subset(test2, select = -propvalue.x)

data1 <- merge(data1,test2, by = "eventid", all.x = TRUE)
data1$propvalue.y[is.na(data1$propvalue.y)] <- 0
data1$propvalue[is.na(data1$propvalue)] <- 0
data1$propvalue = data1$propvalue + data1$propvalue.y

# remove property and nwound columns - not required for this analysis
data1 <- subset(data1, select = -c(property,propextent_txt.x,propextent_txt.y,propvalue.y,nkillter,nwoundus,nwoundte))

# Manage NA in various features -- replace those -99 values by NA
is.na(data1$nperps) = data1$nperps < 0
is.na(data1$nperpcap) = data1$nperpcap < 0
is.na(data1$ishostkid) = data1$ishostkid < 0
is.na(data1$nhostkid) = data1$nhostkid < 0
is.na(data1$nhostkidus) = data1$nhostkidus < 0
is.na(data1$nhours) = data1$nhours < 0
is.na(data1$ndays) = data1$ndays < 0
is.na(data1$ransom) = data1$ransom < 0
is.na(data1$ransomamt) = data1$ransomamt < 0
is.na(data1$ransomamtus) = data1$ransomamtus < 0
is.na(data1$ransompaid) = data1$ransompaid < 0
is.na(data1$ransompaidus) = data1$ransompaidus < 0
is.na(data1$nreleased) = data1$nreleased < 0

# add a row with 1 value - this is very helpful to perform aggrigation operations
data1$ones <- 1
data1$region <- as.factor(data1$region)
rm(test)
rm(test2)

# rename the levels
levels(data1$weapon_type) <- c("Biological","Chemical","Bombs", "Fake Weapons", "Firearms","Incendiary","Melee","Other",
                                 "Radiological","Sabotage_Equipment","Unknown","Vehicle")

levels(data1$attack_type) <- c("Armed Assault","Assassination", "Explosion", "Facility Attack",
                                 "Hijacking", "Barricade", "Kidnapping", "Unarmed Assault", "Unknown")

# Save data for Machine Learning
data2 <- data1  # ML for Bomb attacks
data3 <- data1  # ML to classify terrorist group

# For ploting and display purpurpose merger few factors in target type

levels(data1$target_type) <- c("Hate crimes", "Public services", "Private property", "Public services", "Public services",
                                                              "Government", "Government","Public services", "Private property", "Government", 
                                                              "Private property", "Private property", "Government", "Private property", "Hate crimes",
                                                              "Public services", "Government", "Public services", "Public services","Public services",
                                                              "Public services","Hate crimes" )
      

# For Map display change latitude of 9/11 attack little bit to avoid overlaping (only for a better view)

data1[data1$eventid == 200109110004, ][7] $latitude <- 40.71288
data1[data1$eventid == 200109110005, ][7] $latitude <- 40.71268

saveRDS(data1,"data1.rds")

################################################################################# 

########  MACHINE LEARNING #############

################################################################################

# Powerlaw - fitting 

data2 <- data2[complete.cases(data1$nkill),]

ngtx <- function(x) {
      return(sum(data2$nkill >= x))
}

data2$ngtx <- lapply(data2$nkill,ngtx)
data2$ngtx <- as.numeric(data2$ngtx)
data2$prob <- data2$ngtx / nrow(data2)

data3 <- subset(data2, select = c(nkill, ngtx, prob)) 
data3 <- data3[data3$nkill >0,]

log_model <- lm(log10(data3$prob) ~ log10(data3$nkill))

data4 <- data3[data3$nkill >10,]
plot(data3$nkill, data3$prob)
abline(lm(log10(data4$prob) ~ log10(data4$nkill)))

nls1 <- nls( prob ~ theta1*(nkill^theta2) + theta3, start = list(theta1 = 0.2, theta2 = -0.2, theta3 = 0.00005), data = data4, 
             trace = T,control=nls.control(maxiter=500))


# GTD - Machine Learning

# 1) Bomb Atacks Vs Non-Bomb atacks

data2 <- data2[complete.cases(data2$nkill),]

# Lets saperate bomb attack from other types

bomb <- function(x) {
  if (x == "Explosion") { 
    return(1)
  } else {
      return(0)
    }
}

data2$bomb <- sapply(data2$attack_type,bomb)

# Re-level target types for Machine Learning

levels(data2$target_type) <- c("Hate Crime", "Airports & Aircraft", "Business", "Educational Institution",
                               "Utilities", "Government", "Government", "Journalists & Media", "Maritime","Military", "NGO", "Other", "Police",
                               "Private Citizens & Property", "Hate Crime", "Utilities", "Non-State Militia","Tourists", "Transportation",
                               "Other", "Utilities", "Hate Crime" )


bomb_ml <- subset(data2, select = c(bomb, success, suicide, nkillus, multiple,target_type,nkill,attack_type, propvalue))
bomb_ml <- bomb_ml[complete.cases(bomb_ml),]

mod1 <- glm(bomb ~ success, data = bomb_ml, family = "binomial") 
mod2 <- glm(bomb ~ success + suicide, data = bomb_ml, family = "binomial")
mod3 <- glm(bomb ~ success + suicide + nkillus, data = bomb_ml, family = "binomial")

anova(mod1,mod2,mod3, test = "Chisq")

mod4 <- glm(bomb ~ success + suicide + nkillus + target_type -1, data = bomb_ml, family = "binomial")
mod5 <- glm(bomb ~ success + suicide + nkillus + multiple + target_type -1, data = bomb_ml, family = "binomial")

# Add pro[erty value
mod6 <- glm(bomb ~ success + suicide + nkillus + multiple + target_type + propvalue -1, data = bomb_ml, family = "binomial")
anova(mod1, mod2, mod3,mod4 ,mod5,mod7,test ="Chisq")

# Let's remove 9/11 Attack and see changes - No effect  - 
bomb_ml_without <- bomb_ml[!(bomb_ml$propvalue == 2700000000),]
mod6 <- glm(bomb ~ success + suicide + nkillus + multiple + target_type + propvalue, data = bomb_ml_without, family = "binomial")

# Not a significant difference propvalue makes into the model! Let's not include it. 

final_model <- glm(bomb ~ success + suicide + nkillus + multiple + target_type -1, data = bomb_ml, family = "binomial")
importance <- varImp(final_model)

anova(mod1, mod2,mod4 ,mod5,test ="Chisq")

rownames(importance) <- c("Suicide", "US Fetalities", "Multiple Attacks", "Hate Crimes", "Airports & Aircraft", "Business", "Educational Institution",
                             "Utilities","Government", "Journalists & Media", "Maritime","Military", "NGO", "Other", "Police",
                             "Private Citizens & Property","Non-State Militia","Tourists", "Transportation")

logistic_coff <- data.frame(summary(final_model)$coefficients)

rownames(logistic_coff) <- c("Success", "Suicide", "US Fetalities", "Multiple Attacks", "Hate Crimes", "Airports & Aircraft", "Business", "Educational Institution",
                             "Utilities","Government", "Journalists & Media", "Maritime","Military", "NGO", "Other", "Police",
                             "Private Citizens & Property","Non-State Militia","Tourists", "Transportation")
                            
logistic_coff$Parameter <- rownames(logistic_coff)

shiny_logistic <- logistic_coff[logistic_coff$Parameter %in% c("Success", "Suicide", "Multiple Attacks", "Transportation", "Utilities", "Business","Tourists",  "Journalists & Media"),]
shiny_logistic <- subset(shiny_logistic, select = c(Parameter,Estimate,Std..Error))
colnames(shiny_logistic) <- c("Parameter", "Coefficient", "Error")
shiny_logistic <- shiny_logistic[order(shiny_logistic$Coefficient, decreasing = T),]

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

shiny_logistic <- round_df(shiny_logistic, digits = 3)
saveRDS(shiny_logistic, "shiny_logistic.rds")



############### TESTS ############################

# gov <- function(x) {
#   if (x == "Government (Diplomatic)" |
#       x == "Government (General)"|
#       x == "Military" |
#       x == "Police") { 
#     return(1)
#   } else {
#     return(0)
#   }
# }
# 
# private <- function(x) {
#   if (x == "Business" |
#       x == "Maritime"|
#       x == "Private Citizens & Property" |
#       x == "NGO") { 
#     return(1)
#   } else {
#     return(0)
#   }
# }
# 
# 
# 
# #data2$gov <- sapply(data2$target_type,gov)
# #data2$private <- sapply(data2$target_type,private)
# 
# # Remove 9/11 Attack
# data2 <- data2[-c(47641:47644),]
# 
# bomb_ml <- subset(data2, select = c(bomb, success, suicide, nkillus, target_type))
# bomb_ml <- bomb_ml[complete.cases(bomb_ml),]
# 
# # Normalize Features
# #bomb_ml$nkill = (bomb_ml$nkill - mean(bomb_ml$nkill)) / (max(bomb_ml$nkill) - min(bomb_ml$nkill))
# #bomb_ml$propvalue = (bomb_ml$propvalue - mean(bomb_ml$propvalue)) / (max(bomb_ml$propvalue) - min(bomb_ml$propvalue))
# 
# # Build Model
# mod1 <- glm(bomb ~ success + suicide -1, data = bomb_ml, family = "binomial")
# mod2 <- glm(bomb ~ success + suicide + nkillus + target_type -1, data = bomb_ml, family = "binomial")
# anova(mod1, mod2, test ="Chisq")
# 
# # Absolute value of the t-statistic
# varImp(mod_fit)
# 
# mylogit <- glm(bomb ~ . -1, data = bomb_ml, family = "binomial")
# summary(mylogit)
# 
# logistic_coff <- data.frame(summary(mylogit)$coefficients)
# 
# rownames(logistic_coff) <- c("success", "suicide", "US Fetalities", "Abortion Related", "Airports & Aircraft", "Business", "Educational Institution", 
#                              "Food or Water Supply", "Diplomatic", "Government", "Journalists & Media", "Maritime","Military", "NGO", "Other", "Police", 
#                              "Private Citizens & Property", "Religious Figures/Institutions", "Telecommunication", "Non-State Militia","Tourists", "Transportation", 
#                              "Unknown", "Utilities", "Violent Political Party" )
# 
# logistic_coff$Parameter <- rownames(logistic_coff)
# 
# shiny_logistic <- logistic_coff[logistic_coff$Parameter %in% c("success", "suicide", "US Fetalities", "Airports & Aircraft", "Utilities","Tourists",  "Journalists & Media"),]
# shiny_logistic <- subset(shiny_logistic, select = c(Parameter,Estimate,Std..Error))
# colnames(shiny_logistic) <- c("Parameter", "Coefficient", "Error")
# 
# round_df <- function(df, digits) {
#   nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
#   
#   df[,nums] <- round(df[,nums], digits = digits)
#   
#   (df)
# }
# 
# shiny_logistic <- round_df(shiny_logistic, digits = 3)
# saveRDS(shiny_logistic, "shiny_logistic.rds")
# 
# # Alternate
# #fit <- logistf(bomb ~ ., data = bomb_ml)





# 2) Predicting type of terrorist group

data3 <- data3[data3$group_name != "Unknown",]

data_ml <- subset(data3, select = c(date,latitude, suicide, success, multiple,nkill, longitude,weapon_type,attack_type,target_type,group_name))
data_ml <- data_ml[complete.cases(data_ml),]

# Grops by killing
groups <- aggregate(nkill ~ group_name, data = data_ml, FUN = sum)
groups <- data.frame(groups)
groups <- groups[order(groups$nkill, decreasing = TRUE),]
groups <- groups[1:30,]

# Group by attacks

groups_freq <- table(data_ml$group_name)
groups_freq <- data.frame(groups_freq)
groups_freq <- groups_freq[order(groups_freq$Freq, decreasing = TRUE),]
groups_freq <- groups_freq[1:30,]
colnames(groups_freq) <- c("group_name","nkill")

group <- rbind(groups, groups_freq)
group <- data.frame(unique(group$group_name))
colnames(group) <- c("group_name")

data_ml <- data_ml[data_ml$group_name %in% group$group_name,]

data_ml$group_name <- factor(data_ml$group_name)

#levels(data_ml$weapon_type) <- c("Biological","Chemical","Bombs", "Fake Weapons", "Firearms","Incendiary","Melee","Other",
                               #  "Radiological","Sabotage Equipment","Unknown","Vehicle")

#levels(data_ml$attack_type) <- c("Armed Assault","Assassination", "Explosion", "Facility Attack",
                                # "Hijacking", "Barricade", "Kidnapping", "Unarmed Assault", "Unknown")


intrain <- createDataPartition(y = data_ml$group_name, p = 0.7, list = FALSE)

training <- data_ml[intrain,]
test <- data_ml[-intrain,]

# Fit - Random Forest
mod1 <- randomForest(training$group_name ~., data = training)

test.forest <- predict(mod1, test)
postResample(test.forest, test$group_name) # 94% accurate on test data

varImpPlot(mod1)

rf <- data.frame(mod1$importance)
rf$name <- rownames(rf)

p12 <- plot_ly(
               x = c("Suicide", "Success","Multiple","Weapon","Attack","Fatalities","Target"),
               y = c(78.9,94.7,231.3,424.1,599.2,634.8,1121.0),
               type = "bar",
               color = c("Suicide:78.9","Success:94.7","Multiple:231.3","Weapon Type:424.1", "Attack Type:599.2","Fatalities:634.8",
                         "Target Type: 1121.0"),
               showlegend = FALSE
) %>%
  layout(xaxis = list(title = "Importance of Paramters",categoryarray = c("Suicide", "Success","Multiple","Weapon","Attack","Fatalities","Target"), categoryorder = "array"),
         yaxis = list(title = "Mean Decrease in Gini Impurity Index"))
         

save(mod1, file = "rf_model1.rda")

# Fit - Knn 

knn_ml <- data_ml

knn.training <- subset(training, select = -c(group_name))
knn.test <- subset(test, select = -c(group_name))

knn.training.labels <- subset(training, select = c(group_name))
knn.test.labels <- subset(test, select = c(group_name))

class = knn.training.labels[,1]

mod2 <- knn(train = knn.training, test = knn.test, cl = class, k = 40) #  56%

mod2 <- train(group_name ~ ., data = training, method = "ada")

mod2 <- train(group_name ~ ., data = training, method = "rpart") # 30% Accuracy

mod3<- train(group_name ~ ., data = training, method = "nb") # 65% accurate

method <- c("DT","KNN","NB", "RF")
accuracy <- c(28,56,65,94)

ml_group <- data.frame(method,accuracy)

p11 <- plot_ly(ml_group,
  x = c("DT","KNN","NB", "RF"),
  y = c(28,56,65,94),
  type = "bar",
  color = c("DT: 28%","KNN: 56%","NB: 65","RF: 94"),
  showlegend = FALSE
) %>%
  layout(xaxis = list(title = "Method"), yaxis = list(title = "Accuracy (%)"))

# 2) By country 
country = data2[data2$country == "United States",]

country <- data2
country <- country[country$group_name != "Unknown",]

data_ml <- subset(country, select = c(date,latitude, nkill, longitude,weapon_type,attack_type,target_type,group_name))

data_ml <- data_ml[complete.cases(data_ml),]

# Grops by killing
groups <- aggregate(nkill ~ group_name, data = data_ml, FUN = sum)
groups <- data.frame(groups)
groups <- groups[order(groups$nkill, decreasing = TRUE),]
groups <- groups[1:20,]

# Group by attacks

groups_freq <- table(data_ml$group_name)
groups_freq <- data.frame(groups_freq)
groups_freq <- groups_freq[order(groups_freq$Freq, decreasing = TRUE),]
groups_freq <- groups_freq[1:20,]
colnames(groups_freq) <- c("group_name","nkill")

group <- rbind(groups, groups_freq)
group <- data.frame(unique(group$group_name))
colnames(group) <- c("group_name")
  
data_ml <- data_ml[data_ml$group_name %in% group$group_name,]

data_ml$group_name <- factor(data_ml$group_name)

levels(data_ml$weapon_type) <- c("Biological","Chemical","Bombs", "Fake", "Firearms","Incendiary","Melee","Other",
                                  "Radiological","Sabotage_Equipment","Unknown","Vehicle")

levels(data_ml$attack_type) <- c("Armed_Assault","Assassination", "Explosion", "Facility_Attack",
                                             "Hijacking", "Barricade", "Kidnapping", "Unarmed_Assault", "Unknown")


intrain <- createDataPartition(y = data_ml$group_name, p = 0.7, list = FALSE)

training <- data_ml[intrain,]
test <- data_ml[-intrain,]

# 1) Random Forest
mod1 <- randomForest(training$group_name ~., data = training)
pred <- predict(mod1, test)
test$preright <- pred == test$group_name
table(pred,test$group_name)


########################################################################################

#######  BY COUNTRY VISULIZATION SHINY APP ########

#######################################################################################

# function to convert number into words - Need this for converting financial loss
convert_to_word <- function(x) {
  if (x >= 1.0E+9) {
    return(paste(round(x / 1.0E+9,2),"Billion",sep = " "))
  }
  if (x >= 1.0E+6) {
    return(paste(round(x / 1.0E+6,2),"Million",sep = " "))
  }}


# Filter Data

country_name <- c("India", "United States", "Pakistan","Sri Lanka",
                  "United Kingdom", "Spain", "China", "Peru", "Iraq",
                  "Iran", "Afghanistan")
saveRDS(country_name,"country_name.rds")

region_name <- levels(data1$region)
saveRDS(region_name,"region_name.rds")


# For Value Box
country = "Pakistan"
country_data <- data1[data1$country == country,]
country_no_attack <- nrow(country_data)
country_finance_loss <- convert_to_word(sum(country_data$propvalue, na.rm = TRUE))
country_life_loss <- sum(country_data$nkill, na.rm = TRUE)


# Attack Type Plot + Box
country_type_attack <- data.frame(aggregate(ones ~ attack_type, data = country_data, sum))
country_type_attack <- country_type_attack[order(country_type_attack$ones, decreasing  = TRUE),]
country_type_attack <- country_type_attack[1:5,]

p1 <- plot_ly(country_type_attack, labels = ~attack_type, values = ~ones, type = 'pie',hole = 0.8, rotation = 90,
              textposition = 'inside', textinfo = 'percent',insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',text = ~paste('No of Attacks:', ones),showlegend = T) %>%
  layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    legend = list(x = 0.3, y = 0.5))

# Target Type

country_type_target <- data.frame(aggregate(ones ~ target_type, data = country_data, sum))

p2 <- plot_ly(country_type_target, labels = ~target_type, values = ~ones, type = 'pie',hole = 0.8, rotation = 90,
              textposition = 'outside', textinfo = 'values',insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',text = ~paste('No of Attacks:', ones),showlegend = T) %>%
  layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    legend = list(x = 0.3, y = 0.5))

# Weapon Type Plot + Box
country_type_weapon <- data.frame(aggregate(ones ~ weapon_type, data = country_data, sum))
country_type_weapon <- country_type_weapon[order(country_type_weapon$ones, decreasing  = TRUE),]
country_type_weapon <- country_type_weapon[1:5,]

p3 <- plot_ly(country_type_weapon, labels = ~weapon_type, values = ~ones, type = 'pie',hole = 0.8, rotation = 90,
              textposition = 'outside', textinfo = 'values',insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',text = ~paste('No of Attacks:', ones),showlegend = T) %>%
  layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    legend = list(x = 0.3, y = 0.5))


# Terrorist Group Table
country_group_name <- aggregate( nkill ~ group_name, data = country_data, FUN = sum)
country_group_name <- country_group_name[order(country_group_name$nkill, decreasing = T), ]
country_group_name$group_name <- as.character(country_group_name$group_name)
country_group_name <- country_group_name[!country_group_name$group_name == "Unknown",]
colnames(country_group_name) <- c("Group", "Fetalities")

# Country Map Visulization

country_data <- country_data[complete.cases(country_data$latitude),]
country_data <- country_data[complete.cases(country_data$nkill),]
country_data <- country_data[country_data$nkill > 0, ]
country_data$sqrt <- log(country_data$nkill)


m1 = leaflet(country_data) %>% addProviderTiles("CartoDB.DarkMatterNoLabels", options= providerTileOptions(opacity = 0.8)) 

pal <- colorNumeric(palette = "YlOrRd", domain = country_data$sqrt)
m1 %>% addCircleMarkers(radius = 2, color = ~pal(sqrt),popup = ~paste(paste0("Fetalities: ", nkill),paste0("City: ", city),
                                                                      paste0("Date: ", date), sep = '<br />') , opacity = 0.4)


# Country Time Series


#sd <- aggregate(ones ~ date + attack_type, data = country_data, FUN = sum)
#nkill <- xts(country_data$nkill, as.POSIXct(country_data$date))
#sd <- xts(sd$ones, as.POSIXct(sd$date))
#dygraph(nkill)

attack_category <- aggregate(ones ~ iyear + attack_type, data = data1, FUN = sum)
attack_category$onessqrt <- sqrt(attack_category$ones)

p5 <- plot_ly(attack_category) %>%
  add_trace(x = ~iyear, y = ~onessqrt, color = ~attack_type, type = 'scatter', 
            mode = 'lines+markers',line = list(width = 2), hoverinfo = "text", 
            text = ~paste(paste("Total Attacks:", ones), attack_type,
                          sep = "<br />"), colors = c("red","blue","green","orange")) %>%
  layout(
    xaxis = list(range = c(1969, 2016),zeroline = TRUE, title = ""),
    yaxis = list(side = 'left', rangemode = "tozero", overlaying = "y", 
                 title = 'SQRT(Number of Attacks)',showgrid = TRUE, 
                 zeroline = TRUE,range = c(0, 100),showticklabels = TRUE),
    legend = list(x = 0.06, y = 0.98)) %>%
  config(displayModeBar = F)



# Print World Map on First page

country_code <- read.csv('country_code.csv')[ ,c('COUNTRY', 'CODE')]
a <- table(data1$country)
b <- aggregate(nkill ~ country, data = data1, FUN = sum)
a <- data.frame(a)
colnames(a) <- c("COUNTRY","FREQ")
colnames(b) <- c("COUNTRY","DEATH")
attack_freq_country <-merge(a,country_code,by = "COUNTRY")
attack_freq_country <-merge(attack_freq_country,b,by = "COUNTRY", all.x = TRUE)
attack_freq_country$DEATH[is.na(attack_freq_country$DEATH)] <- 0
saveRDS(attack_freq_country, "attack_freq_country.rds")


# Three pie charts for TOP 10

# 1. No of attacks

a <- data.frame(table(data1$country))
colnames(a) <- c("COUNTRY","FREQ")
a <- a[order(a$FREQ, decreasing = TRUE),]
attack_country <- a[1:10,]

p1 <- plot_ly(attack_country, labels = ~COUNTRY, values = ~FREQ, type = 'pie',
             textposition = 'inside', textinfo = 'label',insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',text = ~paste('No of Attacks:', FREQ),showlegend = FALSE
             ) %>%

  layout(title = 'Total Attacks',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# 2. No of Deaths

b <- aggregate(nkill ~ country, data = data1, FUN = sum)
colnames(b) <- c("COUNTRY","DEATH")
b <- b[order(b$DEATH, decreasing = TRUE),]
death_country <- b[1:10,]


p2 <- plot_ly(death_country, labels = ~COUNTRY, values = ~DEATH, type = 'pie',
             textposition = 'inside', textinfo = 'label',insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',text = ~paste('No. of Fatalities:', DEATH),showlegend = FALSE
) %>%

  layout(title = 'Number of Fatalities',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# 3. Financial Loos

c <- aggregate(propvalue ~ country, data = data1, FUN = sum)
colnames(c) <- c("COUNTRY","LOSS")
c <- c[order(c$LOSS, decreasing = TRUE),]
loss_country <- c[1:10,]

convert_to_word <- function(x) {
                      if (x >= 1.0E+9) {
                        return(paste(round(x / 1.0E+9,0),"Billion",sep = " "))
                      }
                     if (x >= 1.0E+6) {
                        return(paste(round(x / 1.0E+6,0),"Million",sep = " "))
                      }

}

loss_country$TEXT <- lapply(loss_country$LOSS,convert_to_word)


p3 <- plot_ly(loss_country, labels = ~COUNTRY, values = ~LOSS, type = 'pie',
             textposition = 'inside', textinfo = 'label',insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',text = ~paste('$', TEXT),showlegend = FALSE
) %>%

  layout(title = 'Propery Damage (US $)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Time Series of No of attacks by type


attack_category <- aggregate(ones ~ iyear + attack_type, data = data1, FUN = sum)
attack_category$onessqrt <- sqrt(attack_category$ones)


p5 <- plot_ly(attack_category) %>%
  add_trace(x = ~iyear, y = ~onessqrt, color = ~attack_type, type = 'scatter', mode = 'lines+markers',
            line = list(width = 2), hoverinfo = "text", text = ~paste(ones,":",attack_type),
            colors = c("red","blue","green","orange")) %>%
  layout(title = 'Deaths by Year',
         xaxis = list(range = c(1969.5, 2015.5),zeroline = TRUE),
         yaxis = list(side = 'left', rangemode = "tozero", overlaying = "y", title = 'SQRT(Number of Attacks)',
                      showgrid = TRUE, zeroline = TRUE,range = c(0, 100),showticklabels = TRUE),
         legend = list(x = 0.1, y = 0.9))


# Time series of fetalities

death_year <- aggregate(nkill ~ iyear + attack_type, data = data1, FUN = sum)
death_year$nkill <- sqrt(death_year$nkill)
levels(death_year$attack_type) <- c("Armed_Assault","Assassination", "Bomb_Explosion", "Facility_Attack",
                                        "Hijacking", "Barricade", "Kidnapping", "Unarmed_Assault", "Unknown")
attack_category2 <- cast(death_year,iyear ~ attack_type, sum)


p6 <- plot_ly(attack_category2, x = ~iyear, y = ~Armed_Assault, type = 'bar', name = 'Armed Assault',
              text = ~paste(Armed_Assault*Armed_Assault,":Armed Assault"),hoverinfo = 'text') %>%
      add_trace(y = ~Assassination, name = 'Assassination',text = ~paste(Assassination*Assassination,":Assassination")) %>%
      add_trace(y = ~Bomb_Explosion, name = 'Bomb Explosion',text = ~paste(Bomb_Explosion*Bomb_Explosion,":Bomb Explosion")) %>%
      add_trace(y = ~Facility_Attack, name = 'Facility Attack',text = ~paste(Facility_Attack*Facility_Attack,":Facility Attack")) %>%
      add_trace(y = ~Hijacking, name = 'Hijacking',text = ~paste(Hijacking*Hijacking,":Hijacking")) %>%
      add_trace(y = ~Barricade, name = 'Barricade',text = ~paste(Barricade*Barricade,":Barricade")) %>%
      add_trace(y = ~Kidnapping, name = 'Kidnapping',text = ~paste(Kidnapping*Kidnapping,":Kidnapping")) %>%
      add_trace(y = ~Unarmed_Assault, name = 'Unarmed Assault',text = ~paste(Unarmed_Assault*Unarmed_Assault,":Unarmed Assault")) %>%
      add_trace(y = ~Unknown, name = 'Unknown',text = ~paste(Unknown*Unknown,":Unknown") ) %>%
      layout(yaxis = list(title = ' SQRT(Fatalities)'), barmode = 'stack',legend = list(x = 0.1, y = 0.9))

# Google GlobeGL Visulization
terror <- subset(data1, select = c(latitude,longitude,nkill))
colnames(terror) <- c("Latitude","Longitude","Population")
terror <- terror[complete.cases(terror),]
terror <- terror[terror$Population > 10,]
terror$Population <- terror$Population / max(terror$Population)
saveRDS(terror,"terror.rds")




######################################################################################################

############ EXPERIMENTS ############

######################################################################################################


# p6 <- plot_ly(attack_category) %>%
#   add_trace(x = ~iyear, y = ~nkill, type = 'bar', name = 'Death',
#             marker = list(color = '#C9EFF9'),
#             hoverinfo = "text",
#             text = ~paste(iyear,": Total Deaths : ",nkill)) %>%
#   layout(title = 'Deaths by Year',
#          xaxis = list(title = "",range = c(1970, 2015.5)),
#          yaxis = list(side = 'left', title = 'Deaths', showgrid = FALSE, zeroline = FALSE,range = c(0, 44000)),
#          legend = list(x = 0.1, y = 0.9))
# 
# 
# # 2D World Plot
# 
# wplot <- subset(data3,select = c(latitude,longitude,country_txt,nkill,freq_attack))
# wplot <- wplot[complete.cases(wplot),]
# wplot <- wplot[wplot$nkill>100,]
# 
# heat <- log(wplot$nkill * wplot$freq_attack)
# 
# mapWorld <- borders("India", colour="grey50", fill="grey50") # create a layer of borders
# mp <- ggplot() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                        panel.background = element_blank(),axis.title.x=element_blank(),
#                        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#                        axis.title.y=element_blank(),axis.text.y=element_blank(),
#                        axis.ticks.y=element_blank()) + scale_color_gradient(low = "red", "High", "Low") + mapWorld
# mp <- mp + geom_point(aes(x=wplot$longitude, y=wplot$latitude, colour = heat),
#                      size=1.0, alpha = 1)
# ggplotly(mp)






