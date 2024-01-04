library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gghighlight)
library(tibble)
library(stringr)

dat <- read_xls("data/suc turu ve egitim durumuna gore ceza infaz kurumuna giren hukumluler.xls")
index2020 <- read_xlsx("data/indexes.xlsx", sheet = "2020-2018", col_names = FALSE)
index2017 <- read_xlsx("data/indexes.xlsx", sheet = "2017-2013", col_names = FALSE)
index2012 <- read_xlsx("data/indexes.xlsx", sheet = "2012-2011", col_names = FALSE)
index2020 <- sapply(index2020, as.character)
index2017 <- sapply(index2017, as.character)
index2012 <- sapply(index2012, as.character)

temp2020 <- sapply(index2020, as.character)
temp2017 <- c(sapply(index2017, as.character), rep(NA, times = 1))
temp2012 <- c(sapply(index2012, as.character), rep(NA, times = 4))

for (val in dat$'2011-2020') {
  if (identical(val,"2020")){
    suc2020 <- dat[20:59, 2]
    suc2020 <- suc2020[complete.cases(suc2020),]
    suc2020 <- cbind(index2020, suc2020)
    names(suc2020) <- c("type_of_crime", "num2020")
  }
  if (identical(val,"2019")){
    suc2019 <- dat[62:101, 2]
    suc2019 <- suc2019[complete.cases(suc2019),]
    suc2019 <- cbind(index2020, suc2019)
    names(suc2019) <- c("type_of_crime", "num2019")
  }
  if (identical(val,"2018")){
    suc2018 <- dat[104:143, 2]
    suc2018 <- suc2018[complete.cases(suc2018),]
    suc2018 <- cbind(index2020, suc2018)
    names(suc2018) <- c("type_of_crime", "num2018")
  }
  if (identical(val,"2017")){
    suc2017 <- dat[147:185, 2]
    suc2017 <- suc2017[complete.cases(suc2017),]
    suc2017 <- cbind(index2017, suc2017)
    names(suc2017) <- c("type_of_crime", "num2017")
  }
  if (identical(val,"2016")){
    suc2016 <- dat[189:227, 2]
    suc2016 <- suc2016[complete.cases(suc2016),]
    suc2016 <- cbind(index2017, suc2016)
    names(suc2016) <- c("type_of_crime", "num2016")
  }
  if (identical(val,"2015")){
    suc2015 <- dat[231:269, 2]
    suc2015 <- suc2015[complete.cases(suc2015),]
    suc2015 <- cbind(index2017, suc2015)
    names(suc2015) <- c("type_of_crime", "num2015")
  }
  if (identical(val,"2014")){
    suc2014 <- dat[273:311, 2]
    suc2014 <- suc2014[complete.cases(suc2014),]
    suc2014 <- cbind(index2017, suc2014)
    names(suc2014) <- c("type_of_crime", "num2014")
  }
  if (identical(val,"2013")){
    suc2013 <- dat[315:353, 2]
    suc2013 <- suc2013[complete.cases(suc2013),]
    suc2013 <- cbind(index2017, suc2013)
    names(suc2013) <- c("type_of_crime", "num2013")
  }
  if (identical(val,"2012")){
    suc2012 <- dat[357:391, 2]
    suc2012 <- suc2012[complete.cases(suc2012),]
    suc2012 <- cbind(index2012, suc2012)
    names(suc2012) <- c("type_of_crime", "num2012")
  }
  if (identical(val,"2011")){
    suc2011 <- dat[395:429, 2]
    suc2011 <- suc2011[complete.cases(suc2011),]
    suc2011 <- cbind(index2012, suc2011)
    names(suc2011) <- c("type_of_crime", "num2011")
  }
}

d2020 <- data.frame(rep(2020, times = nrow(suc2020)))
d2019 <- data.frame(rep(2019, times = nrow(suc2020)))
d2018 <- data.frame(rep(2018, times = nrow(suc2018)))
d2017 <- data.frame(c(rep(2017, times = nrow(suc2017)), rep(NA,times=1)))
d2016 <- data.frame(c(rep(2016, times = nrow(suc2016)),rep(NA,times=1)))
d2015 <- data.frame(c(rep(2015, times = nrow(suc2015)),rep(NA,times=1)))
d2014 <- data.frame(c(rep(2014, times = nrow(suc2014)),rep(NA,times=1)))
d2013 <- data.frame(c(rep(2013, times = nrow(suc2013)),rep(NA,times=1)))
d2012 <- data.frame(c(rep(2012, times = nrow(suc2012)),rep(NA,times=4)))
d2011 <- data.frame(c(rep(2011, times = nrow(suc2011)),rep(NA,times=4)))

df <- data.frame(d2020,d2019,d2018,d2017,d2016,d2015,d2014,d2013,d2012,d2011)

years <- data.frame(years = unlist(df, use.names = FALSE))
years <- na.omit(years)

type_frame <- data.frame(temp2020,temp2020,temp2020,temp2017,temp2017,temp2017,temp2017,temp2017,temp2012,temp2012)
types <- data.frame(type_of_crimes = unlist(type_frame, use.names = FALSE)) |> na.omit()

numbers <- data.frame(years = years, type_of_crime = types, na.omit(data.frame(gen_total = dat$...2[20:nrow(dat)], male = dat$...3[20:nrow(dat)], female = dat$...4[20:nrow(dat)])))
numbers$gen_total <- as.numeric(numbers$gen_total)
numbers$male <- as.numeric(numbers$male)
numbers$female <- as.numeric(numbers$female)
numbers[is.na(numbers)] <- 0
numbers <- pivot_longer(numbers, cols = c(male,female), names_to = "gender", values_to = "number")

gendered_numbers <- data.frame(rep(NA, times = 410))

for (i in 1:ncol(dat)){
  if (is.na(dat[16,i])){
    next
  }
  if (dat[16,i] == "Male" || dat[16,i] == "Female"){
    column_name <- paste( dat[16,i],i, sep = "")
    gendered_numbers[[column_name]] <- dat[20:429,i]
  }
}

gendered_numbers <- gendered_numbers[,2:19]

colnames(gendered_numbers) <- c("Male_t", "Female_t", "Male_illit", "Female_illit", 
                                "Male_lit", "Female_lit", "Male_pris", "Female_pris",
                                "Male_prie", "Female_prie", "Male_jhs", "Female_jhs",
                                "Male_hs", "Female_hs", "Male_bsc", "Female_bsc", 
                                "Male_unk", "Female_unk")

x <- 1:18

gendered_numbers[ , x] <- apply(gendered_numbers[ , x], 2,            # I CAN NOT BELIEVE I HAD TO DO THIS JUST TO CONVERT ACTUAL NUMBERS TO NUMERIC. R SUCKS SOMETIMES.
                                function(y) as.numeric(as.character(y)))

gendered_numbers <- gendered_numbers[rowSums(is.na(gendered_numbers)) != ncol(gendered_numbers),]
gendered_numbers[is.na(gendered_numbers)] <- 0
gendered_numbers <- data.frame(years, gendered_numbers)

education_cat <- pivot_longer(gendered_numbers, cols = -years, names_to = c("gender","education_level"),names_sep = "_", values_to = "number")
full_names <- data.frame(rep(NA, times = nrow(education_cat)))
for (i in 1:nrow(education_cat)){
  if (education_cat$education_level[i] == "t"){
    full_names[i,1] <- "total"
  }
  else if (education_cat$education_level[i] == "illit"){
    full_names[i,1] <- "illiterate"
  }
  else if (education_cat$education_level[i] == "lit"){
    full_names[i,1] <- "literate"
  }
  else if (education_cat$education_level[i] == "pris"){
    full_names[i,1] <- "primary school"
  }
  else if (education_cat$education_level[i] == "prie"){
    full_names[i,1] <- "primary education"
  }
  else if (education_cat$education_level[i] == "jhs"){
    full_names[i,1] <- "junior high school"
  }
  else if (education_cat$education_level[i] == "hs"){
    full_names[i,1] <- "high school"
  }
  else if (education_cat$education_level[i] == "bsc"){
    full_names[i,1] <- "higher education"
  }
  else if (education_cat$education_level[i] == "unk"){
    full_names[i,1] <- "unknown"
  }
}
education_cat <- data.frame(education_cat, full_names)
colnames(education_cat)[5] <- "full_names"
edu_names <- data.frame(rep(NA, times = nrow(education_cat)))
edu_names[which(education_cat$years == 2020),1] <- unlist(lapply(index2020, function(x) rep(x,times = sum(education_cat$years == 2020)/27)))
edu_names[which(education_cat$years == 2019),1] <- unlist(lapply(index2020, function(x) rep(x,times = sum(education_cat$years == 2020)/27)))
edu_names[which(education_cat$years == 2018),1] <- unlist(lapply(index2020, function(x) rep(x,times = sum(education_cat$years == 2020)/27)))
edu_names[which(education_cat$years == 2017),1] <- unlist(lapply(index2017, function(x) rep(x,times = sum(education_cat$years == 2017)/26)))
edu_names[which(education_cat$years == 2016),1] <- unlist(lapply(index2017, function(x) rep(x,times = sum(education_cat$years == 2017)/26)))
edu_names[which(education_cat$years == 2015),1] <- unlist(lapply(index2017, function(x) rep(x,times = sum(education_cat$years == 2017)/26)))
edu_names[which(education_cat$years == 2014),1] <- unlist(lapply(index2017, function(x) rep(x,times = sum(education_cat$years == 2017)/26)))
edu_names[which(education_cat$years == 2013),1] <- unlist(lapply(index2017, function(x) rep(x,times = sum(education_cat$years == 2017)/26)))
edu_names[which(education_cat$years == 2012),1] <- unlist(lapply(index2012, function(x) rep(x,times = sum(education_cat$years == 2012)/23)))
edu_names[which(education_cat$years == 2011),1] <- unlist(lapply(index2012, function(x) rep(x,times = sum(education_cat$years == 2012)/23)))
education_cat <- data.frame(education_cat, edu_names)
colnames(education_cat)[6] <- "crime_names"

plot <- ggplot(numbers, aes(years, gen_total, color = type_of_crimes)) + 
  geom_line() + 
  scale_y_log10() +
  scale_x_continuous(breaks=seq(2011, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text = element_text(size = 8)) +
  gghighlight::gghighlight() +
  facet_wrap(~type_of_crimes) +
  ylab("Total Number of Crimes") +
  xlab("Years")
plot

comp_2019_2020 <- filter(numbers, years == c(2020,2019) & type_of_crimes == c("Total")) |> 
  ggplot(aes(years, gen_total)) +
  geom_col() +
  scale_x_continuous(breaks=c(2019,2020)) +
  ylab("total number of crimes")

comp_2019_2020

total_male_female <- filter(education_cat, years == 2020 & education_level == "t") |>
  ggplot(aes(x = "", y = number, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank())+
  scale_fill_manual(values = c("violet", "skyblue"))

total_male_female

all_total <- filter(numbers, type_of_crimes == "Total")$gen_total |> sum()/2
total_assault <- filter(numbers, type_of_crimes == "Assault")$gen_total |> sum()/2
total_theft <- filter(numbers, type_of_crimes == "Theft")$gen_total |> sum()/2
total_traffic <- filter(numbers, type_of_crimes == "Traffic crimes")$gen_total |> sum()/2
total_law <- filter(numbers, type_of_crimes == "Opposition  to the  Bankruptcy  and Enforcement Law")$gen_total |> sum()/2

rate_assault <- total_assault/all_total
rate_theft <- total_theft/all_total
rate_traffic <- total_traffic/all_total
rate_law <- total_law/all_total

all_vs_2020 <- data.frame(type = c(rep("all", times = 4), rep("2020", times = 4)), ratio = c(rate_assault,rate_theft,rate_traffic,rate_law, 0.157, 0.152, 0.059, 0.053), crime_type = c("Assault", "Theft","Traffic crimes","Opposition to the Bankruptcy \nand Enforcement Law")) |>
  ggplot(aes(x = crime_type, y = ratio)) +
  geom_point(aes(color = type))+
  ggtitle("Most Committed Crimes in 2020 and Their Ratios")

all_vs_2020

data.frame(number = rowSums(data.frame(filter(education_cat, years == 2020 & crime_names == "Assault" & full_names != "total" & gender == "Male")$number,
                                       filter(education_cat, years == 2020 & crime_names == "Assault" & full_names != "total" & gender == "Female")$number)),
           education_level = filter(education_cat, years == 2020 & crime_names == "Assault" & full_names != "total" & gender == "Female")$education_level) |>
  ggplot(aes(education_level,number))+
  geom_point(color = "magenta", size = 2)+
  ggtitle("Education - Assault Relation")

data.frame(number = rowSums(data.frame(filter(education_cat, years == 2020 & crime_names == "Theft" & full_names != "total" & gender == "Male")$number,
                                       filter(education_cat, years == 2020 & crime_names == "Theft" & full_names != "total" & gender == "Female")$number)),
           education_level = filter(education_cat, years == 2020 & crime_names == "Theft" & full_names != "total" & gender == "Female")$education_level) |>
  ggplot(aes(education_level,number))+
  geom_point(color = "darkblue", size = 2)+
  ggtitle("Education - Theft Relation")

data.frame(number = rowSums(data.frame(filter(education_cat, years == 2020 & full_names == "higher education" & gender == "Male" & crime_names != "Total" & crime_names != "Other crimes")$number,
                                       filter(education_cat, years == 2020 & full_names == "higher education" & gender == "Female" & crime_names != "Total" & crime_names != "Other crimes")$number)),
           crime_name = filter(education_cat, years == 2020 & full_names == "higher education" & gender == "Female" & crime_names != "Total" & crime_names != "Other crimes")$crime_names) |>
  ggplot(aes(crime_name,number))+
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(size = 8))+
  ggtitle("Higher Education Tendencies")

save(numbers, file = "crime_numbers.RData")