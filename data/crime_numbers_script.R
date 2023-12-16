library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gghighlight)

dat <- read_xls("/Users/Public/gitHub/emu430-fall2023-team-4k1e_rda/data/suc turu ve egitim durumuna gore ceza infaz kurumuna giren hukumluler.xls")
index2020 <- read_xlsx("/Users/Public/gitHub/emu430-fall2023-team-4k1e_rda/data/indexes.xlsx", sheet = "2020-2018", col_names = FALSE)
index2017 <- read_xlsx("/Users/Public/gitHub/emu430-fall2023-team-4k1e_rda/data/indexes.xlsx", sheet = "2017-2013", col_names = FALSE)
index2012 <- read_xlsx("/Users/Public/gitHub/emu430-fall2023-team-4k1e_rda/data/indexes.xlsx", sheet = "2012-2011", col_names = FALSE)
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

numbers <- data.frame(years = years, type_of_crimes = types, na.omit(data.frame(gen_total = dat$...2[20:nrow(dat)], male = dat$...3[20:nrow(dat)], female = dat$...4[20:nrow(dat)])))
numbers$gen_total <- as.numeric(numbers$gen_total)
numbers$male <- as.numeric(numbers$male)
numbers$female <- as.numeric(numbers$female)
numbers[is.na(numbers)] <- 0

year_nums <- c(2020,2019,2018,2017,2016,2015,2014,2013,2012,2011)

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

save(numbers, file = "crime_numbers.RData")