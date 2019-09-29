library(data.table)
library(here)
wip <- fread(here("WB-WiP.csv"),
             skip = 4, header = TRUE,
             check.names = TRUE)


wip[, c("Indicator.Name", "Indicator.Code",
        "V64"):=NULL]
setnames(wip, c("Country.Name", "Country.Code"),
         c("Country", "Code"))
head(names(wip))


WP <- melt(wip,
           id.vars = c("Country", "Code"),
           measure = patterns("^X"),
           variable.name = "YearC",
           value.name = c("pctWiP"),
           na.rm = TRUE)

WP[, ':='(Year=as.numeric(gsub("[^[:digit:].]",
                               "", YearC)),
          Ratio = (100-pctWiP)/pctWiP)][
            , YearC:=NULL]
setcolorder(WP, c("Country", "Code", "Year",
                  "pctWiP", "Ratio"))

library(ggplot2)
library(magrittr)
WP[Country %in% "United States"] %>%
  ggplot(aes(Year, pctWiP)) +
  geom_line() + geom_point() +
  scale_y_continuous(limits=c(0, 50)) +
  ylab("% Women in Parliament")

WP[Country %in% c("United States", "Mexico", "Canada",
                  "United Kingdom", "New Zealand", "Australia", "Ireland",
                  "North America", "Guatemala", "Cuba", "Dominican Republic") & !Year == "1990"] %>%
  ggplot(aes(Year, pctWiP, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  scale_y_continuous(limits=c(0, 50),
                     breaks=seq(0, 50, by=10)) +
  ggtitle("Women in Parliament: North American and Anglophile Countries") +
  ylab("% Women in Parliament")


WP[order(-pctWiP), head(.SD, 10)]
WP[order(Year, -pctWiP), head(.SD, 1), by = Year]

library(countrycode)
cl <- as.data.table(codelist)[, .(continent, wb)]
setnames(cl, c("continent", "wb"),
         c("Continent", "Code"))
cWP <- cl[WP, on="Code"]

cWP[Year %in% c(1997, 2018) & !is.na(Continent)][
  order(Year, -pctWiP), head(.SD, 1),
  by = .(Year, Continent)][
    order(Continent, Year),
    .(Continent, Year, Country, pctWiP)]


dWP <- cWP[
  order(Country, Year), .SD[c(2,.N)],
  by=Country][,
              pctDiff := pctWiP - shift(pctWiP), by=Country][
                pctDiff<0][
                  order(pctDiff)]
dWP[!is.na(Continent),
    .(Country, pctWiP, pctDiff)]




dclpct <- unique(dWP[!is.na(Continent) &
                       pctDiff <= -1]$Country) 

WP[Country %in% dclpct & !Year == "1990"] %>%
  ggplot(aes(Year, pctWiP, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 40),
                     breaks=seq(0, 40, by=10)) +
  ggtitle("Women in Parliament: Decline >=5%") +
  ylab("% Women in Parliament")


cWP[!is.na(Continent),
    ':=' (RankG = rank(-pctWiP), TotalG = .N),
    by = .(Year)]
cWP[Country=="United States",
    .(Country, Year, pctWiP, Ratio, RankG, TotalG)][
      order(Year)]


cWP[!is.na(Continent),
    ':='(RankC = rank(-pctWiP), TotalC = .N),
    by = .(Continent, Year)]

cWP[Country=="United States",
    .(Country, Year, pctWiP, Ratio, RankC, TotalC)][
      order(Year)]


cWP[Country %in% c("United States", "Mexico", "Canada",
                   "Haiti", "Guatemala", "Cuba", "Dominican Republic", "Honduras", "El Salvador", "Nicaragua")& !Year == "1990"] %>%
  ggplot(aes(Year, RankC, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 45),
                     breaks=seq(0, 45, by=10)) +
  scale_y_reverse()+
  ggtitle("Women in Parliament: Ranked") +
  ylab("Rank in North America")


cWP[Year %in% c(1997, 2018) & RankC==1][
  order(Continent, Year),
  .(Continent, Year, Country, pctWiP, RankC)]


library(gghighlight)
cWP[is.na(Continent)& !Year == "1990"] %>%
  ggplot(aes(Year, pctWiP, group=Country)) +
  geom_line() +
  gghighlight(Country=="World",
              use_direct_label = FALSE) +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 40),
                     breaks=seq(0, 40, by=10)) +
  ggtitle("Women in Parliament: Global Trends") +
  ylab("% Women in Parliament")

#library(Hmisc)
#cWP[Year %in% c(1997, 2001, 2007, 2013, 2018) & Country %in% c("United States", "Canada", "Mexico")] %>% 
#ggplot(aes(Country, pctWiP))+
 # geom_violin()+
  #geom_boxplot(width = .05)+
  #stat_summary(fun.data="mean_sdl", mult=1, 
   #            geom="crossbar", width=0.2 )

cWP %>% 
  filter(Country %in% c("Antigua and Barbuda", "Bahamas, The", "Barbados", "Belize", "Canada", "Costa Rica", "Cuba"))



library(ggridges)
cWP[Country %in% c("United States", "Mexico", "Canada",
                   "Haiti", "Guatemala", "Cuba", "Dominican Republic", "Honduras", "El Salvador", "Nicaragua")& !Year == "1990"] %>% 
  ggplot(aes(y=Country, x=pctWiP))+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")