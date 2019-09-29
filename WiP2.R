library(data.table)
library(here)
library(dplyr)
library(tidyverse)
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

library(countrycode)
cl <- as.data.table(codelist)[, .(continent, wb)]
setnames(cl, c("continent", "wb"),
         c("Continent", "Code"))
WP2 <- cl[WP, on="Code"]



WPN <- WP2 %>% 
mutate(Continent=replace(Continent, 
                         Country %in% c("Antigua and Barbuda", "Bahamas, The", "Barbados", "Belize", "Canada", "Caribbean small states", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Honduras", "Haiti", "Jamaica", "Mexico", "Nicaragua", "Panama", "Puerto Rico", "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", "Trinidad and Tobago", "United States"),
                         "North America")) 

WPN <- WPN %>%   
mutate(Continent=replace(Continent, 
                         Country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela, RB"), 
                         "South America"))

WPN <- WPN %>% 
  filter(Year != 1990)


library(ggplot2)
library(magrittr)

WPN %>%
  filter(Continent == "North America") %>% 
  ggplot(aes(Year, pctWiP, color=Country)) +
  geom_line() + geom_point() +
  scale_y_continuous(limits=c(0, 50)) +
  ylab("% Women in Parliament") +
  ggtitle("Women in Parliament: North America")


WPN %>%
  filter(Country %in% c("United States", "Mexico", "Canada",
                        "Haiti", "Guatemala", "Cuba", "Dominican Republic", "Honduras", "El Salvador", "Nicaragua")) %>% 
  ggplot(aes(Year, pctWiP, color=Country)) +
  geom_line() + geom_point() +
  scale_y_continuous(limits=c(0, 50)) +
  ylab("% Women in Parliament") +
  ggtitle("Women in Parliament: North America") +
  scale_colour_manual(values = rainbow(10)) +
  theme_minimal() +
  theme(legend.position = "bottom")


orderCountry <- WPN %>% filter(Country %in% c("United States", "Mexico", "Canada",
                                              "Haiti", "Guatemala", "Cuba", "Dominican Republic", "Honduras", "El Salvador", "Nicaragua") & Year == 2018)  %>% 
                arrange(pctWiP) %>% 
                select(Country)

orderCountry <- as.character(orderCountry)

library(ggridges)
library(viridis)
WPN %>%
  filter(Country %in% c("United States", "Mexico", "Canada",
                        "Haiti", "Guatemala", "Cuba", "Dominican Republic", "Honduras", "El Salvador", "Nicaragua"))  %>% 
  ggplot(aes(y=Country, x=pctWiP, fill=..x..))+
  geom_density_ridges_gradient() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(limits=c("Haiti", "Guatemala", "United States", "Honduras", "Dominican Republic", "Canada", "El Salvador", "Nicaragua", "Mexico", "Cuba")) +
  scale_fill_viridis() +
  xlab("% Women in Parliament") +
  ggtitle("Women in Parliament: North America (Since 1997)") +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5))
  
