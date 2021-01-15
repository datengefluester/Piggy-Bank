Analysis of a (German) Piggy Bank
================

## Packages

``` r
library(tidyverse)
```

## Theme

``` r
hp_theme <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75,
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0",
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0,
                                       lineheight = 0.9, margin = margin(),
                                       debug = FALSE),
      plot.margin =       margin(12,10,5,10),
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(0.8), family = '' ,
                                       face = 'bold', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B'),
      plot.subtitle =     element_text(size = rel(0.6), family = '' ,
                                       face = 'plain', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B', 
                                       margin = margin(0,0,15,0)),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )
}
```

## Read in data

``` r
raw <- read.csv("raw.csv")
```

## Create new variables

``` r
# Create value for coin / year combination
d <- raw %>% 
  rowwise() %>% 
  mutate(Count=sum(c_across(Germany:Cyprus))) %>% 
  mutate(Value = Count*Coin) 
```

## Some initial analyses

``` r
# How many coins were in the piggy bank? 671
coin_count <- sum(d$Count)
coin_count
```

    ## [1] 671

``` r
# How much are the coins worth? 52.1€
money_worth <- sum(d$Value)
money_worth
```

    ## [1] 52.1

``` r
# Average Value per coin? 7.8 Cents
money_worth/coin_count
```

    ## [1] 0.07764531

## Contribution to Piggy Bank by:

### Coin

``` r
d %>% 
  group_by(Coin) %>% 
  summarise(Value = sum(Value),
            Count = sum(Count)) %>% 
  mutate(Value_Percent = (Value/money_worth)*100,
         Count_Percent = (Count/coin_count)*100) %>% 
  pivot_longer(cols=c('Value_Percent', 'Count_Percent'), names_to='variable', 
               values_to="value") %>% 
  ggplot(aes(x=as.factor(Coin),y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 62), 
                     breaks = c(seq(10,60,10)) , 
                     expand = c(0, 0), 
                     labels=c("0"="10","20"="20","30"="30","40"="40",
                              "50"="50","60" = "60%")) +
  scale_x_discrete(labels=c("1"="1€","0.5"="50c","0.2"="20c",
                            "0.1"="10c","0.05"="5c","0.02"="2c" ,"0.01"="1c")) +
  scale_fill_manual(breaks=c("Value_Percent","Count_Percent"), 
                    values=c("#1b9e77", "#d95f02"),labels = c("Value", "Count"))  +
  labs(title = "Contribution to Piggy Bank by Coin",
       subtitle = "In Percent. Total Money: 52.1€. Total Coins: 671.",
       caption = "Source: Piggy Bank") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), 
                     plot.title.position = "plot", 
                     axis.title.y = element_blank(), 
                     panel.grid.major.x = element_line(size=.2, color="#656565"), 
                     panel.grid.major.y = element_blank(), 
                     axis.line.x=element_line( size=.3, color="black"), 
                     legend.position = c(0.88, 0.98), 
                     legend.text=element_text(size=8),
                     legend.title=element_blank(), 
                     legend.key.height= unit(0.4,"line"),
                     legend.key = element_blank(), 
                     axis.ticks.y = element_blank(), 
                     axis.ticks.x =element_line( size=.3, color="black"), 
                     plot.caption=element_text(size=7), 
                     axis.text.x=element_text(color="black"))
```

![](README_figs/coin-1.png)<!-- -->

### Year

``` r
d %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value),
            Count = sum(Count)) %>% 
  mutate(Value_Percent = round((Value/money_worth)*100, digits = 2),
         Count_Percent = round((Count/coin_count)*100, digits = 2)) %>% 
  pivot_longer(cols=c('Value_Percent', 'Count_Percent'), names_to='variable', 
               values_to="value") %>% 
ggplot(aes(x=Year,y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  scale_y_continuous(limits = c(0, 36), 
                     breaks = c(seq(5,35,10)), 
                     expand = c(0, 0),
                     labels=c("5"="5","15"="15",
                              "25" = "25","35"="35%")) +
  scale_x_continuous(limits=c(1998,2021),
                     expand = c(0, 0)) +
  labs(title = "Contribution to Piggy Bank by Years of Minting",
       subtitle = "In Percent. Total Money: 52.1€. Total Coins: 671.",
       caption = "Source: Piggy Bank") +
  scale_fill_manual(breaks=c("Value_Percent","Count_Percent"), 
                    values=c("#1b9e77", "#d95f02"),labels = c("Value", "Count"))  +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), 
                     plot.title.position = "plot", 
                     axis.title.y = element_blank(), 
                     panel.grid.major.x = element_blank(),
                     panel.grid.major.y = element_line(size=.2, color="#656565"), 
                     axis.line.x=element_line( size=.3, color="black"), 
                     axis.line.y=element_blank(),
                     legend.position = c(0.9, 0.95), 
                     legend.text=element_text(size=8),
                     legend.title=element_blank(), 
                     legend.key.height= unit(0.4,"line"),
                     legend.key = element_blank(), 
                     axis.ticks.y = element_blank(), 
                     axis.ticks.x =element_line( size=.3, color="black"), 
                     plot.caption=element_text(size=7),
                     axis.text.x=element_text(color="black"))
```

![](README_figs/year-1.png)<!-- -->

### Country

#### preparation

``` r
# Country data frame
country <- d %>% 
  select(-c(Value,Count)) %>% 
  gather(Germany:Cyprus, key = Country, value = Count) %>% 
  mutate(Value=Count*Coin) %>%
  mutate(Germany=0) %>% 
  mutate(Germany=replace(Germany,Country=="Germany",1)) %>% 
  group_by(Germany)
```

#### Germany vs. Rest

``` r
country %>% 
  summarise(Value = sum(Value),
            Count = sum(Count)) %>% 
  mutate(Value_Percent = round((Value/money_worth)*100, digits = 2),
         Count_Percent = round((Count/coin_count)*100, digits = 2)) %>% 
  pivot_longer(cols=c('Value_Percent', 'Count_Percent'), names_to='variable', 
               values_to="value") %>% 
  arrange(desc(Germany)) %>% 
  ggplot(aes(x=as.factor(Germany),y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 81), 
                     breaks = c(seq(20,80,20)), 
                     expand = c(0, 0),
                     labels=c("20"="20","40"="40",
                              "60" = "60","80"="80%")) +
  scale_x_discrete(labels=c("1"="Germany","0"="All Other \n Countries")) +
  scale_fill_manual(breaks=c("Value_Percent","Count_Percent"), 
                    values=c("#1b9e77", "#d95f02"),labels = c("Value", "Count"))  +
  labs(title = "Contribution to Piggy Bank by Country",
       subtitle = "In Percent. Total Money: 52.1€. Total Coins: 671.",
       caption = "Source: Piggy Bank") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), 
                     plot.title.position = "plot", 
                     axis.title.y = element_blank(), 
                     panel.grid.major.x = element_line(size=.2, color="#656565"), 
                     panel.grid.major.y = element_blank(), 
                     axis.line.x=element_line( size=.3, color="black"), 
                     axis.line.y=element_line( size=.3, color="#656565"), 
                     legend.position = c(0.87, 0.3), 
                     legend.text=element_text(size=6),
                     legend.title=element_blank(), 
                     legend.key.height= unit(0.4,"line"),
                     legend.key = element_blank(), 
                     axis.ticks.y = element_blank(), 
                     axis.ticks.x =element_line( size=.3, color="black"), 
                     plot.caption=element_text(size=7),
                     axis.text.x=element_text(color="black"))
```

![](README_figs/germany-1.png)<!-- -->

``` r
### Rest among themselves
country %>% 
  ungroup() %>% 
  filter(Country!="Germany") %>% 
  select(-Germany) %>% 
  group_by(Country) %>% 
  summarise(Value = sum(Value),
         Count = sum(Count)) %>% 
  mutate(Value_Percent = round((Value/sum(Value))*100, digits = 2),
         Count_Percent = round((Count/sum(Count))*100, digits = 2)) %>% 
  pivot_longer(cols=c('Value_Percent', 'Count_Percent'), names_to='variable', 
               values_to="value") %>% 
  arrange(desc(value)) %>% 
  mutate(Country= as.factor(Country)) %>% 
  ggplot(aes(x=reorder(Country, value),y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 25.1), 
                     breaks = c(seq(5,25,5)), 
                     expand = c(0, 0),
                     labels=c("5"="5","10"="10",
                              "15" = "15","20"="20","25"="25%")) +
  scale_x_discrete(labels=c("1"="Germany","0"="All Other")) +
  scale_fill_manual(breaks=c("Value_Percent","Count_Percent"), 
                    values=c("#1b9e77", "#d95f02"),labels = c("Value", "Count"))  +
  labs(title = "Contribution to Piggy Bank by Country \n Other Than Germany",
       subtitle = "In Percent of total. Total Money: 34.86€. Total Coins: 296.",
       caption = "Source: Piggy Bank") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(), 
                     plot.title.position = "plot", 
                     axis.title.y = element_blank(), 
                     panel.grid.major.x = element_line(size=.2, color="#656565"), 
                     panel.grid.major.y = element_blank(), 
                     axis.line.x=element_line( size=.3, color="black"), 
                     axis.line.y=element_line( size=.3, color="#656565"), 
                     legend.position = c(0.89, 0.7), 
                     legend.text=element_text(size=6),
                     legend.margin=margin(t = 0, unit='cm'),
                     legend.title=element_blank(), 
                     legend.key.height= unit(0.4,"line"),
                     legend.key = element_blank(), 
                     axis.ticks.y = element_blank(), 
                     axis.ticks.x =element_line( size=.3, color="black"), 
                     plot.caption=element_text(size=7),
                     axis.text.x=element_text(color="black"))
```

![](README_figs/rest-1.png)<!-- -->
