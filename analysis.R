library(dplyr)
library(tidyr)
library(maps)
library(usmap)
library(ggplot2)

jail_pop <- read.csv("data/incarceration_trends.csv")

# 7 relevant values of interest
spec_year_pop <- filter(jail_pop, year %in%  c("1990", "1991", "1992", "1993",
                                               "1994", "1995", "1996", "1997",
                                               "1998", "1999", "2000", "2001",
                                               "2002", "2003", "2004", "2005",
                                               "2006", "2007", "2008", "2009",
                                               "2010", "2011", "2012", "2013",
                                               "2014", "2015", "2016", "2017",
                                               "2018"))
spec_year_pop[is.na(spec_year_pop)] <- 0

state_jail_pop_grouped <- spec_year_pop %>%
  group_by(state) %>%
  summarize(state_black_jail_pop = sum(black_jail_pop),
            state_white_jail_pop = sum(white_jail_pop))
max_state_black_jail_pop <- max(state_jail_pop_grouped$state_black_jail_pop)
max_state_white_jail_pop <- max(state_jail_pop_grouped$state_white_jail_pop)
name_max_state <- filter(state_jail_pop_grouped,
                         state_black_jail_pop == max_state_black_jail_pop)

year_jail_pop_grouped <- spec_year_pop %>%
  group_by(year) %>% 
  summarize(tot_aapi_jail_pop = sum(aapi_jail_pop),
            tot_black_jail_pop = sum(black_jail_pop),
            tot_latinx_jail_pop = sum(latinx_jail_pop),
            tot_native_jail_pop = sum(native_jail_pop),
            tot_white_jail_pop = sum(white_jail_pop))

race_jail_pop_1990 <- filter(year_jail_pop_grouped, year == "1990")
race_jail_pop_2018 <- filter(year_jail_pop_grouped, year == "2018")

aapi_jail_pop_diff <- race_jail_pop_2018$tot_aapi_jail_pop -
  race_jail_pop_1990$tot_aapi_jail_pop
black_jail_pop_diff <- race_jail_pop_2018$tot_black_jail_pop -
  race_jail_pop_1990$tot_black_jail_pop
latinx_jail_pop_diff <- race_jail_pop_2018$tot_latinx_jail_pop -
  race_jail_pop_1990$tot_latinx_jail_pop
native_jail_pop_diff <- race_jail_pop_2018$tot_native_jail_pop -
  race_jail_pop_1990$tot_native_jail_pop
white_jail_pop_diff <- race_jail_pop_2018$tot_white_jail_pop -
  race_jail_pop_1990$tot_white_jail_pop

# Trends over time chart
spec_states_pop <- filter(spec_year_pop, state %in% c("CA", "TX", "FL", "NY", 
                                                      "PA", "IL", "OH", "GA", 
                                                      "NC", "MI"))
# aapi related with year and state, data from spec_state_pop,
# sum the population of aapi for the same year and state.
spec_states_black_pop <- aggregate(black_jail_pop~year+state,
                                   data = spec_states_pop, sum)

black_pop_graph <- ggplot(spec_states_black_pop, aes(x = year, y = black_jail_pop,
                                                     group = state, color= state)) + 
  geom_line() +
  ylab("Black People") +
  ggtitle("Black People Incarceration in 10 States from 1990 to 2018")

# 不同年份黑白人监狱条形比较图
natl_black_jail_pop <- aggregate(black_jail_pop~year, data = spec_year_pop, sum)
natl_white_jail_pop <- aggregate(white_jail_pop~year, data = spec_year_pop, sum)

natl_black_pop <- aggregate(black_pop_15to64~year, data = spec_year_pop, sum)
natl_white_pop <- aggregate(white_pop_15to64~year, data = spec_year_pop, sum)

natl_black_tot_pop <- merge(natl_black_jail_pop, natl_black_pop, by = "year")
natl_white_tot_pop <- merge(natl_white_jail_pop, natl_white_pop, by = "year")
natl_black_white_pop <- merge(natl_black_tot_pop, natl_white_tot_pop, by = "year")

# calculate the incarceration rate of black and white people
natl_black_white_inca_pop <- natl_black_white_pop %>%
  mutate(black_pop_inca_rate = black_jail_pop / black_pop_15to64 * 100000,
             white_pop_inca_rate = white_jail_pop / white_pop_15to64 * 100000)

natl_black_white_inca_pop <- select(natl_black_white_inca_pop, year,
                                    black_pop_inca_rate, white_pop_inca_rate)

natl_black_white_inca_rate <- gather(natl_black_white_inca_pop, key = "category",
                                     value = "inca_rate", 2:3)

black_white_inca_rate_graph <- ggplot(natl_black_white_inca_rate, aes(x = year,
                                      y = inca_rate, group = category,
                                      color= category)) + 
  geom_line() +
  ylab("Incarceration Rate") +
  ggtitle("National Black and White People Incarceration Rate Comparison")

# map, 不同种族监狱分布图
# Define a minimalist theme for maps
jail_pop_2018 <- filter(jail_pop, year == "2018")
loc_black_pop_2018 <- select(jail_pop_2018, state, black_pop_15to64)
loc_black_pop_2018 <- aggregate(black_pop_15to64~state, data = loc_black_pop_2018, sum)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )
# Coordinate system already present
usmap_black_dist <- plot_usmap(data = loc_black_pop_2018,
                               values = "black_pop_15to64", labels = TRUE) +
  scale_fill_continuous(low = "white", high = "dark blue", 
                        name = "Population (2018)",
                        label = scales::comma) +
  blank_theme +
  labs(title = "Distribution of Black Population in the United States")
