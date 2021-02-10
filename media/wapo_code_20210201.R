#This code pulls together all the data and plots for my op-ed related to rental assistance distribution

library(tidycensus)
library(tidyverse)
library(ggrepel)
census_api_key("add your Census API key here", install="TRUE") 

a <- get_acs(geography = "state",
             variables = c(pop = "B01001_001",
                           total_hu = "B25003_001",
                           rent_hu = "B25003_003",
                           rent = "B25064_001"),
             year = 2019)

#moving to wide format
a <- a %>%
  pivot_wider(names_from = variable, 
              names_sep = "_", 
              values_from = c(estimate, moe))

#manually entering maximum state allocations from Treasury: 
#https://home.treasury.gov/system/files/136/Emergency-Rental-Assistance-Data-and-Methodology-1-11-21.pdf
a$era <- c(326358801.2,200000000,492131217.2,200961311.8,2610593356.2,385124024.5,235873751.1,200000000,
           200000000,1441188973.4,710207372.2,200000000,200000000,834709842.6,447937423.4,209783452.7,
           200000000,296897443.5,308042376.6,200000000,401575013.8,457129720.3,660906592.1,375152158.5,
           200000000,407924164.8,200000000,200000000,208105615.3,200000000,589011704.4,200000000,
           1282268820.9,702966451.5,200000000,775405764.4,263975438.5,281264683.4,847688778.8,200000000,
           346020970.5,200000000,456682774.5,1946983603.8,215507410,200000000,569661203.5,510182193.1,
           200000000,386777591.5,200000000,325000000)

#remove Puerto Rico, calculate some derived variables
a <- a %>% 
  filter(NAME != "Puerto Rico") %>%
  mutate(renter_share = (estimate_rent_hu / estimate_total_hu)*100,
         era_per_rent_hu = era/estimate_rent_hu,
         small_state_min = factor(ifelse(era == 200000000,1,0)))

summary(a$renter_share)
summary(a$era_per_rent_hu)
sum(a$era)/sum(a$estimate_rent_hu) #average would be $547 if split evenly across all renters
sum(a$estimate_rent_hu)/sum(a$estimate_total_hu) #36% renters across the whole country

#plotting share of HHs that are renters by overall pop
print(a %>%
        arrange(estimate_pop) %>%
        select(NAME,estimate_pop,estimate_rent_hu,renter_share,era,estimate_rent,era_per_rent_hu),n=51)

p <- ggplot(a, aes(x=estimate_pop, y = renter_share/100))
p + geom_point() +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Total State Population") + 
  ylab("Share of Households that Rent") +
  ggtitle("Larger States have Proportionately more Renters") +
  labs(caption = "Data drawn from 2015-2019 ACS; x-axis is on a logarithmic scale") +
  geom_text_repel(data = subset(a, NAME %in% 
                                  c("District of Columbia","Nevada","Utah","New York","California","Florida","Texas","Vermont")),aes(label=NAME))

#plotting rental assistance per rental housing unit by the total number of rental units
p <- ggplot(a, aes(x=estimate_rent_hu, y = era_per_rent_hu))
p + geom_point() +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("Number of Renter Households") + 
  ylab("Rental Assistance per Renter Household") +
  ggtitle("Per-Household Rental Aid Declines in States with more Renters") +
  labs(caption = "Data drawn from 2015-2019 ACS; x-axis is on a logarithmic scale") +
  geom_text_repel(data = subset(a, NAME %in% 
                                  c("District of Columbia","Nevada","Utah","New York","California","Florida","Texas","Vermont")),aes(label=NAME))


#histogram of era per renter
a$state<-as.factor(a$NAME)
p <- ggplot(a, aes(x = fct_rev(reorder(state, era_per_rent_hu, na.rm=TRUE)), 
                   y = era_per_rent_hu))
p + geom_col() +
  scale_y_continuous(labels = scales::dollar_format(), expand = c(0, 0)) +
  xlab("") + 
  ylab("Rental Assistance per Renter Household") +
  ggtitle("Per-Household Rental Aid varies across the States") +
  labs(caption = "Data drawn from 2015-2019 ACS") +
  coord_cartesian(ylim = c(0,3000)) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(size=10))
