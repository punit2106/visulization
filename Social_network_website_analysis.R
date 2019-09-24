library(tidyverse)
library(gganimate)
library(ggplot2)
library(dplyr)


options(scipen =999)

users_social_media <- read_csv("users-by-social-media-platform.csv")



Users_formatted <- users_social_media %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Monthly_active_users),
         Value_rel = Monthly_active_users/Monthly_active_users[rank==1],
         Value_lbl = paste0(" ",round(Monthly_active_users/1e7))) %>%
  group_by(Entity) %>%
  filter(rank <=10) %>%
  ungroup()


anim <- ggplot(Users_formatted, aes(rank, group = Entity, 
                                  fill = as.factor(Entity), color = as.factor(Entity))) +
  geom_tile(aes(y = Monthly_active_users/2,
                height = Monthly_active_users,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Entity, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Monthly_active_users,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(Year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Social media users : {closest_state}',  
       subtitle  =  "Top 10 social network websites",
       caption  = "No of users changing every year")