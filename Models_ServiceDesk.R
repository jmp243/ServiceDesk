# Jung Mee Park
# August 12, 2021
# predictive models

# modeling a few variables
mod1 <- glm(ChatDuration ~ WaitTime + chatTime, data = chats)
summary(mod1)
glmplot<-ggplot(mod1, aes(x= chatTime, y= ChatDuration/60, group=WaitTime)) +
  geom_line(size=1, aes(color=WaitTime/60)) + 
  geom_smooth()
glmplot

## chat time of day with as it affects duration and wait time
mod2 <- glm(ChatDuration ~ WaitTime + chatTime, data = chatD_out1)
summary(mod2)

mod2plot <- ggplot(mod2, aes(x = chatTime, y=ChatDuration/60, group = WaitTime)) +
  geom_point(size=1, aes(color=WaitTime/60)) 
  # geom_smooth()

mod2plot

##
mod3 <- glm(ChatDuration ~ chatTime, data = chatD_out1)
summary(mod3)

mod3plot <- ggplot(chatD_out1, aes(x=ChatDuration/60, y = chatTime, group = LiveChatDeployment.DeveloperName)) +
  geom_point(aes(color=LiveChatDeployment.DeveloperName), size=.5) +
  # geom_jitter()+
  stat_smooth(method = "loess", se = FALSE)

mod3plot 

### 2d density graph of chat dur and time of day
mod4plot <- ggplot(chatD_out, aes(x=ChatDuration/60, y = tm1.dechr, fill = LiveChatDeployment.DeveloperName)) +
  stat_density2d(aes(color=LiveChatDeployment.DeveloperName), size=.5) +
  # # geom_jitter()+
  # stat_smooth(method = "loess", se = FALSE)
  labs(x="Chat Duration (minutes)", y="Hour") 

mod4plot <- mod4plot + labs(title = "2D Density Graphy of Chat Duration by Time of Day", subtitle = "outliers removed", 
                            fill = "Live Chat Developer") +
  scale_y_continuous(breaks=seq(0,24,4)) +
  scale_color_discrete(name = "Live Chat Developer")
  # scale_x_discrete(guide = guide_axis(n.dodge=2))

mod4plot
### chat dur and wait time
mod5plot <- ggplot(chat_wait_out, aes(x=ChatDuration/60, y = WaitTime, group = LiveChatDeployment.DeveloperName)) +
  stat_density2d(aes(color=LiveChatDeployment.DeveloperName), size=.5) +
  # # geom_jitter()+
  # stat_smooth(method = "loess", se = FALSE)
  labs(x="Chat Duration (minutes)", y="Wait Time (seconds)") 

mod5plot <- mod5plot + labs(title = "2D Density Graphy of Chat Duration by Wait Time", subtitle = "outliers removed", 
                            fill = "Live Chat Developer")+
  scale_color_discrete(name = "Live Chat Developer")

mod5plot
# scale_x_discrete(guide = guide_axis(n.dodge=2))

# wait time and abandoned chats
# lapply(chats[, c("Abandoned", "WaitTime", "chatTime", "LiveChatDeployment.DeveloperName")], table)
# 
# ## three way cross tabs (xtabs) and flatten the table
# # ftable(xtabs(~ LiveChatDeployment.DeveloperName + Abandoned + WaitTime + chatTime,
# #              data = chats))
# 
# ggplot(chats, aes(x = chatTime, y = LiveChatDeployment.DeveloperName)) +
#   geom_boxplot(size = .75) +
#   geom_jitter(alpha = .5) +
#   facet_grid(Abandoned ~ WaitTime, margins = TRUE) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# setting NA to 0 for Abandoned chat data
chatAb_out[is.na(chatAb_out)] <- 0

# https://stats.idre.ucla.edu/r/dae/robust-regression/
library(MASS)
library(foreign)

summary(ols <- lm(Abandoned ~ WaitTime + LiveChatDeployment.DeveloperName, data = chatAb_out))

summary(ols2 <- lm(Abandoned ~ WaitTime, data = chatAb_out))

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols2, las = 1)

par(opar)

d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(chatAb_out, d1, r)
a[d1 > 4/51, ]

# for the 2nd ols
d2 <- cooks.distance(ols2)
r1 <- stdres(ols2)
a1 <- cbind(chatAb_out, d2, r1)
a1[d1 > 4/51, ]

#
rabs <- abs(r)
a <- cbind(chatAb_out, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]

### Turn Na's into 0
# chat_wait_out[is.na(chat_wait_out)] <- 0

# subset Abandoned data
Ab_chat_only <- chat_wait_out[chat_wait_out$Abandoned != 0, ] # no wait times

summary(chat_wait_out$Abandoned)

mod6 <- lm(Abandoned ~ WaitTime, data = chat_wait_out)
summary(mod6)
# not very useful
mod6plot <- ggplot(chat_wait_out, aes(x= WaitTime, y = Abandoned)) +
  geom_point() +
  geom_jitter()
  # stat_smooth(method = "loess", se = FALSE)
  labs(x="Number of Abandoned Chats", y="Wait Time (seconds)") 

mod6plot <- mod6plot + labs(title = "", subtitle = "outliers removed") 
  # scale_color_discrete(name = "Live Chat Developer")

mod6plot

##
mod7plot <- ggplot(chat_wait_out, aes(x=WaitTime, y = Abandoned, group = LiveChatDeployment.DeveloperName)) +
  geom_line(aes(color=LiveChatDeployment.DeveloperName), size=.5) +
  labs(x="Wait Time (seconds)", y="Number of Abandoned Chats") 

mod7plot <- mod7plot + labs(title = "Abandoned Chats by Wait Time", subtitle = "outliers removed") + 
  scale_color_discrete(name = "Live Chat Developer")

mod7plot
## lookig at other ways to interpret abandoned
library(GGally)

# model OSFA and day of the week and hour
# chat2 <- chat %>% filter(LiveChatDeployment.DeveloperName=="OSFA Chat") %>% dplyr::select(-LiveChatDeployment.DeveloperName)
# ggplot(chat, aes(x=day_of_the_week, y=tm1.dechr, size = LiveChatDeployment.DeveloperName)) +
#   geom_point(alpha=0.7)

chat %>%
  # arrange(desc(pop)) %>%
  # mutate(LiveChatDeployment.DeveloperName = factor(LiveChatDeployment.DeveloperName, LiveChatDeployment.DeveloperName)) %>%
  ggplot(aes(x=day_of_the_week, y=tm1.dechr, size = ChatDuration/60)) +
  geom_point(alpha=0.5) 
  # scale_size(range = c(.1, 24), name="Population (M)")

