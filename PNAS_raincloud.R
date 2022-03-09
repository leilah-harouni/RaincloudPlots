#  Load data
conversations <- read.csv("/Users/lh689/Desktop/Micro/Michael/Conversations/Data/ConvoSocietyPlots.csv")

# Width and height variables for saved plots
w = 11
h = 6

# Make a figures folder if it doesn't exist yet
dir.create('/Users/lh689/Desktop/figures', showWarnings = FALSE)

# Create new dataset for BWwealth
pre = conversations$BWwealth1
post = conversations$BWwealth2
follow1 = conversations$BWwealth3
follow2 = conversations$BWwealth4
condition = conversations$condition
n <- length(pre)
BWwealth_data <- data.frame(Amount = c(pre, post, follow1, follow2), #BWwealth
                            Time = rep(c(1,2,3,4), each=n), #time
                            Condition = condition, #condition
                            Participant.ID = factor(rep(1:n,4))) #participantID

# Delete rows that have NA under Condition
BWwealth <- BWwealth_data %>% drop_na(Condition)

# Add some jitter to avoid that datapoints overlap
set.seed(321)
BWwealth$jit <- jitter(BWwealth$Time, amount=0.08)

# Delete NA under Amount - Can't make a summary table if there are NAs
BWwealth_clean <- BWwealth %>% drop_na(Amount)

# Summary of data
BWwealth_summary <- summarySE(BWwealth_clean, measurevar="Amount", groupvars=c("Condition", "Time"), na.rm=TRUE)

# Create labels for columns and rows
col_labels <- as_labeller(c('1' = "Story", '2' = "Data", '3' = "Combined"))
row_labels <- as_labeller(c('1' = "Pre", '2' = "Post", '3' = "Follow-up1", '4' = "Follow-up2"))

# BWwealth - Plot with all details
BWwealth_F1 <- ggplot(BWwealth_clean, aes(x = Time, y = Amount)) +
  geom_point(aes(x = jit, color = factor(Condition)), size = .5, position = position_nudge (x = .1)) +
  geom_line(aes(x = jit, group = Participant.ID), position = position_nudge (x = .1), color = 'lightgray', alpha = .2) +
  geom_point(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition)), size = .9, position = position_nudge(x = -.05)) +
  geom_errorbar(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition), 
                                             ymin = Amount-se, ymax = Amount+se), width = .05, size = .5, alpha = .6, position = position_nudge(x = -.05)) +
  geom_line(data = BWwealth_summary, aes(x = Time, y = Amount), size = .5, color = 'black', position = position_nudge(x = -.05)) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "1"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "2"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "3"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "4"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  labs(x = "Time", y = "Estimated Black Wealth When White Wealth is $100") +
  theme_classic() +
  facet_wrap(~ Condition, ncol = 3, labeller = col_labels) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_discrete(name = "Condition", labels = c("Story", "Data", "Combined")) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("Pre", "Post", "Follow-\nup 1", "Follow-\nup 2")) +
  scale_y_continuous(labels = scales::dollar_format())
BWwealth_F1

ggsave('/Users/lh689/Desktop/figures/BWwealth_F1.png', width = w, height = h)

#BWwealth - Plot filled
BWwealth_F2 <- ggplot(BWwealth_clean, aes(x = Time, y = Amount)) +
  geom_point(data = BWwealth_summary, aes(x = Time, y = Amount, fill = factor(Condition)), size = .5, position = position_nudge(x = -.05)) +
  geom_errorbar(data = BWwealth_summary, aes(x = Time, y = Amount, fill = factor(Condition), 
                                             ymin = Amount-se, ymax = Amount+se), width = .05, size = .5, alpha = .6, position = position_nudge(x = -.05)) +
  geom_line(data = BWwealth_summary, aes(x = Time, y = Amount), size = .5, color = 'black', position = position_nudge(x = -.05)) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1"),
               aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2"),
               aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3"),
               aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4"),
               aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .2) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "1"),
                   aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "2"),
                   aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "3"),
                   aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "4"),
                   aes(x = Time, y = Amount, fill = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  labs( x = "Time", y = "Estimated Black Wealth When White Wealth is $100") +
  theme_classic() +
  facet_wrap(~ Condition, ncol = 3, labeller = col_labels) +
  theme( panel.spacing = unit(2, "lines")) +
  scale_fill_discrete(name = "Condition", labels = c("Story", "Data", "Combined")) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("Pre", "Post", "Follow-\nup 1", "Follow-\nup 2")) +
  scale_y_continuous(labels = scales::dollar_format())
BWwealth_F2

ggsave('/Users/lh689/Desktop/figures/BWwealth_F2.png', width = w, height = h)

# BWwealth - On top of each other
BWwealth_F3 <- ggplot(BWwealth_clean, aes(x = Time, y = Amount)) +
  geom_point(aes(x = jit, color = factor(Condition)), size = .5, position = position_nudge (x = .1)) +
  geom_line(aes(x = jit, group = Participant.ID), position = position_nudge (x = .1), color = 'lightgray', alpha = .2) +
  geom_point(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition)), size = .9, position = position_nudge(x = -.025)) +
  geom_errorbar(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition), 
                                             ymin = Amount-se, ymax = Amount+se), width = .05, size = .5, alpha = .6, position = position_nudge(x = -.025)) +
  geom_line(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition)), size = .5, position = position_nudge(x = -.025)) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1", Condition == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1", Condition == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2", Condition =="1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2", Condition =="3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3", Condition == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3", Condition == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4", Condition == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4", Condition == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "1"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.4),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "2"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.4),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "3"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.4),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "4"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.4),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  labs(x = "Time",  y = "Estimated Black Wealth When White Wealth is $100") +
  theme_classic() +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_discrete(name = "Condition", labels = c("Story", "Data", "Combined")) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("Pre", "Post", "Follow-\nup 1", "Follow-\nup 2")) +
  scale_y_continuous(labels = scales::dollar_format())
BWwealth_F3

ggsave('/Users/lh689/Desktop/figures/BWwealth_F3.png', width = w, height = h)


# BWwealth - On top of each other with out datapoints
BWwealth_F4 <- ggplot(BWwealth_clean, aes(x = Time, y = Amount)) +
  geom_point(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition)), size = .9, position = position_nudge(x = .1)) +
  geom_errorbar(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition), 
                                             ymin = Amount-se, ymax = Amount+se), width = .05, size = .5, alpha = .6, position = position_nudge(x = .1)) +
  geom_line(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition)), size = .5, position = position_nudge(x = .1)) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1", Condition == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1", Condition == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = 0),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2", Condition =="1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2", Condition =="3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = 0),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3", Condition == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3", Condition == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = 0),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4", Condition == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4", Condition == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.1),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4", Condition == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = 0),
               outlier.shape = NA, alpha = .5, width = .06) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "1"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "2"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "3"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWincome_clean %>% filter(Time == "4"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.3),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  labs(x = "Time",  y = "Estimated Black Wealth When White Wealth is $100") +
  theme_classic() +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_discrete(name = "Condition", labels = c("Story", "Data", "Combined")) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("Pre", "Post", "Follow-\nup 1", "Follow-\nup 2")) +
  scale_y_continuous(labels = scales::dollar_format())
BWwealth_F4

ggsave('/Users/lh689/Desktop/figures/BWwealth_F4.png', width = w, height = h)


# BWwealth - Facet grid by row
BWwealth_F5 <- ggplot(BWwealth_clean, aes(x = Time, y = Amount)) +
  geom_point(aes(x = jit, color = factor(Condition)), size = .1, position = position_nudge (x = .1)) +
  geom_line(aes(x = jit, group = Participant.ID), position = position_nudge (x = .1), color = 'lightgray', alpha = .2) +
  geom_point(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition)), size = .2, position = position_nudge(x = -.05)) +
  geom_errorbar(data = BWwealth_summary, aes(x = Time, y = Amount, color = factor(Condition), 
                                             ymin = Amount-se, ymax = Amount+se), width = .03, size = .3, alpha = .6, position = position_nudge(x = -.05)) +
  geom_line(data = BWwealth_summary, aes(x = Time, y = Amount), color = "black", size = .4, position = position_nudge(x = -.05)) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "1"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .1) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "2"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .1) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "3"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .1) +
  geom_boxplot(data = BWwealth_clean %>% filter(Time == "4"),
               aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.2),
               outlier.shape = NA, alpha = .5, width = .1) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "1"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "2"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "3"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_half_violin(data = BWwealth_clean %>% filter(Time == "4"),
                   aes(x = Time, y = Amount, color = factor(Condition)), position = position_nudge(x = -.35),  
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  labs(x = "Time",  y = "Estimated Black Wealth When White Wealth is $100") +
  facet_grid(rows = vars(Condition), labeller = labeller(Condition = col_labels)) +
  theme_classic() +
  theme(panel.spacing = unit(.5, "lines")) +
  scale_color_discrete(name = "Condition", labels = c("Story", "Data", "Combined")) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("Pre", "Post", "Follow-up 1", "Follow-up 2")) +
  scale_y_continuous(labels = scales::dollar_format())
BWwealth_F5

ggsave('/Users/lh689/Desktop/figures/BWwealth_F5.png', width = nw, height = nh)
