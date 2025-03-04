library(ggplot2);library(dplyr);ibrary(tidyr)
setwd('D:/work/Gamma/')
df <- read.csv('data/TEMP/sp_summary.csv')
tmp <- read.csv('data/spls/sp_assess_all.csv')
tmp <- select(tmp,speciesKey,num_coords)
df <- left_join(df,tmp)
colnames(df)[2:3] <- c('raw','cleaned')

df_long <- pivot_longer(df, cols = c(cleaned, raw), names_to = "type", values_to = "value")
ggplot(df_long) +
  geom_histogram(aes(x = value, fill = type, color = type),
                 alpha = 0.5, bins = 30) +
  scale_x_log10() +
  theme_classic() +
  labs(x = "Number of records", y = "Number of species", fill = "Data Type", color = "Data Type") +
  # 自定义填充颜色和边框颜色
  scale_fill_manual(values = c("cleaned" = "lightgreen", "raw" = NA)) +
  scale_color_manual(values = c("cleaned" = "lightgreen", "raw" = "black"))

# brks <- seq(1,25)
# ggplot(df) +
#   geom_histogram(aes(x = log2(cleaned)), fill = "lightgreen", color = "lightgreen", 
#                  alpha = 0.5, bins = 30) +  
#   geom_histogram(aes(x = log2(raw)), fill = NA, color = "black", 
#                  bins = 30) + 
#   scale_x_continuous(breaks = brks,labels = 2^brks)+
#   theme_classic()+
#   labs(x = "Number of records", y = "Number of species")
ggplot(df) +
  geom_histogram(aes(x = cleaned), fill = "lightgreen", color = "lightgreen",
                 alpha = 0.5, bins = 30) +
  geom_histogram(aes(x = raw), fill = NA, color = "black",
                 bins = 30) +
  scale_x_log10()+
  theme_classic()+
  labs(x = "Number of records", y = "Number of species")
ggsave('outputs/sup/dataSummary/hist.jpg',width = 5,height = 3)

df_long <- pivot_longer(df, cols = c(cleaned, raw), names_to = "type", values_to = "value")
ggplot(df_long) +
  geom_histogram(aes(x = value, fill = type, color = type),
                 alpha = 0.5, bins = 30) +
  scale_x_log10() +
  theme_classic() +
  labs(x = "Number of records", y = "Number of species", fill = "Data Type", color = "Data Type") +
  scale_fill_manual(values = c("cleaned" = "lightgreen", "raw" = 'transparent')) +
  scale_color_manual(values = c("cleaned" = "lightgreen", "raw" = "black"))
