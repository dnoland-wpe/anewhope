# Import libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(keyring)
library(RMySQL)
library(sqldf)

# Create SQL connection
system(key_get("ssh"))

con <- dbConnect(MySQL(),
                 user = key_get("sql_username"),
                 password = key_get("sql_password"),
                 host = '127.0.0.1',
                 port = 3307, 
                 dbname = 'metrics')

query <- function(...) dbGetQuery(con, ...)

options(sqldf.driver = "SQLite")

# Set working directory - add project folder
setwd("~/Documents/Github/cxbi/dnoland/Plugins Analysis/")

# Import all active plugins and save to a .csv file for future analysis
# active_plugins <- query("SELECT * FROM `customer_master`.active_plugins;")
# write.csv(active_plugins, "active_plugins.csv")
active_plugins <- read.csv("active_plugins.csv")

# Drop WP Engine Platform plugin and Site Migration plugin 
active_plugins_trimmed <- active_plugins %>% filter(!plugin_name %in% c("wpengine-common/plugin.php", 
                                                                        "wp-site-migrate/blogvault.php",
                                                                        "wp-site-migrate/wpengine.php", 
                                                                        "wpengine-snapshot/plugin.php",
                                                                        "wp-engine-snapshot/plugin.php"))

# Top 10 plugins on platform
top10_plugins <- sqldf("SELECT plugin_title, count(plugin_title) as 'cnt'
                        FROM active_plugins_trimmed
                        GROUP BY plugin_name
                        ORDER BY cnt DESC
                        LIMIT 10;")


#total number of plugins 
num_plugins = active_plugins_trimmed %>% nrow()
format(num_plugins, big.mark = ",")

top_plugins <- top10_plugins %>% 
                    select(plugin_title)
top_plugins_ungrouped <- top_plugins %>% 
                             left_join(active_plugins_trimmed) %>%
                             select(plugin_title)
top_plugins_ungrouped$plugin_title <- factor(as.ordered(unique(top_plugins_ungrouped$plugin_title)), 
                                             levels = c(top_plugins_ungrouped$plugin_title))

top_plugins_ungrouped %>%
    ggplot(aes(plugin_title)) +
    geom_bar(fill = "red") +
    ggtitle("Count of Top 10 Plugins on Platform") +
    theme(legend.position = "none",
          axis.text.x = element_blank()) +
    geom_text(stat = "count", aes(label = ..count..), hjust = 2) +
    coord_flip()
