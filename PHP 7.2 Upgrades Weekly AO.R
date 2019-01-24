# Lead Author/Developer/Analyst: David T. Noland
# Contributors:Justin Lim
# Date: 2019-01-15
# Relevant JIRA link: https://wpengine.atlassian.net/browse/482
# Description: PHP 7.2 upgrades weekly operational comms by upgrade date phase
# Data Requirements:
# Acceptance criteria:

# Import libraries
library(bigrquery)
library(dplyr)
library(keyring)
library(googlesheets)
library(magrittr)
library(readr)
library(readxl)
library(RMySQL)
library(sqldf)
library(tidyr)
library(tm)

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
setwd("~/Documents/Github/cxbi/dnoland/PHP 7_2 Upgrades 2019")

# Import accounts phased dataset
accounts_dated <- read.csv("jan15-17comms.csv", stringsAsFactors = FALSE)

# Add Region for mykobzar
accounts_dated$Region[accounts_dated$account_name=="mykobzar"] <- "AMER"

# Define Phases data frame
phase_dates = list(c("January 24, 2019", "January 31, 2019",
                "February 7, 2019", "February 14, 2019", "February 21, 2019", "February 28, 2019",
                "March 7, 2019", "March 14, 2019", "March 21, 2019", "March 28, 2019",
                "April 2, 2019"))
phase_numbers = list(c(1:11))
phases <- do.call(rbind, Map(data.frame, phase_numbers, phase_dates))
names(phases)[1] <- "Phase"
names(phases)[2] <- "upgrade_date"

#Import and update deferrals
deferrals <- gs_title("PHP 7.2 Conversion Deferral List") %>% gs_read(ws = 1, skip = 4, colnames = TRUE)
deferrals_trimmed <- deferrals %>% select(Account) %>% unique() #eliminate any duplicates
deferrals_trimmed$deferred <- TRUE
colnames(deferrals_trimmed)[1] <- "account_name"


accounts_dated_seperated <- accounts_dated %>% select(-account_name) %>% separate_rows(account_concat, sep = ",") %>% rename(account_name=account_concat)

accounts_dated_seperated_grouped<-sqldf("SELECT account_name,upgrade_date,region FROM accounts_dated_seperated GROUP BY account_name,upgrade_date")


# accounts_deferral_check <- accounts_dated %>%
#                                left_join(deferrals_trimmed, by = "account_name") %>%
#                                select(account_name, deferred, upgrade_date) %>%
#                                unique() %>%
#                                filter(deferred == TRUE)


accounts_with_deferrals <- accounts_dated_seperated_grouped %>%
                         left_join(deferrals_trimmed, by = "account_name")


accounts_redated <- accounts_with_deferrals %>%
                    mutate(upgrade_date = ifelse(is.na(deferred), upgrade_date,"March 28, 2019"))

# Add Phase Numbers to accounts_phased and prepare phased accounts to remerge back to complete account roles with concatenated email lists
accounts_phased <- accounts_redated %>%
                       left_join(phases, by = "upgrade_date") %>%
                       arrange(Phase)


# Import installs by account
accounts_installs <- query("SELECT account_id,
                                   account_name,
                                   plan,
                                   install_name,
                                   install_email AS user_email,
                                   install_first_name as first_name,
                                   cluster_id
                           FROM `customer_master`.`view_account_install`
                           WHERE account_canceled_on IS NULL
                           AND install_canceled_on IS NULL;")


#Get install php_version site config options
install_php_versions <- query('SELECT sco.`account_name` AS install_name,
                                      sco.`option_name`,
                                      sco.`option_value`,
                                      sco.`timestamp`
                              FROM `customer_master`.`site_config_options` sco
                              WHERE sco.`option_name` = "php_version"
                              AND sco.`timestamp` IN (SELECT max(a.timestamp)
                                                      FROM `customer_master`.`site_config_options` a
                                                      WHERE a.`option_name` = "php_version"
                                                      AND a.`account_name` = sco.`account_name`);')
# Cluster PHP Version
cluster_php_versions <- query("SELECT cluster_id,
                                      value as cluster_php_version
                               FROM `customer_master`.`server_meta`
                               WHERE name = 'php_apache_version';")

# Join cluster_php_version to accounts_installs on the cluster_id
install_php_versions_clusters <- accounts_installs %>%
                                     left_join(cluster_php_versions, by = "cluster_id")

install_php_versions_clusters <- rename(install_php_versions_clusters, php_version=cluster_php_version)


# Define PHP version by cluster_id
installs_php_option <- install_php_versions %>%
                           select(install_name, option_value) %>%
                           rename(sco_php_version = "option_value")

# Join cluster php version with install php version
installs_php_merged <- install_php_versions_clusters %>%
                             left_join(installs_php_option, by = "install_name") %>%
                             drop_na(cluster_id)

# Replace the next block with ifelse statement to change php_version
installs_php_list <- installs_php_merged %>%
                         mutate(php_version = ifelse(php_version != sco_php_version & !is.na(sco_php_version),
                                                         sco_php_version,
                                                         php_version))

# Exclude 7.2 installs from dataset
installs_non_72 <- installs_php_list %>% filter(php_version != "7.2") %>% select(-sco_php_version)

#Loading Mockdata to assign cohorts
mockdata <- read.csv("mockdata.csv")

mockdata_installs_non_72_joined<-installs_non_72 %>% left_join(mockdata) %>% select(-in_december,-plugin_name)

mockdata_installs_non_72_joined[is.na(mockdata_installs_non_72_joined)]<-FALSE

#Adding CSM/TSM Data
csmtsm <- read_csv("~/Downloads/csmtsm.csv")

mockdata_installs_non_72_csm_joined<-mockdata_installs_non_72_joined %>% left_join(csmtsm,by=c("account_id"="WPE Account ID")) %>% rename(csm=Id)

mockdata_installs_non_72_csm_joined<-mockdata_installs_non_72_csm_joined[,c(-10,-12,-13)]


#FORMAT CONTACTS
# Format users and contacts
user_portal <- read.csv("~/Desktop/CXBI GitHub/cxbi/dnoland/WP 5_0 Upgrade Comms Prep/user_portal_contacts.csv", stringsAsFactors = FALSE)


# Format user_portal data
up_owner_contacts <- user_portal %>% select(user_email,account_id,real_role,first_name) %>% filter(real_role=="Owner")
up_owner_contacts$install_name <- NA
up_owner_contacts$user_email <- sapply(up_owner_contacts$user_email, tolower)
up_owner_contacts$user_email <- sapply(up_owner_contacts$user_email, stripWhitespace)
up_owner_contacts$user_email <- sapply(up_owner_contacts$user_email, trimws)

# Format Technical contacts
technical_contacts <- accounts_installs %>% select(user_email,account_id,first_name,install_name)
technical_contacts$real_role <- 'Technical'
technical_contacts$user_email <- sapply(technical_contacts$user_email, tolower)
technical_contacts$user_email <- sapply(technical_contacts$user_email, stripWhitespace)
technical_contacts$user_email <- sapply(technical_contacts$user_email, trimws)


# Split in the beginning for more accurate rolesum
technical_contacts_split <- separate_rows(technical_contacts, user_email, sep = ",")

# Combine dataframes
up_tech_bind <- rbind(up_owner_contacts, technical_contacts_split)

# Clean and standardize useremails
up_tech_bind$user_email <- sapply(up_tech_bind$user_email, tolower)
up_tech_bind$user_email <- sapply(up_tech_bind$user_email, trimws)

# Calculates if an emails or both an Owner and Technical
role_sum <- sqldf("SELECT user_email,account_id,first_name,install_name,
                   SUM(CASE WHEN up.real_role='Owner' THEN 1 ELSE 0 END) as 'Owner',
                   SUM(CASE WHEN up.real_role='Technical' THEN 1 ELSE 0 END) as 'Technical',
                   group_concat(install_name) install_concat
                   FROM up_tech_bind up
                   GROUP BY account_id, user_email
                   HAVING Owner>0
                   OR Technical>0")

# If Owner default to that if not make Technical
role_sum_mutate <- role_sum %>%
  mutate(real_role = ifelse(Owner >= 1, "Owner", "Technical"))

emails <- role_sum_mutate %>% select(user_email, account_id, first_name, real_role, install_name,install_concat)
emails_account_mockdata_phase_joined<- sqldf("SELECT e.user_email, m.account_name, e.first_name, real_role,plan,m.install_name,m.csm,m.php_version,m.GREEN,ap.Phase,ap.upgrade_date,e.install_concat FROM emails e INNER JOIN mockdata_installs_non_72_csm_joined m ON e.account_id=m.account_id LEFT JOIN accounts_phased ap ON ap.account_name=m.account_name")

#account for install creation drift
emails_account_mockdata_phase_joined$Phase[is.na(emails_account_mockdata_phase_joined$Phase)]<-10
emails_account_mockdata_phase_joined$upgrade_date[is.na(emails_account_mockdata_phase_joined$upgrade_date)]<-"March 28, 2019"


# Join emails to accounts for owner users
owner_account_joined <- emails_account_mockdata_phase_joined %>%
                            select(-install_concat) %>%
                              filter(real_role == "Owner")

# Concatenate install list for owner users
owner_concat <- owner_account_joined %>%
                    group_by(user_email,first_name,account_name,GREEN,plan,csm,upgrade_date) %>%
                    summarise(install_concat = paste(install_name, collapse = ","))


owner_concat_spread<-owner_concat%>% spread(GREEN,install_concat) %>% rename(green="TRUE",red="FALSE")


# owner count / sanity check
owner_account_counter <- sqldf("SELECT account_name, user_email, count(user_email)
                               FROM owner_concat
                               GROUP BY account_name, user_email
                               ORDER BY count(user_email) DESC;")



# Join emails to account for technical user by install
technical_emails<-emails %>% filter(real_role=="Technical") %>% select(-install_name) %>% separate_rows(install_concat, sep = ",")

technical_emails<-technical_emails %>% rename(install_name=install_concat)

technical_installs_mockdata_phase_joined<- sqldf("SELECT e.user_email, m.account_name, e.first_name, real_role,plan,e.install_name,m.csm,m.php_version,m.GREEN,ap.Phase,ap.upgrade_date FROM technical_emails e INNER JOIN mockdata_installs_non_72_csm_joined m ON e.install_name=m.install_name LEFT JOIN accounts_phased ap ON ap.account_name=m.account_name")


#accounting for drift
technical_installs_mockdata_phase_joined$Phase[is.na(technical_installs_mockdata_phase_joined$Phase)]<-10
technical_installs_mockdata_phase_joined$upgrade_date[is.na(technical_installs_mockdata_phase_joined$upgrade_date)]<-"March 28, 2019"

technical_concat<-technical_installs_mockdata_phase_joined %>%
  group_by(user_email,first_name,account_name,GREEN,plan,csm,upgrade_date) %>%
  summarise(install_concat = paste(install_name, collapse = ","))

technical_concat_spread<-technical_concat%>% spread(GREEN,install_concat)%>% rename(green="TRUE",red="FALSE")


# Combine owner and technical contacts
owner_technical_bind <- rbind(owner_concat_spread, technical_concat_spread)

smb <- c("comped", "comped personal", "comped professional", "comped business", "sport", "personal", "scale", "growth", "startup", "professional", "business")
non_smb <- c("agency", "business-plus", "premium")

owner_technical_bind_cohort <- owner_technical_bind %>% mutate(cohort = case_when(!is.na(csm) ~ 6,
                                                           upgrade_date=="March 28, 2019" ~ 7,
                                                           plan == "premium" ~ 8,
                                                           !is.na(green) & !is.na(red) & (plan %in% smb) ~ 2,
                                                           !is.na(green) & is.na(red) ~ 3,
                                                           !is.na(green) & !is.na(red)  ~ 4,
                                                           is.na(green) & !is.na(red) ~ 5
)
) %>%
  arrange(cohort)


# Clean known anomalies from contacts
owner_technical_bind_cohort <- owner_technical_bind_cohort %>% filter(user_email != "*@*.com")
owner_technical_bind_cohort <- owner_technical_bind_cohort %>% filter(user_email != "arthur@behive..io")
owner_technical_bind_cohort <- owner_technical_bind_cohort %>% filter(user_email != "omg.i.loves...pam@gmail.com")
owner_technical_bind_cohort <- owner_technical_bind_cohort %>% filter(user_email != "jfrosch@objistics..com")
owner_technical_bind_cohort <- owner_technical_bind_cohort %>% filter(first_name != "Dawit")

#Pasting together green and red
php_fortyeight<-owner_technical_bind_cohort %>% unite("install_concat",c("red","green"), sep = ",",remove = TRUE) %>%
  dplyr::mutate(install_concat = stringr::str_replace_all(install_concat, regex('(^NA,|,NA$)'), ''))


# Correct account geography assignments
geo <- query("SELECT account_name, country
             FROM metrics.account_geo")

CountryRegionCodes <- read_xlsx("../resources/CountryRegionCodes.xlsx")

geo_crc<-geo %>% left_join(CountryRegionCodes,by=c("country"="Country"))

php_fortyeight_phase_region<-sqldf("SELECT php.*,p.phase,geo.region FROM php_fortyeight php LEFT JOIN phases p ON php.upgrade_date=p.upgrade_date LEFT JOIN geo_crc geo ON php.account_name=geo.account_name")

php_fortyeight_phase_region$Region[is.na(php_fortyeight_phase_region$Region)]<-"AMER"

#Fixing date in downgraded CSM/TSM managed accounts
php_fortyeight_phase_region$upgrade_date[is.na(php_fortyeight_phase_region$csm)&php_fortyeight_phase_region$upgrade_date=="April 2, 2019"]<-"March 28, 2019"


#Fixing date in for deferred CSM/TSM accounts
php_fortyeight_phase_region$upgrade_date[php_fortyeight_phase_region$cohort==6]<-"April 2, 2019"




# #Assigning Email Ids and pass fail column
# php_fortyeight_phase_region$email_id<-row.names(php_fortyeight_phase_region)
#
# php_fortyeight_phase_region$pass<- NA
#
# php_fortyeight_phase_region$fail<- NA

email_id_index<-php_fortyeight_phase_region %>% select(email_id,account_name,user_email)

#Final Dataset
write.csv(php_fortyeight_phase_region,"php_fortyeight.csv")
write.csv(email_id_index,"email_id_index_1")


check<-sqldf("SELECT user_email,account_name,count(*) FROM php_fortyeight_phase_region GROUP BY user_email,account_name")



# Join geo to contacts
owner_technical_geo <- owner_technical_bind %>%
                           left_join(geo, by = "account_name")

#If no Geo data defaults to Unknown aka USA
owner_technical_geo$country[is.na(owner_technical_geo$country)] <- "UNKNOWN"

accounts_geo_region <- owner_technical_geo %>%
                           left_join(CountryRegionCodes, by=c("country"="Country")) %>%
                           select(user_email,first_name,account_name,real_role,plan,install_concat,Region)

# Readd phases to email list
phased_comms <- accounts_geo_region %>%
                   left_join(account_phases, by = "account_name") %>%
                   drop_na(Phase) %>%
                   arrange(Phase)
phased_comms$successes_concat <- NA
phased_comms$failures_concat <- NA
phased_comms$Region <- factor(phased_comms$Region, levels = c("EMEA", "AMER", "APAC"), ordered = TRUE)





#wrong dates CSM/TSM
php_fortyeight_phase_region$upgrade_date[!is.na(php_fortyeight_phase_region$csm)] %>% table


phased_comms_final <- as.data.frame(phased_comms) %>%
                          select(Phase, upgrade_date, account_name, plan,
                                 user_email, first_name, real_role, install_concat,
                                 Region, successes_concat, failures_concat)





# Spot checks
# Emails count
phased_comms_final %>% nrow

# Unique install count
phased_comms_final %>%
    select(install_concat) %>%
    separate_rows(install_concat, sep = ",") %>%
    unique() %>%
    nrow()

# Account count
phased_comms_final %>%
    select(account_name) %>%
    unique() %>%
    nrow()

# Accounts by Phase
sqldf("SELECT Phase, upgrade_date, COUNT(DISTINCT(account_name)) as num
      FROM phased_comms_final
      GROUP BY Phase, upgrade_date;")

# Email contacts by phase
phased_comms_final %>%
      select(Phase, upgrade_date) %>%
      group_by(Phase, upgrade_date) %>%
      count() %>%
      arrange(Phase)

# Installs by phase
phased_comms_final %>%
    separate_rows(install_concat, sep = ",") %>%
    rename(install_name=install_concat) %>%
    select(Phase, upgrade_date, install_name) %>%
    unique() %>%
    group_by(Phase, upgrade_date) %>%
    count() %>%
    arrange(Phase)

#Check to see if there are any duplicates
email_account_check <- sqldf("SELECT Phase, account_name, user_email, count(user_email) as cnt
                              FROM phased_comms_final
                              GROUP BY Phase, account_name, user_email
                              ORDER BY Phase, cnt, user_email;")
# Expected result is 0 to confirm single email per user_email per account per phase
email_account_check %>% filter(cnt > 1) %>% nrow()

#OUTPUT FILES
# For Engineering
engineering_phase <- function(p){
    phased_comms_final %>%
        filter(Phase == p) %>%
        separate_rows(install_concat, sep = ",") %>%
        rename(install_name=install_concat) %>%
        select(install_name) %>%
        unique()
}

upgrade_list_phase_1 <- engineering_phase(1)
upgrade_list_phase_2 <- engineering_phase(2)
upgrade_list_phase_3 <- engineering_phase(3)
upgrade_list_phase_4 <- engineering_phase(4)
upgrade_list_phase_5 <- engineering_phase(5)
upgrade_list_phase_6 <- engineering_phase(6)
upgrade_list_phase_7 <- engineering_phase(7)
upgrade_list_phase_8 <- engineering_phase(8)
upgrade_list_phase_9 <- engineering_phase(9)
upgrade_list_phase_10 <- engineering_phase(10)
upgrade_list_phase_11 <- engineering_phase(11)

# update this row for each phase comms to hand off to engineering
write.csv(upgrade_list_phase_1, "upgrade_list_phase_1.csv", row.names = FALSE)

# For Gainsight
write.csv(phased_comms_final, "phased_comms_final.csv", row.names = FALSE)


#Enterprise check
enterprise <- gs_title("7.2 Spot Checks") %>% gs_read(ws = 1, colnames = TRUE)

deferrals_trimmed <- deferrals %>% select(Account) %>% unique() #eliminate any duplicates
deferrals_trimmed$deferred <- TRUE
colnames(deferrals_trimmed)[1] <- "account_name"
