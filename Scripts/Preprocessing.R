rm(list=ls()) #clear the variable workspace
options(width = 110, "scipen"=999, "digits"=1, warn = -1)
library(readr)
library(plyr)
library(Rmisc)
library(ggplot2)
library(lubridate)
library(scales)

url <- "" # deleted url to SurveyGizmo CSV export prior to committing - request from jgaunt / S&I team if you want to run this script on your own

df <- read_csv(url)

df <- plyr::rename(df, c(
  "Date Submitted" = 'date',
  "Country" = 'country',
  "State/Region" = 'state',
  #"URL Variable: action",
  #"URL Variable: controller",
  #"URL Variable: id",
  #"URL Variable: link_id",
  #"URL Variable: module",
  #"URL Variable: reason",
  #"URL Variable: test",
  "URL Variable: variation" = 'treatment',
  "URL Variable: who" = 'clientId',
  #"URL Variable: xname"
  "I don't want to share my data:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_sharedata',
  "Firefox became slow:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_becameslow',
  "Firefox started to crash:Please tell us why you chose to opt out of the study. Check all that apply."= 'optoutwhy_crashed',
  "The study interrupted me too frequently:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_interrupt',
  "Websites I frequently visit didn't work as expected:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_badfaves',
  #"Other:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_other1',
  #"Other:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_other2',
  "Variation" = 'treatment2',
  "Reason Study Ended" = 'reason_study_ended',
  "Do you use any other browsers besides Firefox on a regular basis?" = 'otherbrowsers',
  "How fast would you rate your other browser on the scale below?" = 'speed_rating',
  "Thinking back over the last week of testing, how fast would you rate Firefox on the scale below?" = 'lastweek_speed_rating',
  "Startup Time:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_startuptime',
  "Page Load Time:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_pageload',
  "Scrolling:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_scrolling',
  "Crashing/Hanging:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_crashing',
  "Speed of opening a New Tab:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_newtabspeed',
  "Separate NA" = 'exp_separate', # XXX Check on this, it might be wrong
  "How likely or unlikely are you to use Firefox again tomorrow?" = 'use_fx_tomorrow',
  "Firefox crashes a lot:Why are you unlikely to use Firefox in the next few days?" = 'nouse_crashes',
  "Firefox seems slow:Why are you unlikely to use Firefox in the next few days?" = 'nouse_slow',
  "Firefox doesn't work with my websites:Why are you unlikely to use Firefox in the next few days?" = 'nouse_faves',
  "I'm not a regular Firefox user:Why are you unlikely to use Firefox in the next few days?" = 'nouse_notreg',
  "Another browser seems better:Why are you unlikely to use Firefox in the next few days?" = 'nouse_otherbrowser',
  #"Other -:Why are you unlikely to use Firefox in the next few days?" = 'nouse_other1',
  #"Other -:Why are you unlikely to use Firefox in the next few days?" = 'nouse_other2',
  "Firefox helps me feel in control of my online experience."  = 'control_exp',
  "Firefox allows me to complete tasks on the Internet in a way that works as I expect." = 'complete_tasks',
  "Thinking about the last few times you used Firefox, did it get better, get worse, or stay about the same?" = 'overall_quality',
  #"kbps",
  "Speed test"  = 'speedtest',
  "How likely or unlikely would you be to participate in another test to help improve Firefox?" = 'future_participation',
  "Please tell us why you would no longer be interested in helping to test Firefox" = 'future_attrition'
))

df <- subset(df, treatment != '')


# check types (paste to excel)
#write.table(lapply(df, class), 'clipboard', sep="\t")

df$exp_startuptime <- car::recode(df$exp_startuptime, "
                                  'Got better'      = 1;
                                  'Stayed the same' = 0;
                                  'Got worse'       =-1;
                                  else              = NA
                                  ")

df$exp_pageload <- car::recode(df$exp_pageload, "
                               'Got better'      = 1;
                               'Stayed the same' = 0;
                               'Got worse'       =-1;
                               else              = NA
                               ")

df$exp_scrolling <- car::recode(df$exp_scrolling, "
                                'Got better'      = 1;
                                'Stayed the same' = 0;
                                'Got worse'       =-1;
                                else              = NA
                                ")

df$exp_crashing <- car::recode(df$exp_crashing, "
                               'Got better'      = 1;
                               'Stayed the same' = 0;
                               'Got worse'       =-1;
                               else              = NA
                               ")

df$exp_newtabspeed <- car::recode(df$exp_newtabspeed, "
                                  'Got better'      = 1;
                                  'Stayed the same' = 0;
                                  'Got worse'       =-1;
                                  else              = NA
                                  ")

df$use_fx_tomorrow <- car::recode(df$use_fx_tomorrow, "
                                  'Extremely Likely'   = 1;
                                  'Likely'             = 0.5;
                                  'Neutral'            = 0;
                                  'Unlikely'           =-0.5;
                                  'Extremely Unlikely' =-1;
                                  else                 = NA
                                  ")

df$control_exp <- car::recode(df$control_exp, "
                              'Strongly Agree'   = 1;
                              'Agree'             = 0.5;
                              'Neutral'            = 0;
                              'Disagree'           =-0.5;
                              'Strongly Disagree' =-1;
                              else                 = NA
                              ")

df$complete_tasks <- car::recode(df$complete_tasks, "
                                 'Strongly Agree'   = 1;
                                 'Agree'             = 0.5;
                                 'Neutral'            = 0;
                                 'Disagree'           =-0.5;
                                 'Strongly Disagree' =-1;
                                 else                 = NA
                                 ")

df$overall_quality <- car::recode(df$overall_quality, "
                                  'It got better'      = 1;
                                  'It stayed the same' = 0;
                                  'It got worse'       =-1;
                                  else              = NA
                                  ")

df$future_participation <- car::recode(df$future_participation, "
                                       'Extremely Likely'   = 1;
                                       'Likely'             = 0.5;
                                       'Neutral'            = 0;
                                       'Unlikely'           =-0.5;
                                       'Extremely Unlikely' =-1;
                                       else                 = NA
                                       ")

# recode selected character variables to factor
for(i in c(21, 23, 26:30, 33:35, 43, 45:49, 56, 58)) {
  df[,i] <- factor(df[,i])
}
# recode selected character variables to numeric
for(i in c(38:42, 44, 52:54, 57)) {
  df[,i] <- as.numeric(df[,i])
}
#summary(df)

# Treatment levels
# Client-seconds of wait time for nglayout to render
# https://github.com/gregglind/shield-variations-gestalt/blob/master/x-addon/src/variations.js#L6
# 5 (aggressive)
# 50 (medium)
# 250 (ut/control/default)
# 1000 (weak)
df$treatment <- relevel(df$treatment, ref='ut')
df$treatment <- plyr::mapvalues(df$treatment, from = c('aggressive','medium','ut','weak'), to = c('Aggressive (5)', 'Medium (50)', 'Control (250)', 'Weak (1000)'))

# 2nd iteration - drop the WEAK responders from first run
#df <- df[df$treatment != 'Weak (1000)',]

# filter out from invalid dates
df$date = lubridate::ymd_hms(df$date)
df$wday = lubridate::wday(df$date, label=TRUE, abbr=TRUE)
df$day = lubridate::floor_date(df$date, 'day')

# which sample to use? comment out all but 1 line
#df = df[df$day %within% lubridate::interval(lubridate::mdy('04042016'), lubridate::mdy('04112016')),] # 1st
#df = df[df$day %within% lubridate::interval(lubridate::mdy('04272016'), lubridate::mdy('05042016')),] # 2nd
df = df[df$day %within% lubridate::interval(lubridate::mdy('04042016'), lubridate::mdy('05042016')),] # both