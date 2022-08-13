library(tidyverse)
library(RCurl)
library(XML)
library(stringr)

####################################################

# 伪装报头
myheader <- c(
  "user-agent" = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.134 Mobile Safari/537.36 Edg/103.0.1264.71",
  "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
  "accept-encoding" = "gzip, deflate, br",
  "accept-language" = "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"
)

# URL
url_group_1 <- "https://polls.polldaddy.com/vote-js.php?p=11153662"
url_group_2 <- "https://polls.polldaddy.com/vote-js.php?p=11159933"
url_group_3 <- "https://polls.polldaddy.com/vote-js.php?p=11163826"

####################################################

Apo <- \(x) {
  # 读取网页
  webpage <- getURL(url = x, header = myheader, .encoding = "UTF-8")

  # 整理树形结构
  pagetree <- htmlTreeParse(webpage, encoding = "UTF-8", error = function(...) {}, useInternalNodes = TRUE, trim = TRUE)

  # 提取演员姓名
  actor_name <- pagetree %>%
    getNodeSet("//span[@class='pds-answer-text']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 2, nchar(.) - 1)

  # 提取演员组别
  actor_group <- pagetree %>%
    getNodeSet("//div[@class='pds-question-top']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 46, 46) %>%
    as.factor()

  # 提取演员票数占比
  actor_per <- pagetree %>%
    getNodeSet("//span[@class='pds-feedback-per']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 2, nchar(.) - 1) %>%
    as.numeric()
  actor_per <- actor_per / 100

  # 提取演员票数
  actor_votes <- pagetree %>%
    getNodeSet("//span[@class='pds-feedback-votes']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 4, nchar(.) - 7) %>%
    gsub(",", "", .) %>%
    as.numeric()

  output <- tibble(
    actor = actor_name,
    group = actor_group,
    votes = actor_votes,
    per_group = actor_per,
    date = format(Sys.time(), "%F"),
    time = format(Sys.time(), "%T")
  )

  return(output)
}

####################################################

df_1 <- Apo(url_group_1)
df_2 <- Apo(url_group_2)
df_3 <- Apo(url_group_3)

df <- bind_rows(df_1, df_2, df_3) %>%
  arrange(desc(votes))
df$per_all <- (df$votes / sum(df$votes)) %>%
  round(4)

df <- df[, c("actor", "group", "votes", "per_group", "per_all", "date", "time")]

df

####################################################

rio::export(df, file = paste0("1-3 Groups Votes ", gsub(":", ".", Sys.time()), ".csv"))
