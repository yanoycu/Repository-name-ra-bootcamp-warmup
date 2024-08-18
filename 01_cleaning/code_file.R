# パッケージのインストール
# install.packages("magrittr")

# パッケージの読み込み
library(tidyverse)
library(magrittr)
library(readxl)


# (a)Semester Data の整形

# データの読み込み
semester1 <- read.csv("semester_data_1.csv", header = T, skip = 1)
semester2 <- read.csv("semester_data_2.csv")

# データの結合
rownames <- c("unitid","instnm","semester","quarter","year","Y")
semester2 <- set_colnames(semester2,value = rownames)
semester <- rbind(semester1,semester2)

# `Y`列の削除
semester <- semester[,-6]

# semester制が導入された年の列作成
lastquarter <- semester %>%
  group_by(unitid) %>%
    filter(semester == 0) %>%
      filter(year == max(year,na.rm = F))  
lastquarter <- mutate(lastquarter, semesterstart = year + 1)

firstsemester <- semester %>%
  group_by(unitid) %>%
    filter(semester == 1) %>%
       filter(year == min(year,na.rm = F))  

semester <- left_join(semester,lastquarter, by = "unitid")
semester <- semester[,-c(6,7,8,9)]

semester <- left_join(semester,firstsemester, by = "unitid")
semester <- semester[,-c(7,8,9)]

semester <- mutate(semester,semesteryear = if_else(
  semester.x == "1",
  semesterstart,
  year
  )
)

semester <- semester[,-c(6,7)]
rownames2 <- c("unitid","instnm","semester","quarter","year","semesteryear")
semester <- set_colnames(semester,value = rownames2)

# semester制導入後を示すダミー変数の作成
semester <- mutate(semester,semesterdummy = if_else(
  year >= semesteryear,
  1,
  0 
  )
)


# (b)Gradrate Data の整形

# データの読み込み・結合
filepathes <- list.files(
  "C:/Users/ritsuyano/Desktop/ra-bootcamp-warmup/warmup training package/01_data/raw/outcome"
  ,pattern = "xlsx", recursive = T, full.names = T)
list_outcome <- lapply(filepathes, read_excel)
outcome <- bind_rows(list_outcome)

# 女子学生の4年卒業率を0から1のスケールに変更
outcome %>% 
  mutate(w_gradrate4yr = women_gradrate_4yr * 0.01) -> outcome

# 男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加
outcome %>%   
  mutate(m_4yrgrads = as.numeric(m_4yrgrads)) %>% 
  mutate(totcohortsize = as.numeric(totcohortsize)) %>% 
  mutate(m_gradrate4yr = m_4yrgrads / m_cohortsize) %>% 
  mutate(totgradrate4yr = tot4yrgrads / totcohortsize) -> outcome


# 計算した卒業率を有効数字3桁に調整
outcome %>%
  mutate(m_gradrate4yr = format(round(m_gradrate4yr, 3), nsmall = 3)) %>% 
  mutate(totgradrate4yr = format(round(totgradrate4yr, 3), nsmall = 3)) -> outcome
 
# 1991年から2010年までのデータフレームに変形
outcome %>% 
  filter(year <= 2010) -> outcome


# (c)Covariates Data の整形

# データの読み込み
covariates <- read_excel("covariates.xlsx")

# 列名の変更
covariates　%>% 
  rename(unitid = university_id) %>%  
  mutate(unitid = str_remove_all(unitid, "aaaa")) -> covariates

# ‘category’列に含まれる文字列をそれぞれ別の列として追加
covariates %>% 
  pivot_wider(names_from = "category",values_from = "value") -> covariates

# 他のデータフレームと期間をそろえる
covariates %>% 
  filter(year >= 1991 & year <= 2010 ) -> covariates

# `unitid`を`outcome`にそろえる
outcome %>% 
  arrange(unitid) -> outcome

covariates %>% 
  mutate(unitid = as.numeric(unitid)) %>% 
  mutate(year = as.numeric(year)) -> covariates

covariates_outcome <- inner_join(covariates, outcome, by = c("unitid","year"))


# (d) Master Dataの作成

master <- left_join(semester,covariates_outcome, by = c("unitid","year"))
