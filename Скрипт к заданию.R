library(dbplyr)
library(data.table)
library(tidyr)
library(officer)
library(flextable)


data <- readRDS("TestData.RDs")

# Функция для расчета количественных переменных
calcNumericStat <- function(dataName, name, x, y = 1, na.omit = TRUE) {
  if (na.omit)
    x <- x[!is.na(x)]
  N <- length(x)
  MEAN <- round((mean(x)), y)
  SD <- round((sd(x)), y)
  MEANSD <- paste0(MEAN, " (", SD, ")")
  Median <- round((median(x)), y)
  Q1 <- round((quantile(x, c(0.25))), y)
  Q3 <- round((quantile(x, c(0.75))), y)
  Min <- round((min(x)), y)
  Max <- round((max(x)), y)
  return(
    data.table(
      "Data" = dataName,
      "Parameter" = name,
      N = as.character(N),
      "Mean (SD)" = as.character(MEANSD),
      "Q1" = as.character(Q1) ,
      "Median" = as.character(Median),
      "Q3" = as.character(Q3) ,
      "Min; Max" = paste0(Min, "; ", Max)
    )
  )
}

# Функция для расчета категориальных переменных
calcGroup <- function(dataName, data, parameterName, group = c("Рarameter")){ 
  data <- as.data.table(data)
  colname <- c(group)
  names(data) <- c(colname)
  data <- data[is.na(get(colname[1])), c(colname[1]) := "No data"]
  data<-data[, .(n = .N), by= c(colname)]
  data<-data[, `:=`(Total=sum(n))]
  data<-data[, `:=`(Percent=round(n/Total*100,2))]
  data<-data[, `:=`(Value=paste0(n, " (", Percent, "%)"))]
  data<-data[, `:=`(Data = dataName)]
  data<-data[, `:=`(Parameter = parameterName)]
  data <- data[, c("n","Total","Percent"):=NULL]  
  setcolorder(data, c("Data", "Parameter", colname, "Value"))
  setnames(data, c("Data","Parameter", colname, "Value") )
  return(data)
}

dataA <- dplyr::filter(data, trt == "Drug A")
dataB <- dplyr::filter(data, trt == "Drug B")

# Создадим список с нужными нам датафреймами, чтобы было удобно обращаться к ним в цикле
list_data <- list(dataA, dataB, data)
data_names <- c(paste("Drug A",
                  "N = XX",sep="               "),
                paste("Drug B",
                  "N = XX",sep="               "),
                paste("Total",
                  "N = XX",sep="               "))

res <- data.table()
for(i in 1:3) {  
  tmp <- calcNumericStat(data_names[i] , "Age", list_data[[i]]$age)
  res <- rbind(res, tmp)
}

# Развернем эти таблицы для удобства
rest <- pivot_longer(res, 3:8, names_to = "Рarameter", values_to = "value")
res_age <- pivot_wider(rest, names_from = "Data", values_from = "value")

res <- data.table()
for(i in 1:3) {  
  tmp <- calcGroup(data_names[i], list_data[[i]]$sex, "Sex")
  res <- rbind(res, tmp)
}
res_sex <- pivot_wider(res, names_from = "Data", values_from = "Value")

res <- data.table()
for(i in 1:3) {  
  tmp <- calcGroup(data_names[i], list_data[[i]]$race, "Race")
  res <- rbind(res, tmp)
}
res_race <- pivot_wider(res, names_from = "Data", values_from = "Value")

res <- data.table()
for(i in 1:3) {  
  tmp <- calcNumericStat(data_names[i] , "bl", list_data[[i]]$bl)
  res <- rbind(res, tmp)
}
rest <- pivot_longer(res, 3:8, names_to = "Рarameter", values_to = "value")
res_bl <- pivot_wider(rest, names_from = "Data", values_from = "value")


title_age <-list("ㅤ","Age(years)","ㅤ","ㅤ","ㅤ")
title_sex <- list("ㅤ","Sex","ㅤ","ㅤ","ㅤ")
title_race <- list("ㅤ","Race","ㅤ","ㅤ","ㅤ")
title_result <- list("ㅤ","Result of XXXX test at screening (some units) ","ㅤ","ㅤ","ㅤ")

# Теперь объединим получившиеся таблицы
res_data <- rbind(title_age,res_age,title_sex, res_sex,title_race, res_race, title_result, res_bl)
final_data <- res_data[,-1]


#Форматирование таблицы
table <- flextable(final_data)
table <-bg(table, i=1, bg="grey",part="head")
table <-align(table,j=1,align="center")
table <-align(table,j=2:4, align="right")
table <-align(table,i=c(1,8,11,17), align="left")
table <-align(table,i=1,align="center",part="head")
table <-align(table,j=1,i=1, align="left",part="head")
table <-bold(table,i=c(1,8,11,17),bold=TRUE,part="body")
table <-bold(table,i=1,bold=TRUE,part="head")
std_border = fp_border(color="black", width = 1)
table <- border_inner(table, border = std_border, part = "all")
table <- border_outer(table, border = std_border, part = "all")
table <- merge_at(table, i = 17, part = "body")
table <- width(table, width= 1.5)
table <- height(table, i=1, height = .4, part='head')
table <- height_all(table, height = .18, part="body")
table <- fontsize(table, size = 10, part = "body")
table <- hrule(table, rule = "exact", part = "all")
table <- padding(
  table,
  padding = 2,
  padding.left = 5,
  part = "all"
)
table <- font(
  table,
  fontname="Courier New",
  part = "all"
)
table <- footnote(table, i = c(2,3,4,5,6,7,9,13,17), j = 1,
                  value = as_paragraph(
                    c("Number of non-missing values",
                      "Mean value and standard deviation",
                      "25 percentiles",
                      "Median",
                      "75 percentiles",
                      "Minimal and maximal values",
                      "If any",
                      "if any",
                      "This parameter is given as “bl” in the source data"
                    )
                  ),
                  ref_symbols = c("1", "2,3", "4","5","6","7","8","9","10"),
                  part = "body")
table <- fontsize(table, size = 8, part = "footer")
table <- padding(
  table,
  i=1,
  padding.top=150,
  part = "footer"
)
              
doc <- read_docx() %>%
body_add_par(value = "Table 1    	Demographics and other baseline characteristics
                                        (PP population)
", style = "heading 3") %>% 
body_add_flextable(table)
print(doc, target = "Table.docx")
  
