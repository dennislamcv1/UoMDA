library("foreign")
data <- read.xport('LLCP2020ASC/LLCP2020.XPT', fill = NA)

cell=data|>filter(CCLGHOUS==1) |> nrow()
land=data|>filter(COLGHOUS==1) |> nrow()

cell/land

data |> filter(IMONTH=="09" | IMONTH=="10") |> nrow()

data |> filter(CCLGHOUS==1 | COLGHOUS==1) |> filter(MENTHLTH<=30 & MENTHLTH >=1 & !is.na(MENTHLTH)) |> select(MENTHLTH) |> unlist() |> mean()
