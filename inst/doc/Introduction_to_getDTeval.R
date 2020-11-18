## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(getDTeval)
dat = formulaic::snack.dat

## ----constant,echo=FALSE------------------------------------------------------
id.name <- "User ID"
awareness.name <- "Awareness"
satisfaction.name <- "Satisfaction"
age.name <- "Age"
gender.name <- "Gender"
income.name <- "Income"
region.name <- "Region"
persona.name <- "Persona"
bp.patterns <- "BP_"
consumption.name <- "Consumption"
consideration.name <- "Consideration"
advocacy.name <- "Advocacy"
satisfaction.name <- "Satisfaction"
age.group.name <- "Age Group"
income.group.name <- "Income Group"
product.name<-'Product'
age.decade.name<-"Age_Decade"
mean.awareness.name <- sprintf('Mean %s', awareness.name)
mean.satisfaction.name<- sprintf('Mean %s',satisfaction.name)

## ----simple_average_approaches------------------------------------------------
dat <- data.table::data.table(formulaic::snack.dat)
threshold.age <- 35
## Approach 1
dat[, mean(Age)]
dat[, youngest_cohort := (Age < threshold.age)]
## Approach 2
age.name <- "Age"
youngest.cohort.name <- "youngest_cohort"
dat[, mean(get(age.name))]
dat[, eval(youngest.cohort.name) := (get(age.name) < threshold.age)]

## ----runtime_comparison-------------------------------------------------------
age.name <- "Age"
gender.name <- "Gender"
region.name <- "Region"
set.seed(seed = 293)
sampledat <- dat[sample(x = 1:.N, size = 10^6, replace = TRUE)]
times <- 50
t1 <-
  microbenchmark::microbenchmark(sampledat[, .(mean_age = mean(Age)), keyby = c("Gender", "Region")], times = times)
t2 <-
  microbenchmark::microbenchmark(sampledat[, .(mean_age = mean(get(age.name))), keyby = c(gender.name, region.name)], times = times)
                                 
                                 
results <-
  data.table::data.table(
    Classic_Mean = mean(t1$time),
    Classic_Median = median(t1$time),
    Programmatic_Mean = mean(t2$time),
    Programmatic_Median = median(t2$time)
  ) / 10 ^ 9
results[, Effect_Median := Programmatic_Median/Classic_Median]
round(x = results, digits = 4)

## ----example_1----------------------------------------------------------------
income.name <- "Income"
gender.name <- "Gender"
the.statement.1 <- "dat[,.(mean_income=mean(get(income.name))), keyby = get(gender.name)]"

## ----example_1a---------------------------------------------------------------
getDTeval(the.statement = the.statement.1, return.as = "code")

## ----example 1b---------------------------------------------------------------
getDTeval(the.statement = the.statement.1, return.as = "result")

## ----example 1c---------------------------------------------------------------
getDTeval(the.statement = the.statement.1, return.as = "all")

## ----example_2----------------------------------------------------------------
library(dplyr)
income.name <- "Income"
region.name <- "Region"
awareness.name <- "Awareness"
threshold.income <- 75000
the.statement.2 <-
  expression(
    dat %>% filter(get(income.name) < threshold.income) %>% group_by(get(region.name)) %>% summarise(prop_aware = mean(get(awareness.name)))
  ) 

## ----example_2a---------------------------------------------------------------
getDTeval(the.statement = the.statement.2, return.as = "code", coding.statements.as = "expression")
getDTeval(the.statement = the.statement.2, return.as = "code", coding.statements.as = "character")

## ----example_2b---------------------------------------------------------------
getDTeval(the.statement = the.statement.2, return.as = "result")

## ----example_2c---------------------------------------------------------------
getDTeval(the.statement = the.statement.2, return.as = "all", coding.statements.as = "expression")
getDTeval(the.statement = the.statement.2, return.as = "all", coding.statements.as = "character")

## ----example_3----------------------------------------------------------------
the.statement.3 <- "tab <- dat[, .(prop_awareness = mean(get(awareness.name))), by = eval(region.name)]; data.table::setorderv(x = tab, cols = region.name, order = -1)"

## ----example_3a---------------------------------------------------------------
getDTeval(the.statement = the.statement.3, return.as = "result", eval.type = "as.is")

## ----example_3b---------------------------------------------------------------
getDTeval(the.statement = the.statement.3, return.as = "result", eval.type = "optimized")

## ----example_4----------------------------------------------------------------
sample.dat <- dat[sample(x = 1:.N,
                         size = 10 ^ 6,
                         replace = TRUE)]
the.statement.4 <-
  expression(sample.dat[, .(pct_awareness = mean(get(awareness.name)) * 100), keyby = get(region.name)])
benchmark.getDTeval(the.statement = the.statement.4,
                    times = 50,
                    seed = 282)

## ----ex_5_b, error = TRUE-----------------------------------------------------
the.statement <- 'dat[, .(eval(mean.awareness.name) = mean(get(awareness.name)) * 100), keyby = get(region.name)]'
getDTeval(the.statement = the.statement, return.as = 'all')

## ----ex_6b--------------------------------------------------------------------
the.statement<- 'dat %>% group_by(get(region.name)) %>% summarize(eval(mean.awareness.name)=mean(get(awareness.name),na.rm=T))'
getDTeval(the.statement = the.statement, return.as='all')

## ----example_7b---------------------------------------------------------------
the.statement <- 'dat[1:10,] %>% mutate(eval(age.decade.name) = floor(get(age.name)/10)) %>% select(eval(age.name), eval(age.decade.name))'
getDTeval(the.statement = the.statement, return.as = 'all')

## ----example_8a---------------------------------------------------------------
dat %>% group_by(get(region.name)) %>% summarize(mean_satisfaction = mean(get(satisfaction.name), na.rm=T))

## ----example_8b, warning=FALSE------------------------------------------------
the.statement<- 'dat %>% group_by(get(region.name)) %>% summarize(eval(sprintf("Mean %s", satisfaction.name)) = mean(get(satisfaction.name), na.rm=T))'
getDTeval(the.statement = the.statement, return.as='all')

## ----eample 9b----------------------------------------------------------------
mean.get <- function(x){
  return(mean(x))
}
getDTeval(the.statement = "mean.get (1:5)", return.as = "result")

