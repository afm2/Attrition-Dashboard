
data <- read.csv('WA_Fn-UseC_-HR-Employee-Attrition.csv', header = T)
require(ggplot2)
require(caret)
require(dplyr)
require(devtools)
require(ggiraphExtra)


data$Attrition <- (as.numeric(data$Attrition) - 1)


### Generic Logistic Model: Used in side panel, fitted values indicate likely leavers
full_dat_model <- glm(Attrition ~ OverTime + WorkLifeBalance + DistanceFromHome + Age + JobSatisfaction + YearsInCurrentRole + YearsSinceLastPromotion + Department, data = data, family = 'binomial')
attr_odds <- full_dat_model$fitted.values
attr_odds <- ifelse(attr_odds > .5, T, F)
data$attr_odds <- as.integer(attr_odds)

dept_level_model <- glm(Attrition ~ OverTime + WorkLifeBalance + DistanceFromHome + Age + JobSatisfaction + YearsInCurrentRole + YearsSinceLastPromotion, data = data, family = 'binomial')
attr_odds <- dept_level_model$fitted.values
attr_odds <- ifelse(attr_odds > .5, T, F)
data$attr_dept_odds <- as.integer(attr_odds)

### Make subsets of DF
full_dat <- data

Sales <- data %>%
  filter(Department == 'Sales')


HR <- data %>%
  filter(Department == 'Human Resources')

RD <- data %>%
  filter(Department == 'Research & Development')


### Make Histogram of Previous Attrition using a given department and dataset

attr_plot_maker <- function(dept, dat, fill_color){
  leavers <- table(dat$Attrition)[2]
  overall <- nrow(dat)
  
  Main_Title  <-  paste0(dept, ' Attrition Rate')
  Sub_Title  <-  paste0(leavers,' out of ', overall, ' Employees ', collapse = '')
  
  attr_plot <-  ggplot(data = dat, aes(x = as.numeric(Attrition))) +
  geom_histogram(binwidth = 1, fill = fill_color, col = 'black', alpha = .8) +
  scale_x_continuous(breaks = seq(0,1,1), labels = c('Stayers', 'Leavers')) +
  ggtitle(label = Main_Title, subtitle = Sub_Title) +
  xlab('') +
  ylab('Employees')
  
return(attr_plot)
}

### Make Logistic Model for given formula and dataset
logistic_maker <- function(formulae ,dat){  
 
  dept_levels = length(unique(as.character(dat$Department)))
  if(dept_levels == 1){
    frmla <- dept_level_model$call$formula
  }else frmla <- full_dat_model$call$formula
  
  
  logistic_model <- glm(formula = formulae, data = dat, family = 'binomial') 
return(logistic_model)
}

### Gather Variable Importance Stats (i.e., Log Odds) from Model given a dataset
varImp_Constructor<- function(data){
  require(dplyr)
  dept_levels = length(unique(as.character(data$Department)))
  if(dept_levels == 1){
    frmla <- dept_level_model$call$formula
  }else frmla <- full_dat_model$call$formula

  logistic_model <- logistic_maker(formulae = frmla, dat = data)
  importance <- exp(coef(logistic_model))[-1]
  label <- names(importance)
  attr_causes <- data.frame(label, importance)
  
  attr_causes <- attr_causes %>%
    arrange(desc(importance))
  
  colnames(attr_causes) = c('Variable', 'Effect Size')
  
  return(attr_causes)
}

### Report probability of attrition for Employees with p(Attrition) > .5 in summary table
Likely_Leavers <- function(dataset){

dept_levels = length(unique(as.character(dataset$Department)))
if(dept_levels == 1){
  frmla <- dept_level_model$call$formula
}else frmla <- full_dat_model$call$formula

model <- logistic_maker(formulae = frmla, dat = dataset)
  
colnames(dataset)[length(colnames(dataset))] = 'Probability of Turnover'
dataset$`Probability of Turnover` =  full_dat_model$fitted.values[which(names(model$fitted.values) %in% rownames(dataset))]

at_risk <- dataset %>%
  filter(Attrition != 1 & `Probability of Turnover` > .5) %>%
  arrange(desc(`Probability of Turnover`)) 

return(at_risk)
}





