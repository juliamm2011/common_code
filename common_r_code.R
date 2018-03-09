####################################################################################################
## Author: Julie Morris
## Description: Commonly used R Functions
####################################################################################################

#rm(list=ls())

#load libraries
library(data.table)
library(reshape2)
library(scales)
library(ggplot2)
library(plyr)
library(readstata13)
library(scales)
library(directlabels)
library(foreign)
library(car)
library(PerformanceAnalytics)
library(plm)
library(tableone)
library(ReporteRs)
library(magrittr)
library(patchwork)
set.seed(98109)
# load packages
list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
pacman::p_load(data.table, plyr, reshape2, ggplot2, directlabels, magrittr,
               PerformanceAnalytics, plm, car, scales)


#set directories
data_dir <- "J:/Project/post-secondary-education/Data_Sources/IPEDS/data/"

#load data
DT <- fread(paste0(data_dir, "/data_file.csv"))
#delete a column
DT <- DT[, V1 := NULL]

#load .dta
DF <- read.dta13("J:/Project/post-secondary-education/Forecasting/model_numbers_bach.dta")

#print range of rows
DT[1:5,]

#order rows by unitid then year
DT[order(unitid, svyyear)]

#add two columns together, ignore missing
DT[, new_var := rowSums(.SD, na.rm = TRUE), .SDcols = c("var1", "var2")]

#multiply two columns together
DT[, new_var := var1 * var2]

#recode variable
dt <- dt[var == oldvalue, var := newvalue]
dt <- dt[var == c("vec", "of", "old", "values"), var := c("vec", "of", "new", "values")]
#recode several variables to one value
covstorecode <- c("var1", "var2", "var3")
DT[, (covstorecode) := 0]
#conditional recode
setkey(hrs, hatota)
hrs[hrs[hatota<=0,], hwealthpos := 0]; hrs[hrs[hatota>0,], hwealthpos := 1]
#
hrs[hatota>0, hwealthlog := hatota]

#divide one column by another
dt <- dt[, newvar := var1/var2] 

#average across a list of columns
dt[, so_score := rowMeans(subset(temp, select = c(grep("_scr",colnames(dt),value=T))), na.rm = TRUE)]

#ifelse statement in creating a new var
dt[, newvar := ifelse(othervar=="EXAMPLE", 1, 0)]

#drop completely empty rows
DT <- DT[!which(rowMeans(is.na(DT)) >= ((ncol(DT)-2) / ncol(DT))), ]

#drop rows that are NA for a particular column
#DT <- DT[!which(is.na(DT[, var1]))]
db <- na.omit(db, cols="var1")

#pads numbers - add 0 before number
dt[, monthvar := str_pad(dt[,monthvar], 2, pad = "0")]

#weighted means
DT <- DT[, list(var1 = weighted.mean(var1, weight_var)), by=id.varnames]
DT <- DT[, list(var1 = weighted.mean(var1, weight_var, na.rm=T)), by=id.varnames]

#rounding
##whole number
DT <- DT[, var1 := round(var1, 0)]
##one decimal point
DT <- DT[, var1 := round(var1, 1)]

##subset based on values
dt <- dt[var1 == "JFK" & var1 == 6]
dt <- dt[var1 != 1990]
dt <- dt[!which(dt[, var1 >= 3])]
dt <- dt[!(var1==12&var2==2017)]

##only keep specific columns
#with quotes
DT <- DT[, c("columns", "to", "keep"), with=FALSE]
#without quotes
DT <- DT[, .(var1, var2, var3, var4, var5)]
##delete specific columns
DT <- DT[, !c("columns", "to", "delete"), with=FALSE]
#from vector
DT <- DT[, vec_of_vars_to_keep, with=FALSE]
DT <- DT[, !vec_of_vars_to_del, with=FALSE]
#using grep
DT <- DT(DT, select = grep("prefix1_|prefix_2|prefix_3", names(DT))) #to keep
DT <- DT[, c("var1","var2",grep("prefix", names(DT), value=T)), with=F] #to keep
DT <- DT(DT, select = -grep("prefix1_|prefix_2|prefix_3", names(DT))) #to drop
#lapply using grep to only do it on the column names
DT[, (grep("XYZ",names(DT),value=T)) := lapply(.SD, as.numeric), .SDcols=grep("XYZ",names(DT),value=T)]
##remove all columns with a specific letter ("y", for example)
#the "invert=T" removes them.
dt[, grep("y$", colnames(dt), invert=T, value=T, invert=T), with=F]



##frequency table
dt[, .N, by = var1]
#proportion table
dt[,.N/nrow(dt[!is.na(var1)]),by="var1"]
#proportion table, conditional on another variable value
dt[time==1,.N/nrow(dt[time==1&!is.na(var1)]),by="var1"]
#mean of a variable by year
DT[, mean(var1, na.rm=T), by="svyyear"]
#determine type of variable
typeof(DT[,var1])
#two-way table
with(DT, table(var1, var2))

#mapvalues
#example: create a variable of state names ("new_var") by matching fipcodes (variable = "var_to_match", vector of 51 unique codes = "old_vec_of_values)
##to a vector of 51 state names ("new_vec_of_values")
DT[, new_var :=  mapvalues(DT[, var_to_match], from = Old_vec_of_values, to = new_vec_of_values)]

#re-order columns; including a vector of column names
setcolorder(DT, c("var1", "var2", "var3", "var4", cov_list, "var8", "var9"))

#maximum value
max(dt[, var1])
max(dt[, var1], na.rm=T)
#maximum value, when subsetting by value
max(dt[subset_variable==1][, var_want_max])

##rename all columns
setnames(yearidmatrix, c("unitid", "svyyear"))
##rename specific column
setnames(DT, "oldname", "newname")
##rename specific column
names(DT)[names(DT) == "old_varname"] <- "new_varname"

#remove duplicates
df <- subset(df, !duplicated(df$var1))


#create PDF file
filename <- (paste0(plot_dir, "plot_name.png"))
png(filename, width=1000, height = 600)
print(plot5)
graphics.off()
#set labels
xlabel <- "X Variable Label"
ylabel <- "Y Variable Label"
tlabel <- "Main Plot\nTitle"
#manual legend labels
lname <- "Legend Title"
llabels <- c("Category 1", "Category 2", "Category 3") 
#set data and variables
pdata <- DT
px <- DT[, x_var]
py <- DT[, y_var]
ci_lo <- DT[, y_var_lower_bound]
ci_hi <- DT[, y_var_upper_bound]
#set axis breaks
xbreaks <- 6
ybreaks <- 8
#for facets
n_cols <- 10
facet_var <- DT[, var_distinguishing_plots]

##plotting functions
#single line chart with confidence interval
plot5 <- ggplot(data=pdata, aes(x=px, y=py)) +
  geom_line() + 
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi, fill = "red"), alpha=0.4) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() + guides(fill=FALSE) +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks), labels = waiver()) +
  scale_y_continuous(limits=c(0,pdata[,max(ci_hi)*1.05]), breaks = scales::pretty_breaks(n = xbreaks), labels = comma) + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold"), axis.title.y = element_text(angle=0),
        plot.title = element_text(size=20,face="bold")) +
  ggtitle(tlabel)

#single line chart
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py)) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks), labels = waiver()) +
  scale_y_continuous(limits=c(0,pdata[,max(py)]), breaks = scales::pretty_breaks(n = xbreaks), labels = comma) + 
  ggtitle(tlabel)

#line chart, factored by one variable
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(variable), colour=factor(variable))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks), labels = waiver()) +
  scale_y_continuous(limits=c(0,pdata[,max(py)]), breaks = scales::pretty_breaks(n = xbreaks), labels = comma) + 
  ggtitle(tlabel)
print(plot2)


##line chart, factored by two variables. with vertical lines.
#set labels
xlabels1a <- c("FW 14","FW 15","FW 16","FW 17","FW 18","FW 19","FW 20","FW 21")
xlabel1a <- c("Fiscal Week")
ylabel1a <- "TB Score"
tlabel1a <- "Deployment Play Utilization on Customer Experience"
sublabel1a <- "Customer Connection and Store Operations"
caption1a <- "Deployment Activities Timeline sent to SMs:\nFW17 - Understand the change for yourself and your team\nFW18 - Prepare to lead the change for your team\nFW20 - Lead the change for your team"
# caption1a <- "U.S. Company Operated Stores"
#manual legend labels
lname11a <- "Fiscal Year"
llabels11a <- c("2017","2018")
lname21a <- "Deployment Play Utilization"
llabels21a <- c("Not utilized","Utilized")
#values
pdata1a <- dp
px1a <- dp[,FSCL_WK_IN_YR_NUM]
py1a <- dp[,cc_sc]
py1b <- dp[,so_sc]
groupvar1a <- dp[,useploy]
colourvar1a <- dp[,FSCL_YR_NUM]
#plot itself
plot1a <- ggplot(data=pdata1a, aes(x=px1a, y=py1a, colour=factor(colourvar1a), linetype=factor(groupvar1a), group=interaction(groupvar1a, colourvar1a))) + 
  geom_line(size=1) + annotate(geom = "text", x=14, y=70, hjust=0, label = "Store Operations") +
  geom_line(y=py1b,size=1) + annotate(geom = "text", x=14, y=40, hjust=0, label = "Customer Connection") +
  geom_vline(aes(xintercept = 17)) + annotate(geom = "text", x=17, y=3, hjust=0, vjust=-0.5, label = "Understand", angle=90, size=3, fontface = "italic") +
  geom_vline(aes(xintercept = 18)) + annotate(geom = "text", x=18, y=3, hjust=0, vjust=-0.5, label = "Prepare", angle=90, size=3, fontface = "italic") +
  geom_vline(aes(xintercept = 20)) + annotate(geom = "text", x=20, y=3, hjust=0, vjust=-0.5, label = "Lead", angle=90, size=3, fontface = "italic") +
  xlab(xlabel1a) + ylab(ylabel1a) + 
  scale_colour_discrete(name=lname11a, labels=llabels11a, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_linetype_discrete(name=lname21a, labels=llabels21a, guide=guide_legend(order=1)) +
  scale_x_continuous(breaks=14:21) +
  scale_y_continuous(limits=c(0,70)) + theme_economist() +
  ggtitle(tlabel1a) + labs(subtitle=sublabel1a,caption=caption1a)
print(plot1a)


#rotate x-axis labels
theme(axis.text.x = element_text(face = "bold", size = 12, angle = 90, hjust = 1)) 

##loops
#set state 
fips <- factor(DT[, fips])
fips <- unique(fips)
#multiple line charts, factored by one variable (so, each plot factored by "variable", and one plot for each state)
#loop over states
for(fip in fips) {
  p <- ggplot(data=pdata[fips==fip], aes(x=px, y=py, group=factor(variable), colour=factor(variable))) 
  p <- p + geom_line() + theme_bw()
  p <- p + xlab(xlabel) + ylab(ylabel) + expand_limits(y=c(0,(max(pdata[fips==fip][, value]))*1.15))
  plot(p)
}   

#facet line plot
plot4 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py)) + 
  xlab(xlabel) + ylab(ylabel) + theme_minimal() + theme(text = element_text(color = "darkgreen")) +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks), labels = waiver()) +
  scale_y_continuous(limits=c(0,pdata[,max(py)]), breaks = scales::pretty_breaks(n = xbreaks), labels = comma) + 
  facet_wrap(~facet_var, ncol = n_cols) +
  ggtitle(tlabel)

#facet histogram
#histogram of ces-d outcome: facet plot
p <- qplot(var1, data = DT, geom = "histogram", binwidth=1, 
           main="main title", xlab="x label", ylab="y label")
p + facet_wrap(~ groupvar, scales = 'free_y') + theme_bw() #'free_y' lets y-scale differ by plot

#basic histogram
hist(dt[, n_covs], breaks=12, main="Title Text", xlab="X Label Text")

#sum/find mean by year
DT <- DT[, lapply(.SD, mean, na.rm=TRUE), by="svyyear"]
DT <- DT[, list(var1 = sum(var1, na.rm=TRUE)), by="state"]
#do multiple times by making a vector of column names
draws.required <- 25 #nice to set it up this way, so you only need to change once
id.varnames <- c("id_var1", "id_var2", "id_var3", "id_var4")
draw.colnames <- paste0("draw_", 0:(draws.required-1))
DT[, (draw.colnames) := lapply(.SD, function(x) sum(x)), .SDcols=draw.colnames, by=c(id.varnames[1:2])]
DT[, (colnames(DT)[2:5]) := lapply(.SD, function(x) round(x,2)), .SDcols=colnames(DT)[2:5]]
DT[, (c("var1","var1")) := lapply(.SD, function(x) round(x,2)), .SDcols=c("var1","var2")]

#library(reshape)
#reshape from wide to long
DT <- melt(DT, id="svyyear")
DT <- melt(DT, 
           id=id.varnames[1:2], 
           variable.name = "draw", #default is "variable"
           value.name = "exposure", #default is "value"
           na.rm = T, #if true, NA's will be moved from molten data.
           variable.factor = F) #if true, var will be factor. if else, will be character.
#reshape from long to wide
##"variable" is the factor variable we want to be separate columns. "value" is the variable name that contains the values we want to swing.
DT <- dcast.data.table(DT, id_var1 + id_var2 + id_var3 ~ variable, value.var="value")
DT <- dcast.data.table(DT, id_var1 + id_var2 + id_var3 ~ variable, value.var=c("value1", "value2"))

#library(tidyr)
#reshape from wide to long
#Note we used bare variable names to specify the names of the key and value columns
#variable.name = key; value.names = value
DT <- gather(DT, key, value, -id1, -id2)
#reshape from wide to long
##"variable" is the factor variable we want to be separate columns. "valuevar" is the variable name that contains the values we want to swing.
DT <- spread(DT, variable, valuevar) 



#turn NA's to 0 (zero)
#whole data.table
dt[is.na(dt)] <- 0
#specific variable
dt[is.na(dt[,var]), var := 0]

##merge multiple data tables at one time
newDT <- Reduce(function(x, y) {merge(x, y, by=c("id_var1", "id_var2"), all = TRUE)}, list(DT1, DT2, DT3))

#fill in missing proportions using state- and sector-specific averages
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
edata[, a24u := impute.mean(a24u), by = .(svyyear,fips,sector)][, a2539 := impute.mean(a2539), by = .(svyyear,fips,sector)][, a40p := impute.mean(a40p), by = .(svyyear,fips,sector)]

#subset data and create new variables simultaneously
DT <- DT[, .(unitid, svyyear, var1, var2, var3, var4,
                   new_var1 = var1 * var2,
                   new_var2 = var3 * var4)]

#transform to log and logit scale
DT <- DT[, .(unitid, svyyear, var1, var2, var3, var4,
             ln_new_var1 = log(new_var1),
             logit_new_var2 = logit(new_var2))]

#change infinite values to 0
DT[mapply(is.infinite, DT)] <- 0

#create empty matrix with number of columns equal to number of covariates, and number of models we wish to fill-in (1500)
matrix <- matrix( 
  nrow=1500, 
  ncol=length(vec_of_covariate_names))
matrix <- as.data.table(matrix)
setnames(matrix, vec_of_covariate_names)
matrix[,(vec_of_covariate_names):= lapply(.SD, as.numeric), .SDcols = vec_of_covariate_names]
#for each cell, randomly generate a binary from a binomial distribution with probability 0.5
for (i in c(1:1500)) {
  for (j in names(matrix)) {
    matrix[i,][,j] <- rbinom(1, 1, 0.5)
  }
}

#rbind
dt <- rbind(dt1, dt2)
#rbindlist
l = list(DT1, DT2)
rbindlist(l, use.names=TRUE, fill=TRUE) #If "use.names" = T items will be bound by matching column names. If "fill" = T fills missing columns with NAs. By default FALSE. 
#cbind
dt <- cbind(dt1, dt2)

#basic time trend
#create matrix with unitids and years for prediction
unitids <- unique(fd[,unitid])
svyyear <- c(2015:2025)
newData <- expand.grid(svyyear, unitids)
names(newData) <- c("svyyear","unitid") 
#predict forward
DF <- as.data.frame(DF)
models <- lapply(split(DF, DF$unitid), 'lm', formula = dep_var ~ svyyear)
pred <- mapply('predict', models, split(newData, newData$unitid))
pred <- as.data.frame(pred)

##three-year averages
#make vector of even-numbered and odd-numbered years (even = years with missingness)
year_vec_even <- seq(2002, 2014, 2)
year_vec_odd <- seq(2003, 2013, 2)
#merge enrollment data into empty data.table of all possible id/year combinations
DT_full <- merge(yearidmatrix, original_DT, all.x=TRUE)
#set order by unitid then year
DT_full <- setorder(DT_full, unitid, svyyear)
##do three-year averages
DT_full[, var1_mean := (rowShift(var1,-1) + rowShift(var1,1))/2]
#subset to only even years
setkey(DT_full, svyyear)
DT_even <- DT_full[.(year_vec_even)]
#subset to only odd years (to fill in 0's for NA's when applicable)
setkey(DT_full, svyyear)
DT_odd <- DT_full[.(year_vec_odd)]
#remove "mean" columns from DT_odd DT (as not needed)
DT_odd <- DT_odd[, eval(expr)]
#remove all "mean" values we have observed data for
DT_even[var1 > 0, var1_mean := 0]
#add mean and observed columns together
DT_even[, var1 := rowSums(.SD, na.rm = TRUE), .SDcols = c("var1", "var1_mean")]
#subset to only original columns
DT_even <- DT_even[, eval(expr)]
#rbind to the odd years
DT_full <- rbind(DT_even, DT_odd)
#drop the completely empty rows
#all missing
DT_full <- DT_full[!which(rowMeans(is.na(DT_full)) >= ((ncol(DT_full)-2) / ncol(DT_full))), ]

#correlations with p-values
library(Hmisc)
rcorr(x, type="pearson")
#correlations without p-values
cor(x, y)
#partial correlation
#library(ppcor)
pcor.test(dt[,var1],dt[,var2],dt[,confoundvar],method="pearson")

###correlation matrices
#library(PerformanceAnalytics)
#set up functions
## correlation matrix with p-values
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
## create function to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
#flatten the table
flattenSquareMatrix(cor.prob(DT_with_vars))
#plot the data
chart.Correlation(DT_with_vars)


#merge files together by organizing them in a .csv
## Write function to load files and rename variables -----------------------------------------------
load.data <- function(layout, svy, file, vars) {
  inst.data <- data.table(read.csv(paste(layout[svy, dir], layout[svy, file, with=F], sep="/")))
  setnames(inst.data, tolower(names(inst.data)))
  inst.data <- inst.data[, c("unitid", as.character(layout[svy, vars, with=F])), with=F]
  setnames(inst.data, c("unitid", vars))
  return(inst.data)
}
## Read in variable layout and loop over surveys ---------------------------------------------------
#.csv with the following columns: "svyyear"	"dir"	"inst_file"	"fips"	"sector"	"control"	"countycd"
##each row contains one file. "dir" is the path to the folder. "inst_file" is the file name (including .csv)
###only need do twice (inst1 and inst2) if not all variables are present in all rows.
####column name is what you want the var to be named in the merged dataset, cell contains var name in the file
layout <- fread("ipeds_files_and_variables_instchar.csv") #.csv with 
inst1 <- lapply(1:6, function(svy) {
  cat(paste(svy, "of", nrow(layout), "\n")); flush.console()
  ## Load comopletions  file ---------------------------------------------------------------
  inst <- load.data(layout, svy, "inst_file", c("fips", "sector", "control", "countycd")) 
  ## make unitid a factor variable for later merging
  inst[, unitid := factor(unitid)]
  ## Bind files together ----------------------------------------------------------------------------
  return(cbind(svyyear=layout$svyyear[svy], inst))
})
inst2 <- lapply(7:nrow(layout), function(svy) {
  cat(paste(svy, "of", nrow(layout), "\n")); flush.console()
  ## Load comopletions  file ---------------------------------------------------------------
  inst <- load.data(layout, svy, "inst_file", c("fips", "sector", "control")) 
  ## make unitid a factor variable for later merging
  inst[, unitid := factor(unitid)]
  ## Bind files together ----------------------------------------------------------------------------
  return(cbind(svyyear=layout$svyyear[svy], inst))
})
## Collapse all inst.data and save ----------------------------------------------------------------------
inst1 <- rbindlist(inst1, use.names=T, fill=T)
inst2 <- rbindlist(inst2, use.names=T, fill=T)
inst.data <- rbindlist(list, use.names=T, fill=T)

#create Table 1; library(tableone)
#Create a variable list which we want in Table 1
listVars <- c("Gender", "Age", "Cholesterol", "SystolicBP", "BMI", "Smoking", "Education")
#Define categorical variables
catVars <- c("Gender", "Smoking", "Education")
#Total Population
table1 <- CreateTableOne(vars = listVars, data = dt, factorVars = catVar)
#Stratified by Gender
#Remove Gender from list of vars
table1 <- CreateTableOne(listVars[2:length(listVars)], dt, catVars, strata = catVars[1])
#export Table 1 to Microsoft Word; library(ReporteRs); library(magrittr)
table1 <- print(table1)
# The script
docx( ) %>% 
  addFlexTable(table1 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold( color = "white" ),
                           add.rownames = TRUE ) %>%
                 setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = "table1.docx")

#DATA SCIENCE 

#year-over-year change
DT <- DT[, lapply(.SD, sum, na.rm=TRUE), .SDcols="yvar", by=c("year")]
DT[, yoy := c(NA, diff(yvar))]
#year-over-year change, if var1 and var 2 are identifying variables (like states)
DT <- DT[, lapply(.SD, sum, na.rm=TRUE), .SDcols="yvar", by=c("year","var1","var2")]
DT[, yoy := c(NA, diff(yvar)), by=c("var1","var2")]

#a/b testing

#optimal sample size to pick up effect for a/b test
#Use p1 to give the known value (e.g. current Conversion Rate) and 
#p2 to state the smallest effect you want to detect with the test.
power.prop.test(p1=0.015, p2=0.025, sig.level=0.05, power=0.8)

# Use pwr.2p2n.test {pwr} to compute sample size needed with two unequal groups.
# library(pwr)
pwr.2p2n.test(h = NULL, n1 = NULL, n2 = NULL,
              sig.level = 0.05, power = NULL,
              alternative = c("two.sided", "less","greater"))
# h - Effect size.
# n1 - Number of observations in the first sample
# n2 - Number of observationsz in the second sample
# sig.level - Significance level (Type I error probability)
# power - Power of test (1 minus Type II error probability)
# alternative - a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less"

#The actual test is performed using the prop.test {stats} from the base distribution
prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
# x - a vector of counts of successes, a one-dimensional table with two entries, or a two-dimensional table (or matrix) with 2 columns, giving the counts of successes and failures, respectively.
# n - a vector of counts of trials.
# p - a vector of probabilities of success.
# alternative - a character string specifying the alternative hypothesis.
# conf.level - confidence level of the returned confidence interval.
# correct - a logical indicating whether Yates' continuity correction should be applied where possible.



###hadley wickham exercises

install.packages("tidyverse")
library(tidyverse)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color=class)) +
  geom_smooth()

ggplot(data=mpg) +
  geom_point(aes(x=displ,y=hwy)) + 
  facet_wrap(~class,nrow=2)



foo_foo %>%
  hop(through=forest) %>%
  scoop(up=field_mouse) %>%
  bop(on=head)

my_pipe <- function(.) {
  . <- hop(.,through=forest)
  . <- scoop(.,up=field_mice)
  bop(.,on=head)
}
my_pipe(foo_foo)

#rescale functin
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(c(0,5,10,NaN))

#mean function
meanfunc <- function(x){
  rng <- range(x, na.rm =T, finite = TRUE)
  ((rng[2] - rng[1]) / 2)
}


meanfunc2 <- function(x){
  mean(x,na.rm=T)
}

propfunc <- function(x){
  temp <- sum(x,na.rm=T)
  (x / temp)
}

zfunc <- function(x){
  sd(x,na.rm = T) / mean(x,na.rm = T)
}


if (y < 20) {
  x <- "Too low"
} else {
  x <- "too high"
}

#require's lubridate package
#function to tell you if it's morning, afternoon, or evening
if (hour(Sys.time()) > 0 && hour(Sys.time()) < 12) {
  message("Good Morning")
} else if (hour(Sys.time()) >= 12 && hour(Sys.time()) < 17) {
  message("Good Afternoon")
} else {
  message("Good Evening")
}

#function if numbers are evenly divisible
if (n %% 3 == 0) {
  message("fizz")
}
if (n %% 5 == 0) {
  message("buzz")
}
if (n %% 3 ==0 && n %% 5 ==0) {
  message("fizzbuzz")
}

#to avoid floating integer problem
near(sqrt(2) ^ 2, 2)


#function producing standard error
standard_error <- function(x) {
  sqrt(var(x)/length(x))
}


#function to reverse a string
string_rev <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}


string_rev <- function(x) {
  strsplit(x, NULL) %>% lapply(rev) %>% sapply(paste, collapse="")
}

#to sum values of a vector, ignoring infinite values, and removing missing values
sum(x[is.finite(x)],na.rm=T)


#for loop computing mean of every column in "cars"
output <- vector("integer",length=ncol(cars))
for (i in seq_along(cars)) {
  output[[i]] <- mean(cars[[i]])
}

#for loop determining column type of every column in "cars"
output <- vector("character",length=ncol(cars))
for (i in seq_along(cars)) {
  output[[i]] <- typeof(cars[[i]])
}

#for loop computing number of unique values in each column of "iris"
output <- vector("integer",length=ncol(iris))
for (i in seq_along(iris)) {
  output[[i]] <- length(unique(iris[[i]]))
}

#for loop creating 10 random normals for each Mu=-10, 0, 10, 100
#choosing to compute 50 values for each random normal
vecofmeans <- c(-10,0,10,100)
k <- 50
output <- matrix(nrow=k,ncol=length(vecofmeans))
for (i in seq_along(vecofmeans)){
  output[,i] <- rnorm(k,mean=vecofmeans[[i]])
}

#loop through to create multiple new variables (e.g., top box scores)
tbcols <- grep("TOTAL_TB",colnames(cc),value=T)
trcols <- grep("TOTAL_RSPNS",colnames(cc),value=T)
for (i in c(1:7)){
  cc[, paste0("tbscore_q2_",i)] <- cc[, tbcols[[i]], with=F] / cc[, trcols[[i]], with=F]
}

#for loop to figure out how many coin flips you'll need to get 3 heads
flip <- function() sample(c("T","H"),1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}

#for loop that transforms the variables "disp" and "am" in the "mtcars" data
#the "disp" function scales the values down, and the "am" function turns 0/1 to auto/manual
trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)){
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

#function that returns the absolute distance between x and mean of x, raised to the i'th
absmeantotheith <- function(x,i) abs(x - mean(x)) ^ i

#function to compute column mean, median, and sd
#to use: col_summary(df, mean); col_summary(df, median); col_summary(df, sd)
col_summary <- function(df, fun) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- fun(df[[i]])
  }
  output
}


#mapping; it's the new lapply!
#each function takes a vector as an input, applies a function to each piece, and returns a new vector of the same length
#map() makes a list
#map_lgl() makes a logical vector
#map_int() makes an integer vector
#map_dbl() makes a double vector
#map_chr() makes a character vector

#for example
#returns a list of all columns in "cars" multipled by 2
map(cars, function(x) x * 2)
#returns a double of the "dist" column in "cars" multiplied by 2
map_dbl(cars[,dist], function(x) x * 2)

#these are functionally the same, but one uses piping
map(cars,mean)
cars %>% map(mean)

#select elements of a list of lists by position
x <- list(list(1,2,3),list(4,5,6),list(7,8,9))
#returns the 2nd element of each list
x %>% map_dbl(2)



#checking for duplicate columns
#temp <- dbc[,which(!duplicated(t(dbc))),with=FALSE]
#setdiff(names(dbc), names(temp))

#take db, filter by - Store_Role where the 2nd through 3rd characters equal "ar" (e.g., Barista)
##then, group by Store_Role and and full_time status
##then, summarize the total counts in each category (Store_Role/Full Time status combo)
temp <- db %>%
  filter(Store_Role %>% substr(2,3) %>% equals("ar")) %>%
  group_by(Store_Role, full_time) %>%
  summarise(total = n())

aggregate_db <- db %>%
  group_by(Store_Role) %>%
  summarise(full_time = mean(full_time)) %>%
  #ungroup %>%
  arrange(1-full_time) %>%
  mutate(full_time2 = full_time %>% cumsum)

#proportion in group
db %>%
  group_by(var1, var1) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

#freq table
ce %>% transmute(vis_bin, percent = USER_COUNT/sum(USER_COUNT))


#relative weights analysis -- library(flipRegression)
Regression(yvar ~ xvar1 + xvar2 + xvar3, data=datasetname,
           output = "Relative Importance Analysis")

#residual plot
lm1 <- lm(yvar ~ xvar, data=DT)
lm1res <- resid(lm1)
plot(DT[,xvar],lm1res)

#patchwork plots; library(patchwork)
#any arrangement
p1 + p2
#space between plots
p1 + plot_spacer() + p2
#2 plots horizontally next to each other, 1 wide plot below them
p1 + p2 - p3
#3 plots horizontally next to each other, 1 wide plot below them
(p1 | p2 | p3) / p4

#convert timestamp to numeric; library(chron)
dt[, timestamp := as.numeric(chron(times = timestamp))*24]

#quartile or ntile
dt <- dt %>% mutate(tertile = ntile(valuevariable, 3))
dt <- dt %>% mutate(quartile = ntile(valuevariable, 4))


#CALCULATE MODE FUNCTION
#set up functions
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#GRABS COLUMN NAME OF COLUMN WITH MAX VALUE
dt[, new_var := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("var1","var2","var3","var4","var5")]


#break GUIDs into 10 samples
#random number between 0 and 1
randum <- runif(nrow(mcomp),0,1)
mcomp <- cbind(mcomp,randum)
mcomp <- mcomp %>% mutate(sample = ntile(randum, 10))
setDT(mcomp)


#rolling 2 months cc (topline)
str2m <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_r2m.csv")
#calculate rolling two by day part
str2m[, lag_TB :=lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="TOTAL_TB"]
str2m[, lag_RSPNS :=lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="TOTAL_RSPNS"]
#sum together
str2m[, R2MTB := rowSums(.SD, na.rm = TRUE), .SDcols=c("TOTAL_TB","lag_TB")]
str2m[, R2MRSPNS := rowSums(.SD, na.rm = TRUE), .SDcols=c("TOTAL_RSPNS","lag_RSPNS")]
#drop earliest month
str2m <- str2m[(FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM>=3)|(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM<=3)]
str2m[, R2MCC := round(R2MTB/R2MRSPNS,3)]
#make month year var
str2m[, FPFY := paste0(FSCL_YR_NUM,".",FSCL_PER_IN_YR_NUM)]
str2m[, FPFY := as.numeric(FPFY)]
str2m[, FPFYlabel := paste0(FSCL_YR_NUM,"-",FSCL_PER_IN_YR_NUM)]



##line chart split by two variables, with a geom_text line height variable

#make year-month variable for plotting
can[, fyfp := paste0(FSCL_YR_NUM,".",str_pad(can[,FSCL_PER_IN_YR_NUM],2,pad="0"))]

#make a plotting height label
can[variable=="SO_SCORE"&area=="Canada", value_y := value+2.5]
can[variable=="SO_SCORE"&area=="Vancouver", value_y := value-2.5]
can[variable=="CC_SCORE"&area=="Canada", value_y := value+2.5]
can[variable=="CC_SCORE"&area=="Vancouver", value_y := value-2.5]

#set labels
xlabels <- c("Mar 17", "Apr 17", "May 17", "June 17", "July 17", "Aug 17", 
             "Sep 17", "Oct 17", "Nov 17", "Dec 17", "Jan 18", "Feb 18")
ylabel <- "TB Score"
tlabel <- "Vancouver, Canada Customer Experience"
sublabel <- "Company-Operated Stores, March 2017 - February 2018"
caption <- "Canada Store N = 1,750\nVancouver Store N = 122"
#manual legend labels
lname1 <- "Area"
llabels1 <- c("Canada","Vancouver")
lname2 <- "Metric"
llabels2 <- c("Customer Connection","Store Operations")
#values
pdata <- can
px <- can[,fyfp]
py <- can[,value]
groupvar <- can[,variable]
colourvar <- can[,area]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=px, y=py, colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_point(size=1) + geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(labels=xlabels) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,70)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  annotate(size=5, geom="text", x=1, y=63, label= "Store Operations",hjust = 0) +
  annotate(size=5, geom="text", x=1, y=38, label= "Customer Connection",hjust = 0) +
  geom_text(size = 4, aes(label=py,y=value_y), stat="identity")
print(plot2)

