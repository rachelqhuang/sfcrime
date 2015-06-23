##############################
##sanfrancisco crimes
############################

##############################STEP0: PACKAGES####################################################
multi_instalation("xlsx")
multi_instalation("dplyr")
multi_instalation("data.table")
################################STEP1: FUNCTIONS###################################################
multi_instalation <- function(pkg){
    
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]   # check what packages have been installed and then compared with the input packages to see if they have been installed
    
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)  #only install new packages
}

###################################################################################################
#find directory
getwd()
list.files(getwd())

#unzip the files
unzip("train.csv.zip")
unzip("test.csv.zip")

#quick way to read table
colc <- c("dates","character","character","factor","character","character","character","numeric","numeric")
lapply("train.csv",read.csv,colClasses = colc,header=T)

#read train table
train <- read.csv("train.csv",header=T)  #should specify column class
#read test table
test <- read.csv("test.csv",header=T)  #should specify column class
#change names to lower case
names(train)<- tolower(names(train))
names(test)<-tolower(names(test))

#combine the train and test
# test$category <- NA
# test$resolution <- NA
# test$description <- NA
# head(test)
train$type <- "train"
test$type <- "test"

# function to combine two data sets with different column count and names
            
            rbind.all.columns <- function(x, y) {
                
                x.diff <- setdiff(colnames(x), colnames(y))
                y.diff <- setdiff(colnames(y), colnames(x))
                
                x[, c(as.character(y.diff))] <- NA
                
                y[, c(as.character(x.diff))] <- NA
                
                return(rbind(x, y))
            }

newdf <- rbind.all.columns(train, test)
newdf$dates <-as.POSIXct(strptime(newdf$dates, "%Y-%m-%d %H:%M:%S"))
str(newdf)
head(newdf)
newdf2 <-data.table(newdf)

##################################################Group by different categorical variables to calculate how many event sfor each categry##############
setorder(newdf2, dates,category)

#check NA data in both datasets
nrow(is.na(train$category))
nrow(newdf2[is.na(newdf2$pddistrict),])
nrow(newdf2[is.na(newdf2$dates),])
dfsunday <- subset(newdf2,dayofweek=='Sunday')
nrow(dfsunday[is.na(dfsunday$dates),])

subset(newdf2,address=='VANNESS AV / SACRAMENTO ST' & dayofweek=='Sunday')

nrow(dfsunday)
tail(dfsunday)

group <- group_by(dfsunday,dates)
dfsunday_dates <- summarise(group,count=n())

nrow(dfsunday_dates)

#datamissing for sunday dates, approach: gather all sunday nd relevant addresse the plot it 
all_sunday_data <- subset(newdf2,dayofweek =='Sunday')
all_sunday_data_order <- all_sunday_data[order(dates,pddistrict),]

#add more variables: date and time separate
newdf2$date <- as.Date(newdf2$dates) #already got this one from the answers above
newdf2$time <- format(as.POSIXct(newdf2$dates) ,format = "%H:%M:%S")

#add more variables: street names and house numbers



#plot1: total crimes by date and time (January has high crime rate)
group <- group_by(newdf2,dates,dayofweek)
summ1<- summarise(group,count=n())
require(ggplot2)
summ1<-summ1[!is.na(summ1$dates),]
names(summ1)
summ1 <- subset(summ1,count >50,)
View(summ1)
ggplot(data = summ1, aes( dates, count )) + geom_line() 

#plot2: total crimes by day of the week, maybe calculate average crime rate by year by day of the week
group <- group_by(newdf2,dayofweek)
summ1 <- summarise(group,count=n())
ggplot(data = summ1 ,aes(dayofweek,count)) + geom_point()

#plot3: 