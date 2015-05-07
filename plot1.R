


loadDataset <- function(url) {	
	temp <- tempfile()
	download.file(url,temp)
	dataset <-read.csv(unz(temp, "household_power_consumption.txt"), sep=";", na.strings = "?")
	unlink(temp)
	
	dataset
}


processAndFilterDataset <- function (dataset) {
	##convert date & time to POSIXlt
	dateTime <- mapply(function(x, y) paste(x,y, sep=" "), dataset$Date, dataset$Time)
	dataset$DateTime <- as.POSIXlt(strptime(dateTime, format="%d/%m/%Y %H:%M:%S"))

	require(lubridate)
	subset(dataset, year(DateTime) == 2007 & month(DateTime) == 2 & day (DateTime) %in% c(1,2))	
}



dataset <- loadDataset("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
dataset2days <- processAndFilterDataset(dataset)

par(mfrow=c(1,1), mai=c(1.5,1.5,1,0.1), cex.lab=0.9)
with(dataset2days, {
    hist(Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
})

dev.copy(png,'plot1.png')
dev.off()

 