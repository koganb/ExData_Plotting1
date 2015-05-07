


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



#dataset <- loadDataset("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
#dataset2days <- processAndFilterDataset(dataset)


par(mfrow=c(1,1), mar=c(4,4,2,1), cex.lab=0.9)
with(dataset2days, {
	plot(DateTime, Sub_metering_1, type ="n", xlab="", ylab="Energy sub metering")
	lines(DateTime, Sub_metering_1)
	lines(DateTime, Sub_metering_2, col="red")
	lines(DateTime, Sub_metering_3, col="blue")
    
    #fix the png renderig move the labels left
    legendLeftGap <-paste0(rep(" ",25), collapse="")
	legend("topright", c(paste0("Sub_metering_1", legendLeftGap),
	                     paste0("Sub_metering_2", legendLeftGap), 
                         paste0("Sub_metering_3", legendLeftGap)), col=c("black", "red", "blue"), lty=c(1,1,1), cex=0.7, pt.cex = 1)
 })

dev.copy(png,'plot3.png')
dev.off()

 