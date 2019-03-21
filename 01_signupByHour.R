###
### Data Visualization Society challenge nr. 1
### ------------------------------------------
###
### Visualize the number of people who joined the Data Visualization Society for each
### hour of the day, split up in three groups (data, visualization, society) based on
### their self-assigned score.
###
### Alexander Koch, 2019
###

library(data.table)
library(lutz)
library(sf)

x = fread('https://raw.githubusercontent.com/emeeks/datavizsociety/master/challenge_data/dvs_challenge_1_membership_time_space.csv', data.table=F)

# The timestamp of the signup data corresponds to US Eastern time, regardless of the
# timezone of the person that signed up. So before we can combine the time of signup
# of all the people in the dataset, we will have to recalculate the time of signup to
# the local time in everyone's respective timezone. We can figure out the timezones
# using the lat and long coordinates and the lutz and sf packages.
x$timezone = tz_lookup_coords(lat=x$lat, lon=x$long, method='accurate')
x = x[!is.na(x$timezone),]

# Convert each hour of signup to the corresponding hour in the timezone
# where the signup happened.
x$signupTime = NA
for (i in 1:nrow(x)) {
	# Convert the date and hour variable to a standardized format.
	hour = gsub('^.+ ', '', x$date_with_hour[i])
	hour = paste0(hour, ':00')
	date = gsub(' .+$', '', x$date_with_hour[i])
	date = unlist(strsplit(date, '/'))
	date = paste0(date[3], '-', date[1], '-', date[2])
	dateHour = paste0(date, ' ', hour)
	posixDate = as.POSIXct(dateHour, tz='America/New_York')
	formattedDate = format(posixDate, tz=x$timezone[i], usetz=T)
	if (grepl('\\d+:\\d+:\\d+', formattedDate)) {
		signupTime = sub('^.+( \\d+:\\d+:\\d+ ).+$', '\\1', formattedDate)
		signupTime = trimws(signupTime)
	} else {
		signupTime = NA
	}
	x$signupTime[i] = signupTime
}

# Remove the rows without a signup time.
x = x[!is.na(x$signupTime),]

# Convert the timepoint of signup, which includes half hours (e.g. 08:30) to full
# hours (e.g. 08:30 -> 08:00).
x$signupHour = gsub(':.+$', '', x$signupTime)
x$signupHour = as.numeric(x$signupHour)

# We will visualize the number of people that signed up at a particular timepoint, split
# up by their highest scoring characteristic (data, viz, society), using a barplot. First,
# we have to count the number of signups at each hour of the day per group. We will only
# count those people for whom one of the three groups had a higher score than the two others.
timepoints = seq(0, 23, by=1)
plotData = matrix(nrow=length(timepoints), ncol=3, data=0)
colnames(plotData) = c('data', 'visualization', 'society')
rownames(plotData) = timepoints
for (i in 1:length(timepoints)) {
	xSub = x[which(x$signupHour == timepoints[i]),c('data', 'visualization', 'society')]
	if (nrow(xSub) > 0) {
		for (j in 1:nrow(xSub)) {
			highestScore = which(xSub[j,] == max(xSub[j,]))
			if (length(highestScore) == 1) {
				plotData[i,highestScore] = plotData[i,highestScore] + 1
			}
		}
	}
}
plotData = t(plotData)

# Create the barplot.
pdf('signupByHour.pdf', width=12, height=5)
background = '#1d1919'
textColor = '#c5c5c5'
categoryColors = c('#00b9a7', '#dfb429', '#bb6bab')
par(lwd=2, bg=background, mar=c(5, 4, 6, 2))
b = barplot(plotData, beside=T, border=background, col=categoryColors, xaxt='n', yaxt='n')

# Mark the peak signup times for the different groups.
peakData = which(plotData['data',] == max(plotData['data',]))
peakViz = which(plotData['visualization',] == max(plotData['visualization',]))
peakSoc = which(plotData['society',] == max(plotData['society',]))
par(xpd=T)
segments(b[1,peakData], max(plotData['data',]), b[1,peakData], max(plotData['data',]) + 25, lwd=0.5, col=textColor)
text(b[1,peakData], max(plotData['data',]) + 27, expression('Peak signup time for the ' * phantom('data') * ' group'), col=textColor, adj=c(0, 0))
text(b[1,peakData], max(plotData['data',]) + 27, expression(phantom('Peak signup time for the ') * 'data' * phantom(' group')), col=categoryColors[1], adj=c(0, 0))
segments(b[2,peakViz], max(plotData['visualization',]), b[2,peakViz], max(plotData['visualization',]) + 20, lwd=0.5, col=textColor)
text(b[2,peakViz], max(plotData['visualization',]) + 22, expression('Peak signup time for the ' * phantom('visualization') * ' group'), col=textColor, adj=c(0, 0))
text(b[2,peakViz], max(plotData['visualization',]) + 22, expression(phantom('Peak signup time for the ') * 'visualization' * phantom(' group')), col=categoryColors[2], adj=c(0, 0))
segments(b[3,peakSoc], max(plotData['society',]), b[3,peakSoc], max(plotData['society',]) + 45, lwd=0.5, col=textColor)
text(b[3,peakSoc], max(plotData['society',]) + 47, expression('Peak signup time for the ' * phantom('society') * ' group'), col=textColor, adj=c(0, 0))
text(b[3,peakSoc], max(plotData['society',]) + 47, expression(phantom('Peak signup time for the ') * 'society' * phantom(' group')), col=categoryColors[3], adj=c(0, 0))

# Figure annotation.
title(xlab='Hour of the day at signup (local time)', col.lab=textColor, cex.lab=1.2)
timepointNames = paste0(timepoints, ':00')
timepointNames[which(nchar(timepointNames) == 4)] = paste0('0', timepointNames[which(nchar(timepointNames) == 4)])
axis(1, at=b[2,], labels=timepointNames, lwd=0, col.axis=textColor, cex.axis=0.7, line=-0.5, font=2)
axis(2, at=seq(0, 90, 10), labels=NA, lwd=0, lwd.ticks=0.5, col=textColor, tck=-0.02)
axis(2, at=seq(0, 90, 10), lwd=0, col.axis=textColor, las=1, line=-0.5, cex.axis=0.7, font=2)
text(-2, 100, 'Number of signups', col=textColor, cex=1.2, adj=c(0, 0))
legend(0, max(plotData) / 2, c('data', 'visualization', 'society'), col=categoryColors, pch=15, bty='n', text.col=textColor, yjust=0.5)
dev.off()
