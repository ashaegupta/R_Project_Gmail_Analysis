
# Before proceeding, please ensure IMAP has been enabled in your gmail settings, in order
# to fetch emails. Go to EMAIL SETTINGS --> FORWARDING AND POP/IMAP --> IMAP ACCESS --> ENABLE

#-----------------INITIALIZING AND FETCHING METHODS--------------------#
# Internal function to load packages required to run program
.onLoad <- function(libname, pkgname) {
	.libPaths("~aegupta/RLibrary")
	require(png)
	require(snippets)
	require(ggplot2)
	require(slam)
	require(tm)
}
   

# Retrieve all emails between a gmail account and a friend's account for a specified time period.
# Stores data as a data.frame.
get.emails.between <- function(username, password, friendname, startdate, enddate){
  email <- .fetch.emails(username, password, friendname, startdate, enddate)
  # Add a column and calculate reply times
  email <- .set.reply.times(email)
}

.split.list <- function(s) {
    if(!is.null(s)) {
      s <- gsub("\\[", "", s)
      s <- gsub("\\]", "", s)
      s <- gsub("'", "", s)
      s <- strsplit(s, ", ")
  }
}

# Internal function used for spliting RFC 2882 output into an R formatted list 
  # Converts UNIX "DATE" from RFC 2882 IMAP Fetch into a date in POSIXct form
  setClass("RFC.date")
    setAs("character", "RFC.date",
            function(from) as.POSIXct(ISOdatetime(1970,1,1,0,0,0) + as.integer(from)))

  # Converts RFC string "['a','b','c']" into an R list of strings ["a" "b" "c"]
    setClass("RFC.list")
    setAs("character", "RFC.list",
            function(from) .split.list(from))
  
# Internal function to fetch emails using python
.fetch.emails <- function(username, password, friendname, startdate, enddate){
  
  # Call python file to fetch and parse gmail data
  pyfilename <- system.file("python/gmail.py", package = "Gmail")
  system(paste("python", pyfilename, username, password, friendname,
               startdate, enddate))

  # The output produced by python is stored in [package root]/output/
  # Uses ls, grep and head to get the latest output file in this directory
  filename <- system("ls -t | grep output | head -1", intern=TRUE)

  # Converts UNIX "DATE" from RFC 2882 IMAP Fetch into a date in POSIXct form
  #setClass("RFC.date")
  #  setAs("character", "RFC.date",
  #          function(from) as.POSIXct(ISOdatetime(1970,1,1,0,0,0) + as.integer(from)))

  # Converts RFC string "['a','b','c']" into an R list of strings ["a" "b" "c"]
  #  setClass("RFC.list")
  #  setAs("character", "RFC.list",
  #          function(from) .split.list(from))
  
  # Read-in data from CSV file  
  email <- read.csv(filename, header = TRUE, sep = ",",
			colClasses = c("character", "RFC.date", "character", "RFC.list", "character","character", "character", "RFC.list"),
                    fill = TRUE)
  
  #email$Date <- as.POSIXct(ISOdatetime(1970,1,1,0,0,0) + as.integer(email$Date))
  #email$To <- .split.list(email$To)
  #email$Refs <- .split.list(email$Refs)
  
}
       
# Internal function to add a column to email data.frame that calculates & stores reply time
.set.reply.times <- function(email){
  email$Reply.Time <- as.double(email$Date - email$Date[match(email$ReplyID, email$MsgID)])
  email$Reply.Time[email$Reply.Time >= 1000000000 | email$Reply.Time < 0] <- NA
  email
}




#------------------DATA MANIPULATIONS------------------------#

# Create a data.frame of user reply times and reply dates
user.reply.time <- function(email, username){
  isRelevant <- (!(email$From == username) & !(is.na(email$Reply.Time)))
  data <- email[isRelevant,c(2,9)] 
}

# Create a data.frame of friend reply times and reply dates
friend.reply.time <- function(email, username){
  isRelevant <- ((email$From == username) & !(is.na(email$Reply.Time)))
  data <- email[isRelevant,c(2,9)]
}

# Returns a data.frame of all emails only between the user and friend (e.g., no group emails)
one.to.one <- function(email){
  one.to.one <- email[as.logical(lapply(email$To, length)==1),]
}

# Returns a data.frame of all group emails
one.to.many <- function(email){
  one.to.many <- email[as.logical(lapply(email$To, length) > 1),]
}

# Returns ratio of emails sent to vs. from the user
to.from.ratio <- function(email, username){
  from.user <- sum(email$From == username)
  to.user <- sum(!email$From == username)
  to.user / from.user
}

# Returns a table with all words in the subject lines of emails and word frequencies
words.in.subject <- function(email, remove.common.words = TRUE){
  subject <- email$Subject
  subject <- gsub("\n", " ", subject)
  subject <- gsub("[[:punct:]]", " ", subject)
  subject <- gsub("[[:digit:]]", " ", subject)
  subject <- tolower(subject)
  subject <- paste(subject, collapse=" ")
  subject <- (strsplit(subject, " ")[[1]])
  subject <- subject[(nchar(subject) > 1)]
  if (remove.common.words) {
    common.words <- c("am","are","back","be","been","being","fb","get","going","got","get","your","each","up","way","let","not","now","new","see","should","would","could","let's","lets","til","trying","just","did","last","cause","i'm","still","our","there","try","sure","how","add","day","don't","having","here","may","next","want","email","has","have","later","pls","too","here's","then","what's","yes","had","i've","again","we've","and","any","as","at","by","for","from","i","if","in","into","it","its","me","of","off","on","or","than","that","their","theirs","them","these","they","this","those","till","to","unto","upto","us","w","we","what","whatever","when","which","whichever","while","who","whoever","whom","whomever","whose","with", "the", "fwd")
    subject <- subject[(nchar(subject) > 2) & !(subject %in% common.words)]
  }
  w <- sort(table(subject),decreasing=TRUE)
} 



#------------------VISUALIZATIONS------------------------#

# Create a word cloud from words used in subject lines
subject.cloud <- function(email){
  w <- words.in.subject(email)
  cloud(w, col = col.br(w, fit = TRUE))
}


# Plot bar graph of number of emails sent vs. received between user and friend
# Options for plotting by category (day of week or hour of day) OR over time (by month or day)
sent.vs.received <- function(email, username, friendname, plotby="weekday"){

  # Get emails sent only between username and friendname
  email <- one.to.one(email)
  sent <- (email$From == username)
  received <- (email$From == friendname)
  email <- email[(sent | received),]

  # Specify chart title depending on plot increments
  if (plotby=="weekday"){
    plottype = "%A"
    title <- "Sent vs. Received by Week Day"
    xlabel = "Day of Week"
  } else if (plotby =="hour") {
    plottype = "%H"
    title <- "Sent vs. Received by Hour of Day"
    xlabel = "Hour of Day"
  } else if (plotby =="month") {
    plottype = "%Y-%m"
    title = "Sent vs. Received Over Time"
    xlabel = "Month"
  } else if (plotby =="day") {
    plottype = "%Y-%m-%d"
    title = "Sent vs. Received Over Time Per Day"
    xlabel = "Day"
  }

  # Create a new data.frame for plotting
  date <- strftime(email$Date, plottype)
  from <- email$From
  all <- data.frame(cbind(from, date))
  
  # Create plot
  if(!(plotby=="day" | plotby=="time")){
    ggplot(all, aes(date)) + geom_bar(aes(fill=from), position="dodge") +
      opts(title=title)
  } else {
      r <- range(date)
      ggplot(all, aes(date)) + geom_bar(aes(fill=from), position="dodge") +
      opts(title=title)
  }
}


# Plot the reply times between user and friend
# Options for plotting by day of week or by hour of day
# Option to remove or keep outliers
reply.times <- function(email, username, remove.outliers=TRUE, plotby="weekday") {

  # Get reply times
  user <- user.reply.time(email, username)
  friend <- friend.reply.time(email, username)
  
  # Convert reply time in secs into hours
  user$Reply.Time <- user$Reply.Time/3600
  friend$Reply.Time <- friend$Reply.Time/3600
  
  # Convert date into plotby
  if (plotby=="weekday"){
    plottype="%w"
  }else{
    plottype="%H"
  }
  user$Date <- as.integer(strftime(user$Date, plottype))
  friend$Date <- as.integer(strftime(friend$Date, plottype))

  # Remove outliers
  if (remove.outliers) {
    qnt1 <- quantile(user$Reply.Time, probs=c(.25, .75))
    H1 <- 1.5 * IQR(user$Reply.Time)
    is.outlier1 <- ((user$Reply.Time < (qnt1[1] - H1)) |
                   (user$Reply.Time > (qnt1[2] + H1)))
    user <- user[!is.outlier1,]
    
    qnt2 <- quantile(friend$Reply.Time, probs=c(.25, .75))
    H2 <- 1.5 * IQR(friend$Reply.Time)
    is.outlier2 <- ((friend$Reply.Time < (qnt2[1] - H2)) |
                   (friend$Reply.Time > (qnt2[2] + H2)))
    friend <- friend[!is.outlier2,]
  }

  # Get mean
  umean <- mean(user$Reply.Time)
  ulabel <- paste("user mean: ",umean)
  fmean <- mean(friend$Reply.Time)
  flabel <- paste("user mean: ",umean)
  
  # Create plot
  p <- ggplot(data=user, aes(x=Date, y=Reply.Time)) +
    geom_point(colour = "purple", size=2) +
    geom_line(stat="hline", yintercept="mean", colour = "purple", size=0.5) +
      geom_text(aes(x,y, label = "user mean"), 
                data = data.frame(x = 0, y = umean), size = 3, hjust = 0,
                vjust = 0, angle = 0) +
    geom_point(data=friend, aes(x=Date, y=Reply.Time), colour = "orange", size=2) +
    geom_line(y=fmean, colour = "orange", size=0.5) +
      geom_text(aes(x,y, label = "friend mean"), 
                data = data.frame(x = 0, y = fmean), size = 3, hjust = 0,
                vjust = 0, angle = 0) +
    opts(title = "Reply Times Comparison (purple = user)")

    if (plotby=="weekday") {  
     scaleX =  scale_x_continuous("Day of Week", breaks=c(0,1,2,3,4,5,6),
                       labels=c("Sunday", "Monday", "Tuesday","Wednesday","Thursday","Friday",
                         "Saturday"))
    } else {
      scaleX = scale_x_continuous("Hour of Day", breaks=c(0:23), labels=c(0:23))
      }
  p + scaleX +  scale_y_continuous("Reply Time (hours)")
}

