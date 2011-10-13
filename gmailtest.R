library(Gmail)
username = "ashaegupta@gmail.com"
friendname = "shreyansb@gmail.com"

#test
# Define user, friend, and previously stored filename
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

filename = "~aegupta/output-2011-03-16-00-16-06.csv"

email <- read.csv(filename, header = TRUE, sep = ",",
                        colClasses = c("character", "RFC.date", "character", "RFC.list", "character","character", "character", "RFC.list"),
                    fill = TRUE)

# Internal function to add a column to email data.frame that calculates & stores reply time
.set.reply.times <- function(email){
  email$Reply.Time <- as.double(email$Date - email$Date[match(email$ReplyID, email$MsgID)])
  email$Reply.Time[email$Reply.Time >= 1000000000 | email$Reply.Time < 0] <- NA
  email
}

email <- .set.reply.times(email)

nrow(email)
email$Reply.Time
names(email)
head(email)

i = 2
d <- email$Date[i]
z <- as.POSIXlt(Sys.time())
z - d
email$MsgID[i]
email$To[i]
email$From[i]
email$Subject[i]
email$ReplyID[i]
email$Refs[i]
email$Reply.Time[i]

userreply <- user.reply.time(email, username)
head(userreply)

friendreply <- friend.reply.time(email, username)
head(friendreply)

reply.times(email, username, plotby="weekday")
reply.times(email, username, plotby="hour")

reply.times(email, username, remove.outliers=FALSE, plotby="weekday")
reply.times(email, username, remove.outliers=FALSE, plotby="hour")

one <- one.to.one(email)
head(one)

many <- one.to.many(email)
head(many)

to.from.ratio(email, username)

words <- words.in.subject(email)
subject.cloud(email)

sent.vs.received(email, username, friendname)
sent.vs.received(email, username, friendname, plotby="hour")
sent.vs.received(email, username, friendname, plotby="month")
sent.vs.received(email, username, friendname, plotby="day")

