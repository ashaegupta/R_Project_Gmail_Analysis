\name{sent.vs.received}
\alias{sent.vs.received}
\title{ Plot bar graph of number of emails sent vs. received between user and friend
}
\description{
Plot bar graph of number of emails sent vs. received between user and friend.
}
\usage{
sent.vs.received(email, username, friendname, plotby = "weekday")
}

\arguments{
  \item{email}{A data.frame of emails to be analyzed}
  \item{username}{email address of the user}
  \item{friendname}{email address of the friend}
  \item{plotby}{Plotting options. "weekday" == by day of week, "hour" == hour of day, "month" == by month over time, and "day" == over time by day. Default is set to "weekday"}
}

\value{A bar chart of emails sent vs. received.}

\author{
Asha Gupta
}

\examples{
## create a sample email data.frame
username = "ashaegupta@gmail.com"
friendname = "shreyansb@gmail.com"
MsgID = c("<AANLkTik4rk1fKWfoXWv2B_M1s1+EQ17QJX+3pM_b3AJq@mail.gmail.com>","<AANLkTikzg_3CeVnSeJxsaTsNOhia3uNeypmfswnWGhFG@mail.gmail.com>","<AANLkTimhm2TMHB5+UDREx7Sc+anQErrtEo_5krWYQ5d1@mail.gmail.com>")
Date = c(as.POSIXlt("2011-01-01 08:35:36"), as.POSIXlt("2011-01-02 19:16:24"), as.POSIXlt("2011-01-03 01:45:26"))
Friend = c("shreyansb@gmail.com","shreyansb@gmail.com", "shreyansb@gmail.com")
To = c("shreyansb@gmail.com", "ashaegupta@gmail.com", "ashaegupta@gmail.com")
From = c("ashaegupta@gmail.com", "ashaegupta@gmail.com", "shreyansb@gmail.com")
Subject = c("Hello", "Re: Hello", "Hello World")
ReplyID = c("<AANLkTinikCecFBJu1KvL4diN+sX15iBS6PB6f7+jhxcV@mail.gmail.com>", "<AANLkTikzg_3CeVnSeJxsaTsNOhia3uNeypmfswnWGhFG@mail.gmail.com>", "<AANLkTimhm2TMHB5+UDREx7Sc+anQErrtEo_5krWYQ5d1@mail.gmail.com>")
Refs = c("<AANLkTin5tGLn7BavjKoUvgA5z0TMb4Huzvgwe1m0Oq=V@mail.gmail.com>, <AANLkTinikCecFBJu1KvL4diN+sX15iBS6PB6f7+jhxcV@mail.gmail.com>", "", "<AANLkTikzg_3CeVnSeJxsaTsNOhia3uNeypmfswnWGhFG@mail.gmail.com>")
Reply.Time = c(NA, NA, 23342)
email <- data.frame(MsgID, Date, Friend, To, From, Subject, ReplyID, Refs, Reply.Time) 


sent.vs.received(email, username, friendname)
sent.vs.received(email, username, friendname, plotby="hour")

}
