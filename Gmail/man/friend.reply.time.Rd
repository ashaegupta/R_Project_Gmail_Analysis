\name{friend.reply.time}
\alias{friend.reply.time}
\title{
Creates a data.frame of friend reply times and reply dates
}
\description{
Creates a data.frame of friend reply times and reply dates
Input a data.frame containing emails
}
\usage{
friend.reply.time(email, username)
}
\arguments{
  \item{email}{A data.frame to perform analysis on}
  \item{username}{The email address of the user}
}

\value{
Returns a data.frame with two columns reply date and reply time
  \item{Date }{date reply was sent}
  \item{Reply.Time }{time it took to reply to original message}
}
\author{
Asha Gupta
}


\seealso{
	\code{\link{user.reply.time}}
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

friendreply <- friend.reply.time(email, username)
head(friendreply)
friendreply$Date
friendreply$Reply.Time
}
