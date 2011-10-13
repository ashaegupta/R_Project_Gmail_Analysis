\name{Gmail-package}
\alias{Gmail-package}
\alias{Gmail}
\docType{package}
\title{Fetch and run analysis on gmail usage}
\description{
Fetches body headers of emails, stores data in data frame, provides basic statistical analysis and visuals of email behavior. Meant primarily to compare email behavior between two people.
}
\details{
\tabular{ll}{
Package: \tab Gmail\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-03-15\cr
License: \tab GLP (>=2)\cr
LazyLoad: \tab yes\cr
}

}
\author{
Asha Gupta
Maintainer: <ashaegupta@gmail.com>
}

\keyword{package}

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

# show email data
head(email)

# get to vs. from ratio
to.from.ratio(email, username)

# word cloud of subject data
subject.cloud(email)

# plot reply time
reply.times(email, username, plotby="weekday")
}
