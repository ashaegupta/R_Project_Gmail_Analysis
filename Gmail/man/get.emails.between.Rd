\name{get.emails.between}
\alias{get.emails.between}
\title{
Get emails between two users
}
\description{
Fetches emails using python's imaplib from the user's gmail account for analysis. Takes in a username, password, a friend's email address and start and end times. Creates a data.frame of emails.
}
\usage{
get.emails.between(username, password, friendname, startdate, enddate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{email address of the user}
  \item{password}{user's gmail password}
  \item{friendname}{email address of the friend to conduct analysis with}
  \item{startdate}{start of the email fetch date range with the format mm-dd-yyyy}
  \item{enddate}{end of the email fetch date range with the format mm-dd-yyyy}
}

\value{
Returns a data.frame of emails
	\item{data.frame}{Columns are MsgID, Friend, To, From, Date, Subject, ReplyID, Refs, Reply.Time}
}

\author{
Asha Gupta
}

\examples{}
