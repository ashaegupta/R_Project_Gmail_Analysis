import sys
import re
import imaplib
import getpass
import csv
import datetime
import email
from email.utils import parsedate_tz, mktime_tz
#
#   For Each Email Store the following data in the following order, separated by ",":
#       1. MsgID
#       2. Date
#       3. Friend
#       4. To
#       5. From
#       6. Subject
#       7. ReplyID
#       8. References
#
# Consider All Mail for this analysis.
# This analysis compares correspondence habits between "numoffriends" friends 
mailbox_name = "[Gmail]/All Mail"

# IMAP connect to gmail prompting user to input name and password
def connect(username, password, verbose=True):
    # Connect to the gmail server
    HOSTNAME = "imap.gmail.com"
    if verbose: sys.stdout.write("connecting to %s\n" % HOSTNAME)
    c = imaplib.IMAP4_SSL(HOSTNAME)

    # Ask the user to provide their credentials; Encrypt password with getpass 
    #username = raw_input("Please enter your gmail username: ")
    #password = getpass.getpass("Please enter password (passwords will not be stored): ")
    if verbose: sys.stdout.write("logging in as: %s to %s\n" % (username, HOSTNAME))
    try:
        c.login(username, password)
    except:
        sys.stdout.write("Couldn't connect. Please check your email login credentials and try again.\n")
        sys.exit()
    return c

# Connect to thdoe mailboxes
def getList(c):
    data = None
    try:
        typ, data = c.list()
    except:
        pass
    return data

# End connection
def logout(c):
    c.logout()

# UNUSED - Friends for analysis
def storeFriends():
    num = None
    inputIsNotInt = True
    num_attempt = 0
    while inputIsNotInt:
        try:
            num = int(num)
            inputIsNotInt = False
        except:
            if num_attempt > 0:
                sys.stdout.write("Please enter an integer\n")
            num = raw_input("How many email addresses does your friend have?: ")
        num_attempt += 1
    
    friends = []
    for i in range(0,num):
        if i == 0:
            friend = raw_input("Enter a email address for your friend (e.g., chris.lee@gmail.com): ")
        if i > 0: 
            friend = raw_input("Enter another email address for your friend (e.g., chris_lee@stanford.edu): ")
        friends.append(friend)
    return friends

def formatDate(date_string):
    m = re.match("^([0-9]{1,2})-([0-9]{1,2})-([0-9]{4})$", date_string)
    try:
        month = int(m.group(1))
        day = int(m.group(2))
        year = int(m.group(3))
        d = datetime.date(year=year, month=month, day=day)
        dStr = d.strftime("%d-%b-%Y")
        return d, dStr
    except:
        return False, False

def getDates(startDateInput, endDateInput):
    dateError = False
    startDate, startDateStr = formatDate(startDateInput)
    endDate, endDateStr = formatDate(endDateInput)
    try:
        if(startDate > endDate):
            dateError = True
    except:
        dateError = True
    if dateError:
        sys.stdout.write("There was an error. Please ensure that the entered date format is correct and that the start date is before the end date.\n")
        sys.stdout.write("Required date format is: mm-dd-yyyy\n")
        sys.exit()
    return startDateStr, endDateStr

# Gets and parses email between x and y since a specified date
def getEmailBetween(c, data, friend, startDateStr, endDateStr, writer):
    try:
        msg_ids = getMsg_ids(c, data, friend, startDateStr, endDateStr)
        num_emails = doFetchParsePrint(c, msg_ids, friend, writer)
        sys.stdout.write("\nSaved %s emails with %s\n" % (str(num_emails), friend))
    except:
        pass

# Get msg_ids for TO or FROM correspondence with 'friend' since Date
def getMsg_ids(c, data, friend, startDateStr, endDateStr):
    sys.stdout.write("Getting emails with %s...\n" % friend)
    friendstr = '"%s"' % friend
    msg_ids = []
    try: 
        for line in data:
            c.select(mailbox_name, readonly=True)
            typ, msg_ids = c.search(None, "((OR TO %s FROM %s) (NOT (OR (BEFORE %s) (SINCE %s))))" % (friendstr, friendstr, startDateStr, endDateStr))
    except:
        pass
    return msg_ids

# Fetch and parse all msgs in the array msg_ids
def doFetchParsePrint(c, msg_ids, friend, writer):
    sys.stdout.write("Parsing emails (this may take a few minutes)...\n")
    msg_ids = msg_ids[0].split()
    l = len(msg_ids)
    c.select(mailbox_name, readonly=True)
    for i in range(0,l):
        msg_data = doFetch(c, msg_ids[i])
        msg = doParse(c, msg_data)
        
        if msg:
            msg = RFCtoRFormat(msg)
        
            # Standarize format of data for each msg, writing '' when slots are empty 
            result = [
                msg.get('Message-ID'),
                msg.get('Date'),
                friend,
                msg.get('To'),
                msg.get('From'),
                msg.get('Subject') or '',
                msg.get('In-Reply-To') or '',
                msg.get('References') or ''
            ]
            
            # Append to CSV file
            writer.writerow(result)
        
        if (l%10==0):
            sys.stdout.write('.')
            
    return l

# Series of functions to convert an email from RFC format into a digestable format for R
def RFCtoRFormat(msg):
    #Convert RCF 822 date-time value into Unix time format
    dateRCF = msg.get('Date')
    datePy = int(mktime_tz(parsedate_tz(dateRCF)))
    #datePy = datePy[0:len(datePy)-1]
    #date = "%s-%s-%s %s:%s:%s" % (datePy[0], datePy[1], datePy[2], datePy[3], datePy[4], datePy[5])
    msg.replace_header('Date', datePy)

    #Convert RFC 822 TO string that contains both names and email addresses into a list of tos that only has email addresses
    toRFC = msg.get('To')
    tos = toRFC.split(',')
    newTos = []
    for t in tos:
        t = re.search('\S+\@\S+', t)
        if t and t.group(0):
            newTos.append(t.group(0))
    #tos = re.findall('\S+\@\S+', tos)
    tos = [(x.lstrip('<')).rstrip('>,') for x in newTos]
    tos = [(x.lstrip('"')).rstrip('"') for x in tos]
    tos = list(set(tos))
    msg.replace_header('To', tos)

    #Remove leading and trailing carrots of FROM address
    fromRFC = msg.get('From')
    fromEmail = re.findall('\S+\@\S+', fromRFC)
    fromEmail = (fromEmail[0].lstrip('<')).rstrip('>')
    msg.replace_header('From', fromEmail)

    #Convert RFC format for References into a list of strings of references
    refRFC = msg.get('References')
    if refRFC:
        refs = re.findall('\S+', refRFC)
        msg.replace_header('References', refs)

    return msg

# IMAP fetch msg with msg_id
def doFetch(c, msg_id):
    msg_data = []
    try:
        typ, msg_data = c.fetch(msg_id, '(FLAGS BODY[HEADER.FIELDS (MESSAGE-ID DATE TO FROM SUBJECT IN-REPLY-TO REFERENCES)])')
    except:
        pass
    return msg_data

# Parse a msg from RCF 822 format into a list of tuples
def doParse(c, msg_data):
    msg = None
    try:
        for response_part in msg_data:
            if isinstance(response_part, tuple):
                msg = email.message_from_string(response_part[1])
    except:
        pass
    return msg

def main(args):
    # parse command line arguments
    if len(args) != 5:
        sys.stdout.write("Not enough arguments. Required arguments are:\n")
        sys.stdout.write("[username] [password] [friend's email] [start date in the form mm-dd-yyyy] [end date in the same format]\n")
        return 
    else:
        username        = args[0]
        password        = args[1]
        friend          = args[2]
        startDateInput  = args[3]
        endDateInput    = args[4]

    # Open and append a new CSV file
    dt = datetime.datetime.now()
    filename = "output-%s.csv" % dt.strftime('%Y-%m-%d-%H-%M-%S')    
    headers = [
        "MsgID",
        "Date",
        "Friend",
        "To",
        "From",
        "Subject",
        "ReplyID",
        "Refs"
    ]
    writer = csv.writer(open(filename, "a"))
    writer.writerow(headers)
       
    c = None
    try:
        # Get and parse data
        c = connect(username, password)
        startDateStr, endDateStr = getDates(startDateInput, endDateInput)
        data = getList(c)
        getEmailBetween(c, data, friend, startDateStr, endDateStr, writer)
        sys.stdout.write("Saved output to file: %s\n" % filename)
    except Exception, e:
        sys.stdout.write("Sorry, could not extract emails. Error: %s\n" % e)
        sys.exit()
    finally:
        if c:
            c.logout()

if __name__ == '__main__':
    main(sys.argv[1:])
