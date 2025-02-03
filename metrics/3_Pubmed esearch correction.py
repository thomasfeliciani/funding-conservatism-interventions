# The date of first pairing that I obtained via querying PubMed records (via NIH/NLM/NCBI API) has
# some missing value and artifacts. With this script I try to amend them.
#
# The first issue is with the missing values stemming from server-side errors or timed-out requests.
#
# The second issue is that the "equery" API do not order results (i.e. lists of publications using a
# certain MeSH term pair) by date in ascending order. To obtain the date of publication of the
# oldest publication (i.e. the date of first pairing) I therefore asked the serve to order results
# by date in descending order, and try extract the publication date of tge last result.
# The issue with this approach is that the API only returns up to the 9998th results only. 

# Last edit on 20240711 by Thomas Feliciani.
# Runs in Python 3.12.3


# Loading resources
from progress.bar import Bar
#import os
import pandas as pd
from time import sleep
import requests
from xml.etree import ElementTree
from datetime import datetime

DEBUG = False

# Defining a function to query PubMed APIs
def queryAPI (url, params, timeout = 10, maxAttempts = 10):
    success = False
    attempts = 1
    while not success:
        try:
            attempts += 1
            return requests.get(url, params = params, timeout = timeout)
            success = True
        except Exception as e: 
            print(datetime.now())
            print(e)
            print("Making attempt number", attempts, "in 5 seconds...")
            sleep(5)
            if attempts >= maxAttempts: 
                raise SystemExit("Can't reach server. Giving up :)")

    
# These are the API URLs
esearchBaseURL = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
efetchBaseURL = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"


# Reading API credentials.
# These are stored in a two-line txt file. The first line stores the email,
# and the second line the key.
# API keys can be generated through the account settings page of the 
# NIH/NLM/NCBI at https://account.ncbi.nlm.nih.gov/settings/
with open("./NIH MeSH/api key NCBI.txt", 'r') as f: data = f.read()
API_user, API_key = data.split('\n')
del data

delay = 0.08 # seconds. Waiting time between API calls.




# First issue: missing values ______________________________________________________________________
# Loading the dataset
with open("./NIH MeSH/output/MeSH_pairs_date_b.csv") as csvfile:
    d = pd.read_csv(csvfile, delimiter = ";")


# Let's see if there are entries to repair
torepair = d[d["NCBIapiStatus"] != "done"].index.tolist()


# If there are entries to repair, for each of them I query the server again:
if len(torepair) > 0:
    for i in torepair: 
        print("Attempting to repair MeSH pair #" + str(i))
        
        
        # I identify the two terms in the pair and costruct a PubMed query
        # accordingly:
        term1 = d["term1"][i]
        term2 = d["term2"][i]
        pubmedQuery = f'"{term1}"[MeSH Terms] AND "{term2}"[MeSH Terms]'
        
        
        # First query ________________________________________________________________ 
        # Finding how many results we get.
        response = queryAPI(
            url = esearchBaseURL, 
            params = {
                "api_key": API_key,
                "db": "pubmed",
                "term": pubmedQuery,
                "retmode": "json",
                "sort": "pub+date",  # Sort by publication date in descending order
                "rettype": "count",
                }
            )
        sleep(delay)
    
        if response.ok:
            count = int(response.json()["esearchresult"]["count"])
            d.loc[i, "nPairings"] = count
            if count == 0:
                d.loc[i, "NCBIapiStatus"] = "done"
                #writeNewLine(d, i)
                continue
        else: 
            d.loc[i, "NCBIapiStatus"]= "failed q 1" 
            #writeNewLine(d, i)
            continue
            
        # Search results are capped at 10k papers. However, the API won't return
        # the 10,000th result, but only the 9,998th. Thus:
        lastIndex = min(9998, count - 1)
        
        
        # Second query _______________________________________________________________
        # Finding the PubMed ID of the oldest paper:
        response = queryAPI(
            url = esearchBaseURL,
            params = {
                "api_key": API_key,
                "db": "pubmed",
                "term": pubmedQuery,
                "retmode": "json",
                "sort": "pub+date", 
                "retstart": lastIndex,
                }
            )
        sleep(delay) 
        
        if response.ok:
            pmid = response.json()['esearchresult']['idlist'][0]
        else: 
            d.loc[i, "NCBIapiStatus"] = "failed q 2" 
            #writeNewLine(d, i)
            continue
            
        
        # Third query ________________________________________________________________
        # Fetching the publication date of the oldest paper
        # We use a different PubMed eSearch API for this:
        response = queryAPI(
            url = efetchBaseURL, 
            params = {
                "api_key": API_key,
                "db": "pubmed",
                "id": pmid,
                "retmode": "xml",
                }
            )
        sleep(delay) 
    
        if response.ok:
            document = ElementTree.fromstring(response.text)
            pub_date = None
    
            article = document.findall(".//PubmedArticle")[0]
            if pmid != article.findtext(".//PMID"):
                print("PMID not matching:", pmid)
            
            # Extracting the date
            pub_date_node = article.findall(".//PubDate")[0]
            year = pub_date_node.findtext("Year")
            month = pub_date_node.findtext("Month")
            day = pub_date_node.findtext("Day")
            #pub_date = f"{year}-{month}-{day}" if year and month and day else year
            
            
            if year: 
                if month and day:
                    pub_date = f"{year}-{month}-{day}"
                    
                    # Parsing the retrieved date and then converting it to the 
                    # target format
                    date = datetime.strptime(pub_date,  "%Y-%b-%d")
                    date = date.strftime("%Y%m%d")
                
                else: 
                    date = year#int(year)
            
            
            # Adding the date to our data frame
            d.loc[i, "dateOldestPairing"] = date
            d.loc[i, "NCBIapiStatus"] = "done"
            
        else:
            d.loc[i, "NCBIapiStatus"] = "failed q 3" 
            

# Exporting to csv our dataset with the repaired entries:
if len(torepair) > 0: 
    d.to_csv(
        "./NIH MeSH/output/MeSH_pairs_date_b.csv", 
        header = True, 
        index = True,
        sep = ";",
        )




# Second issue: first pairing artifact _____________________________________________________________

# Loading the datasets
with open("./NIH MeSH/output/MeSH_pairs_date_b.csv") as csvfile:
    d = pd.read_csv(csvfile, delimiter = ";")



# Adding a new column to keep track of entries that need repairing.
if "repaired" not in d.columns:
    torepair = d[d["nPairings"] > 9998].index.tolist() # These are the affected entries
    d["repaired"] = None
    d.loc[torepair, "repaired"] = 0


# But if there already was a "repaired" column, then we only need to repair those entries that
# needed repairing and were not yet repaired:
else: 
    torepair = d[(d["nPairings"] > 9998) & (d["repaired"] == 0)].index.tolist()
        


# Now, I repair these entries by finding their true year of first pairing. I do so by means of
# a binary search within the time window 1900-2022. Here are two functions I'll use.
def binarySearch (test, pubmedQuery, API_key, startYear = 1900, endYear = 2024, cut = 1990):
    global DEBUG
    
    left = startYear
    right = endYear
    

    while left < right:
        if DEBUG: 
            print (left, cut, right)
        #result = test(left, cut)
        result = test(
            startYear = left,
            endYear = cut,
            pubmedQuery = pubmedQuery,
            API_key = API_key,
            )
        #
        
        if result < 9999 and result > 0:  # If we have sufficiently few results
        
            # Second query _______________________________________________________________
            # Finding the PubMed ID of the oldest paper:
            response = queryAPI(
                url = esearchBaseURL,
                params = {
                    "api_key": API_key,
                    "db": "pubmed",
                    "term": pubmedQuery,
                    "retmode": "json",
                    "sort": "pub+date", 
                    "retstart": result - 1,
                    "mindate": left,
                    "maxdate": cut,
                    }
                )
            sleep(delay) 
            
            if response.ok:
                pmid = response.json()['esearchresult']['idlist'][0]
            else: 
                return (False, None, "failed q2")
                
            
            # Third query ________________________________________________________________
            # Fetching the publication date of the oldest paper
            response = queryAPI(
                url = efetchBaseURL, 
                params = {
                    "api_key": API_key,
                    "db": "pubmed",
                    "id": pmid,
                    "retmode": "xml",  # Use 'xml' to get detailed structured data
                    }
                )
            sleep(delay) 
        
            if response.ok:
                document = ElementTree.fromstring(response.text)
                pub_date = None
        
                article = document.findall(".//PubmedArticle")[0]
                if pmid != article.findtext(".//PMID"):
                    print("PMID not matching:", pmid)
                
                # Extracting the date
                pub_date_node = article.findall(".//PubDate")[0]
                year = pub_date_node.findtext("Year")
                month = pub_date_node.findtext("Month")
                day = pub_date_node.findtext("Day")
                #pub_date = f"{year}-{month}-{day}" if year and month and day else year
                
                
                if year: 
                    if month and day:
                        pub_date = f"{year}-{month}-{day}"
                        
                        # Parsing the retrieved date and then converting it to the 
                        # target format
                        date = datetime.strptime(pub_date,  "%Y-%b-%d")
                        date = date.strftime("%Y%m%d")
                        return (True, date, "done")
                    
                    else: 
                        return (True, year, "done")
                        #date = year#int(year)
                        
                
                
                # Adding the date to our data frame
                else:
                    MedlineDate = pub_date_node.findtext("MedlineDate")
                    
                    if MedlineDate: 
                        year = MedlineDate.split(sep = "-")[0]
                        return(True, year, "done")
                    return (False, None, "year not found")
            
                #d.loc[i, "dateOldestPairing"] = date
                #d.loc[i, "NCBIapiStatus"] = "done"
                
            else:
                return (False, None, "failed q3") ###############3
        
        
        if result > 0: # If we have some results in this range, I divide it further.
            if DEBUG: 
                print("left", result)
            right = cut
            cut = (left + cut) // 2
            
        else: # If we don't have results in this range, I look at the next interval.
            if DEBUG:
                print("right", result)
            left = cut + 1
            cut = (left + right) // 2
            
        if cut == left:
            return (True, left, "done")
        if cut == right:
            return (True, right, "done")
            
    return (False, None, "something went wrong")


def test(startYear, endYear, pubmedQuery, API_key):
    response = queryAPI(
        url = esearchBaseURL, 
        params = {
            "api_key": API_key,
            "db": "pubmed",
            "term": pubmedQuery,
            "retmode": "json",
            "sort": "pub+date",
            "rettype": "count",
            "mindate": startYear,
            "maxdate": endYear,
            }
        )
    # My API key can only make 10 queries per second. I wait 0.1 seconds after
    # each query to ensure I don't exceed the limit:
    sleep(delay)

    if response.ok:
        return(int(response.json()["esearchresult"]["count"]))
#
#def test(startYear, endYear):
#    if 2001 in range(startYear, endYear + 1) or 2005 in range(startYear, endYear + 1):
#        return 1
#    else:
#        return 0
#success, y = binarySearch (test, pubmedQuery, API_key, startYear = 1900, endYear = 2022, cut = 1990)

#print(success, y)





# Now I can run a loop to repair them one-by-one:
with Bar(
        'Fetching date of first pairing...',
        fill = '-', 
        max = len(torepair),
        ) as bar:
    if len(torepair) > 0:
        for i in torepair: 
            
            # Updating progress bar
            bar.next()
            
            
            # I identify the two terms in the pair and costruct a PubMed query
            # accordingly:
            term1 = d["term1"][i]
            term2 = d["term2"][i]
            pubmedQuery = f'"{term1}"[MeSH Terms] AND "{term2}"[MeSH Terms]'
            
            
            # Let's explore the time span 1900-2022 with a binary search. I am looking for the first
            # year where there was at least one pairing found in PubMed. That's my year of first
            # pairing.
            success, y, NCBIapiStatus = binarySearch(
                test = test,
                pubmedQuery = pubmedQuery,
                API_key = API_key,
                startYear = 1900,
                endYear = 2024,
                cut = 1990,
                )
            
            
            d.loc[i, "dateOldestPairing"] = y
            d.loc[i, "NCBIapiStatus"] = NCBIapiStatus
            if success:
                d.loc[i, "repaired"] = 1
            else: 
                print(f"\nError with entry index {i}: {NCBIapiStatus}")
            
            
            
         
            
         
#round(d["dateOldestPairing"]).astype(int)
            
# Dropping unnecessary columns:
d = d[['term1', 'term2', 'dateOldestPairing', 'nPairings', 'NCBIapiStatus', 'repaired']]

if len(torepair) > 0: 
    d.to_csv(
        "./NIH MeSH/output/MeSH_pairs_date_b.csv", 
        header = True, 
        index = True,
        sep = ";",
        )


