# This script queries PubMed records (via NIH/NLM/NCBI API) to fetch the date of 
# publication of the oldest paper associated with the two given MeSH terms.
# I call it the "date of first pairing".

# Last edit on 20240708 by Thomas Feliciani.
# Runs in Python 3.12.3


from progress.bar import Bar
import os
import pandas as pd
#import numpy as np
from time import sleep
import requests
from xml.etree import ElementTree
from datetime import datetime


# Defining some auxiliary functions ___________________________________________________

# This function writes a new csv line for a pair of MeSH terms. I'll call this function
# everytime I fetch a new date of first pairing, so that results are saved as they are
# being generated.
def writeNewLine (d, i):
    with open('./NIH MeSH/output/MeSH_pairs_date_b.csv', 'a', newline='') as csvfile:
        printHeader = i == 0 # Write header if we are at i == 0
        d.loc[[i]].to_csv(csvfile, header = printHeader, index = True, sep = ";")

# This function sends a query, awaits a response, and then tries again a few times
# if the request timeouts.
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
    
    

# Loading necessary resources _______________________________________________________

# Reading API credentials.
# These are stored in a two-line txt file. The first line stores the email,
# and the second line the key.
# API keys can be generated through the account settings page of the 
# NIH/NLM/NCBI at https://account.ncbi.nlm.nih.gov/settings/
with open("./NIH MeSH/api key NCBI.txt", 'r') as f: data = f.read()
API_user, API_key = data.split('\n')
del data

delay = 0.01  # seconds. Waiting time between API calls.


# These are the API URLs
esearchBaseURL = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
efetchBaseURL = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"


# Importing our list of MeSH term pairs.
with open("./NIH MeSH/output/MeSH_pairs_b.csv") as csvfile:
    d = pd.read_csv(csvfile, delimiter = ";")
    
    
# Check if the variable "dateOldestPairing" already exists. If it doesn't,
# I create it:
if "dateOldestPairing" not in d.columns:
    d["dateOldestPairing"] = None#np.nan


# I also create a variable to store the number of identified papers for each
# MeSH term pair:
if "nPairings" not in d.columns:
    d["nPairings"] = None#np.nan


# ... and a variable to keep track of possible API errors.
if "NCBIapiStatus" not in d.columns:
    d["NCBIapiStatus"] = "no query made"


# Checking if there's already some output
startIndex = 0
if os.path.exists("./NIH MeSH/output/MeSH_pairs_date_b.csv"):
    with open('./NIH MeSH/output/MeSH_pairs_date_b.csv', mode = 'r', newline='') as temp:
        startIndex = len(pd.read_csv(temp, delimiter = ";"))



# Querying API to fetch dates of first pairing ______________________________________

# For every pair of MeSH terms in our data:
with Bar(
        'Fetching date of first pairing...',
        fill = '-', 
        max = len(d) - startIndex,
        ) as bar:
    for i in range(startIndex, len(d)):
    #for i in range(50, 75):
        
        # Updating progress bar
        if i > startIndex: bar.next()
        
        # I identify the two terms in the pair and costruct a PubMed query
        # accordingly:
        term1 = d["term1"][i]
        term2 = d["term2"][i]
        pubmedQuery = f'"{term1}"[MeSH Terms] AND "{term2}"[MeSH Terms]'
        
        
        # First query ________________________________________________________________ 
        # Finding how many results we get.
        # The esearch API can order papers by publication date, but only in 
        # descending order. The oldest paper -- the one I need -- is therefore
        # at the end of the list of results. To identfy the last of the list I
        # need to know how long the list is. Thus:
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
        # My API key can only make 10 queries per second. I wait 0.1 seconds after
        # each query to ensure I don't exceed the limit:
        sleep(delay)
    
        if response.ok:
            count = int(response.json()["esearchresult"]["count"])
            d.loc[i, "nPairings"] = count
            if count == 0:
                d.loc[i, "NCBIapiStatus"] = "done"
                writeNewLine(d, i)
                continue
        else: 
            d.loc[i, "NCBIapiStatus"]= "failed q 1" 
            writeNewLine(d, i)
            continue
            
        # Search results are capped at 10k papers. However, the API won't return
        # the 10,000th result, but only the 9,998th. Thus:
        lastIndex = min(9998, count - 1)
        
        
        # Second query _______________________________________________________________
        # Finding the PubMed ID of the oldest paper:
        # Now that I know how long the list is ("count"), I can request the ID of
        # the last paper in the list:
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
            writeNewLine(d, i)
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
                "retmode": "xml",  # Use 'xml' to get detailed structured data
                #"rettype": "abstract"
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
            
            
        # Writing to file
        writeNewLine(d, i)
        
     
        
#pd.writerow()

#datetime.now()
   
#date.timestamp()

#d["term1"][50:70]
#d["dateOldestPairing"][50:70]
#d["dateOldestPairing"][50:70]
#d["dateOldestPairing"][50:70]
#d["dateOldestPairing"][50:70]
#d["dateOldestPairing"][50:70]

#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
#d.loc[50:70, "NCBIapiStatus"]
