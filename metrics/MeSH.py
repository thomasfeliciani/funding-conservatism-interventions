# Runs in Python 3.11.7


# Loading resources
import pandas as pd
from datetime import datetime
import os
from time import sleep
import csv
from progress.bar import Bar
from skr_web_api import Submission#, SEMREP_INTERACTIVE_URL


# Reading application text from the Parquet data file
txt = pd.read_parquet(
    'NNF Project 20231228/2.DATA/call_texts/Platform_ApplicationText.parquet'
    )


# Dividing the dataset into chunks. Here we run the script for only one of these chunks.
txt = txt.iloc[0:8000]
#txt = txt.iloc[8000:14000]
#txt = txt.iloc[14000:len(txt)]


# Reading API credentials.
# These are stored in a two-line txt file. The first line stores the username,
# and the second line the key.
with open("./NIH MeSH/api key.txt", 'r') as f: data = f.read()
API_user, API_key = data.split('\n')
del data


# Creating connection with API server
inst = Submission(API_user, API_key)
del API_user
del API_key


# Checking whether we already have generated MeSH terms for some of the applications.
# We save the IDs of those applications that we already have.
startAt = 0
if os.path.exists("./NIH MeSH/MeSH terms.csv"):
    with open("./NIH MeSH/MeSH terms.csv") as csvfile:
        #csv_reader = csv.reader(csvfile, delimiter=';')
        d = pd.read_csv(csvfile, delimiter = ";")#["Application source record id"]
    lastEntry = d.iloc[-1]["Application source record id"]
    lastEntryBoolean = txt["Application source record id"] == lastEntry
    startAt = lastEntryBoolean[lastEntryBoolean].index
    startAt = startAt.to_numpy()[0] + 1



# Setting up a progress bar.
with Bar(
        'Processing...',
        #start = (startAt - 1) / len(txt),
        fill = '-', 
        suffix = '%(percent).2f%% - %(eta)ds',
        ) as bar:
    
    # We loop through the applications in txt.
    for i in range(startAt, len(txt)): #for i in range(len(txt)):
        
        # Checking if we already had the MeSH terms for this application
        # (that is, if the ID of "i" already appears in "d").
        # Horrendous implementation, but it works :) 
        #if txt["Application source record id"][i] in d.values: continue 
        
    
        # Pasting projects' titles and brief descriptions, when available.
        # The resulting string is what MeSH terms will be based on.
        # Note that applications with no title are ignored.
        if txt["Project title"][i] == None:
            continue
        
        if txt["Brief project description clean"][i] == None:
            x = txt["Project title"][i]
        else:
            x = txt["Project title"][i] + "\n\n" \
                + txt["Brief project description clean"][i]
        
        
        # Now we have all we need to query the server.
        # Note the options argument, which we set to "-MoD_PP". This queries the
        # MeSH on Demand API. As per instructions at
        # https://ii.nlm.nih.gov/resource/MTI_help_info.html :
        #    "This option uses the MeSH on Demand filtering and provides a list of the top
        #    10 related citations in PubMed as it's first line followed by each MeSH term
        #    summarizing the input text. Each MeSH term also includes the MeSH Unique
        #    Identifier (DUI/CUI), UMLS Concept Identifier (CUI), and a MTI score.
        #    This is not intended to be a replacement for using the existing MeSH on
        #    Demand web page which provides a richer output."
        inst.init_mti_interactive(x, args = "-MoD_PP")
        
        success = False
        attempts = 1
        while not success:
            try:
                attempts += 1
                response = inst.submit()
                success = True
            except Exception as e: #requests.exceptions.RequestException as e:
                print(datetime.now())
                print(e)
                print("Making attempt number", attempts, "in 10 seconds...")
                sleep(10)
                if attempts == 1000: 
                    raise SystemExit("Can't reach server. Giving up :)")
        
        
        ri = response.content.decode()
        
        # Formatting the output to remove end-of-lines and field separators
        # that would be inconsistent in the csv:
        ri = ri.replace('\n', '\\n')
        ri = ri.replace(';', ',')
        
        row = {
            'Application source record id': txt['Application source record id'][i],
            "terms": ri,
            }
        
        # Writing to file
        with open('./NIH MeSH/MeSH terms.csv', 'a', newline='') as csvfile:
            fieldnames = ['Application source record id', 'terms']
            writer = csv.DictWriter(
                csvfile, 
                fieldnames = fieldnames,
                delimiter = ';',
                )
            # If we are writing the first line, we also add the csv header:
            if i == 0: writer.writeheader()
            
            writer.writerow(row) # Appending the new row
            
        bar.next(i / len(txt))#len(txt)) # Updating progress bar




