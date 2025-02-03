'''
 This script repairs the MeSH terms stored in MeSH terms_complete.csv.
 It queries the NIH APIs again to amend the rows in the csv that contain invalid 
 server output.

 Runs in Python 3.12.3
'''

# Loading resources
import pandas as pd
from datetime import datetime
#import os
from time import sleep
#import csv
from skr_web_api import Submission


# These are the IDs of the applications that need repairing:
torepair = [1188507, 1191337, 1195163, 1404954, 1407427, 1671427]


# Reading the csv that needs to be repaired:
with open("./NIH MeSH/MeSH terms_complete.csv") as csvfile: 
    d = pd.read_csv(csvfile, delimiter = ";")


# Reading application text from the Parquet data file
txt = pd.read_parquet(
    'NNF Project 20231228/2.DATA/call_texts/Platform_ApplicationText.parquet'
    )


# Finding the row indexes of the csv with the target application id
dIndex = d.loc[d['Application.source.record.id'].isin(torepair)].index.tolist()
#d["terms"][dIndex] # checking that we got the right ones.

# And the correspodning row index from the txt dataframe:
#txtIndex = d.loc[txt['Application source record id'].]


# Now we have all we need to call the NIH API for MeSH on Demand and query the server
# again for these applications. Hopefully, this time we'll get a valid output.
#
# So, let's start with reading API credentials. They are stored in a two-line txt file. 
# The first line stores the username, and the second line the key.
with open("./NIH MeSH/api key.txt", 'r') as f: data = f.read()
API_user, API_key = data.split('\n')
del data


# Then we create a connection with the API server
inst = Submission(API_user, API_key)
del API_user
del API_key


# Now I loop through each of the applications to repair, querying the server again 
# and updating the corresponding row in the dataframe d. 
for i in range(0, len(torepair)):
    print("Attempting to repair MeSH terms for application id " + str(torepair[i]))
    
    # Pasting projects' titles and brief descriptions, when available.
    # The resulting string is what MeSH terms will be based on.
    # Note that applications with no title are ignored.
    txti = txt[txt['Application source record id'] == torepair[i]]
    
    if txti["Project title"] is None:
        continue
    
    if txti["Brief project description clean"] is None:
        x = txti["Project title"]
    else:
        x = txti["Project title"] + "\n\n" + txti["Brief project description clean"]
    #print("Input text for MeSH on Demand:")
    #print(x)
    
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
    attempts = 0
    while not success:
        try:
            attempts += 1
            response = inst.submit()
            success = True
        except Exception as e: #requests.exceptions.RequestException as e:
            print(datetime.now())
            print(e)
            print("Making new attempt in 10 seconds...")
            sleep(10)
            if attempts == 3: 
                raise SystemExit("Couldn't reach server after 3 attempts.")
    
    ri = response.content.decode()
    
    # Formatting the output to remove end-of-lines and field separators
    # that would be inconsistent in the csv:
    ri = ri.replace('\n', '\\n')
    ri = ri.replace(';', ',')
    
    # Adding the new server output to our d table.
    d.loc[dIndex[i], "terms"] = ri
    

# All is left to do is to write the updated/repaired dataframed d to file.
# I replace the original csv we imported with the new one.
print("Saving updated dataframe to csv")
d.to_csv('./NIH MeSH/MeSH terms_complete.csv', sep = ';', index = False)

