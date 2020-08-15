#This script reads the CHILDES transcripts in /corpora and creates a csv of transcripts
#with words standardized and inflections parsed

import os
import re
import pandas as pd
import numpy as np
import nltk
os.chdir("corpora")


OVERRIDE_CHI_INFO = {
'HSLLD/MT/aprmt52.cha' : None,
'HSLLD/TP/contp1.cha' : [4,3,13],
'HSLLD/MD/aprmd7.cha' : None,
'HSLLD/MD/acemd7.cha': None,
'HSLLD/MD/jammd7.cha': None,
'HSLLD/MD/annmd7.cha': None,
'HSLLD/MD/jermd7.cha': None,
'HSLLD/MD/bramd7.cha': None,
'HSLLD/MD/rasmd7.cha': None,
'HSLLD/MD/aimmd7.cha': None,
'HSLLD/MD/catmd7.cha': None,
'HSLLD/MD/melmd7.cha': None,
'HSLLD/MD/ethmd7.cha': None,
'HSLLD/MD/susmd7.cha': None,
'HSLLD/MD/raumd7.cha': None,
'HSLLD/MD/jebmd7.cha': None,
'HSLLD/MD/brimd7.cha': None,
'HSLLD/MD/jenmd7.cha': None,
'HSLLD/MD/maymd7.cha': None,
'HSLLD/MD/kevmd7.cha': None,
'HSLLD/MD/sarmd7.cha': None,
'HSLLD/MD/jesmd7.cha': None,
'HSLLD/MD/vicmd7.cha': None,
'HSLLD/MD/conmd7.cha': None,
'HSLLD/MD/joymd7.cha': None,
'HSLLD/MD/rosmd7.cha': None,
'HSLLD/MD/bobmd7.cha': None,
'HSLLD/MD/geomd7.cha': None,
'HSLLD/MD/tommd7.cha': None,
'HSLLD/MD/gremd7.cha': None,
'HSLLD/MD/petmd7.cha': None,
'HSLLD/MD/brnmd7.cha': None,
'HSLLD/MD/anamd7.cha': None,
'HSLLD/MD/marmd7.cha': None,
'HSLLD/MD/jeamd7.cha': None,
'HSLLD/MD/casmd7.cha': None,
'HSLLD/MD/kurmd7.cha': None,
'HSLLD/MD/trimd7.cha': None,
'HSLLD/MD/rocmd7.cha': None,
'HSLLD/MD/paumd7.cha': None,
'HSLLD/MD/rilmd7.cha': None,
'HSLLD/MD/nedmd7.cha': None,
'HSLLD/MD/admmd7.cha': None,
'HSLLD/ET/ctret7.cha': None,
'Cornell/haas/haas1919.cha': [1,9,0],
'Cornell/schacter/schmidw.cha': [3,0,0],
'Cornell/schacter/schmidb.cha': [3,0,0],
'Cornell/schacter/schlowb.cha': [3,0,0],
'Cornell/horn/horn1712.cha': None,
'Cornell/horn/horn1602.cha': [3,0,0],
'Cornell/moore/moore188.cha': [1,7,0],
'Cornell/mom/mom01b.cha': -2,
'Cornell/mom/mom01a.cha': -2,
'Cornell/mom/mom0282.cha': -1,
'Cornell/mom/mom0281.cha': -1,
'Cornell/mom/mom0501.cha': -1,
'Cornell/mom/mom03.cha': -2,
'Cornell/mom/mom0886.cha': -1,
'Cornell/mom/mom0502.cha': -1,
'Cornell/mom/mom0885.cha': -1,
'Cornell/mom/mom04.cha': -1,
'Cornell/mom/mom10.cha': -2,
'Cornell/mom/mom07.cha': -2,
'Cornell/mom/mom06.cha': -2,
'MacWhinney/91b2.cha': -2,
'MacWhinney/91b1.cha': -2,
'MacWhinney/85a1.cha': -2,
'MacWhinney/91a1.cha': -2,
'MacWhinney/91a2.cha': -2,
'Gleason/Father/eddie.cha': [4,4,6],
'Gathercole/04.cha': -2,
'Rollins/ch09.cha': -1,
'Rollins/ch06.cha': -1,
'Bernstein/Kay/kay2.cha': [1,4,0],
'Bernstein/Cindy/cindy1.cha': -1,
'Bernstein/Amelia/amelia3.cha': -1,
'Providence/Naima/030000.cha':[3,0,0],
'McMillan/triplets.cha':-1,
'Braunwald/0diary/020810.cha': [2,8,10],
'Brent/v1/000910.cha':-1,
'Brent/v1/010203.cha':-1,
'Brent/v1/010105.cha':-1,
'Brent/s2/000905.cha':-1,
'Brent/s2/010204.cha':-1,
'Brent/s2/010006.cha':-1,
'Brent/s3/001028.cha':-1,
'Brent/s3/000913.cha':-1,
'Brent/s3/001128.cha':-1,
'Brent/s3/010009.cha':-1,
'Brent/d1/000925.cha':-1,
'Brent/j1/001214.cha':-1,
'Brent/j1/001229.cha':-1,
'Brent/j1/001115.cha':-1,
'Brent/j1/001023.cha':-1,
'Brent/f2/001303.cha':-1,
'Brent/f2/001121.cha':-1,
'Brent/f2/000827.cha':-1,
'Brent/f2/000927.cha':-1,
'Brent/c1/000930.cha':-1,
'Brent/q1/000900.cha':-1,
'Brent/q1/001012.cha':-1,
'Brent/q1/001128.cha':-1,
'Brent/q1/001109.cha':-1,
'Brent/q1/001026.cha':-1,
'Brent/s1/001001.cha':-1,
'Brent/v2/000903.cha':-1,
'Brent/v2/001014.cha':-1,
'Brent/v2/010308.cha':-1,
'Brent/v2/010023.cha':-1,
'Brent/v2/010107.cha':-1,
'Brent/m2/000928.cha':-1,
'Brent/m2/010122.cha':-1,
'Brent/m2/000907.cha':-1,
'Brent/m2/001113.cha':-1,
'Brent/m2/010023.cha':-1,
'Brent/f1/001206.cha':-1,
'Brent/f1/000910.cha':-1,
'Brent/f1/001318.cha':-1,
'Brent/t1/000830.cha':-1,
'Brent/t1/001025.cha':-1,
'Brent/w1/001123.cha':-1,
'Brent/w1/010006.cha':-1,
'Feldman/010325.cha': [1,3,25],
}

#######Read the raw data into a csv

def find_indices(lst, condition):
     return [i for i, elem in enumerate(lst) if condition(elem)]

def readcha(filenm, study, context):
    f = open(filenm)
    lines = f.readlines()
    #read metadata
    session_id = [l for l in lines if l.startswith("@PID")][0][6:-1]
    try:
        i = [l for l in lines if "@Participants" in l][0].split().index('CHI')+1
        child_name = [l for l in lines if "@Participants" in l][0].split()[i]
    except:
        child_name = session_id
    child_id = study + child_name
    study_name = study
    context_id = study + context
    if filenm in OVERRIDE_CHI_INFO.keys():
        print('overriding metadata for {}'.format(filenm))
        child_age = OVERRIDE_CHI_INFO[filenm]
        if child_age is not None and child_age != -1 and child_age != -2:
            child_age = (int(child_age[0])*12 
                + int(child_age[1])
                + round(int(child_age[2])/30.5))
        print(child_age)
    else:
        try:
            tmp = [l for l in lines if "@ID" in l and "CHI" in l][0].split("|")
            child_age = tmp[tmp.index("CHI")+1]
            child_age = re.split('[;.]', child_age)
            #print(child_age)
            try:
                child_age = (int(child_age[0])*12 
                    + int(child_age[1])
                    + round(int(child_age[2])/30.5))
            except:
                #no days place
                child_age = (int(child_age[0])*12 
                    + int(child_age[1]))
        except:
            child_age = None
    try:
        situation_dsc = [l for l in lines if "@Situation" in l][0][11:].strip()
    except:
        situation_dsc = None
        
    #read utterances
    #first find indices of parent utterances
    utt_ind = find_indices(lines, lambda e: "*MOT:" in e or "*FAT:" in e or "*MOM:" in e or "*DAD:" in e)
    utterance = []
    mor = []
    #read the following *MOT/FAT and %mor line(s)
    for i in utt_ind:
        j = i
        new_utt = ""
        while j < len(lines):
            if len(lines[j]) > 0 and "%" == lines[j][0]: 
                break
            new_utt += " "
            new_utt += lines[j].strip()
            j += 1
            if j == len(lines):
                break
            if len(lines[j]) > 0 and "*" == lines[j][0]:
                break
        new_mor = ""
        while j < len(lines):
            if "%mor" in lines[j]:
                new_mor += " "
                new_mor += lines[j].strip()
                break
            j += 1
        while j < len(lines): 
            if len(lines[j]) > 0 and ("%" == lines[j][0] or "*" == lines[j][0]):
                break
            new_mor += " "
            new_mor += lines[j].strip()
        utterance.append(new_utt)
        mor.append(new_mor)
    
    #print file name if child info is absent
    if len(utterance) > 0 and child_age is None:
        print(filenm)

    #write to data frame
    length = len(utterance)

    d = {'utterance':utterance,
         'mor':mor,
         'session_id':[session_id]*length,
         'child_id':[child_id]*length,
         'age':[child_age]*length,
         'study':[study_name]*length,
         'situation':[situation_dsc]*length,
         'context_id':[context_id]*length}
         
    df = pd.DataFrame(data = d)
    return df
  
#Main code to loop through all files
dfs = []
for study in [s for s in os.listdir() if not s.startswith('.')]:
    studyname = study
    print(studyname)
    by_folder = open(study+"/0metadata.cdc").readline() == "context:folder\n"
    if any([f.endswith('.cha') for f in os.listdir(study)]):
        #read .cha files in main directory
        for f in [f for f in os.listdir(study)
        if not f.startswith('.') and f.endswith('cha')]:
            df = readcha(os.path.join(study,f),study,"") #no add'l context code
            dfs.append(df)
    else:
        #open directories and read them
        for d in [d for d in os.listdir(study)
        if not d.startswith('.') and not d.endswith('.html') 
        and not d.endswith('.cdc') and not d.endswith('txt')
        and not d.endswith('xls')]:
            for f in [f for f in os.listdir(study + '/' + d)
            if not f.startswith('.') and f.endswith('cha')]:
                ctxt = (d if by_folder else "")
                df = readcha(os.path.join(study,d,f), study, ctxt)
                dfs.append(df)

dataset = dfs[0].append(dfs[1:])

#get out of data subdirectory
os.chdir('..')
dataset.to_csv('childes.csv')

####Parse, clean, and standardize

#Below, imports are repeated for convenience and the csv is saved and reloaded 
#Only the below code needs to be run to re-clean the raw data if the source transcripts have not changed
import os
import pandas as pd
import numpy as np
import re
import nltk

print('load data for cleaning')
df=pd.read_csv('childes.csv', index_col = 0, dtype = {
    'age':np.float,
    'child_id':str,
    'context_id':str,
    'mor':str,
    'session_id':str,
    'situation':str,
    'study':str,
    'utterance':str
})
df.dropna(subset = ['utterance'], inplace = True)

#remove 4-year-olds and up
df = df[(df.age < 48) & (df.age > -2)]

#list of words to replace
print('load words to replace')
replace_words = pd.read_csv('replace_words.csv', header = None, 
    names = ['before', 'after'])
replace_words = replace_words.drop_duplicates()
replace_words_2 = pd.read_csv('replace_words_2.csv', header = None, 
    names = ['before', 'after'])
replace_mcdi_words = pd.read_csv('replace_mcdi_words.csv', header = None,
    names = ['before', 'after'])
replace_dict = {a.lower():b.lower() for a,b in
    list(zip(replace_words.before, replace_words.after))}
replace_dict_2 = {a.lower():b.lower() for a,b in
    list(zip(replace_words_2.before, replace_words_2.after))}
replace_mcdi_words_dict = {a.lower():b.lower() for a,b in
    list(zip(replace_mcdi_words.before, replace_mcdi_words.after))}

print('clean utterances with rules')
utts = [u for u in df.utterance if not pd.isnull(u)]

#add spaces for non-overlapping delimiter parsing
utts2 = [" "+u+" " for u in utts]
utts2 = [re.sub(" ", "  ", u) for u in utts2]

#(.) is a pause, remove
utts2 = [re.sub("\(\.\)", "", u) for u in utts2]

#gi(ve)
utts2 = [re.sub("gi(ve)", "give", u) for u in utts2]

#remove anything in parens or brackets
utts2 = [re.sub("\(.+?\)", "", u) for u in utts2]
utts2 = [re.sub("\[.+?\]", "", u) for u in utts2]

#drop &= codes and remove &- from &- and & codes
utts2 = [re.sub("&=.+? "," ",u) for u in utts2]
utts2 = [re.sub("&\-?([a-zA-Z]+?) ", r"\1 ", u) for u in utts2]

#drop r"@." codes and parse spellings
utts2 = [re.sub(" ([a-z])@l ", r" \1 ",u) for u in utts2]
utts2 = [re.sub(" ([a-z]+)@k ", r" \1 ",u) for u in utts2]
utts2 = [re.sub("@[a-zA-Z$:\-]+ ", "  ", u) for u in utts2] 

#drop r"0[a-zA-Z]+" codes (means omission)
utts2 = [re.sub("0[a-zA-Z]+", " ", u) for u in utts2] 

#.www www codes are names
utts2 = [re.sub(".www|www", "NAME",u) for u in utts2]

#drop x:x codes
utts2 = [re.sub("([a-z]):([a-z])", r"\1\2", u) for u in utts2]

#remove any remaining punctuation characters other than sentence boundaries
utts2 = [re.sub("[~`\|/<>:,;\"\{\}\[\]\+=_\(\)\*&\“”„^%$#↑@†‡(...)\t]", "  ", u) 
    for u in utts2]

#remove time codes
utts2 = [re.sub("[0123456789]*\\x15[0123456789]*", "  ", u) for u in utts2]

#eliminate 0
utts2 = [re.sub("0", " ", u) for u in utts2] 

#remove MOT and FAT headers    
utts2 = [re.sub("MOT|FAT|MOM|DAD", "", u) for u in utts2]

#insert spaces around known delimiters
utts2 = [re.sub("([\.?!,])", "  \g<1>  ", u) for u in utts2]

#parse 'd 's 've 'm 're n't  automatically
utts2 = [re.sub("([a-zA-Z]+)(')(d|s|ve|m|re|ll) ", r"\1  \2\3 ", u) for u in utts2]
utts2 = [re.sub("([a-zA-Z]+)(n't) ", r"\1  \2  ", u) for u in utts2]

#delete anything after Comment
utts2 = [re.sub("Comment.*", "  ", u) for u in utts2]

#substitute words according to dictionary
print('replace words with dict')
def multiple_replace(text, adict):
    #multiple replace from dict when surrounded by spaces
    def lookforspaces(str): 
        return "(?<= )"+str+"(?= )"
    rx = re.compile('|'.join(map(lookforspaces, adict)))
    def one_xlat(match):
        return adict[match.group(0)]
    return rx.sub(one_xlat, text)

big_string = "$$".join(utts2)
#replace case sensitive items
big_string = multiple_replace(big_string, {
    "Will":"INFNAME",
    "Booboo":"INFNAME",
    "Child":"INFNAME"
})
#collapse multiple spaces and lowercase
big_string = re.sub("( +)", r" ", big_string).lower()
#replace from main replacement dictionary
big_string = multiple_replace(big_string, replace_dict)
#second round to avoid conflicts
big_string = multiple_replace(big_string, replace_dict_2)
#modify words to be consistent with MCDI
big_string = multiple_replace(big_string, replace_mcdi_words_dict)
#second round to avoid conflicts
big_string = re.sub(" fry 's ", " french_fry 's ", big_string)
#split utterances
utts2 = big_string.split("$$")
#replace boo boo with booboo when not preceded or followed by boo
utts2 = [
    re.sub("boo boo", "booboo", u) if "boo boo boo" not in u
    else u 
    for u in utts2
    ]
#hush to shh when not hush a bye or hush little baby
utts2 = [
    re.sub("hush", "shh", u) if "hush a bye" not in u and "hush little" not in u
    else u 
    for u in utts2
    ]

#combine affixes to words (if doing non-parsed morphology)
#utts2_orig = utts2
#utts2 = [re.sub("([a-zA-Z]+)( +)('d|'s|'ve|'m|'re|'ll|ing|n't|ed) ", 
# r"\1_\3 ", u) for u in utts2]

print('postprocessing')
#split into tokens
utts3 = [u.split() for u in utts2]
#add utterance terminator
utts3 = [u + ['.'] if len(u) > 0 and u[-1] not in ['!','?'] else u for u in utts3 ]

#tabulate words
words = [word for utt in utts3 for word in utt]
fdist = nltk.FreqDist(words)
freqs = [(word, frequency) for word, frequency in fdist.most_common(len(fdist))]

#find the cdi words, then the words with freq > 50, then the punctuation
cdi_words = pd.read_csv('cdi_words.csv', header=None)[0].values
punct_chars = ['?', '!', '.']
cdi_vocab = [(word,freq) for word,freq in fdist.most_common(len(fdist)) if word in cdi_words]
frequent_vocab = [(word,freq) for word,freq in fdist.most_common(len(fdist)) if freq>=50
    and not word in cdi_words
    and not word in punct_chars]
punct_vocab = [(word,freq) for word, freq in fdist.most_common(len(fdist))
    if word in punct_chars]

#assign words to indices and vice versa
all_words = ([w for w,f in cdi_vocab]
    +[w for w,f in frequent_vocab]
    +[w for w,f in punct_vocab]
    +['RW'])
#from gentools import indexify
#word_to_ind, ind_to_word = indexify(all_words)

#obscure rare words (frequency below 50)
utts4 = [[w if w in all_words else 'RW' for w in u] for u in utts3]

#eliminate -'s -es -ing -ed -'ll -'m -'d -'ve -'re after unknown word
utts4 = [([u[0]]+[y for x,y in zip(u,u[1:]) 
    if not (x == 'RW' and y in ["'s", "ing","ed","'ll","'m","'d", "'ve", "'re"])]) 
    if len(u) > 0 else u for u in utts4]

#eliminate utterances with no words
df["words"] = [' '.join(utt) for utt in utts4]
df = df[df["words"] !=  '']

#save corpus and indices 
print('write to file')
df.to_csv('childes_cleaned.csv') #childes_cdi_raw if not parsing morphology
pd.Series(all_words).to_csv('all_words.csv') #first 656 are cdi words
print('done')

####################
####################
####################
####################
#Don't run below code when rebuilding; it's used only to help find words that need to be edited

#propose transformations to be checked by hand (stored in replace_words.csv)
# vocab = [word for word,freq in fdist.most_common(len(fdist)) if freq>1]
# transformations = set()
# for word in [w for w,frq in freqs[:5000]]:
#     print(word)
#     stems = [word]
#     if len(word) > 3:
#         stems.append(word[:-1])
#     if len(word) > 2 and word[-1] in 'rtplkgfdszbnm':
#         stems.append(word+word[-1])
#     for form in stems:
#         if form+'ing' in vocab:
#             transformations.add((form+'ing', form+' ing'))
#         if form+'in' in vocab:
#             transformations.add((form+'in',form+' ing'))
#         if form+"in'" in vocab:
#             transformations.add((form+"in'",form+' ing'))
#         if form+"s" in vocab:
#             transformations.add((form+"s",form + " 's"))
#         if form+"'s" in vocab:
#             transformations.add((form+"'s",form + " 's")) 
#         if form+"s'" in vocab:
#             transformations.add((form+"s'",form + " 's")) 
#         if form+"es" in vocab:
#             transformations.add((form+"es",form + " 's")) 
#         if form+"'s" in vocab:
#             transformations.add((form+"'s",form + " 's"))
#         if form+'ed' in vocab:
#             transformations.add((form+"ed",form + " ed"))

# transformations = list(transformations)

# #page through common words to identify spelling variants
# min_i = 0
# for i in range(5000):
#     if i > min_i:
#         print(i)
#         print(freqs[i])
#         print(similar_words(freqs[i][0], minfreq = 2))
#         x = input("Press Enter to continue...")
#         if x == 'stop':
#             break

# #Show utterances with a word
# def find_word(word):
#     from numpy.random import permutation
#     examples = [(ind, utts[ind]) for ind, u in enumerate(utts2) if " "+word+" " in u]
#     for i in permutation(range(len(examples)))[0:10]:
#         print (str(examples[i][0]) + " " + "".join(examples[i][1]))
#     #return examples
    
# #Show similar words that might be variants
# def similar_words(start = "", end = "", minfreq = 5):
#     return[(w,f) for w, f in freqs if w.startswith(start) and w.endswith(end)
#     and f >= minfreq]
