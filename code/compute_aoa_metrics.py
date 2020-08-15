#This script calculates and saves the stats/metrics for each CDI word
#which are used in R to model kids' vocabularies.

#Toggle
IGNORE_INFLECTIONS = True
MIN_FRAME_FREQ = 500
MIN_COALS_FREQ = 500

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
from gentools import flatten
from gentools import indexify
from gentools import topn
import re
import nltk

######
######Load and prepare the data
df = pd.read_csv('childes_cleaned.csv')

#tokenize the words field to a list
utts = [utt.split() for utt in df.words]

#build the vocabulary
vocab = pd.read_csv('all_words.csv', header = None)[1].values
word_to_ind, ind_to_word = indexify(vocab)

# make sure referenced type indices are the correct ones
period = word_to_ind['.']
question = word_to_ind['?']
exclam = word_to_ind['!']
ed = word_to_ind["ed"]
s = word_to_ind["'s"]
ing = word_to_ind["ing"]
d = word_to_ind["'d"]
ve = word_to_ind["'ve"]
re = word_to_ind["'re"]
nt = word_to_ind["n't"]
ll = word_to_ind["'ll"]
rw = word_to_ind["RW"]
xxx = word_to_ind["xxx"]

n_words = len(word_to_ind)

n_cdi_words = 656
vocabsize = len(vocab) #all words that were frequent enough or appeared on CDI

#sort by age first
df.sort_values(by=['age'],kind = 'mergesort', inplace = True)
words = flatten([w.split() for w in df.words])
data = words
data = [word_to_ind[w] for w in data]
#collapse utterance boundary tokens
data = [x if not x in [period, question, exclam] else period for x in data]
#ignore inflection tokens if running with that flag
if IGNORE_INFLECTIONS:
    data = [x for x in data if not x in [ed, s, ing, d, ve, re, nt, ll]]

#count the frequency of each frame
frame_freqs = np.zeros((n_words,n_words), dtype = int)
for i in range(len(data)-2):
    if not data[i+1] == period: #frames must enclose a word, not an utterance boundary
        frame_freqs[data[i], data[i+2]]+= 1

######
######Get the frequencies of the words in individual frames
#first filter for frames that are more frequent than MIN_FRAME_FREQ
freq_frames = [(a,b)
    for a in range(n_words) for b in range(n_words) 
    if frame_freqs[a,b] > MIN_FRAME_FREQ and not a == rw and not b == rw and not a == xxx and not b == xxx] #not RW or xxx
ff_to_ind, ind_to_ff = indexify(freq_frames)
word_frame_freqs = np.zeros((n_words, len(freq_frames)+1), dtype = int)
for i in range(len(data)-2):
    try:
        word_frame_freqs[data[i+1], ff_to_ind[(data[i], data[i+2])]] += 1
    except: 
        #frame doesn't exist, increment catch-all "frame"
        word_frame_freqs[data[i+1], len(freq_frames)] += 1
#create headers and copy frame frequencies into two dfs, one for all words and one for cdi words only
frame_names = [ind_to_word[a]+ "_" +ind_to_word[b] for a,b in freq_frames]
frames_df = pd.DataFrame(word_frame_freqs)
frames_df.columns = frame_names + ['OTHER']
frames_df_full = frames_df
frames_df_full['word'] = [ind_to_word[i] for i in range(n_words)]
frames_df = frames_df.loc[0:n_cdi_words-1]
frames_df['word'] = [ind_to_word[i] for i in range(n_cdi_words)]
#export to csv
if IGNORE_INFLECTIONS:
    frames_df.to_csv('word_frame_freqs_noinfl_' + str(MIN_FRAME_FREQ) + '.csv')
else:
    frames_df.to_csv('word_frame_freqs_inflections_' + str(MIN_FRAME_FREQ) + '.csv')

######
######Compute MLU
not_words = (".", "?", "!", "ed", "'s", "'d", "'ve", "'re", "'m", "n't", "ll", "ing")
utts = [u.split() for u in df.words]
mlu_numerator = np.zeros((n_words), dtype = int)
mlu_denominator = np.zeros((n_words), dtype = int)
for u in utts:
    words_in_utt = [word_to_ind[w] for w in u if not w in not_words ]
    for w in words_in_utt:
        mlu_denominator[w] += 1
        mlu_numerator[w] += len(words_in_utt)
mlu = mlu_numerator/mlu_denominator

######
######Calculate the solo and final frequency 
solo_freqs = np.zeros((n_words), dtype = int)
final_freqs = np.zeros((n_words), dtype = int)
for utt in utts:
    words = [w for w in utt if not w in ['?', '.', '!',"'s", "ing","ed","'ll","'m","'d", "'ve", "'re"]]
    if len(words) == 1:
        try:
            solo_freqs[word_to_ind[words[0]]] += 1
        except:
            pass
    try:
        final_freqs[word_to_ind[words[-1]]] += 1
    except:
        pass

#######
#######export one-dimensional metrics
logfreq = np.log(frames_df.loc[:, :"OTHER"].sum(axis = 1) / frames_df_full.loc[:,:"OTHER"].sum().sum())
solo_freqs = solo_freqs[0:n_cdi_words]
final_freqs = final_freqs[0:n_cdi_words]
mlu = mlu[0:n_cdi_words]
d = pd.DataFrame({'word':[ind_to_word[i] for i in range(656)],
    'logfreq':logfreq, 
    'MLU':mlu,
    'solo_freqs':solo_freqs,
    'final_freqs':final_freqs})
d.to_csv('word_stats.csv')

#######
#######Compute COALS features
numdocs = df.session_id.nunique()
session_to_ind = dict(zip(df.session_id.unique(),list(range(numdocs))))
sessions = df.session_id.unique()

period = word_to_ind['.']
question = word_to_ind['?']
exclam = word_to_ind['!']
ed = word_to_ind["ed"]
s = word_to_ind["'s"]
ing = word_to_ind["ing"]
d = word_to_ind["'d"]
ve = word_to_ind["'ve"]
re = word_to_ind["'re"]
nt = word_to_ind["n't"]
ll = word_to_ind["'ll"]
rw = word_to_ind["RW"]
xxx = word_to_ind["xxx"]

#calculate sliding window counts for each document
total_counts = np.zeros((vocabsize, vocabsize))
#create the window to convolve
ramp = np.array([1,1,1,1,0,0,0,1,1,1,1])
ramp_size = int((len(ramp)-1)/2)
#select the document
for doc_num in range(len(sessions)):
    doc = df[df.session_id == sessions[doc_num]]
    #convert the document to a list of words
    docwords = [w for utt in doc.words for w in utt.split() if w in word_to_ind]
    #convert to list of numbers, ignoring inflections, utterance-boundaries, xxx, and RW
    if IGNORE_INFLECTIONS:
        doclist = [word_to_ind[w] for w in docwords if word_to_ind[w] not in [ed, s, ing, d, ve, re, nt, ll,rw,xxx,question,period,exclam]]
    else:
        doclist = [word_to_ind[w] for w in docwords if word_to_ind[w] not in [xxx,rw,period,question,exclam]]
    doclist = np.array(doclist)
    #convolve the document with the window
    counts = np.zeros([vocabsize, vocabsize])
    for i in range(len(doclist)):
        counts[
            doclist[i], #index of target word
            doclist[ range(max(0,(i-ramp_size)), min(len(doclist), (i+ramp_size+1)))] #indices of context words
            ] += ramp[range(max(0, ramp_size - i), min(len(ramp),len(doclist)-i+ramp_size))]
    total_counts += counts

#convert counts to correlation scores
total_counts[np.less(total_counts,0)] = 0
total = np.ones_like(total_counts)*sum(sum(total_counts))
marginal_row = np.outer(sum(total_counts), np.ones(vocabsize))
marginal_col = marginal_row.transpose()

corr = (
    (total*total_counts - marginal_row*marginal_col)
    / np.sqrt(
        marginal_row*(total-marginal_row)*marginal_col*(total-marginal_col)
    )
)
#postprocessing
corr[np.isnan(corr)] = 0
corr[np.less(corr, 0)] = 0
corr = np.sqrt(corr)
#remove rare words from features
corr_common = corr[:, np.greater(marginal_col[0,:], MIN_COALS_FREQ)]
#export to csv
p = pd.DataFrame(corr_common)
p['word'] = [ind_to_word[i] for i in range(n_words)]
p.columns =  [ind_to_word[i] for i in np.where(np.greater(marginal_col[0,:], MIN_COALS_FREQ))[0]] + ["word"]
#p.to_csv('coals_features_all_words.csv')
if IGNORE_INFLECTIONS:
    p.loc[0:n_cdi_words-1].to_csv('coals_features_noinfl_' + str(MIN_COALS_FREQ) + '.csv')
else:
    p.loc[0:n_cdi_words-1].to_csv('coals_features_inflections_' + str(MIN_COALS_FREQ) + '.csv')
