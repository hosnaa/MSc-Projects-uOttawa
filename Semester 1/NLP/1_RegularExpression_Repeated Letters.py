# -*- coding: utf-8 -*-
"""
Created on Sat May 15 12:58:46 2021

@author: hosna
"""

# A query may match the string by stretching the query

import re

def get_regex_of_string(S):
    ## Function for extracting the regEx: to be revisitied for summarizing
    var_list = []
    count_char = 1
    for i in range(len(S)):
        # print(S[i])

        # For the last character
        if i == len(S)-1:
            # var_list.append(S[i]+'{'+str(count_char)+'}')
            if count_char < 3:
                repeat = '{'+str(count_char)+'}'
            else: 
                repeat= '{1,}'  # '{1,'+str(count_char)+'}'               
                var_list.append(S[i]+repeat)
            word_elements = "".join(var_list)
            # print(word_elements)
            
        #for other characters: compare char with the subsequent one
        else:
            if S[i]== S[i+1]:
                count_char +=1
            else:
                if count_char < 3:
                    repeat = '{'+str(count_char)+'}' # if count<3; then char should be found at least by its count
                else: 
                    repeat= '{1,}'  #'{1,'+str(count_char)+'}'   # since this letter is stretched it can be found by any shape            
                var_list.append(S[i]+repeat)
                count_char = 1 
    return word_elements

def query_match_string (string, word_list):
# # Input: string--> stretchy word, query_words: list of words--> can any yield this string by stretching ?  
    
    if len(S) >0 and len(S) <=100 and len(word_list) <=100 and len(word_list) >0:
        count = 0 # count of words matching
        pat = '^' + get_regex_of_string(string) +'$'
        
        for word in word_list:
            if len(word)>0 and len(word)<=100:
                y = re.search(pat, word)
                if y != None:
                    count +=1
    
    return count


S = "heeellooo"
# can these words be stretched to reach the string: if the letter in 'S' is stretched to 3 or more then it can be in query of any length;
# however if letter in 'S' is of less than 3 repeatitions then it should be as it is (since I won't be able to stretch it)
words = ["hello", "heeeeeeloo", "helo", "helloo", "hhello"]
counts = query_match_string(S, words)
print(counts)
# #####################################################

# [a-z]{3,}

# m = re.search(r'^[a-z]{3,}$', 'hello') # search for whole letters to be more than 3 
# print(m)

 

# toy_list = ['hello','hi','lena']
# count = 0
# for x in toy_list:
#     y = re.search(r'^h', x)
#     if y != None:
#         count +=1  
# print(count)        



















