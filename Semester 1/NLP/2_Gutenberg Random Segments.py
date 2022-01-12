# -*- coding: utf-8 -*-
"""
Created on Wed May 19 16:50:26 2021

@author: hosna
"""

# Fields of gutenberg: https://www.nltk.org/book/ch02.html
# For random selection without replacement (1 item cannot be chosen more than once): https://note.nkmk.me/en/python-random-choice-sample-choices/#:~:text=To%20get%20random%20elements%20from,list%20of%20multiple%20random%20elements.
# Change variable name with loop: https://stackoverflow.com/questions/1060090/changing-variable-names-with-python-for-loops
# value of dictionary: https://www.geeksforgeeks.org/python-get-key-from-value-in-dictionary/
# Tokenize by punctuation: https://machinelearningmastery.com/clean-text-machine-learning-python/#:~:text=Split%20by%20Whitespace%20and%20Remove%20Punctuation&text=One%20way%20would%20be%20to,nothing%20(e.g.%20remove%20it).&text=Python%20offers%20a%20function%20called,set%20of%20characters%20to%20another.

import nltk
import random


def tokenize_books (random_books, labeled_docs = []):
    n= len(random_books)
    books_dict = {}
    tokenized_words = {}
    
    # To tokenize the books into words and remove punctuation
    for i in range(n):
        # make a dictionary containing the five books with their sentences
        books_dict[random_books[i]] = nltk.corpus.gutenberg.raw(str(random_books[i]))
        
        book_content= books_dict.get(random_books[i])
        # tokenize words
        tokens = nltk.word_tokenize(book_content)
        tokenized_words[random_books[i]] = [word for word in tokens if word.isalpha()]
        for key, value in tokenized_words.items():
            book_name = key
            book_words = value
        partition_book(book_name, book_words, labeled_docs)
    return labeled_docs
    
def partition_book(name, words, labeled_docs):
    
    # name, words = tokenize_books(random_books, n)
    for i in range(200):
        
        # get a random book
        # rand_book = random_books[random.choice(range(n))]
        # tokenize into words to count them
        
        # value_dict = tok_words.get(tok_words)
        
        ######################################### For Random words #########################################
        # rand_records = random.sample(value_dict, 100)
        
        # # The sentence has no mening, is this correct? or should I by some means partition them into sentence, count sentence words and clip at specific point
        # rand_records_sent = ' '.join(rand_records)
        # # dictionary not allow duplicates
        # # books_dict_records[rand_book] = rand_records_sent
        # labeled_docs.append([(rand_book, rand_records_sent) ])
        
        ###################################### For random sentences #########################################
        
        start_token = random.choice(range(len(words)-100))
        end_token = start_token+100
            
        # print(words[start_token:end_token])
        rand_words = words[start_token:start_token+100]
        rand_partition = ' '.join(rand_words)
        # dictionary not allow duplicates
        labeled_docs.append([(name, rand_partition)])
    return labeled_docs
        
# nltk.download('gutenberg')

# Gutenberg constituents

books = nltk.corpus.gutenberg.fileids()
books_to_check = random.sample(books, 5)
doc_and_author = tokenize_books(books_to_check)   
print(doc_and_author)    
    
    






