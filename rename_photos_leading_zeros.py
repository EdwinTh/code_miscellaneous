#!/usr/bin/env python
from sys import argv, exit
from os import listdir, rename

'''
This script will add leading zeros to a number in a .jpg name. Purpose is that the files will be correctly sorted.

example if we have 3 files they will be renamed like:
photo_1.jpg         photo_001.jpg
photo_10.jpg		photo_010.jpg
photo_100.jpg       photo_100.jpg

The explicit assumption is that all file names are similar except for the numeric part.

It takes one parameter:
-The position at which the zeros should be inserted.
'''

script, insert_position = argv
insert_position = int(insert_position)

# listdir will extract all files in the current folder
files    = listdir('.')
files_cl = [i for i in files if i[-3:] == 'jpg']

# find the longest filename
max_name_length = max([len(i) for i in files_cl])

# determine for each file how many zeros we need to add
nr_leading_zeros = [max_name_length-len(i) for i in files_cl]

# create the list with new filenames
files_with_zeros = []
for i, f in enumerate(files_cl):
	first = f[0:insert_position] 
	last  = f[insert_position:]
	zeros =  '0' * nr_leading_zeros[i]
	filename_clean.append(first + zeros + last)

# rename all the files to its new name
for i, original in enumerate(files_cl):
	rename(original, files_with_zeros[i])
