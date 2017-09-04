#!/usr/bin/env python
from sys import argv, exit
from os import listdir, rename

'''
Mac's photos has the annoying feature of exporting files as .m4v, which GoProStudio won't swallow.
Setting them to .mp4 is easy, just rename it. This file will rename all .m4v files in a folder to .mp4.
'''

# open all files in the current directory
files    = listdir('.')
files    = [i.lower() for i in files]
files_cl = [i for i in files if i[-3:] == 'm4v']

for i in files_cl:
	new_file_name = i[:-3] + 'mp4'
	rename(str(i), new_file_name)