#!/usr/bin/env python
from sys import argv
from os import listdir, rename
from exifread import process_file
import datetime

'''
This script will extract the exif data from all .jpg files in a folder
and rename all the .jpg files to its datetime. It takes one parameter:
the timedifference that should be applied in case the pictures are taken 
is different timezone.
'''

# unpacking the variable
script, hours_diff = argv
hours_diff = int(hours_diff)

# listdir will extract all files in the current folder
files = listdir('.')
files_cl = [i for i in files if i[-3:] == 'jpg']

# function that extracts the datetime from the exif data
def get_exif(file_dir):
	file_info = open(file_dir, 'rb')
	tags      = process_file(file_info)
	raw       = tags['Image DateTime']
	ret       = datetime.datetime.strptime(str(raw), "%Y:%m:%d %H:%M:%S")
	return ret 

for i in files_cl:
	time        = get_exif(i)
	name_change = time + datetime.timedelta(hours = hours_diff)
	name_change = str(name_change)
	day         = name_change[0:10]
	time        = name_change[11:]
	time        = time.replace(':', '-')
	new_file_name = day + '_' + time + '.jpg'

	rename(str(i), new_file_name)                                                                                                                                                                                   

#	print i + ' ' + time + ' ' + new_file_name