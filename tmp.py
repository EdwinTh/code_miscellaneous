#!/usr/bin/env python
from sys import argv, exit
from os import listdir, rename
import datetime
import exiftool

script, hours_diff, print_or_rename = argv

hours_diff = int(hours_diff)

if print_or_rename not in ['print', 'rename']:
	exit('print_or_rename can only be print or rename')

files    = listdir('.')
files    = [i.lower() for i in files]

with exiftool.ExifTool() as et:
	metadata = et.get_metadata_batch(files)

nr_skipped = 0
for i in range(1, len(metadata)):
	try:
		dt = metadata[i][u'EXIF:DateTimeOriginal']
	except:
		nr_skipped += 1
		print "Skipped " + str(nr_skipped) + " without exif info" 
		continue
	
	dt_cleaned = datetime.datetime.strptime(str(dt), "%Y:%m:%d %H:%M:%S")
	name_change = dt_cleaned + datetime.timedelta(hours = hours_diff)
	name_change = str(name_change)
	day         = name_change[0:10]
	time        = name_change[11:]
	time        = time.replace(':', '-')
	new_file_name = day + '_' + time + '.jpg'

	if print_or_rename == 'print':
		print str(files[i]) + ' ' + new_file_name

	else:
		rename(str(files[i]), new_file_name)

