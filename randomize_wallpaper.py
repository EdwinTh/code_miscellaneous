"""
Randomly rename files to a unique number up to 10000 with leading zeros 
to randomize the order Mac OS shows the images in the wallpaper folder
"""

FOLDER_PATH = "/users/edwinthoen/Documents/wallpaper_pics/"

import os
import random

current_filepaths = [FOLDER_PATH + fn for fn in os.listdir(FOLDER_PATH)]
current_filepaths_randomized = random.sample(current_filepaths, len(current_filepaths))

for i in range(len(current_filepaths_randomized)):
    new_filename_base = "0000" + str(i)
    new_filename_path = FOLDER_PATH + new_filename_base[-5:] + ".jpg"
    os.rename(current_filepaths_randomized[i], new_filename_path)