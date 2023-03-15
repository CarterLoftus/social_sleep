#!/usr/bin/env python
# coding: utf-8

# possible commands in the terminal 
# cd /raven/ptmp/rharel/thermal/raw_videos
# module load ffmpeg/4.4
# 'ffmpeg  -i temp2/%06d.jpg -c:v libx264 -qp 0 -preset veryslow ' + vid_name + '.mp4'
# 'ffmpeg  -i temp2/%06d.jpg -c:v libx264 -pix_fmt yuv420p -qp 0 -preset veryslow ' + vid_name + '.mp4'
# 
# print('ffmpeg -i temp2/%06d.tiff -c:v libx264 -qp 0 -preset veryslow ' + vid_name + '.mp4')
# 
# 
# to sort out
# tiff or jpg
# final ffmpeg setting
# it is possible to pararalize frames preperation
# save time stamps
# check subsampling effect

# In[ ]:


# make sure you are on Python 3.9
# possible installations
#!pip install guppy3
#!conda install -c conda-forge guppy3
#!pip uninstall FileSDK
#!pip install opencv-python
#!conda install -c conda-forge opencv
#!module load ffmpeg/4.4
#!conda install -c conda-forge opencv
#!pip install opencv-python

#installing Flir File SDK if needed 
#os.chdir('/raven/ptmp/rharel/thermal/raw_videos/sdks/file/python/dist')
#!pip install FileSDK-4.1.0-cp36-cp36m-win_amd64.whl
#!module load anaconda/3/2021.11
#os.chdir('/raven/ptmp/rharel/thermal/raw_videos/python')
#!python setup.py install --user
#os.chdir('/raven/ptmp/rharel/thermal/raw_videos')
#!python3 -m pip install --upgrade pip


# In[ ]:


# load packages
import os
import glob

import time
import numpy as np
import seaborn as sns
import datetime as dt
import pandas as pd
from tkinter import filedialog
from tkinter import *

import matplotlib.pyplot as plt
import matplotlib.image as plt_img
import matplotlib.image as plt_img
from matplotlib.offsetbox import AnchoredText

import h5py
import cv2
from guppy import hpy
import gc
import shlex
import pipes
from subprocess import check_call
import fnv
import fnv.reduce
import fnv.file  
from tqdm import tqdm
from joblib import Parallel, delayed
   
gc.collect()
#h5py.run_tests()


# In[ ]:


def plot_thermal_frame(i: int, it: int):
    """
    Plot a thermal frame with a timestamp and save it as a TIFF file.

    Args:
        i (int): Index of the frame.
        it (Iterator): An iterator object.

    Returns:
        None
    """
    fig = plt.figure(figsize=(width, height), dpi=100)
    
    # Get the current frame and reshape it to the height and width of the image.
    im.get_frame(it)
    final = np.array(im.final, copy=False).reshape((im.height, im.width))
    final1 = final - final.mean()
    
    # Plot the thermal image with rocket colormap.
#     plt.imshow(final1, vmin=-np.std(final1), vmax=final1.max(), cmap='rocket')
    plt.imshow(final1,vmin = -np.quantile(final1,.5), vmax = final1.max(), cmap='rocket',interpolation='catrom')
    
    # Add timestamp to the plot.
    timestamp = im.frame_info[0]['value'][4:]
    timestamp = dt.datetime.strptime(timestamp, '%H:%M:%S.%f')
    text_box_date = AnchoredText(timestamp.strftime("%H:%M:%S.%f")[:-5], frameon=True, loc=1, pad=0.5,
                        prop=dict(fontsize=12))
    text_box_frame = AnchoredText(str(i), frameon=True, loc=2, pad=0.5,
                            prop=dict(fontsize=12))
    plt.setp(text_box_date.patch, facecolor='white', alpha=0.3)
    plt.setp(text_box_frame.patch, facecolor='white', alpha=0.3)

    plt.gca().add_artist(text_box_date)
    plt.gca().add_artist(text_box_frame)
    
    # Set axis and save the plot as a TIFF file.
    plt.gca().set_position([0, 0, 1, 1])
    plt.axis('off')
    plt.savefig(f'{i:06}' +  '.tiff', bbox_inches='tight', pad_inches=0)
    
    plt.close()


# In[ ]:


## writing all frames from a given thermal recording, separated into minute bins. Timestamps unrounded
zoom = 1
vid_length = 1*60*60

main_folder = '/raven/ptmp/rharel/thermal/raw_videos/'
data_folder =  main_folder + 'viewpoint_1/T1020/20190818/'
output_folder =  main_folder + 'output/'


if not os.path.isdir( output_folder ):
    os.makedirs ( output_folder)
    
os.chdir(data_folder)
file_names = glob.glob('*.seq')
file_names


# In[ ]:


for file_name in file_names:
    
    im = fnv.file.ImagerFile(data_folder + file_name)

    im.get_frame(0)
    #im.frame_info[1]
    #specifying first and last frame after loading the specific file
    print('got_frame')

    # selected frames to make the video
    start_frame = 0
    end_frame = im.num_frames
    step_frame = 5
    frame_range = range(start_frame,end_frame,step_frame)
    print(frame_range)

    vid_name = file_name[:8] + '_' + str(end_frame) + '_' + str(step_frame)
    image_folder = main_folder + vid_name + '/'
    if not os.path.exists(output_folder + vid_name + '.mp4'):
        # where to write the images to   
        #day1 = int(im.frame_info[0]['value'][:3])
        #start = f'{start_dt.year:04}-' + f'{start_dt.month:02}-' + f'{start_dt.day:02} ' + im.frame_info[0]['value'][4:]
        #start_dt = dt.datetime.strptime(start, '%Y-%m-%d %H:%M:%S.%f')
        
        if not os.path.isdir( image_folder ):
            os.makedirs ( image_folder)
        os.chdir( image_folder )
        for f in glob.glob(image_folder + "*.tiff"):
            os.remove(f)

        final = np.array(im.final, copy=False).reshape((im.height, im.width))
        width = final.shape[1] / (100 / zoom)  
        height = final.shape[0] / (100 / zoom)

        fig = plt.figure( figsize = ( width, height ) )  
        fig.set_dpi(100)

#         if start_frame > 0:
#             counter = 0
#         else:
#             counter = 1

#         print((im.width, im.height))


        # Loop through frame_range and make frames with plot_thermal_frame function
        for idx, item in tqdm(enumerate(frame_range)):
            plot_thermal_frame(idx, item)

        del im
        gc.collect()

        #os.chdir(main_folder)
        #command = 'ffmpeg -i ' + image_folder + '%06d.tiff -c:v libx264 -qp 0 '  + output_folder + vid_name + '_' + str(step_frame) + '.mp4'
        #check_call(shlex.split(command))

        os.chdir(main_folder)
        command = 'ffmpeg -i ' + image_folder + '%06d.tiff -c:v libx264 -crf 12 '  + output_folder + vid_name + '_low_res.mp4'
        check_call(shlex.split(command))