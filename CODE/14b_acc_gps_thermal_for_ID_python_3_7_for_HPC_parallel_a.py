#!/usr/bin/env python
# coding: utf-8

# ## The python code starts here

# In[1]:


## import necessary modules
import subprocess
import os
import glob
import sys
import time
import pandas as pd
import numpy as np
import datetime as dt
from sortedcontainers import SortedSet
import gc

from decord import VideoReader, cpu, gpu
import cv2
import subprocess as sp
from tqdm import tqdm

import matplotlib.pyplot as plt

from osgeo import gdal
import rasterio
from rasterio.plot import show

import matplotlib.cm as cm

# Use Agg backend for canvas
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas

import torch

import multiprocessing as mp

from functools import partial
from itertools import repeat
from multiprocessing import Pool, freeze_support

# from joblib import Parallel, delayed


# In[51]:


import multiprocessing
# We must import this explicitly, it is not imported by the top-level
# multiprocessing module.
import multiprocessing.pool


class NoDaemonProcess(multiprocessing.Process):
    # make 'daemon' attribute always return False
    def _get_daemon(self):
        return False
    def _set_daemon(self, value):
        pass
    daemon = property(_get_daemon, _set_daemon)

# We sub-class multiprocessing.pool.Pool instead of multiprocessing.Pool
# because the latter is only a wrapper function, not a proper class.
class MyPool(multiprocessing.pool.Pool):
    Process = NoDaemonProcess
    


# define the write video function. The write video function will take a tag name as an input and will use this tag name to determine which individual's accelerometry data will be plotted for the video. Which thermal video will be plotted has to be defined before the function. The output is a video created by open-cv (cv2.VideoWriter), which contains the thermal video plotted with the specified individuals accelerometry data and all individual's GPS data (with the focal individual's GPS tag name plotted in red). The accelerometry and GPS data is synchronized to the thermal video 
def write_video( main_tag, input_video_file ):
    
    print( main_tag )
    print( input_video_file )
    
    # save the full filepath of the input video
    input_video_file_path = os.path.join( curr_night_dir, input_video_file )
    
    # save the name of the input video, without the "final's" that have been added to the end as a result of manual track correction. This is the name that will match the txt file corresponding to this video
    vid_name = input_video_file.split( "." )[ 0 ].split( '_', 1 )[ 1 ].split( '-' )[ 0 ]

    # save the full filepath of the txt file that contains the timestamp associated with each frame of the input video
    txt_filename = os.path.join( curr_night_dir, str( vid_name + '.txt') )
    
    #open the text file
    with open(txt_filename) as f:
        lines = f.readlines()

    # extract the timestamp of each frame from each line of the txt file
    frame_timestamps = list( map( lambda x: x.split( "'")[ 1 ].split( '.' ) [ 0 ].split( "_", 1 )[ 1 ] , lines ) )
    
    # turn the character string timestamps into datetime elements
    frame_timestamps = list( map( lambda x: dt.datetime.strptime(x, '%Y%m%d_%H%M%S%f'), frame_timestamps ) ) 

    # add three hours to the timestamps of the frames to convert them from UTC into local Kenyan time (East Africa Time)
    frame_local_timestamps = list( map( lambda x: x + dt.timedelta( hours = 3 ), frame_timestamps ) )

    # remove the lines from the txt file from the memory. We no longer need them
    del lines

    # save the start time of the video
    start_time = frame_local_timestamps[ 0 ]

    # save the end time of the video
    end_time = frame_local_timestamps[ ( len( frame_local_timestamps ) - 1 ) ]

    ### next we will read in the GPS data and trim it to the time corresponding to this video
    ## it might be more efficient to read in the GPS data before the function, and then have this step just trim it to the relevant times. But given that there is a lot of GPS data, keeping the full GPS data always in the memory might be problematic
    
    # change the directory to where the GPS and ACC data is stored
    os.chdir('/raven/ptmp/cloftus/social_sleep/DATA/gps_acc_data/')

    # read in the gps data    
    gps_dat = pd.read_csv('gps_dat.csv', header=0)

    # keep only the relevant columns of the GPS data. Namely, the tag identity, the local timestamp, and the x-y location
    gps_dat = gps_dat[ [ 'tag', 'corr_local_timestamp', 'x', 'y' ] ]

    # save the tag names of all the individuals
    tag_names = SortedSet( gps_dat[ 'tag' ] )

    # turn the character string timestamp into a datetime element
    gps_dat[ 'corr_local_timestamp' ] = pd.to_datetime( gps_dat[ 'corr_local_timestamp' ] )

    # extract the GPS data that is concurrent with the input video
    gps_trim = gps_dat[ ( gps_dat[ 'corr_local_timestamp' ] > ( start_time - buffer ) ) & ( gps_dat[ 'corr_local_timestamp' ] < ( end_time + buffer ) ) ]

    # remove the full GPS data from the memory
    del gps_dat
    gc.collect()
    
    
    ### read in the thermal video and the background map that we will use to plot our GPS on.
    ## it would be more efficient if both of these were read in before the function, because they will be the same for all individuals over which we are parallelizing. But joblib doesn't allow calls to the same object from mulitple workers
    ## but given that it only takes a minute or two to read these in, compared to the 6-8 hours that it takes to produce the video for each individual, it doesn't actually have an influence on run time
    
    torch.cuda.empty_cache()
    
    # read in the input video with decord's video reader
    vr1 = VideoReader( input_video_file_path ) 
    
    # read in the background map on which we will plot the GPS (this would be done more efficiently above the function, because it is always the same for all individuals and for all input videos, but joblib doesn't allow repeated calls to the same object from several workers simultaneously)
    src1 = rasterio.open( cliff_crop_path )

    ### extract just the focal individual's ACC and GPS data (the individual specified above in the function)
    ## change the directory to wherer the ACC data that corresponds to the the time of the input video
    os.chdir( '/raven/ptmp/cloftus/social_sleep/DATA/gps_acc_data/parsed_acc_for_animation/' + night + '/' + vid_name )

    # read in the focal individual's ACC data
    acc = pd.read_csv( str( main_tag ) + '_' + vid_name + '.csv' )

    # extract just the focal individual's gps data from the trimmed gps data that is concurrent with the input video
    tag_gps = gps_trim[ gps_trim[ 'tag' ] == main_tag ]

    ## if the focal individual doesn't have any accelerometry or gps data that is concurrent with this input video, we will not produce an output video. Instead we will delete the variables we have saved thus far and move onto the next individual
    ## if however, the focal individual has either ACC or GPS data concurrent with the input video, we will enter the if loop below to produce a visualization of its data next to the thermal video input data
    
    if ( acc.shape[ 0 ] != 0 ) | ( tag_gps.shape[ 0 ] != 0 ): #if the focal individual has either acc or gps data concurrent with the input video...
        
        if acc.shape[ 0 ] != 0: # if the focal individual has acc data concurrent with the input video...

            # turn the character string timestamp of it's ACC data into a datetime element
            acc[ 'corr_local_timestamp' ] = pd.to_datetime( acc[ 'corr_local_timestamp' ] )

            # extract the acc data that is concurrent with the input thermal video (I believe this step is redundant, because the input ACC data is already separated according which video it matches, and we read in only the data that we know to match this video)
            whole_print = acc[ (acc[ 'corr_local_timestamp' ] > (start_time - dt.timedelta(seconds = (time_frame + pad))) ) & (acc[ 'corr_local_timestamp' ] < (end_time + dt.timedelta(seconds = (time_frame + pad)) ) ) ]

            # find the overall minimum and maximum accelerometer values. We will use this to set the y limits of our accelerometry plot for the video we will produce 
            overall_min = np.min( [np.min(whole_print['x']), np.min(whole_print['y']), np.min(whole_print['z']) ] )
            overall_max = np.max( [np.max(whole_print['x']), np.max(whole_print['y']), np.max(whole_print['z']) ] ) + 1

        # save the directory where we will save the output video. We will separate the videos into directories by night. The night to process is a user-input in the code
        print_dir =  os.path.join( '/raven/ptmp/cloftus/social_sleep/RESULTS/thermal_acc_animation/', night )

        # make the directory where we will save the output, if it has not already been made. And then change to this directory 
        os.makedirs( print_dir, exist_ok = True )
        os.chdir( print_dir )

        # start a counter at 0. We will increment this counter by 1 with every frame we add to the output video, and use it to determine when our video is so big that we should release it and start writing a new video
        counter = 0

        # save the full file path to the output video, which will include tag name of the focal individual, the core name of the input thermal video, as well as the counter number (as we will produce three small videos for each input video and tag, rather than one large video)
        video_out_file = os.path.join( print_dir, str( str( main_tag ) + '_' + vid_name + '.mp4' ) )

        ### next we will go through each frame of the input thermal video and produce an output frame, that contains the associated frame from the input video, with the concurrent GPS and ACC data plotted above it
        
        for i in tqdm( range( ( len( frame_local_timestamps ) - 1 ) ) ): # for each frame of the input video

            ### first, trim the GPS data to the relevant times to plot on this frame (this will be the GPS data up until the current timestamp of the given input video frame, that is within the gps_tail parameter set above)

            # save the timestamp of the current frame of the input video
            curr_time = frame_local_timestamps[ i ]
            
            # trim the gps data to the times less than or equal to the time of the current frame of the input video and greater than the 10 seconds prior to the time of the current frame (if gps_tail is set to 10 seconds)
            short_gps = gps_trim[ ( gps_trim[ 'corr_local_timestamp' ] > (curr_time - dt.timedelta(seconds =  gps_tail ) ) ) & ( gps_trim[ 'corr_local_timestamp' ] <= curr_time ) ]

            # find the location where each individual currently is (or at least where they were within the last 0.99 seconds). This is where we will print their tag name (with the label_dist added to it)
            curr_gps = gps_trim[ ( gps_trim[ 'corr_local_timestamp' ] > ( curr_time - dt.timedelta( microseconds = 990000 ) ) ) & ( gps_trim[ 'corr_local_timestamp' ] <= curr_time ) ]
            # save the x values of the locations of where to print the tag names
            tag_loc_x = curr_gps[ 'x' ] + label_dist
            # save the y values of the locations of where to print the tag names
            tag_loc_y = curr_gps[ 'y' ] + label_dist
            # save the names of the tags to be printed at the locations that we just saved
            tags_to_print = curr_gps[ 'tag' ]

            # extract the current frame of the input video, which we read in before with decord
            frame = vr1[ i ]
            # rotate the frame 90 degrees and turn it into a numpy array. It needs to be rotated so that the background map doesn't need to be rotated (which would be challenging because the background map is georeferenced and should stay that way)
            im = np.flip( np.transpose( frame.asnumpy(), axes = ( 1, 0 , 2) ), axis = 0 )

            # creating matplotlib figure where we will create subplots
            fig = plt.figure( ) 

#             # creating matplotlib figure where we will create subplots
#             fig = plt.figure( tight_layout = True ) 

#             # make the layout tight so it doesn't print wide margins
#             fig.tight_layout( pad = 0 )

            # set the figure size. This will determine the resolution of the final video. 40 is large, but high resolution is important -- without it, we can't see the distant baboons very well
            fig.set_figheight(20)
            fig.set_figwidth(20)

            ### if there is GPS data. Plot the map and plot the GPS data (trimmed to the GPS data just prior to the timestamp of this current frame) on top of it
            
            if short_gps.shape[ 0 ] != 0: # if there is GPS data within the last 10 (gps_tail) seconds of this frame...

                # create a subplot for the GPS data
                ax1 = plt.subplot2grid(shape=(3, 3), loc=(1, 0), rowspan=2 )

                # plot the background map with rasterio's function show
                show(src1.read(), ax = ax1, transform=src1.transform)

                # set the limits of the subplot to be the bounds of the map (so we won't plot GPS data that is not contained within the background map)
                ax1.set_xlim( src1.bounds[ 0 ], src1.bounds[ 2 ] )
                ax1.set_ylim( src1.bounds[ 1 ], src1.bounds[ 3 ] )

                # plot the GPS points
                ax1.scatter( short_gps[ 'x' ], short_gps[ 'y' ], color = 'orange', s = 50 )

                # remove the x and y ticks 
                plt.xticks( [] ), plt.yticks( [] )

                # for each individual, plot their tag name at their current location. Plot the focal individual's tag name in red
                for loc_x, loc_y, print_tag in zip(tag_loc_x, tag_loc_y, tags_to_print ): # for each individual (and their respective location)...

                    if print_tag == main_tag: # if it the focal individual, make the plot color of the tag label red. If not, make it black
                        col = 'red'
                    else:
                        col = 'black'

                    plt.text(  loc_x, loc_y, print_tag, color = col, rotation = 90, clip_on = True, fontsize = 24 ) # plot the tag name on the background map

            ### next we will plot the focal individual's ACC data (if it has ACC data)
            if acc.shape[ 0 ] != 0: # if the focal indivdual has ACC data that is concurrent with this input video... 

                # trim the focal individual's ACC data to the timestamp of the current frame of the input video (plus or minus X seconds: X determined by the pad variable)
                short = acc [ ( acc[ 'corr_local_timestamp' ] > (curr_time - dt.timedelta(seconds = (time_frame + pad ))) ) & (acc[ 'corr_local_timestamp' ] < (curr_time + dt.timedelta(seconds = (time_frame + pad ) ) ) )  ]

                # if there is acc data for the individual at this time, plot it. Remember that the acc is subsetted with the time pad, so "at this time" really means at this time +/- the time pad (so if there is ACC data within 1 minute in the future and past)
                if short.shape[ 0 ] != 0:

                    # create a subplot for the focal individual's ACC data
                    ax2 = plt.subplot2grid(shape=(3, 3), loc=(0, 0), colspan=1)

                    # draw a red vertical at the timestamp of the current frame of the input video
                    ax2.axhline( curr_time, c = 'r' , linewidth = 3 ) 

                    # these x and y limits seem backwards, but that is because we are printing the ACC data rotated by 90 degrees
                    # set the y limits to be the time window of acc data around the current timestamp that we show (determined by the time_frame variable)
                    ax2.set_ylim((curr_time - dt.timedelta(seconds = (time_frame ))),  (curr_time + dt.timedelta(seconds = (time_frame)) ))
                    # set the x limits to be the overall min and max accelerometry values for the video
                    ax2.set_xlim( overall_min, overall_max )

                    # plot the ACC data
                    ax2.plot( short['x'], short[ 'corr_local_timestamp' ],  linewidth = 2)
                    ax2.plot( short['y'], short[ 'corr_local_timestamp' ],  linewidth = 2)
                    ax2.plot( short['z'], short[ 'corr_local_timestamp' ],  linewidth = 2)

                    # remove the x and y ticks
                    plt.xticks( [] ), plt.yticks( [] )

            ### next we will plot the current frame of the input video
            # create a subplot where we will plot the thermal video frame
            ax3 = plt.subplot2grid(shape=(3, 3), loc= (0, 1), rowspan=3, colspan = 2 )

            # plot the thermal video frame    
            plt.imshow( im )
            
            # remove the x and y ticks
            plt.xticks( [] ), plt.yticks( [] )

            # remove the margins between the plot
            plt.subplots_adjust(wspace=0, hspace=0)

            # draw the canvas
            canvas = FigureCanvas(fig)
            canvas.draw()

            # turn the plot canvas into an array
            mat = np.array(canvas.renderer._renderer)
            # turn the array into an image with open-cv
            mat = cv2.cvtColor(mat, cv2.COLOR_RGB2BGR)

            # crop the left side of the video, that for some reason doesn't get cropped with tight layout above
            mat_cropped =  mat[ 170:1205, 178:1300 ]

            # start writing the video if this is our first iteration through the loop
            if( counter == 0 ):

                # declare the codec for the video
                fourcc = (cv2.VideoWriter_fourcc)(*'mp4v')

                # instantiate the video at the desired file location with the codec declared above. Pull the dimensions for the video from the first frame extracted from the input video
                video_writer = cv2.VideoWriter( video_out_file , fourcc, 15, (mat_cropped.shape[1], mat_cropped.shape[0]) ) 
                
            # write frame to video
            video_writer.write( mat_cropped )

            # clean up
            fig.clear()
            plt.close()
            gc.collect()

            # advance the counter
            counter += 1

#             ## note that in the next few lines, we will release the video and start writing a new one, if the video surpasses a threshold number of frames. This is only because I don't want to run out of memory
#             ## in a perfect world, in which I didn't have to worry about memory, I would only release one output video for each input video, at the very end of iterating through all the input video frames 
            
#             # if the counter reaches a multiple of 6200 (number chosen because it produces three roughly equal length output videos for each input video (which are each about 18000 frames long))
#             if( counter % 4000 == 0 ):

#                 # release the video
#                 video_writer.release()

#                 # empty the memory cache. This step is important. I tried running without this step and it causes a memory leak
#                 torch.cuda.empty_cache()

#                 # start a new video, named the same as above, but now with the counter advanced
#                 video_writer = cv2.VideoWriter( os.path.join( print_dir, str( str( main_tag ) + '_' + vid_name + '_' + f'{counter:05}' + '.mp4' ) ), fourcc, 10, (mat.shape[1], mat.shape[0]) ) # instantiate the video at the desired file location with the codec declared above. Pull the dimensions for the video from the first frame extracted from the input video

        # after all frames of the input video have been iterated through, release the output video
        video_writer.release()
        
        print( 'finished with ' + str( main_tag ) + ' ' + input_video_file )

        # empty the memory cache
        torch.cuda.empty_cache()

        ## delete the variables that were saved to clear up as much memory as possible 
        ## this has a try and except statement because some of the variables won't get saved if, for example, there is no ACC data of focal individual to read in
        ## so we will try to deleting all the variables, and then just the variables that would be saved if the individual didn't have ACC data. If that doesn't work, we will just skip this step, because none of these variables here take up much memory (it is more the input video which takes up memory, and that gets deleted below) 
        try:

            del short_gps, whole_print, short, tags_to_print, curr_gps, im, mat, mat_cropped, canvas, tag_loc_x, tag_loc_y, frame, counter, video_writer, fourcc, overall_min, overall_max, print_dir

        except:

            try: 

                del short_gps, tags_to_print, curr_gps, im, mat, canvas, tag_loc_x, tag_loc_y, frame, counter, video_writer, fourcc, print_dir

            except:

                pass

    del vr1, src1, acc, tag_gps

    gc.collect()


# In[ ]:




def video_loop( input_video_file, tag_names ):
        
    inner_loop_starttime=dt.datetime.now() 
    
    pool = MyPool( 7 ) # this was 13
        
    pool.map( partial( write_video, input_video_file = input_video_file ), tag_names )

    pool.close()
    pool.join()
    
    inner_loop_endtime=dt.datetime.now() 
    print('Total "parallelized inner loop" time = ', inner_loop_endtime-inner_loop_starttime)

    torch.cuda.empty_cache()

    


# In[52]:


night = '20190806' ## this is a user-input night to be processed. The data is divided up into nights, each of which contains several 1 hour videos of data, as well as GPS and accelerometry data. The nights could technically be parallelized as well, but as we don't have many nigths of data, I don't think it is necessary. Parellelizing the processing of the videos within each night and the individuals that have data during each video is, however, important 


# In[53]:


### plotting parameters. These are not arguments of the function because they will not be changed.

# amount of time in seconds that you want to keep of GPS data before and after each video
buffer = dt.timedelta( seconds = 120 ) 

# time frame is half the number of seconds shown within the acc plot. (The x limits are set by the current time minus the time frame and the current time plus the time frame)
time_frame = 4

# label_dist is how far away the GPS label is from the current position of the baboon (in meters). So '1' would be 1 meter to the east and 1 meter north of the baboon's current position 
label_dist = 1 

# seconds of gps tail 
gps_tail = 10 

# number of seconds on either side of the subplot limits that we will print acc for (so it goes up to the edge of the frame I believe)
pad = 100


# In[54]:


# save the file path to the background map that we 
cliff_crop_path = '/raven/ptmp/cloftus/social_sleep/DATA/cliff_tiff/less_cropped_cliff.tif'


# In[55]:


# potential videos

# the save the directory where the input video files to be processed are
curr_night_dir = os.path.join( '/raven/ptmp/cloftus/social_sleep/DATA/tracking_output/finalized_tracks/', night )

# change to this directory
os.chdir( curr_night_dir )

# list the videos that are contained within this directory. These are the videos that we can process
potential_input_video_files = glob.glob( '*.mp4' ) ## need to set up a way to only choose the video file with the longest name in case there is more than one

## if you want to process all the potential input video files
inds = range( len( potential_input_video_files ) )

## if you only want to process certain video files. Can enter a single index here or a list of indices of the videos to process
#inds = range( 0, 2 )

# make a list of the input video files that we actually want to process
input_video_files = sorted( [ potential_input_video_files[i] for i in inds ] )


# In[56]:


# change the directory to where the GPS and ACC data is stored
os.chdir('/raven/ptmp/cloftus/social_sleep/DATA/gps_acc_data/')

# read in the gps data    
gps_dat = pd.read_csv('gps_dat.csv', header=0)

# keep only the relevant columns of the GPS data. Namely, the tag identity, the local timestamp, and the x-y location
gps_dat = gps_dat[ [ 'tag', 'corr_local_timestamp', 'x', 'y' ] ]

# save the tag names of all the individuals
tag_names = SortedSet( gps_dat[ 'tag' ] )

tag_names = tag_names[ :13 ]

# turn the character string timestamp into a datetime element
gps_dat[ 'corr_local_timestamp' ] = pd.to_datetime( gps_dat[ 'corr_local_timestamp' ] )


# remove the full GPS data from the memory
del gps_dat
gc.collect()


outer_loop_starttime=dt.datetime.now() 

freeze_support()

if __name__ == '__main__':
    
    pool = MyPool(3) # this was 3

    pool.map( partial( video_loop, tag_names = tag_names ), input_video_files )
        
    pool.close()
    pool.join()
    
outer_loop_endtime=dt.datetime.now() 
    
print('Total "parallelized outer loop" time = ', outer_loop_endtime-outer_loop_starttime)

# In[47]:


print("Number of processors: ", mp.cpu_count())


# In[26]:



