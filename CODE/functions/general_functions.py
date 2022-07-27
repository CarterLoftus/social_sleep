import os
import glob
import numpy as np
import matplotlib.pyplot as plt
import csv

def compute_mean(images):
    """Computer mean of each color channel from a list of images.
    
    Args:
        images: list of 3D numpy array HWC
    Returns:
        [mean red, mean green, mean blue]
    """
    color_sums = np.zeros(3)
    total_pixels = 0
    for image in images:
        color_sums += np.sum(np.sum(image, 0), 0)
        total_pixels += image.shape[0] * image.shape[1]
    return color_sums / total_pixels
        
def compute_std(images):
    """Computer standard deviation of each color channel from a list of images.
    
    Args:
        images: list of 3D numpy array HWC
    Returns:
        [std red, std green, std blue]
    """
    
    color_dif_sums = np.zeros(3)
    total_pixels = 0
    for image in images:
        color_means = np.mean(np.mean(image, 0), 0)
        dif = image - color_means
        dif2 = dif ** 2
        color_dif_sums += np.sum(np.sum(dif2, 0), 0)
        total_pixels += image.shape[0] * image.shape[1]
    std = np.sqrt(color_dif_sums / (total_pixels-1))
    return std

def get_mean_and_std(image_files):
    """ Get mean and standard devitaion of a set of images.
    
    Save mean and std in 
    
    Args:
        image_files: list of full paths to images
    """
    
    images = [plt.imread(file) for file in image_files]
    
    mean = compute_mean(images)
    mean = [str(val) for val in mean]
    std = compute_std(images)
    std = [str(val) for val in std]
    
    out_folder = os.path.dirname(os.path.dirname(image_files[0]))
    
    mean_file = os.path.join(out_folder, 'mean.csv')
    std_file = os.path.join(out_folder, 'std.csv')
    
    for file, names in zip([mean_file, std_file], [mean, std]):
        with open(file, 'w+', newline ='') as csvfile:
            out_csv = csv.writer(csvfile, delimiter=',')
            out_csv.writerow(names)
    
    