import os
import numpy as np
import csv

def create_train_val_split(image_files, fraction_val, 
                           random_seed=10,
                           ):
    """ Create a list of training and validation images.
    
    Saved in two .csv files.
    
    Args:
        image_files: list of full path strings to training images
        fraction_val: fraction of total values to put in the validation
            set. (.25 -> 25% of total images put in validation set)
        random_seed: so can get reproducable splits
        
    """
    
    def _get_image_name(file):
        """Get file name with extension removed."""
        
        name = os.path.splitext(os.path.basename(file))[0]
        
        return name
        
    
    image_names = [_get_image_name(file) for file in image_files]
    # important to have consistent sort to get reproducabale splits
    image_names = sorted(image_names)
    num_val = int(len(image_names) * fraction_val)
    val_inds = np.random.choice(len(image_names), num_val, replace=False)
    
    train_names = []
    val_names = []
    
    for ind, name in enumerate(image_names):
        if ind in val_inds:
            val_names.append(str(name))
        else:
            train_names.append(str(name))
            
    print('There are {} training images and {} validation images.'.format(
        len(train_names), len(val_names)))
     
    # save in folder above the images folder
    save_folder = os.path.dirname(os.path.dirname(image_files[0]))
    train_file = os.path.join(save_folder, 'train_names.csv')
    val_file = os.path.join(save_folder, 'val_names.csv')
    
    # opening the csv file in 'w+' mode 
    for file, names in zip([train_file, val_file], [train_names, val_names]):
        with open(file, 'w+', newline ='') as csvfile:
            out_csv = csv.writer(csvfile, delimiter=',')
            out_csv.writerow(names)
    
    
    
    