import cv2
import os
import glob

import torch.utils.data as data

class SegmentationDataset(data.Dataset):
    def __init__(self, folder_paths, transform=None, 
                 keep_orig=False, names=None, max_num_examples=None):
        """
        Args:
            folder_paths (list): list of paths each contains 
                .jpg image files in a folder clled 'images'
                and in folder called 'masks' .png masks
            transform (callable, optional): Optional transforms to be 
                applied on a sample
            keep_orig: keep un-transformed image
            names: list of specific files to uses
            max_num_examples: how many examples to used from folder, 
                if None read all
        """
        self.mask_files = []
        self.img_files = []
        self.keep_orig = keep_orig
        self.num_examples = 0
        
        if names:
            assert len(folder_paths) == 1, "only one folder allowed if using names"
            folder_path = folder_paths[0]
            for num, name in enumerate(names):
                if max_num_examples:
                    if num >= max_num_examples:
                        break
                self.img_files.append(
                    os.path.join(folder_path, 'images', name + '.jpg')
                )
                self.mask_files.append(
                    os.path.join(folder_path, 'masks', name + '.png')
                )
                self.num_examples += 1
        else:
            for folder_path in folder_paths:
                possible_img_files = glob.glob(
                    os.path.join(folder_path, 'images', '*.jpg')
                )
                if self.num_examples is None:
                    self.num_examples += len(possible_img_files)
                for img_num, img_path in enumerate(possible_img_files):
                    if not max_num_examples:
                        self._add_annotation(folder_path, img_path)
                    else:
                        if self.num_examples < max_num_examples:
                            self._add_annotation(folder_path, img_path)
                        else:
                            break
            
        self.transform = transform
        
    def _add_annotation(self, folder_path, img_path):
            mask_name = os.path.splitext(os.path.basename(img_path))[0]
            self.mask_files.append(os.path.join(folder_path, 'masks', 
                                                mask_name + '.png')
                                  )
            self.img_files.append(img_path)
            self.num_examples += 1
                                   
    def __getitem__(self, index):
            img_path = self.img_files[index]
            mask_path = self.mask_files[index]
            image = cv2.cvtColor(cv2.imread(img_path), cv2.COLOR_BGR2RGB)
            mask = cv2.imread(mask_path, cv2.IMREAD_GRAYSCALE)
            sample = {'image': image[2:-2, 2:-2], 'mask': mask[2:-2, 2:-2]}
            if self.keep_orig:
                sample['orig'] = image
            
            if self.transform:
                sample = self.transform(sample)
            return sample

    def __len__(self):
        return len(self.img_files)