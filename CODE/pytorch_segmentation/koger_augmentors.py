import imgaug as ia
import imgaug.augmenters as iaa
import numpy as np
import torch
import torchvision.transforms.functional as TF
import cv2

class MaskRandomCrop:
    """Rotate by one of the given angles."""

    def __init__(self, crop_height, crop_width, deterministic=False):
        # size of square crop
        self.crop_height = crop_height
        self.crop_width = crop_width
        self.deterministic = deterministic

    def __call__(self, sample):
        im = sample['image']
        mask = sample['mask']

        im_height = im.shape[0]
        im_width = im.shape[1]
        top = np.random.randint(im_height - self.crop_height)
        left = np.random.randint(im_width - self.crop_width)
        
        if self.deterministic:
            top = 500
            left = 500

        im_crop = im[top:top+self.crop_height, left:left+self.crop_width]
        mask_crop = mask[top:top+self.crop_height, left:left+self.crop_width]
        
        sample['image'] = im_crop
        sample['mask'] = mask_crop

        return sample
    
class MaskResize:
    """ Resize image and mask"""
    
    def __init__(self, ratio):
        """ Args:
                ratio: size of new image size / old image size
        """
        
        self.ratio = ratio
        
    def __call__(self, sample):
        im = sample['image']
        mask = sample['mask']

        im_height = im.shape[0]
        im_width = im.shape[1]
        new_height = int(im_height * self.ratio)
        new_width = int(im_width * self.ratio)
        
        if self.ratio > 1:
            inerpolation = cv2.INTER_CUBIC
        else:
            interpolation = cv2.INTER_AREA
        new_image = cv2.resize(im, (new_width, new_height), interpolation=interpolation)
        new_mask = cv2.resize(mask, (new_width, new_height), interpolation=interpolation)
        
        sample['image'] = new_image
        sample['mask'] = new_mask

        return sample
    
class MaskImgAug:
    
    def __init__(self):
        # size of square crop
        self.transform = iaa.Sequential([
#             iaa.AdditiveGaussianNoise(scale=(0, 0.1*255)),
            iaa.Sometimes(0.4, iaa.blur.GaussianBlur(sigma=(0.0, 6.0))),
            iaa.Sometimes(0.4, iaa.AdditivePoissonNoise((0, 30)))
        ])

    def __call__(self, sample):
        im = sample['image'].astype(np.uint8)
        im_transform = self.transform(image=np.array(im))
        sample['image'] = im_transform.astype(float)
        
        return sample
    
class Mask2dMultiplyAndAddToBrightness():
    def __init__(self, add, multiply):
        """Add and multiply image values by given amount randomly within range.
        
        Expects image to be between 0 and 255.
        
        Args:
            add: either number of tuple, if tuple randomly choose from range
            multiply: either number ot tuple, if tuple randomly choose from range
        """
        self.add = add
        self.multiply = multiply
        self.rng = np.random.default_rng()
        
    def __call__(self, sample):
        im = sample['image'].astype(np.float64)
        if isinstance(self.add, tuple):
            add_val = self.rng.uniform(self.add[0], self.add[1])
        else:
            add_val = self.add
        if isinstance(self.multiply, tuple):
            multiply_val = self.rng.uniform(self.multiply[0], self.multiply[1])
        else:
            multiply_val = self.multiply
        im += add_val
        im *= multiply_val
        im = np.maximum(im, 0)
        im = np.minimum(im, 255)
        
        sample['image'] = im
        
        return sample
    
    
class MaskToTensor:
    def __call__(self, sample):
        im = sample['image']
        mask = sample['mask']
        
        im_tensor = torch.from_numpy(im).float() / 255
        im_dim = len(im_tensor.size())
        if im_dim == 3:
            im_tensor = im_tensor.permute(2, 0, 1)
        if im_dim == 2:
            im_tensor = torch.unsqueeze(im_tensor, 0)
        mask_tensor = torch.from_numpy(np.array(mask, np.int64, copy=False))
        if mask_tensor.max == 255:
            mask_tensor = mask_tensor // 255
        
        sample['image'] = im_tensor
        sample['mask'] = mask_tensor
        
        return sample
    
class Mask3dto2d:
    """ Expects images of size NxMxC"""
    def __init__(self, channel_to_use):
        self.channel_to_use = channel_to_use
    
    def __call__(self, sample):
        im = sample['image']
        sample['image'] = im[..., self.channel_to_use]
       
        return sample
    
class MaskCompose:
    def __init__(self, transform_list):
        """Chain together transforms in transform.
         Expect transform on both image and mask in dict
         
         Args:
             transform_list (list): list of custom augmentations
         """
        self.transform_list = transform_list
    
    def __call__(self, sample):
        for transform in self.transform_list:
            sample  = transform(sample)
        return sample
    
class MaskNormalize:
    def __init__(self, mean, std):
        """Normalize image, leave mask unchanged"""
        self.mean = mean
        self.std = std
        
    def __call__(self, sample):
        im = sample['image']
        im_norm = TF.normalize(im, self.mean, self.std)
        
        sample['image'] = im_norm
        return sample
    
class MaskContrast:
    def __init__(self, contrast_factor):
        """Change image contrast, leave mask unchanged
        
        Args:
            contrast_factor: single value or tuple"""
        self.contrast_factor = contrast_factor
        self.rng = np.random.default_rng()

    def __call__(self, sample):
        im = sample['image']
        if isinstance(self.contrast_factor, tuple):
            contrast_factor = self.rng.uniform(self.contrast_factor[0], self.contrast_factor[1])
        else:
            contrast_factor = self.contrast_factor
        im_height, im_width = im.shape[:2]
        aprox_mean = np.mean([im[0,0], im[-1, 0], im[0, -1], im[-1, -1], 
                              im[im_height//2, im_width//2], im[-im_height//2, -im_width//2]])
        im_contrast = aprox_mean + contrast_factor * (im - aprox_mean)
        im_contrast = np.maximum(im_contrast, 0)
        im_contrast = np.minimum(im_contrast, 255)
        sample['image'] = im_contrast
        return sample
    
class MaskCenterImage:
    def __init__(self):
        pass
    def __call__(self, sample):
        im = sample['image']
        mean = np.mean(im[::10, ::10])
        bias = 125 - int(mean)
        bias = np.max([0, bias])
        sample['image'] += bias
        sample['image'] = np.where(sample['image'] > 255, 255, sample['image'])
        return sample