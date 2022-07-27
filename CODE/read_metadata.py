def read_metadata(vid):

    class struct:
        pass

    metadata = struct()
    metadata.folder_main = "/home/baboonfield/Documents/CV/feeding/"
    metadata.folder_detectron = '/home/baboonfield/Documents/CV/detectron2/'
    metadata.baboon_weights = metadata.folder_main + "weights/For_rharel_maxiter-2000_lr-0.00025_detectPerIm-100_minsize-640_batchsize-2/model_final.pth"
    metadata.folder_output = metadata.folder_main + "output/" + vid + "/"
    metadata.videoname = vid
    metadata.folder_images = metadata.folder_main + "im/"
    metadata.folder_data = metadata.folder_main + "data/"
    metadata.folder_data_server = '/home/baboonfield/server/EAS_shared/baboon/working/video/feeding/videos' 
    metadata.folder_code = '/home/baboonfield/Documents/CV/general-detection/'
    metadata.folder_annotations =  metadata.folder_data +  'annotations/'

    # bionic_field local metadata.folder_main + "data/" 
    # bionic_field server /home/baboonfield/server/EAS_shared/baboon/working/video/feeding
    return(metadata)