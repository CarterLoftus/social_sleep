def read_metadata(vid):

    class struct:
        pass

    metadata = struct()
    metadata.folder_main = "C:/Users/meerkat/Documents/thermal_baboon/"
    metadata.folder_detectron = "C:/Users/meerkat/detectron2-windows/"
    metadata.baboon_weights = metadata.folder_main + "RESULTS/detectron_output/output/fuller_annos_three_class_maxiter-2000_lr-0.019_detectPerIm-200_minsize-0_batchsize-8/model_final.pth"
    metadata.folder_output = metadata.folder_main + "RESULTS/tracking_output/" + vid + "/"
    metadata.videoname = vid
    metadata.folder_images = metadata.folder_main + "RESULTS/im/"
    metadata.folder_data = metadata.folder_main + "RESULTS/data/"
    metadata.folder_data_server = 'X:/baboon/archive/processed/video/thermal/2019_summer/cliff_data/mp4_files/viewpoint_1/T1020/20190806/' 
    metadata.folder_code =  "C:/Users/meerkat/Documents/thermal_baboon/CODE/inference_to_tracking/"
    metadata.folder_annotations =  metadata.folder_data +  'annotations/'

    # bionic_field local metadata.folder_main + "data/" 
    # bionic_field server /home/baboonfield/server/EAS_shared/baboon/working/video/feeding
    return(metadata)