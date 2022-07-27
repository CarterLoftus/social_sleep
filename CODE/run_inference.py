import os
from decord import VideoReader, cpu, gpu
import cv2
import subprocess as sp


def extract_frames(video_path, frames_dir, overwrite=False, start=-1, end=-1, every=1):
    """
    Extract frames from a video using decord's VideoReader
    :param video_path: path of the video
    :param frames_dir: the directory to save the frames
    :param overwrite: to overwrite frames that already exist?
    :param start: start frame
    :param end: end frame
    :param every: frame spacing
    :return: count of images saved
    """

    #video_path = os.path.normpath(video_path)  # make the paths OS (Windows) compatible
    #frames_dir = os.path.normpath(frames_dir)  # make the paths OS (Windows) compatible

    video_dir, video_filename = os.path.split(video_path)  # get the video path and filename from the path

    assert os.path.exists(video_path)  # assert the video file exists

    # load the VideoReader
    vr = VideoReader(video_path, ctx=cpu(0))  # can set to cpu or gpu .. ctx=gpu(0)
                     
    if start < 0:  # if start isn't specified lets assume 0
        start = 0
    if end < 0:  # if end isn't specified assume the end of the video
        end = len(vr)

    frames_list = list(range(start, end, every))
    saved_count = 0

    if every > 50 and len(frames_list) < 1000:  # this is faster for every > 25 frames and can fit in memory
        frames = vr.get_batch(frames_list).asnumpy()

        for index, frame in zip(frames_list, frames):  # lets loop through the frames until the end
            save_path = os.path.join(frames_dir, "{:06d}.jpg".format(index))  # create the save path
            if not os.path.exists(save_path) or overwrite:  # if it doesn't exist or we want to overwrite anyways
                cv2.imwrite(save_path, cv2.cvtColor(frame, cv2.COLOR_RGB2BGR))  # save the extracted image
                saved_count += 1  # increment our counter by one

    else:  # this is faster for every <25 and consumes small memory
        for index in range(start, end):  # lets loop through the frames until the end
            frame = vr[index]  # read an image from the capture
            
            if index % every == 0:  # if this is a frame we want to write out based on the 'every' argument
                save_path = os.path.join(frames_dir, "{:06d}.jpg".format(index))  # create the save path
                if not os.path.exists(save_path) or overwrite:  # if it doesn't exist or we want to overwrite anyways
                    cv2.imwrite(save_path, cv2.cvtColor(frame.asnumpy(), cv2.COLOR_RGB2BGR))  # save the extracted image
                    saved_count += 1  # increment our counter by one

    return saved_count  # and return the count of the images we saved


def video_to_frames(video_path, frames_dir, overwrite=False, every=1):
    """
    Extracts the frames from a video
    :param video_path: path to the video
    :param frames_dir: directory to save the frames
    :param overwrite: overwrite frames if they exist?
    :param every: extract every this many frames
    :return: path to the directory where the frames were saved, or None if fails
    """
    video_dir, video_filename = os.path.split(video_path)
    video_filename = video_filename
    os.makedirs((os.path.join(frames_dir)), exist_ok=True)
    print('Extracting frames from {}'.format(video_filename))
    extract_frames(video_path, frames_dir, every=every)
    return os.path.join(frames_dir)

def get_gpu_memory():
  _output_to_list = lambda x: x.decode('ascii').split('\n')[:-1]

  ACCEPTABLE_AVAILABLE_MEMORY = 1024
  COMMAND = "nvidia-smi --query-gpu=memory.free --format=csv"
  memory_free_info = _output_to_list(sp.check_output(COMMAND.split()))[1:]
  memory_free_values = [int(x.split()[0]) for i, x in enumerate(memory_free_info)]
  print(memory_free_values)
  return memory_free_values

def run_inference(vid_name, every=1):
    from IPython import get_ipython
    import torch, torchvision, detectron2
    from detectron2.utils.logger import setup_logger
    import subprocess as sp, numpy as np, cv2, random, glob, torch, time, pickle, matplotlib.pyplot as plt
    from detectron2 import model_zoo
    from detectron2.engine import DefaultPredictor
    from detectron2.config import get_cfg
    from detectron2.utils.visualizer import Visualizer
    from detectron2.data import MetadataCatalog, DatasetCatalog
    from detectron2.modeling import build_model
    from detectron2.checkpoint import DetectionCheckpointer
    from detectron2.data import detection_utils as utils
    from detectron2.data import samplers
    from torch.utils.data import Dataset, DataLoader
    from read_metadata import read_metadata
    from tqdm import tqdm
    import subprocess

    torch.cuda.empty_cache()

    class PredictDataset(Dataset):

        def __init__(self, glob_string):
            self.image_files = sorted(glob.glob(glob_string))

        def __len__(self):
            return len(self.image_files) // 2

        def __getitem__(self, idx):
            image_raw = cv2.imread(self.image_files[(idx * 2)])
            height, width = image_raw.shape[:2]
            image = torch.as_tensor(image_raw.astype('float32').transpose(2, 0, 1)).contiguous()
            image_dict0 = {'image':image,  'height':height,  'width':width,  'file_name':self.image_files[idx * 2]}
            image_raw = cv2.imread(self.image_files[(idx * 2 + 1)])
            height, width = image_raw.shape[:2]
            image = torch.as_tensor(image_raw.astype('float32').transpose(2, 0, 1)).contiguous()
            image_dict1 = {'image':image,  'height':height,  'width':width,  'file_name':self.image_files[idx * 2 + 1]}
            return [image_dict0, image_dict1]

    metadata = read_metadata(vid_name)
    os.chdir(metadata.folder_main + 'data/')
    original_name = metadata.videoname + '.mp4'
    converted_name = metadata.videoname + '_n.mp4'
    conv_vid = 'ffmpeg -i ' + original_name + ' -vcodec copy -an ' + converted_name
    os.system(conv_vid)
    os.chdir(metadata.folder_code)
    os.rename(metadata.folder_main + 'data/' + converted_name, metadata.folder_main + 'tmp/' + converted_name)
    videopath = metadata.folder_main + 'tmp/' + converted_name
    if not os.path.exists(metadata.folder_output):
        os.makedirs(metadata.folder_output)
        
    cfg = get_cfg()
    cfg.merge_from_file(metadata.folder_detectron + 'configs/COCO-Detection/faster_rcnn_R_50_FPN_3x.yaml')
    cfg.MODEL.WEIGHTS = metadata.baboon_weights
    cfg.MODEL.ROI_HEADS.SCORE_THRESH_TEST = 0.2
    cfg.MODEL.ROI_HEADS.BATCH_SIZE_PER_IMAGE = 512
    cfg.MODEL.ROI_HEADS.NUM_CLASSES = 1
    cfg.TEST.DETECTIONS_PER_IMAGE = 50
    cfg.SOLVER.BASE_LR = 0.005
    cfg.SOLVER.MAX_ITER = 4000
    cfg.MODEL.ROI_HEADS.SCORE_THRESH_TEST = 0.8
    cfg.MODEL.ROI_HEADS.NMS_THRESH_TEST = 0.3


    #video_to_frames(video_path=videopath, frames_dir=(metadata.folder_images), overwrite=True, every=every)
    
    #os.chdir(metadata.folder_images)
    #subprocess.call(['ffmpeg', '-i', videopath, '-r', '24/1', '%06d.jpg'])
    #subprocess.call(['ffmpeg', '-i', videopath, '%06d.jpg'])
    #os.chdir(metadata.folder_code)
    os.remove(videopath)
    dataset = PredictDataset(os.path.join(metadata.folder_images, '*.jpg'))
    model = build_model(cfg)
    _ = model.eval()
    checkpointer = DetectionCheckpointer(model)
    _ = checkpointer.load(cfg.MODEL.WEIGHTS)
    torch.cuda.empty_cache()
    data_loader = DataLoader(dataset, batch_size=1, shuffle=True, num_workers=4)
    cuda0 = torch.device('cuda:0')
    max_batches = 15000
    print('started detections extraction')
    t = time.time()
    with torch.no_grad():
        for batch_num, image_batch in tqdm(enumerate(data_loader)):
            if batch_num >= max_batches:
                break
            for i in range(len(image_batch)):
                image_batch[i]['image'] = np.squeeze(image_batch[i]['image'])
                image_batch[i]['image'] = image_batch[i]['image'].to(cuda0)
                image_batch[i]['width'] = image_batch[i]['width'].to(cuda0).item()
                image_batch[i]['height'] = image_batch[i]['height'].to(cuda0).item()

            predictions = model(image_batch)
            for preds, im_dict in zip(predictions, image_batch):
                name = os.path.splitext(os.path.basename(im_dict['file_name'][0]))[0]
                file = os.path.join(metadata.folder_output, '{}-predictions.pkl'.format(name))
                preds_instance = preds['instances'].to('cpu')
                with open(file, 'wb') as (out):
                    pickle.dump(preds_instance, out)
                    out.close()

    print('finished detections extraction ' + str(time.time() - t))
    files = sorted(glob.glob(os.path.join(metadata.folder_output, '*-predictions.pkl')))
    all_detections = []
    raw_instances = []
    for file in files[:]:
        with open(file, 'rb') as (readfile):
            detections = pickle.load(readfile)
        detection_dict = detections.get_fields()
        detection_dict['pred_boxes'] = detection_dict['pred_boxes'].tensor.numpy()
        detection_dict['scores'] = detection_dict['scores'].numpy()
        detection_dict['pred_classes'] = detection_dict['pred_classes'].numpy()
        detection_dict['image_name'] = os.path.basename(file).split('-')[0]
        all_detections.append(detection_dict)
        raw_instances.append(detections)

    np_detections_file = metadata.folder_output + 'detections_' + vid_name + '.npy'
    np.save(np_detections_file, all_detections)
    files = [
     np_detections_file]
    fig = plt.figure(figsize=(24, 28))
    for file in files[0:1]:
        detections = np.load(file, allow_pickle=True)
        for image_ind in random.sample(range(0, len(detections)), 5):
            img = plt.imread(metadata.folder_images + detections[image_ind]['image_name'] + '.jpg')
            plt.imshow(img)
            ax = plt.gca()
            for item in range(0, len(detections[image_ind]['pred_boxes'])):
                x1 = detections[image_ind]['pred_boxes'][item][0]
                x2 = detections[image_ind]['pred_boxes'][item][2]
                y1 = detections[image_ind]['pred_boxes'][item][1]
                y2 = detections[image_ind]['pred_boxes'][item][3]
                scoretext = str('{0:.2g}'.format(detections[image_ind]['scores'][item]))
                wid = x2 - x1
                hei = y2 - y1
                rect = plt.Rectangle((x1, y1), wid, hei, linewidth=1, edgecolor='c', facecolor='none')
                ax.add_patch(rect)
                ax.annotate(scoretext, (x1, y1), size=20)
                plt.scatter(x=[x1, x2], y=[y1, y2], c='r', s=10)

            plt.savefig((metadata.folder_main + 'annotated/' + vid_name + '_' + detections[image_ind]['image_name'] + '.jpg'), bbox_inches='tight')
            plt.clf()

    files_in_directory = os.listdir(metadata.folder_output)
    filtered_files = [file for file in files_in_directory if file.endswith('.pkl')]
    for file in filtered_files:
        path_to_file = os.path.join(metadata.folder_output, file)
        os.remove(path_to_file)
# okay decompiling roi_inference.cpython-36.pyc