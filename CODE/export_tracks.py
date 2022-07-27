def calc_distance(a, b):
    import numpy as np
    dx = a[1] - b[1]
    dy = a[1] - b[1]
    return np.sqrt(np.sum(dx ** 2 + dy ** 2))


def get_color(f):
    import numpy as np
    from matplotlib import cm
    c = np.array(cm.hsv(f)[:3]) * 255
    return (int(c[0]), int(c[1]), int(c[2]))


def export_tracks(vid_name, sample_range = range(1,1)):
    from IPython import get_ipython
    import pandas as pd, numpy as np, cv2, os, glob, matplotlib.pyplot as plt
    from matplotlib import cm
    from read_metadata_HIWI import read_metadata
    from tqdm import tqdm
    from scipy import interpolate
    from scipy.ndimage import gaussian_filter1d
    metadata = read_metadata(vid_name)
    video_out_file = metadata.folder_output + vid_name + '_tracks.mp4'
    os.path.join(metadata.folder_images, '*.jpg')
    frame_files = sorted(glob.glob(os.path.join(metadata.folder_images, '*.jpg')))
    tracks_file = metadata.folder_output + 'tracks_' + vid_name + '.npy'
    tracks = np.load(tracks_file, allow_pickle=True)
    im = cv2.imread(frame_files[0])
    fourcc = (cv2.VideoWriter_fourcc)(*'mp4v')
    video_writer = cv2.VideoWriter(video_out_file, fourcc, 23.976, (im.shape[1], im.shape[0]))
    frame_ind = 1

    steps = 24*4  # 90
    step_inc = 1 # 5
    circle_radius = 3 # 20
    interpolation_factor = 10

    with_tail = True
    colors = np.random.uniform(size=(len(tracks)))
    colors = [get_color(c) for c in colors]
    all_tracking = pd.DataFrame({'frame':[],  'id':[],  'x':[],  'y':[],  'class':[]})
    all_distance = [] # len(frame_files)


    if len(sample_range) == 0:
        sample_range = range(1, len(frame_files), 1)
    for frame_ind in tqdm(sample_range):
        im = cv2.imread(frame_files[frame_ind])
        for track_ind, track in enumerate(tracks):
            rel_frame = frame_ind - track['first_frame']
            if rel_frame >= 0 and track['last_frame'] >= frame_ind:
                center_pos = track['track'][rel_frame]
                #cv2.circle(im, (int(center_pos[0]), int(center_pos[1])), circle_radius, colors[track_ind], -1)
                #cv2.putText(im, str(track_ind), (int(center_pos[0]), int(center_pos[1])), 0, 1, colors[track_ind],3)
                to_append = [
                frame_ind, track_ind, int(center_pos[0]), int(center_pos[1]), track['class']]
                a_series = pd.Series(to_append, index=(all_tracking.columns))
                all_tracking = all_tracking.append(a_series, ignore_index=True)
                if with_tail:
                    if rel_frame > steps:
                        pos = track['track'][range(rel_frame - steps,rel_frame - step_inc, step_inc)]
                        interp_pos = np.zeros((len(range(step_inc, len(pos)*interpolation_factor-interpolation_factor, 1)),2))
                        interp = interpolate.interp1d(range(step_inc, len(pos)*interpolation_factor, step_inc*interpolation_factor), pos[:,0], kind = "linear")
                        interp_pos[:,0] = gaussian_filter1d(interp(range(step_inc, len(pos)*interpolation_factor-interpolation_factor, 1)),interpolation_factor)
                        interp = interpolate.interp1d(range(step_inc, len(pos)*interpolation_factor, step_inc*interpolation_factor), pos[:,1], kind = "linear")
                        interp_pos[:,1] = gaussian_filter1d(interp(range(step_inc, len(pos)*interpolation_factor-interpolation_factor, 1)),interpolation_factor)
                        size_vec = np.power(2,np.linspace(0.5,circle_radius,len(range(0,len(interp_pos),1))))
                        for inc in range(0,len(interp_pos),1):
                            pos_loc = interp_pos[inc]
                            distance = calc_distance(pos_loc, center_pos)
                            # if distance < circle_radius:
                            #     continue
                            cv2.circle(im, (int(pos_loc[0]), int(pos_loc[1])), int(np.round(size_vec[inc])), colors[track_ind], -1)
                                    
        video_writer.write(im)

    video_writer.release()
    all_tracking.to_csv((metadata.folder_output + 'tracks_' + vid_name + '.csv'), index=False)
    files_in_directory = os.listdir(metadata.folder_images)
    filtered_files = [file for file in files_in_directory if file.endswith('.jpg')]
    for file in filtered_files:
        path_to_file = os.path.join(metadata.folder_images, file)
        os.remove(path_to_file)
