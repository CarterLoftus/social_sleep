import numpy as np
import torch
import matplotlib.pyplot as plt


def get_conf_matrix(conf_matrix, num_classes, pred_batch, mask_batch):
    """ Calculate confusion matrix and add to passed confusion matrix.
    
    Args:
        conf_matrix (np array): confusion matrix size 
            (num_classes + 1, num_classes + 1)
        num_classes (int): number of classes being segmented
        pred_batch (np array): NxM or BxNxM
        mask_batch (np array): NxM or BxNxM
    """
    
    N = num_classes + 1

    if len(pred_batch.shape) == 2:
        pred_batch = np.expand_dims(pred_batch, 0)
        mask_batch = np.expand_dims(mask_batch, 0)
    for pred, mask in zip(pred_batch, mask_batch):
        conf_matrix += np.bincount(
            N * pred.reshape(-1) + mask.reshape(-1), minlength=N ** 2
        ).reshape(N, N)
    return conf_matrix

def evaluate(num_classes, conf_matrix):
    """ Caclculate Accuracy and IOU scores.
    
    Args:
        num_classes (int): num classes
        conf_matrix (np array): confusion matrix of size 
            (num_classes + 1, num_classes + 1)
    """
    acc = np.full(num_classes, np.nan, dtype=np.float)
    iou = np.full(num_classes, np.nan, dtype=np.float)
    tp = conf_matrix.diagonal()[:-1].astype(np.float)
    pos_gt = np.sum(conf_matrix[:-1, :-1], axis=0).astype(np.float)
    class_weights = pos_gt / np.sum(pos_gt)
    pos_pred = np.sum(conf_matrix[:-1, :-1], axis=1).astype(np.float)
    acc_valid = pos_gt > 0
    acc[acc_valid] = tp[acc_valid] / pos_gt[acc_valid]
    iou_valid = (pos_gt + pos_pred) > 0
    union = pos_gt + pos_pred - tp
    iou[acc_valid] = tp[acc_valid] / union[acc_valid]
    macc = np.sum(acc[acc_valid]) / np.sum(acc_valid)
    miou = np.sum(iou[acc_valid]) / np.sum(iou_valid)
    fiou = np.sum(iou[acc_valid] * class_weights[acc_valid])
    pacc = np.sum(tp) / np.sum(pos_gt)
    
    return iou, acc

import time

def train(model, num_classes, train_dl, val_dl, loss_fn, optimizer, 
          acc_fn, epochs=1, lr_scheduler=None, save_best_val=False,
          model_file='model.tar', accumulation_steps=1, val_epoch=1, 
          show_images=False):
    start = time.time()
    
    train_loss, valid_loss = [], []
    
    # calc max usuable batchnum
    max_batchnum = ((len(train_dl) // accumulation_steps) 
                    * accumulation_steps)
    print("using {} batches ({} images)".format(
        max_batchnum, max_batchnum * train_dl.batch_size))
    
    model.cuda()
    
    top_val_loss = 100000000000

    for epoch in range(epochs):
        
        if epoch % 1 == 0:
            print('Epoch {}/{}'.format(epoch, epochs - 1))
        
        optimizer.zero_grad()
        
         #delete
        last_output = None
        last_batch = None
        
        for phase in ['train', 'val']:
            phase_start = time.time()
            if phase == 'train':
                model.train(True)
                dataloader = train_dl
            else:
                if epoch % val_epoch != 0:
                    continue
                model.train(False)
                dataloader = val_dl
                conf_matrix = np.zeros((num_classes+1, num_classes+1), dtype=np.int64)
                
            running_loss = 0.0
            running_acc = 0.0
            
            step = 0
            
            
            # iterate over data
            for batch_ind, batch in enumerate(dataloader):
                if batch_ind >= max_batchnum:
                    break
                im_batch = batch['image'].cuda()
                
            
                masks = batch['mask'][:, 46:-46, 46:-46].cuda()
                
                if phase == 'train':
                    outputs = model(im_batch)
                    loss = loss_fn(outputs, masks)
                    loss.backward()
                    if (batch_ind+1) % accumulation_steps == 0:
                        # Wait for several backward steps
                        # Now we can do an optimizer step
                        optimizer.step()
                        if lr_scheduler:
                            lr_scheduler.step()
                        model.zero_grad() 
                else:
                    with torch.no_grad():
                        outputs = model(im_batch)
                        loss = loss_fn(outputs, masks)
                        np_outputs = outputs.cpu().numpy()
                        np_preds = np.argmax(np_outputs, axis=1)
                        np_masks = masks.cpu().numpy()
                        conf_matrix = get_conf_matrix(conf_matrix, num_classes, 
                                                          np_preds, np_masks)
                        
                if show_images:
                    def denorm_image(im, mean, std):
                        """ Take image the was normalized and return to 0 to 255"""
                    #     im = np.copy(im)
                        im *= std
                        im += mean
                        im *= 255
                        im = np.maximum(im, 0)
                        im = np.minimum(im, 255)
                        im = im.astype(np.uint8)

                        return im
                                        
                    mean = np.array([163.25460427, 144.9049417, 124.14896274])
                    std = np.array([76.85116161, 65.67809484, 51.92746243])
                    np_im_batch = batch['image'].numpy()
                    for im in np_im_batch:
                        plt.figure()
                        im = np.squeeze(im)
                        if len(im.shape) == 3:
                            im = np.transpose(im, (1, 2, 0))
                        im = denorm_image(im, mean, std)
                        plt.imshow(im)
                        plt.title(phase)

                running_loss += loss.item()*dataloader.batch_size
                
                
                step += 1
                
            epoch_loss = running_loss / len(dataloader.dataset)
            if epoch % 1 == 0:
                
                phase_length = time.time() - phase_start
                fps =  (len(dataloader) * len(batch)) / phase_length
                
                print('{} Loss: {:.4f}  AllocMem (Mb): {:.3f} MaxMem(Mb) {:.3f}Time: {:.3f} fps: {:.3f}'.format(
                    phase, epoch_loss, torch.cuda.memory_allocated()/1024/1024, 
                    torch.cuda.max_memory_allocated()/1024/1024, phase_length, fps))
            if phase == 'val':
                valid_loss.append(epoch_loss)
                if epoch_loss < top_val_loss:
                    top_val_loss = epoch_loss
                    if save_best_val:
                        torch.save(model.state_dict(), model_file)
                        print('saved new model. val loss {}'.format(epoch_loss))
                    
                iou, acc = evaluate(num_classes, conf_matrix)
                print('IOU: {}, Acc: {}'.format(iou, acc))
            else:
                train_loss.append(epoch_loss) 

        time_elapsed = time.time() - start   

    return train_loss, valid_loss 
    
def acc_metric(predb, yb):
    return (predb.argmax(dim=1) == yb.cuda()).float().mean()