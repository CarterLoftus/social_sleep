import torch 
import torch.nn as nn

class SuperSimpleSemSegNet(nn.Module):
    def __init__(self, in_channel, out_channel):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(in_channel, out_channel, 
                                     kernel_size=3, padding=1, stride=1)
        self.ReLU = torch.nn.ReLU()
        self.softmax = torch.nn.LogSoftmax(dim=1)
        
    def forward(self, x):
        x = self.conv1(x)
        x = self.ReLU(x)
        x = self.softmax(x)
        
        return x
    
class ThreeLayerSemSegNetWideView(nn.Module):
    def __init__(self, in_channel, out_channel):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(in_channel, 6, kernel_size=3, padding=1, stride=1)
        self.conv1d100 = torch.nn.Conv2d(in_channel, 2, kernel_size=3, padding=101, 
                                         stride=1, dilation=101)
        self.conv2d1 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=2, stride=1, dilation=2)
        self.conv2d5 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=6, stride=1, dilation=6)
        self.conv3 = torch.nn.Conv2d(8, out_channel, kernel_size=3, padding=1, stride=1)
        self.ReLU1 = torch.nn.ReLU()
        self.ReLU2 = torch.nn.ReLU()
        self.softmax = torch.nn.LogSoftmax(dim=1)
        self.batchnorm1 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0) 
        self.batchnorm2 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0)

        
    def forward(self, x):
        x1 = self.conv1(x)
        x2 = self.conv1d100(x)
        x = torch.cat((x1, x2), dim=1)
        x = self.batchnorm1(x)
        x = self.ReLU1(x)
        x1 = self.conv2d1(x)
        x2 = self.conv2d5(x)
        x = torch.cat((x1, x2), dim=1)
        x = self.batchnorm2(x)
        x = self.ReLU2(x)
        x = self.conv3(x)
        x = self.softmax(x)
        
        return x
    
class ThreeLayerSemSegNetWideViewHighDim(nn.Module):
    """Each layer has more channels than the standard model"""
    def __init__(self, in_channel, out_channel):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(in_channel, 12, kernel_size=3, padding=1, stride=1)
        self.conv1d100 = torch.nn.Conv2d(in_channel, 4, kernel_size=3, padding=101, 
                                         stride=1, dilation=101)
        self.conv2d1 = torch.nn.Conv2d(16, 8, kernel_size=3, padding=2, stride=1, dilation=2)
        self.conv2d5 = torch.nn.Conv2d(16, 8, kernel_size=3, padding=6, stride=1, dilation=6)
        self.conv3 = torch.nn.Conv2d(16, out_channel, kernel_size=3, padding=1, stride=1)
        self.ReLU1 = torch.nn.ReLU()
        self.ReLU2 = torch.nn.ReLU()
        self.softmax = torch.nn.LogSoftmax(dim=1)
        self.batchnorm1 = torch.nn.BatchNorm2d(16, track_running_stats=False, momentum=1.0) 
        self.batchnorm2 = torch.nn.BatchNorm2d(16, track_running_stats=False, momentum=1.0)

        
    def forward(self, x):
        x1 = self.conv1(x)
        x2 = self.conv1d100(x)
        x = torch.cat((x1, x2), dim=1)
        x = self.batchnorm1(x)
        x = self.ReLU1(x)
        x1 = self.conv2d1(x)
        x2 = self.conv2d5(x)
        x = torch.cat((x1, x2), dim=1)
        x = self.batchnorm2(x)
        x = self.ReLU2(x)
        x = self.conv3(x)
        x = self.softmax(x)
        
        return x
    
class FourLayerSemSegNetWideView(nn.Module):
    def __init__(self, in_channel, out_channel):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(in_channel, 6, kernel_size=3, padding=1, stride=1)
        self.conv1d100 = torch.nn.Conv2d(in_channel, 2, kernel_size=3, padding=101, 
                                         stride=1, dilation=101)
        self.conv2d1 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=2, stride=1, dilation=2)
        self.conv2d5 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=6, stride=1, dilation=6)
        self.conv3d0 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=1, stride=1)
        self.conv3d3 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=4, stride=1, dilation=4)
        self.conv4 = torch.nn.Conv2d(8, out_channel, kernel_size=3, padding=1, stride=1)
        self.ReLU1 = torch.nn.ReLU()
        self.ReLU2 = torch.nn.ReLU()
        self.ReLU3 = torch.nn.ReLU()
        self.softmax = torch.nn.LogSoftmax(dim=1)
        self.batchnorm1 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0) 
        self.batchnorm2 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0)
        self.batchnorm3 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0)

        
    def forward(self, x):
        x1a = self.conv1(x)
        x1b = self.conv1d100(x)
        x1 = torch.cat((x1a, x1b), dim=1)
        x1 = self.batchnorm1(x1)
        x1 = self.ReLU1(x1)
        x2a = self.conv2d1(x1)
        x2b = self.conv2d5(x1)
        x2 = torch.cat((x2a, x2b), dim=1)
        x2 = self.batchnorm2(x2)
        x2 = self.ReLU2(x2)
        x3a = self.conv3d0(x2)
        x3b = self.conv3d3(x2)
        x3 = torch.cat((x3a, x3b), dim=1)
        x3 = self.batchnorm3(x3)
        x3 = self.ReLU3(x3)
        x4 = self.conv4(x3)
        xout = self.softmax(x4)
        
        return xout
    
class ThreeLayerSemSegNet(nn.Module):
    def __init__(self, in_channel, out_channel):
        super().__init__()
        self.conv1 = torch.nn.Conv2d(in_channel, 8, kernel_size=3, padding=1, stride=1)
        self.conv2d1 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=2, stride=1, dilation=2)
        self.conv2d5 = torch.nn.Conv2d(8, 4, kernel_size=3, padding=6, stride=1, dilation=6)
        self.conv3 = torch.nn.Conv2d(8, out_channel, kernel_size=3, padding=1, stride=1)
        self.ReLU1 = torch.nn.ReLU()
        self.ReLU2 = torch.nn.ReLU()
        self.softmax = torch.nn.LogSoftmax(dim=1)
        self.batchnorm1 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0) 
        self.batchnorm2 = torch.nn.BatchNorm2d(8, track_running_stats=False, momentum=1.0)

        
    def forward(self, x):
        x = self.conv1(x)
        x = self.batchnorm1(x)
        x = self.ReLU1(x)
        x1 = self.conv2d1(x)
        x2 = self.conv2d5(x)
        x = torch.cat((x1, x2), dim=1)
        x = self.batchnorm2(x)
        x = self.ReLU2(x)
        x = self.conv3(x)
        x = self.softmax(x)
        
        return x

class ThreeLayerSemSegNetHighDim(nn.Module):
    def __init__(self, in_channel, out_channel, conv1_outdim, conv2_outdim, dialation):
        super().__init__()
        self.name = 'three_layer_semseg_net_highdim'
        self.conv1 = torch.nn.Conv2d(in_channel, conv1_outdim, 
                                     kernel_size=3, padding=1, stride=1)
        self.conv2d1 = torch.nn.Conv2d(conv1_outdim, conv2_outdim//2, kernel_size=3, 
                                       padding=2, stride=1, dilation=2)
        self.conv2d5 = torch.nn.Conv2d(conv1_outdim, conv2_outdim//2, kernel_size=3, 
                                       padding=dialation, stride=1, dilation=dialation)
        self.conv3 = torch.nn.Conv2d(conv2_outdim, out_channel, kernel_size=3, padding=1, stride=1)
        self.ReLU1 = torch.nn.ReLU()
        self.ReLU2 = torch.nn.ReLU()
        self.softmax = torch.nn.LogSoftmax(dim=1)
        self.batchnorm1 = torch.nn.BatchNorm2d(conv1_outdim, track_running_stats=False, momentum=1.0) 
        self.batchnorm2 = torch.nn.BatchNorm2d(conv2_outdim, track_running_stats=False, momentum=1.0)

        
    def forward(self, x):
        x = self.conv1(x)
        x = self.batchnorm1(x)
        x = self.ReLU1(x)
        x1 = self.conv2d1(x)
        x2 = self.conv2d5(x)
        x = torch.cat((x1, x2), dim=1)
        x = self.batchnorm2(x)
        x = self.ReLU2(x)
        x = self.conv3(x)
        x = self.softmax(x)
        
        return x

class SimpleSemSegNet(nn.Module):
    def __init__(self, in_channel, out_channel):
        super().__init__()
        
        self.conv1 = self.contract_block(in_channel, 32, 7, 3)
#         self.conv2 = self.contract_block(32, 64, 3, 1)
#         self.conv3 = self.contract_block(64, 128, 3, 1)
        
#         self.upconv3 = self.expand_block(128, 64, 3, 1)
#         self.upconv2 = self.expand_block(64*2, 32, 3, 1)
        self.upconv1 = self.expand_block(32, out_channel, 3, 1)
        self.log_softmax = torch.nn.LogSoftmax(dim=1)
        
    def forward(self, x):
        conv1 = self.conv1(x)
#         conv2 = self.conv2(conv1)
#         conv3 = self.conv3(conv2)
        
#         upconv3 = self.upconv3(conv3)
#         upconv2 = self.upconv2(torch.cat([upconv3, conv2], 1))
#         upconv1 = self.upconv1(torch.cat([upconv2, conv1], 1))
        upconv1 = self.upconv1(conv1)
        
        output = self.log_softmax(upconv1)
        
        return output
  
    def contract_block(self, in_channels, out_channels, kernel_size, padding):
        contract = torch.nn.Sequential(
            torch.nn.Conv2d(in_channels, out_channels, kernel_size=kernel_size, 
                           stride=1, padding=padding),
            torch.nn.BatchNorm2d(out_channels),
            torch.nn.ReLU(),
            torch.nn.Conv2d(out_channels, out_channels, kernel_size=kernel_size,
                         stride=1, padding=padding),
            torch.nn.BatchNorm2d(out_channels),
            torch.nn.ReLU(),
            torch.nn.MaxPool2d(kernel_size=3, stride=2, padding=1)                                  
        )
        return contract

    def expand_block(self, in_channels, out_channels, kernel_size, padding):
        expand = torch.nn.Sequential(
            torch.nn.Conv2d(in_channels, out_channels, kernel_size=kernel_size, 
                           stride=1, padding=padding),
            torch.nn.BatchNorm2d(out_channels),
            torch.nn.ReLU(),
            torch.nn.Conv2d(out_channels, out_channels, kernel_size=kernel_size,
                         stride=1, padding=padding),
            torch.nn.BatchNorm2d(out_channels),
            torch.nn.ReLU(),
            torch.nn.ConvTranspose2d(out_channels, out_channels, kernel_size=3, 
                                     stride=2, padding=1, output_padding=1)                                  
        )
        return expand
    
    
class UNET(nn.Module):
    def __init__(self, in_channels, out_channels, should_pad=True):
        super().__init__()
        self.name = 'UNET'
        if should_pad:
            conv1_pad = 3
            gen_pad = 1
        else:
            conv1_pad = 0
            gen_pad = 0
        self.conv1 = self.contract_block(in_channels, 32, 7, conv1_pad)
        self.conv2 = self.contract_block(32, 64, 3, gen_pad)
        self.conv3 = self.contract_block(64, 128, 3, gen_pad)

        self.upconv3 = self.expand_block(128, 64, 3, gen_pad)
        self.upconv2 = self.expand_block(64*2, 32, 3, gen_pad)
        self.upconv1 = self.expand_block(32*2, out_channels, 3, gen_pad)
        
        self.softmax = torch.nn.LogSoftmax(dim=1)

    def __call__(self, x):

        # downsampling part
        conv1 = self.conv1(x)
        conv2 = self.conv2(conv1)
        conv3 = self.conv3(conv2)
        upconv3 = self.upconv3(conv3)

        cat1_trim = 6
        cat2_trim = 18
        upconv2 = self.upconv2(torch.cat([upconv3, conv2[:, :, cat1_trim:-cat1_trim, cat1_trim:-cat1_trim]], 1))
        upconv1 = self.upconv1(torch.cat([upconv2, conv1[:, :, cat2_trim:-cat2_trim, cat2_trim:-cat2_trim]], 1))
        xout = self.softmax(upconv1)

        return xout

    def contract_block(self, in_channels, out_channels, kernel_size, padding):

        contract = nn.Sequential(
            torch.nn.Conv2d(in_channels, out_channels, kernel_size=kernel_size, stride=1, padding=padding),
            torch.nn.BatchNorm2d(out_channels, track_running_stats=False),
            torch.nn.ReLU(),
            torch.nn.Conv2d(out_channels, out_channels, kernel_size=kernel_size, stride=1, padding=padding),
            torch.nn.BatchNorm2d(out_channels, track_running_stats=False),
            torch.nn.ReLU(),
            torch.nn.MaxPool2d(kernel_size=3, stride=2, padding=1)
                                 )

        return contract

    def expand_block(self, in_channels, out_channels, kernel_size, padding):

        expand = nn.Sequential(torch.nn.Conv2d(in_channels, out_channels, kernel_size, stride=1, padding=padding),
                            torch.nn.BatchNorm2d(out_channels, track_running_stats=False),
                            torch.nn.ReLU(),
                            torch.nn.Conv2d(out_channels, out_channels, kernel_size, stride=1, padding=padding),
                            torch.nn.BatchNorm2d(out_channels, track_running_stats=False),
                            torch.nn.ReLU(),
                            torch.nn.ConvTranspose2d(out_channels, out_channels, kernel_size=3, stride=2, padding=1, output_padding=1) 
                            )
        return expand