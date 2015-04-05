import os
import numpy as np
import scipy.io.wavfile
import scipy
import random

import scikits.talkbox.features

def write_mfcc(filename):
    print "Writing mfcc components for: ", filename
    sample_rate, X = scipy.io.wavfile.read(filename)
    ceps, mspec, spec = scikits.talkbox.features.mfcc(X)
    np.save(filename.replace(".wav", "mfcc"), ceps)

def write_fft(file_name):
    print "Writting FFT components for: ", file_name
    sample_rate, X = scipy.io.wavfile.read(file_name)
    fft_features = abs(scipy.fft(X)[:1000])
    np.save(file_name.replace(".wav", ".fft"), fft_features)

def transform_subdir(root):
     for (path, dirnames, filenames) in os.walk(root):
         for f in filenames:
             if f.endswith(".wav"):
                 write_fft(path + "/" + f)
                 write_mfcc(path + "/" + f)

transform_subdir("./music")

#def construct_tenfold(data_array):

class TestSet:
    def __init__(self, data_array):
        size = int(1/10 * len(data_array))
        self.testing = random.sample(size)
        self.training = random.sample(len(data_array) - size)
        
