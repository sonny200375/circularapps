## "CIRCULAR ASSET TRACKING AND RECOMMENDER MANAGEMENT SYSTEM"
## Circular Product/ Asset Material Classification through Ultrasonic Image Scanned
## and Deep Learning Model Image Classification
## Output Classification will feed to Circular Product/ Asset Tracking and Recommender Management System
## Further Data Validation Insight will be save in Inter Planetary File System (IPFS)
## Front End dApps will be avaliable in Web3, Android, and IOS
## Applicable Services : Circular Supply Chain Recommendation, Asset Tracking, Circular Recommendation, 
##                       Integration with Carbon Emission Equivalency, etc  

# Import Dependencies Libraries
import numpy as np
import pandas as pd
from sklearn.utils.multiclass import unique_labels
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import seaborn as sns
import itertools
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
import tensorflow as tf

from tensorflow.keras.models import Sequential
from tensorflow.python.keras.applications.resnet import ResNet50
# from tensorflow.python.keras.applications.resnet_v2 import 
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.optimizers import SGD,Adam
from tensorflow.keras.callbacks import ReduceLROnPlateau
from tensorflow.keras.layers import Flatten, Dense, BatchNormalization, Activation,Dropout
from keras.utils.np_utils import to_categorical

import random
import cv2

#Import the libraries
import zipfile
import os

import pandas as pd
import matplotlib.pyplot  as plt
# from PIL import Image
from pathlib import Path
# import imagesize
import numpy as np

import cv2

import os

from tensorflow.keras import layers
from tensorflow import keras
import tensorflow as tf
import random

import tensorflow.compat.v1 as tf
# tf.disable_v2_behavior()

from skimage import transform
from PIL import Image
from functools import partial

#importing some useful packages
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import cv2

import numpy as np
%matplotlib inline
import matplotlib.image as mpimg

def augment_brightness_camera_images(image):
    image1 = cv2.cvtColor(image,cv2.COLOR_RGB2HSV)
    random_bright = .25+np.random.uniform()
    #print(random_bright)
    image1[:,:,2] = image1[:,:,2]*random_bright
    image1 = cv2.cvtColor(image1,cv2.COLOR_HSV2RGB)
    return image1

def transform_image(img,ang_range,shear_range,trans_range,brightness=0):
    '''
    This function transforms images to generate new images.
    The function takes in following arguments,
    1- Image
    2- ang_range: Range of angles for rotation
    3- shear_range: Range of values to apply affine transform to
    4- trans_range: Range of values to apply translations over.

    A Random uniform distribution is used to generate different parameters for transformation

    '''
    # Rotation

    ang_rot = np.random.uniform(ang_range)-ang_range/2
    rows,cols,ch = img.shape    
    Rot_M = cv2.getRotationMatrix2D((cols/2,rows/2),ang_rot,1)

    # Translation
    tr_x = trans_range*np.random.uniform()-trans_range/2
    tr_y = trans_range*np.random.uniform()-trans_range/2
    Trans_M = np.float32([[1,0,tr_x],[0,1,tr_y]])

    # Shear
    pts1 = np.float32([[5,5],[20,5],[5,20]])

    pt1 = 5+shear_range*np.random.uniform()-shear_range/2
    pt2 = 20+shear_range*np.random.uniform()-shear_range/2

    # Brightness


    pts2 = np.float32([[pt1,5],[pt2,pt1],[5,pt2]])

    shear_M = cv2.getAffineTransform(pts1,pts2)

    img = cv2.warpAffine(img,Rot_M,(cols,rows))
    img = cv2.warpAffine(img,Trans_M,(cols,rows))
    img = cv2.warpAffine(img,shear_M,(cols,rows))

    if brightness == 1:
      img = augment_brightness_camera_images(img)

    return img

def test2():
    path = '../input/material/image/fabric/fabric_moderate_001_new.jpg'
    image = cv2.imread(path)
    gs1 = gridspec.GridSpec(10, 10)
    gs1.update(wspace=0.01, hspace=0.02) # set the spacing between axes.
    plt.figure(figsize=(12,12))
    for i in range(100):
        ax1 = plt.subplot(gs1[i])
        ax1.set_xticklabels([])
        ax1.set_yticklabels([])
        ax1.set_aspect('equal')
        img = transform_image(image,20,10,5,brightness=1)

        plt.subplot(10,10,i+1)
        plt.imshow(img)
        plt.axis('off')

    plt.show()
    return img

img = test2()

def visualize(original, augmented, augmented_title='Augmented image'):
    fig = plt.figure()
    plt.subplot(1,2,1)
    plt.title('Original image')
    plt.imshow(original)

    plt.subplot(1,2,2)
    plt.title(augmented_title)
#     plt.imshow(augmented)
    plt.imshow(augmented)
#     print(type(augmented))
    plt.show()
#     visualize(image, flipped)

def flip_image(im):
    flipped_1 = tf.image.flip_left_right(im)
    flipped_2 = tf.image.flip_up_down(im)
    return [flipped_1, flipped_2]

def rot_image(im):
    flip_up = transform.rotate(im, angle=180, mode='reflect')
    flip_left = transform.rotate(im, angle=90, mode='reflect')
    flip_right = transform.rotate(im, angle=270, mode='reflect')
    
    rot_40 = transform.rotate(im, angle=40, mode='reflect')
    rot_n40 = transform.rotate(im, angle=320, mode='reflect')
    
    return [flip_up, flip_left, flip_right, rot_40]

def scale_image(im):
#     scale_out = transform.rescale(im, scale=2.0, mode='constant')
    img_half = cv2.resize(im, (0, 0), fx=0.5, fy=0.5)
    img_scale_up = cv2.resize(im, (0, 0), fx=1.5, fy=1.5)
    return [img_half, img_scale_up]

def test():
    path = '../input/material/image/fabric/fabric_moderate_001_new.jpg'
    im = cv2.imread(path)
    print(type(im))
    print(im.shape)
    result = scale_image(im)
    print(result[0].shape)
#     plt.imshow(result[0])
    for i in result:
        visualize(im, i)

!mkdir image
categories = os.listdir('../input/material/image')
for cat in categories:
    os.mkdir('image/{}'.format(cat))

%%time
count = 0
for dirname, _, filenames in os.walk('../input/material/image'):
    for filename in filenames:
#         print(os.path.join(dirname, filename))
#         print(filename)
        cat = dirname.split('/')[4]
        save_dir = 'image/{}/{}'.format(cat, filename.split('.')[0])
#         print('save dir {}'.format(save_dir))
        im = cv2.imread(os.path.join(dirname, filename))
        if im is None:
            continue
        
        im_list = [im]
#         im_list.extend(rot_image(im))
        for i in range(3):
            new_im = transform_image(im,20,10,5,brightness=1)
            im_list.append(new_im)
#         im_list.extend(flip_image(im))
#         print(type(im_list[3]))
#         visualize(im, rot[0])
#         visualize(im, rot[1])
#         print(type(rot[0]))
#         print(rot[0])
        
#         im = Image.fromarray(rot[0])
        for i in range(len(im_list)):
            save_im = Image.fromarray((im_list[i] * 255).astype(np.uint8))
            out = "{}_{}.jpg".format(save_dir, i)
#             print(out)
            save_im.save(out)
#             print(save_dir)
#         count+=1
#         if count == 5:
#             break
#     break
# #         img = x
#         # NumPy.'img' = A single image.
#         flip_1 = np.fliplr(im)
#         # TensorFlow. 'x' = A placeholder for an image.
#         shape = [244, 244, 3]
#         x = tf.placeholder(dtype = tf.uint8, shape = shape)
#         flip_2 = tf.image.flip_up_down(x)
#         flip_3 = tf.image.flip_left_right(x)
#         print(type(flip_2))
#         image =flip_2.numpy()
#         print(type(image))
# #         image_2 = tf.image.convert_image_dtype(flip_2, tf.float32)
#         plt.imshow(image)

    
    
# plt.show()

def preprocessing_func(im):
#     INP_SIZE = (224, 224) 
    kernel = np.array([[0, -1, 0],
                   [-1, 5,-1],
                   [0, -1, 0]])
    
#     im = cv2.imread(filename)
#     im = cv2.resize(im, (INP_SIZE[0] , INP_SIZE[1]))
    im = cv2.filter2D(src=im, ddepth=-1, kernel=kernel)
    return im

image_generator = ImageDataGenerator(
    rescale = 1.0/255.0,
    horizontal_flip = True,
    preprocessing_function = preprocessing_func,
    zoom_range=0.1, 
    shear_range = 0.2,
    validation_split = 0.20
)

image_generator_test = ImageDataGenerator(rescale = 1.0/255.0)

# paramaters
batch_size= 32
epochs= 50

train_dir = './image'
# val_dir = '../input/plant-seedling-normalise-v4/data_val'
img_size = [224, 224]

train_ds = image_generator.flow_from_directory(
  train_dir,
  shuffle = True,
  target_size=(img_size[0], img_size[1]),
  class_mode = 'categorical',
  batch_size=batch_size,
    subset="training")

val_ds = image_generator.flow_from_directory(
  train_dir,
  shuffle = True,
  target_size=(img_size[0], img_size[1]),
  class_mode = 'categorical',
  batch_size=batch_size,
    subset="validation"
)

print(len(train_ds.class_indices))

# define model

#Initializing ResNet50
base_model_resnet = ResNet50(include_top = False, weights = 'imagenet', input_shape = (224,224,3), classes = len(train_ds.class_indices))
#Adding layers to the ResNet50
model_resnet=Sequential()
#Add the Dense layers along with activation and batch normalization
model_resnet.add(base_model_resnet)
model_resnet.add(Flatten())
#Add the Dense layers along with activation and batch normalization
model_resnet.add(Dense(512,activation='relu')) 
model_resnet.add(Dropout(0.3))
model_resnet.add(Dense(512,activation='relu')) 
model_resnet.add(Dropout(0.3))
model_resnet.add(Dense(512,activation='relu')) 
model_resnet.add(Dropout(0.3))
#     model_resnet.add(Dense(64,activation=('relu')))
#     model_resnet.add(Dropout(.3))
model_resnet.add(Dense(len(train_ds.class_indices),activation=('softmax')))

#Compiling ResNet50
learn_rate=.001
# # learn_rate=
sgd=SGD(lr=learn_rate)
# adam = Adam(learning_rate=0.0005, epsilon=0.1, beta_1=0.9, beta_2=0.999)

model_resnet.compile(optimizer ='sgd', loss = 'categorical_crossentropy', metrics = ['accuracy'] )

# train data size / batch size
train_steps = train_ds.samples//batch_size
val_steps = val_ds.samples//batch_size

history = model_resnet.fit_generator(
                train_ds,
                steps_per_epoch = train_steps,
                validation_data = val_ds, 
                validation_steps = val_steps,
                epochs = epochs)

f,ax=plt.subplots(2,1) #Creates 2 subplots under 1 column
#Training loss and validation loss
ax[0].plot(history.history['loss'],color='b',label='Training Loss')
ax[0].plot(history.history['val_loss'],color='r',label='Validation Loss')
#Training accuracy and validation accuracy
ax[1].plot(history.history['accuracy'],color='b',label='Training  Accuracy')
ax[1].plot(history.history['val_accuracy'],color='r',label='Validation Accuracy')



