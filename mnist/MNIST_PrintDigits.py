# -*- coding: utf-8 -*-
"""
Created on Wed Feb 15 22:19:55 2017

@author: cmoreno
"""

num = 30

import matplotlib.pyplot as plt
for i in range(num):
    plt.subplot(1,num,i+1)
    plt.imshow(train_img[i], cmap='Greys_r')
    plt.axis('off')
plt.show()
print('label: %s' % (train_lbl[0:num],))