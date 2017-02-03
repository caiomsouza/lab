# -*- coding: utf-8 -*-
"""
Created on Fri Feb  3 23:26:19 2017

@author: caiomsouza
"""

from keras.models import Sequential

model = Sequential()

from keras.layers import Dense, Activation

model.add(Dense(output_dim=64, input_dim=100))
model.add(Activation("relu"))
model.add(Dense(output_dim=10))
model.add(Activation("softmax"))