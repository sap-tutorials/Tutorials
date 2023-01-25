import logging

FORMAT = "%(asctime)s:%(name)s:%(levelname)s - %(message)s"
# Use filename="file.log" as a param to logging to log to a file
logging.basicConfig(format=FORMAT, level=logging.INFO)


"""# Inferencing"""

import os
import logging
import numpy as np
"""### Load Model"""

#loading best model
class Model:
  def __init__(
      self,
      source_path='/content'
  ):
    self.source_path = source_path
    self.model = None
    self.load_model()
  #
  def load_model(
      self,
      filename = 'model.h5'
    ):
    from tensorflow.keras.models import load_model
    self.model = load_model(f'{self.source_path}/{filename}')
    self.model.summary()
    return self.model
  def predict(self, seq):
    '''
    Parameters
    ---
    seq : numpy.ndarray
      <1, MAX_PAD_LEN>
    '''
    return self.model.predict(seq)


"""### Text Process - pre and post"""

from tensorflow.keras.preprocessing.sequence import pad_sequences

class TextProcess:
  def __init__(
      self,
      source_path='/content'
    ):
    self.source_path = source_path
    #
    self.max_pad_len = None # assinged in next line
    self.get_pad_len()
    #
    self.encoder = None 
    self.load_label_encoder()
    #
    self.load_tokenizer()
  #
  def get_pad_len(self, filename='max_pad_len.txt'):
    with open(f'{self.source_path}/{filename}') as file:
      self.max_pad_len = int(file.read())
  #
  def load_label_encoder(
      self,
      filename='label_encoded_classes.npy'
    ):
    from sklearn.preprocessing import LabelEncoder
    self.encoder = LabelEncoder()
    self.encoder.classes_ = np.load(f'{self.source_path}/{filename}')
    return self.encoder
  #
  def load_tokenizer(self,
      filename = 'tokens.json'
    ):
    # read as <str> from JSON file
    with open(f'{self.source_path}/{filename}', 'r') as tokenfile:
      tokens_info = tokenfile.read()
    #
    from tensorflow.keras.preprocessing.text import tokenizer_from_json
    self.tokenizer = tokenizer_from_json(tokens_info)
    return self.tokenizer
  #
  def pre_process(self, sentence):
    '''converts sentence to token
    
    Parameters
    ---
    sentence : str

    Returns
    ---
    x_seq : numpy.ndarray shape <1, self.max_pad_len>
    '''
    x_seq = self.tokenizer.texts_to_sequences(sentence)
    x_seq = pad_sequences(x_seq, maxlen=self.max_pad_len)
    return x_seq
  #
  def post_process(self, prediction):
    '''Convert back to orginial class name

    Parameters
    ---
    prediction : numpy.ndarray (dtype = float32, shape = (1, n_classes) #n_classes captured within the model
    
    Returns:
    ---
    dict 
      <predicted_class_name, probability>
      ONLY the maximum confident class
    '''
    prediction_flatten = prediction.flatten()
    probability_argmax = np.argmax(
        prediction_flatten
    )
    probability = prediction_flatten[probability_argmax]
    #
    predicted_class_name = self.encoder.inverse_transform(
        [probability_argmax]
    )[0]
    return {predicted_class_name: float(probability)}


