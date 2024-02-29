import os

import numpy as np
import tensorflow as tf

from sklearn.preprocessing import LabelEncoder
from tensorflow import keras

DEFAULT_SERVE_PATH = 'tf_files'
SERVE_PATH = os.environ.get('SERVE_FILES_PATH') or DEFAULT_SERVE_PATH
AVAILABLE_GPUS = tf.config.list_physical_devices('GPU')

class Model:
    """
    A class representing a machine learning model for prediction.

    Parameters:
    - source_path: The path to the directory containing the model file. Default is 'tf_files'.

    Attributes:
    - source_path: The path to the directory containing the model file.
    - model: The loaded TensorFlow/Keras model.
    """
    def __init__(self, source_path: str = SERVE_PATH) -> None:
        self.source_path = source_path
        self.model = None
        self.__load_model()

    def __load_model(self, filename: str = 'model.h5') -> None:
        model_filepath = os.path.join(self.source_path, filename)
        self.model = keras.models.load_model(model_filepath, compile=False)

    def predict(self, seq: np.ndarray) -> np.ndarray:
        """
        Generates predictions using the loaded model.

        Parameters:
        - seq: Input sequence for prediction -- shape <1, MAX_PAD_LEN>

        Returns:
        - Predicted values based on the input sequence.
        """
        return self.model.predict(seq)


class TextProcess:
    """
    A class for text processing including tokenization, padding, and post-processing predictions.

    Parameters:
    - source_path: The path to the directory containing the required files. Default is 'tf_files'.

    Attributes:
    - source_path: The path to the directory containing the required files.
    - max_pad_len: The maximum length for padding sequences.
    - encoder: The label encoder for mapping classes to integers.
    - tokenizer: The text tokenizer.
    """
    def __init__(self, source_path: str = SERVE_PATH) -> None:
        self.source_path = source_path

        self.max_pad_len = None
        self.__get_pad_len()

        self.encoder = None
        self.__load_label_encoder()

        self.tokenizer = None
        self.__load_tokenizer()

    def __get_pad_len(self, filename: str = 'max_pad_len.txt') -> None:
        pad_len_filepath = os.path.join(self.source_path, filename)
        with open(pad_len_filepath) as file:
            self.max_pad_len = int(file.read())

    def __load_label_encoder(self, filename: str = 'label_encoded_classes.npy') -> None:
        encoded_classes_path = os.path.join(self.source_path, filename)
        self.encoder = LabelEncoder()
        self.encoder.classes_ = np.load(encoded_classes_path)

    def __load_tokenizer(self, filename: str = 'tokens.json') -> None:
        tokenizer_path = os.path.join(self.source_path, filename)
        with open(tokenizer_path, 'r') as tokenfile:
            tokens_info = tokenfile.read()
        self.tokenizer = keras.preprocessing.text.tokenizer_from_json(tokens_info)

    def pre_process(self, sentence: str) -> np.ndarray:
        """
        Pre-processes a given sentence by tokenizing and padding it.

        Parameters:
        - sentence: input sentence for processing

        Returns:
        - the pre-processed sequence -- shape <1, MAX_PAD_LEN>
        """
        x_seq = self.tokenizer.texts_to_sequences([sentence])
        return keras.utils.pad_sequences(x_seq, maxlen=self.max_pad_len)

    def post_process(self, prediction: np.ndarray) -> dict:
        """
        Post-processes the model's prediction by extracting the predicted class and its probability.

        Parameters:
        - prediction: the model's raw prediction (dtype = float32, shape = (1, n_classes) #n_classes captured within the model

        Returns:
        - a dictionary containing the predicted class and its probability. Only the maximum confident class
        """
        prediction_flatten = prediction.flatten()
        probability_argmax = np.argmax(prediction_flatten)
        probability = prediction_flatten[probability_argmax]

        predicted_class_name = self.encoder.inverse_transform([probability_argmax])[0]
        return {predicted_class_name: float(probability)}
