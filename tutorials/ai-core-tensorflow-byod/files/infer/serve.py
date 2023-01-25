# -*- coding: utf-8 -*-
"""
Inference script that extends from the base infer interface
"""
import os
from os.path import exists
from joblib import load
import logging

from flask import Flask
from flask import request as call_request

from tf_template import Model, TextProcess

app = Flask(__name__)

text_process = None
model = None

@app.before_first_request
def init():
    """
    Load the model if it is available locally
    """
    import tensorflow as tf
    import logging
    logging.info(f"Num GPUs Available: {len(tf.config.list_physical_devices('GPU'))}")

    global text_process, model
    #
    # Load text pre and post processor
    text_process = TextProcess(os.environ['SERVE_FILES_PATH'])
    text_process.max_pad_len
    #
    # load model
    model = Model(
        os.environ['SERVE_FILES_PATH']
    )

    return None


@app.route("/v1/predict", methods=["POST"])
def predict():
    """
    Perform an inference on the model created in initialize

    Returns:
        String prediction of the label for the given test data
    """
    global model, text_process
    #
    input_data = dict(call_request.json)
    text = str(input_data['text'])
    #
    # Log first
    logging.info("Requested text: " +
        str(text)
    )
    #
    # Prediction
    prediction = model.predict(
        text_process.pre_process([text]) # Important to pass as list
    )
    logging.info(f"Prediction: {str(prediction)}")
    #
    output = text_process.post_process(prediction)
    #
    # Response
    return output


if __name__ == "__main__":
    init()
    app.run(host="0.0.0.0", debug=True, port=9001)

# curl --location --request POST 'http://localhost:9001/v1/predict' --header 'Content-Type: application/json' --data-raw '{"text": "A restaurant with great ambiance"}'
