"""
A client that talks to tensorflow_model_server loaded with flowers model.
"""
from __future__ import print_function

import sys
import threading

# This is a placeholder for a Google-internal import.

from grpc.beta import implementations
import numpy
import tensorflow as tf

from tensorflow_serving.apis import predict_pb2
from tensorflow_serving.apis import prediction_service_pb2

from hdbcli import dbapi

tf.app.flags.DEFINE_string ('tmshost', 'localhost' , 'PredictionService host')
tf.app.flags.DEFINE_integer('tmsport', 8500        , 'PredictionService port')

tf.app.flags.DEFINE_string ('hxehost', 'localhost' , 'HXE host')
tf.app.flags.DEFINE_integer('hxeport', 39015       , 'HXE port')
tf.app.flags.DEFINE_string ('hxeusr' , 'ML_USER'   , 'HXE user name')
tf.app.flags.DEFINE_string ('hxepwd' , '<< password >>'  , 'HXE password')

args = tf.app.flags.FLAGS


def main(_):
    channel = implementations.insecure_channel(args.tmshost, args.tmsport)

    request = predict_pb2.PredictRequest()
    request.model_spec.name = 'flowers'
    request.model_spec.signature_name = 'serving_default'

    stub = prediction_service_pb2.beta_create_PredictionService_stub(channel)

    connection = dbapi.connect(address=args.hxehost, port=args.hxeport, user=args.hxeusr, password=args.hxepwd)
    cursor_flowers = connection.cursor()
    cursor_flowers_image = connection.cursor()

    query_flowers  = 'SELECT IMAGE_ID, IMAGE_LABEL, IMAGE_FILE_PATH, IMAGE_FILE_NAME, IMAGE_RAW_DATA FROM TF_DATA.FLOWERS_DATA'
    query_flowers_image = 'SELECT IMAGE_RAW_DATA FROM TF_DATA.FLOWERS_DATA WHERE IMAGE_ID = :id'

    cursor_flowers.execute(query_flowers)

    results = cursor_flowers.fetchall()
    for row in results:
        image_id    = row['IMAGE_ID']
        image_label = row['IMAGE_LABEL']
        image_path  = row['IMAGE_FILE_PATH']
        image_file  = row['IMAGE_FILE_NAME']
        image_data  = bytes(row['IMAGE_RAW_DATA'])

        request.inputs['image_blob'].CopyFrom(tf.contrib.util.make_tensor_proto(image_data, shape=[1]))
        response = stub.Predict(request, 100)

        predicted_class_name        = response.outputs['class'].string_val[0]
        predicted_class_probability = response.outputs['probability'].float_val[0]
        print("Expected class: " + image_label.ljust(10) + " -> Predicted class: " + predicted_class_name.ljust(10) + " with probability: " + str(round(predicted_class_probability * 100, 2) ) + "% (image file: " + image_file + ")")

    cursor_flowers.close()
    connection.close()

if __name__ == '__main__':
    tf.app.run()
