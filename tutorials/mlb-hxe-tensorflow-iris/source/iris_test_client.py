"""
A client that talks to tensorflow_model_server serving the iris model.
"""
from __future__ import print_function

import sys, os
import threading

# This is a placeholder for a Google-internal import.

from grpc.beta import implementations
import numpy
import tensorflow as tf

from tensorflow_serving.apis import predict_pb2
from tensorflow_serving.apis import prediction_service_pb2_grpc

sys.path.append(os.path.expanduser("~") + '/models/samples/core/get_started')
import iris_data

from hdbcli import dbapi

tf.app.flags.DEFINE_string ('tmshost', 'localhost' , 'PredictionService host')
tf.app.flags.DEFINE_integer('tmsport', 8500        , 'PredictionService port')

tf.app.flags.DEFINE_string ('hxehost', 'localhost' , 'HXE host')
tf.app.flags.DEFINE_integer('hxeport', 39015       , 'HXE port')
tf.app.flags.DEFINE_string ('hxeusr' , 'ML_USER'   , 'HXE user name')
tf.app.flags.DEFINE_string ('hxepwd' , '<< password >>'  , 'HXE password')

args = tf.app.flags.FLAGS

def main(_):
    max_species_len = len(max(iris_data.SPECIES, key=len))

    channel = implementations.insecure_channel(args.tmshost, args.tmsport)

    request = predict_pb2.PredictRequest()
    request.model_spec.name = 'iris'
    request.model_spec.signature_name = 'serving_default'

    stub = prediction_service_pb2_grpc.PredictionServiceStub(channel._channel)

    connection = dbapi.connect(address=args.hxehost, port=args.hxeport, user=args.hxeusr, password=args.hxepwd)
    cursor = connection.cursor()

    query_iris  = 'SELECT ID, SEPALLENGTH, SEPALWIDTH, PETALLENGTH, PETALWIDTH, SPECIES FROM TF_DATA.IRIS_DATA'
    cursor.execute(query_iris)

    results = cursor.fetchall()
    for row in results:
        id          = row["ID"]
        PetalLength = row["PETALLENGTH"]
        PetalWidth  = row["PETALWIDTH"]
        SepalLength = row["SEPALLENGTH"]
        SepalWidth  = row["SEPALWIDTH"]
        label       = row["SPECIES"]

        request.inputs['PetalLength'].CopyFrom(tf.contrib.util.make_tensor_proto(PetalLength, shape=[1]))
        request.inputs['PetalWidth' ].CopyFrom(tf.contrib.util.make_tensor_proto(PetalWidth , shape=[1]))
        request.inputs['SepalLength'].CopyFrom(tf.contrib.util.make_tensor_proto(SepalLength, shape=[1]))
        request.inputs['SepalWidth' ].CopyFrom(tf.contrib.util.make_tensor_proto(SepalWidth , shape=[1]))
        response = stub.Predict(request, 100)

        # in case you are using SPS02 or prior, the int64 output is not supported
        if(len(response.outputs['predicted_class_id'].int64_val) > 0) :
          predicted_class_id = response.outputs['predicted_class_id'].int64_val[0]
        else:
          predicted_class_id = int(response.outputs['predicted_class_id'].float_val[0])
        predicted_class_name = iris_data.SPECIES[predicted_class_id]
        predicted_class_probability = response.outputs['probabilities'].float_val[int(predicted_class_id)]

        print("Expected class: " + iris_data.SPECIES[label].ljust(max_species_len) + " -> Predicted class: " + predicted_class_name.ljust(max_species_len) + " with probability: " + str(predicted_class_probability))
    cursor.close()
    connection.close()

if __name__ == '__main__':
    tf.app.run()
