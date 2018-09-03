---
title: Deploy a TensorFlow Iris Classifier in SAP HANA, express edition
description: Provide details on the deployment of a classifier model based on the Iris dataset in SAP HANA, express edition.
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>how-to, tutorial>intermediate, products>sap-hana\, express-edition ]
---

## Prerequisites  
- Proficiency: Intermediate

### You will learn

During this tutorial, you will learn how to build and deploy a TensorFlow Deep Neural Network Classifier for the Iris dataset and then integrate it with SAP HANA, express edition.

As part of the [TensorFlow models repository](https://github.com/tensorflow/models), you can find the script for a simple Deep Neural Network using the Iris dataset for classification.

This model consumes ***traditional*** numerical data to produce a probability.

However, this script doesn't include a step to export the model using the `SavedModel` format to be used with the TensorFlow Serving.

Therefore, you will use a modified version of the script to train then export the model in the right format.

The SAP HANA External Machine Learning integration also requires the of the ***raw*** tensors in the model signature instead of the `tf.train.Example` approach.

## Details

### Time to Complete
**20 Min**.

[ACCORDION-BEGIN [Step 1: ](Build and Export a TensorFlow Model)]

Now, you can switch to the `tmsadm` user if not done yet:

```shell
sudo su -l tmsadm
```

Activate the Python Virtual Environment (named `tms`) created previously using the following commands:

```shell
source ~/tms/bin/activate
```

You can now clone the models repository locally:

```shell
cd ~
git clone https://github.com/tensorflow/models
```

The scripts that you will leverage to consume the Iris data uses Pandas, that you will need to install:

```shell
pip install pandas
```

Then, you can create a model export directory:

```shell
mkdir -p ~/export/iris
```

Now, you can create the following Python script `~/export/iris_export_savedmodel.py` using the content of the following [script](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-iris/source/iris_export_savedmodel.py):

> **Note:** The below script will work with SAP HANA 2.0, express edition SPS03. If you are using an earlier version, please use the following [script link](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-iris/source/iris_export_savedmodel_pre_sps03.py)

```python
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os, shutil, time, tempfile, errno

sys.path.append('/home/tmsadm/models/samples/core/get_started')
import iris_data
import tensorflow as tf

tf.app.flags.DEFINE_integer('steps'           , 10000           , 'number of training steps.')
tf.app.flags.DEFINE_integer('batch_size'      , 100             , 'batch size.')
tf.app.flags.DEFINE_string ('export_path'     , os.path.expanduser("~") + '/export/iris' , 'export path')
args = tf.app.flags.FLAGS

def main(unused_argv):
  tf.logging.set_verbosity(tf.logging.INFO)

  ####################################################################
  # Beginning of training section
  ####################################################################
  # Get the training data
  (train_x, train_y), (test_x, test_y) = iris_data.load_data()

  # Define the features used for training
  feature_columns = [tf.feature_column.numeric_column(key=key, shape=1, default_value=None, dtype=tf.float32) for key in train_x.keys()]

  # Build 3 layer DNN with 10, 20, 10 units respectively.
  classifier = tf.estimator.DNNClassifier(
    feature_columns=feature_columns,
    hidden_units=[10, 20, 10],
    n_classes=3,
  )
  # Train the model
  classifier.train(
    input_fn = lambda:iris_data.train_input_fn(train_x, train_y, args.batch_size),
    steps=args.steps
  )

  ####################################################################
  # End of training section
  ####################################################################

  ####################################################################
  # Beginning of export section
  ####################################################################
  # Define the input receiver spec
  feature_spec = {
      'PetalLength': tf.placeholder(dtype=tf.float32, shape=[None,1], name='PetalLength'),
      'PetalWidth' : tf.placeholder(dtype=tf.float32, shape=[None,1], name='PetalWidth'),
      'SepalLength': tf.placeholder(dtype=tf.float32, shape=[None,1], name='SepalLength'),
      'SepalWidth' : tf.placeholder(dtype=tf.float32, shape=[None,1], name='SepalWidth'),
  }
  # Define the input receiver for the raw tensors
  def serving_input_receiver_fn():
    return tf.estimator.export.build_raw_serving_input_receiver_fn(feature_spec)()

  # decreate a temp dir for the raw model export
  tmp_model_dir = tempfile.mkdtemp()

  # export the raw model
  raw_model_path = classifier.export_savedmodel(tmp_model_dir, serving_input_receiver_fn=serving_input_receiver_fn)

  with tf.Graph().as_default():
    with tf.Session() as session:
      # reload the model to add the default signature
      tf.saved_model.loader.load(session, [tf.saved_model.tag_constants.SERVING], raw_model_path)

      # build tensors info for the inputs & the outputs into the signature def
      signature_def = (
        tf.saved_model.signature_def_utils.build_signature_def(
          inputs = {
            'PetalLength': tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('PetalLength:0')),
            'PetalWidth' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('PetalWidth:0')),
            'SepalLength': tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('SepalLength:0')),
            'SepalWidth' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('SepalWidth:0')),
          },
          outputs = {
            'predicted_class_id' : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('dnn/head/predictions/ExpandDims:0')),
            'probabilities'      : tf.saved_model.utils.build_tensor_info(tf.get_default_graph().get_tensor_by_name('dnn/head/predictions/probabilities:0'))
          },
          method_name = tf.saved_model.signature_constants.PREDICT_METHOD_NAME
        )
      )

      # remove all previous final models
      try:
        shutil.rmtree(args.export_path)
      except OSError as e:
        if e.errno != errno.EEXIST and e.errno != errno.ENOENT:
          raise

      # save the model with proper format
      builder = tf.saved_model.builder.SavedModelBuilder(args.export_path + "/" + str(int(round(time.time()))))
      builder.add_meta_graph_and_variables (
        session,
        [tf.saved_model.tag_constants.SERVING],
        signature_def_map = {
            tf.saved_model.signature_constants.DEFAULT_SERVING_SIGNATURE_DEF_KEY : signature_def
        },
      )
      # save the model with a default signature
      builder.save(as_text=False)

  # remove the intermediate saved model
  try:
    shutil.rmtree(tmp_model_dir)
  except OSError as e:
    if e.errno != errno.EEXIST and e.errno != errno.ENOENT:
      raise
  ####################################################################
  # End of export section
  ####################################################################

if __name__ == '__main__':
    tf.app.run()
```

Once saved, you can run the following commands to build the model:

```shell
cd ~/export/
python iris_export_savedmodel.py
```

For a more accurate version of the model, you can increase the value of the `training_iteration` switch, but this will also increase the processing time and the resources used.

You can check that your model was properly saved using the following utility:

```shell
cd ~/export/iris
saved_model_cli show --dir ./* --tag_set serve --signature_def serving_default
```

It will display the `SavedModel SignatureDef` needed to configure the SAP HANA EML.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import a Test Dataset in SAP HANA)]

In order to test your model, you will need to import the test data set made available online.

The use of this script assumes that you have installed the SAP HANA Client and imported the HDBCLI python package into your virtual environment:

```bash
pip install /usr/sap/hdbclient/hdbcli-2.2.36.tar.gz
```

> **Note:** the `hdbcli` tar file may be with a different version than the one displayed above

Now, you can create the following file `~/export/iris_import_data.py` using the content of the following [script](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-iris/source/iris_import_data.py):

```python
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os

sys.path.append('/home/tmsadm/models/samples/core/get_started')
import iris_data

import tensorflow as tf
from hdbcli import dbapi

tf.app.flags.DEFINE_string ('hxehost', 'localhost' , 'HXE host')
tf.app.flags.DEFINE_integer('hxeport', 39015       , 'HXE port')
tf.app.flags.DEFINE_string ('hxeusr' , 'ML_USER'   , 'HXE user name')
tf.app.flags.DEFINE_string ('hxepwd' , '<< password >>'  , 'HXE password')

args = tf.app.flags.FLAGS

def main(_):
    connection = dbapi.connect(address=args.hxehost, port=args.hxeport, user=args.hxeusr, password=args.hxepwd)
    cursor = connection.cursor()

    # Create a TF_DATA schema
    try:
        cursor.execute('CREATE SCHEMA TF_DATA;')
    except (RuntimeError, TypeError, NameError, dbapi.Error):
        pass

    # Set the current schema to TF_DATA schema
    cursor.execute('SET SCHEMA TF_DATA;')

    # Drop the table before creating them
    try:
        cursor.execute('DROP TABLE TF_DATA.IRIS_DATA;')
    except (RuntimeError, TypeError, NameError, dbapi.Error):
        pass

    # Create the tables
    cursor.execute('CREATE TABLE TF_DATA.IRIS_DATA  (ID INTEGER, SEPALLENGTH FLOAT, SEPALWIDTH FLOAT, PETALLENGTH FLOAT, PETALWIDTH FLOAT, SPECIES INT);')

    query_iris  = 'INSERT INTO TF_DATA.IRIS_DATA  (ID, SEPALLENGTH, SEPALWIDTH, PETALLENGTH, PETALWIDTH, SPECIES) VALUES (?,?,?,?,?,?)'

    # Load the IRIS data in the table
    imported_row = 0
    (train_x, train_y), (test_x, test_y) = iris_data.load_data()
    for index, row in test_x.iterrows():
        cursor.execute(query_iris, (index, row["SepalLength"], row["SepalWidth"], row["PetalLength"], row["PetalWidth"], test_y[index]))
        imported_row +=1
    print("Import row(s) " + str(imported_row))

    cursor.close()
    connection.close()
if __name__ == '__main__':
    tf.app.run()
```

> **Note:** Make sure to update the `<< password >>` in the script

To upload the data, you can execute the following command:

```shell
cd ~/export
python iris_import_data.py
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Start TensorFlow ModelServer)]

Now that our model is built and saved in the right format, you can adjust the TensorFlow Serving `ModelServer` configuration and start (or restart) it.

Update the model configuration file `~/export/config.cnf` like:

```js
model_config_list: {
    config: {
        name: "iris",
        base_path: "/home/tmsadm/export/iris",
        model_platform: "tensorflow"
    }
}
```

> **Note 1:** If you already have models defined, make sure to separate each `config` section with a comma.

> **Note 2:** Make sure to update the base path in case you not using the proposed one

You can now start the TensorFlow Serving `ModelServer` using the following command:

> **Note:** As of the publication of this tutorial, there is no ***graceful*** shutdown command for the TensorFlow Serving `ModelServer`. Therefore you will need to kill the process manually.

```shell
tensorflow_serving/model_servers/tensorflow_model_server --model_config_file=./export/config.cnf
```

You can use the following command if you prefer to run it as a background process with all outputs redirected to a log file:

```shell
nohup  tensorflow_model_server --model_config_file=./export/config.cnf > ./tensorflow_model_server.log 2>&1  </dev/null &
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test the model locally)]

You can now test your model with a local client program.

Create the following file `~/export/iris_test_client.py` using the content of the following [script](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-iris/source/iris_test_client.py):

```python
"""
A client that talks to tensorflow_model_server serving the iris model.
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

    stub = prediction_service_pb2.beta_create_PredictionService_stub(channel)

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

```

> **Note:** Make sure to update the `<< password >>` in the script

Then, you can execute the following command:

```shell
cd ~/export
python iris_test_client.py
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Reload the SAP HANA EML Configuration)]

Now that the model is deployed and the TensorFlow Serving `ModelServer` is up and running, you will need to add the model configuration to your SAP HANA, express edition instance.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to register the Iris deployed models with the following SQL statements:

```SQL
INSERT INTO _SYS_AFL.EML_MODEL_CONFIGURATION VALUES('iris' , 'RemoteSource', 'TensorFlow');
```

Now that a new model configuration was added, you will need to reload the EML configuration as this one is only loaded at the SAP HANA, express edition startup time.

```SQL
CREATE SCHEMA EML_DATA;
SET SCHEMA EML_DATA;
-- Uncomment the following lines if you want to re-run the script
-- DROP TABLE UPDATE_CONFIGURATION_PARAMS;
-- DROP TABLE UPDATE_CONFIGURATION_RESULT;
-- DROP PROCEDURE UPDATE_CONFIGURATION;

CREATE TABLE UPDATE_CONFIGURATION_PARAMS ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
CREATE TABLE UPDATE_CONFIGURATION_RESULT ("Key" VARCHAR(100), "Value" INTEGER, "Text" VARCHAR(100));
CREATE PROCEDURE UPDATE_CONFIGURATION() AS
BEGIN
    DECLARE CURSOR CUR FOR
        SELECT VOLUME_ID FROM SYS.M_VOLUMES WHERE SERVICE_NAME = 'indexserver';
    FOR CUR_ROW AS CUR DO
        EXEC 'CALL _SYS_AFL.EML_CTL_PROC(''UpdateModelConfiguration'', UPDATE_CONFIGURATION_PARAMS, UPDATE_CONFIGURATION_RESULT)'
            || ' WITH OVERVIEW WITH HINT(ROUTE_TO(' || :CUR_ROW.VOLUME_ID || '))';
    END FOR;
END;
TRUNCATE TABLE UPDATE_CONFIGURATION_RESULT;
CALL UPDATE_CONFIGURATION();
SELECT * FROM UPDATE_CONFIGURATION_RESULT;
```

It should return the following result:

|    Key | Value |  Text |
|--------|-------|-------|
| Status |     0 |    OK |

Now, you can check the registered model:

```SQL
SET SCHEMA EML_DATA;
-- Uncomment the following lines if you want to re-run the script
-- DROP TABLE CHECK_PARAMS;
CREATE TABLE CHECK_PARAMS ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
INSERT INTO CHECK_PARAMS VALUES ('Model', 'iris');
CALL _SYS_AFL.EML_CHECKDESTINATION_PROC(CHECK_PARAMS, ?);
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test the Model from SAP HANA SQLScript)]

The following SQL script will allow you to test the deployed Iris model using the test dataset uploaded before.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL script:

```SQL
SET SCHEMA EML_DATA;
-- Uncomment the following lines if you want to re-run the script
-- DROP TYPE TT_IRIS_PARAMS;
-- DROP TYPE TT_IRIS_FEATURES_SEPALLENGTH;
-- DROP TYPE TT_IRIS_FEATURES_SEPALWIDTH;
-- DROP TYPE TT_IRIS_FEATURES_PETALLENGTH;
-- DROP TYPE TT_IRIS_FEATURES_PETALWIDTH;
-- DROP TYPE TT_IRIS_RESULTS;
--
-- DROP TABLE IRIS_PROC_PARAM_TABLE;
-- DROP TABLE IRIS_PARAMS;
-- DROP TABLE IRIS_RESULTS;
--
-- DROP VIEW IRIS_FEATURES_SEPALLENGTH;
-- DROP VIEW IRIS_FEATURES_SEPALWIDTH;
-- DROP VIEW IRIS_FEATURES_PETALLENGTH;
-- DROP VIEW IRIS_FEATURES_PETALWIDTH;
-- Define table types for iris
CREATE TYPE TT_IRIS_PARAMS                AS TABLE ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
CREATE TYPE TT_IRIS_FEATURES_SEPALLENGTH  AS TABLE (SEPALLENGTH   FLOAT);
CREATE TYPE TT_IRIS_FEATURES_SEPALWIDTH   AS TABLE (SEPALWIDTH    FLOAT);
CREATE TYPE TT_IRIS_FEATURES_PETALLENGTH  AS TABLE (PETALLENGTH   FLOAT);
CREATE TYPE TT_IRIS_FEATURES_PETALWIDTH   AS TABLE (PETALWIDTH    FLOAT);
CREATE TYPE TT_IRIS_RESULTS   AS TABLE (
    -- when SPS02 or prior, make PREDICTED_CLASS_ID type FLOAT instead of INTEGER
    PREDICTED_CLASS_ID INTEGER,
    PROBABILITIES0 FLOAT,  PROBABILITIES1 FLOAT,  PROBABILITIES2 FLOAT
);
-- Create description table for procedure creation
CREATE COLUMN TABLE IRIS_PROC_PARAM_TABLE (
    POSITION        INTEGER,
    SCHEMA_NAME     NVARCHAR(256),
    TYPE_NAME       NVARCHAR(256),
    PARAMETER_TYPE  VARCHAR(7)
);
-- Drop the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_DROP(CURRENT_SCHEMA, 'IRIS');
-- Populate the wrapper procedure parameter table
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (1, CURRENT_SCHEMA, 'TT_IRIS_PARAMS'               , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (2, CURRENT_SCHEMA, 'TT_IRIS_FEATURES_PETALLENGTH' , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (3, CURRENT_SCHEMA, 'TT_IRIS_FEATURES_PETALWIDTH'  , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (4, CURRENT_SCHEMA, 'TT_IRIS_FEATURES_SEPALLENGTH' , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (5, CURRENT_SCHEMA, 'TT_IRIS_FEATURES_SEPALWIDTH'  , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (6, CURRENT_SCHEMA, 'TT_IRIS_RESULTS'              , 'out');
-- Create the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_CREATE('EML', 'PREDICT', CURRENT_SCHEMA, 'IRIS', IRIS_PROC_PARAM_TABLE);
-- Create and populate the parameter table
CREATE TABLE IRIS_PARAMS   LIKE TT_IRIS_PARAMS;
INSERT INTO IRIS_PARAMS   VALUES ('Model', 'iris');
INSERT INTO IRIS_PARAMS   VALUES ('RemoteSource', 'TensorFlow');
INSERT INTO IRIS_PARAMS   VALUES ('Deadline', '10000');

CREATE VIEW IRIS_FEATURES_SEPALLENGTH  AS SELECT SEPALLENGTH  FROM TF_DATA.IRIS_DATA ORDER BY ID;
CREATE VIEW IRIS_FEATURES_SEPALWIDTH   AS SELECT SEPALWIDTH   FROM TF_DATA.IRIS_DATA ORDER BY ID;
CREATE VIEW IRIS_FEATURES_PETALLENGTH  AS SELECT PETALLENGTH  FROM TF_DATA.IRIS_DATA ORDER BY ID;
CREATE VIEW IRIS_FEATURES_PETALWIDTH   AS SELECT PETALWIDTH   FROM TF_DATA.IRIS_DATA ORDER BY ID;

CREATE TABLE IRIS_RESULTS LIKE TT_IRIS_RESULTS;

-- Call the TensorFlow model
CALL IRIS (IRIS_PARAMS, IRIS_FEATURES_PETALLENGTH, IRIS_FEATURES_PETALWIDTH, IRIS_FEATURES_SEPALLENGTH, IRIS_FEATURES_SEPALWIDTH, IRIS_RESULTS) WITH OVERVIEW;

SELECT
  D.ID, D.SEPALLENGTH, D.SEPALWIDTH, D.PETALLENGTH, D.PETALWIDTH, D.SPECIES , R.*
FROM
  (SELECT *, ROW_NUMBER() OVER() AS RN FROM IRIS_RESULTS) R
JOIN
  (SELECT *, ROW_NUMBER() OVER( ORDER BY ID ) AS RN FROM TF_DATA.IRIS_DATA) D ON R.RN = D.RN;
```
The output result includes both the from the test set and the result. You can compare the `SPECIES` and `CLASS_IDS` columns which should be identical.

> ### **Note:** You may have noticed that the parameter table for the input data follows exactly the `SavedModel SignatureDef` despite the fact that the initial data file used for training used a different order.
> This is because the TensorFlow Serving `ModelServer` build the signature using the alphabetical order from the tensor names.

Provide an answer to the question below then click on **Validate**.

[DONE]
[ACCORDION-END]
