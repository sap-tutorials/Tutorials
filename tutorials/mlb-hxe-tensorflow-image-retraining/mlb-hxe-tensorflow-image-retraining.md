---
title: Deploy a TensorFlow ImageNet Classifier in SAP HANA, express edition
description: Provide details on the deployment of a classifier model based on an existing ImageNet model in SAP HANA, express edition.
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>how-to, tutorial>intermediate, products>sap-hana\, express-edition ]
---

## Prerequisites  
- Proficiency: Intermediate

### You will learn

During this tutorial, you will learn how to customize a pre-trained `ImageNet` TensorFlow model, deploy it and then integrate it with SAP HANA, express edition.

As part of the [TensorFlow Hub](https://www.tensorflow.org/hub/), a library to foster the publication, discovery, and consumption of reusable parts of machine learning models, you can now easily import and customize a pre-trained model.

You will reuse a pre-trained image recognition model from the [How to Retrain an Image Classifier for New Categories](https://www.tensorflow.org/tutorials/image_retraining) tutorial from the TensorFlow website.

However, this script currently includes a step to export the model using a 3 dimensions shape as the input which is not supported by the SAP HANA External Machine Learning integration.

Therefore, you will use a script that will save the retrained model a the raw image blob as input.

The retrain will be done using the provided flowers data set but you are free to try out with your own labeled photo library.

## Details

### Time to Complete
**20 Min**.

[ACCORDION-BEGIN [Step 1: ](Prepare the retraining data)]

First, you have to activate the Python Virtual Environment (named `tms`) created previously using the following commands:

```shell
source ~/tms/bin/activate
```

Then you can create a model export directory and the flowers pictures extract directory:

```shell
mkdir -p ~/export/flowers
mkdir -p ~/models/tutorials/image_retraining/data
```

Now, you can download the flowers pictures library:

```shell
cd ~/models/tutorials/image_retraining/data
curl -LO http://download.tensorflow.org/example_images/flower_photos.tgz
tar xzf flower_photos.tgz
```

Before starting the retraining, you need to save some of the input files for testing the model later:

```shell
cd ~/models/tutorials/image_retraining
mkdir -p ./test/flower_photos/daisy
mkdir -p ./test/flower_photos/dandelion
mkdir -p ./test/flower_photos/roses
mkdir -p ./test/flower_photos/sunflowers
mkdir -p ./test/flower_photos/tulips

find ./data/flower_photos/daisy \
  | tail -n 5 \
  | xargs -I{} mv {} ./test/flower_photos/daisy
find ./data/flower_photos/dandelion \
  | tail -n 5 \
  | xargs -I{} mv {} ./test/flower_photos/dandelion
find ./data/flower_photos/roses \
  | tail -n 5 \
  | xargs -I{} mv {} ./test/flower_photos/roses
find ./data/flower_photos/sunflowers \
  | tail -n 5 \
  | xargs -I{} mv {} ./test/flower_photos/sunflowers
find ./data/flower_photos/tulips \
  | tail -n 5 \
  | xargs -I{} mv {} ./test/flower_photos/tulips
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Retrain the pre-trained model)]

The scripts that you will leverage requires the TensorFlow Hub Python package, that you will need to install:

```shell
pip install tensorflow_hub
```

Next, you can download the retrain script:

```shell
cd ~/models/tutorials/image_retraining/
curl -LO https://github.com/tensorflow/hub/raw/r0.1/examples/image_retraining/retrain.py
```

And finally, you can start the retrain process:

```shell
cd ~/models/tutorials/image_retraining/
rm -r ./flowers-tmp
python retrain.py \
  --image_dir ./data/flower_photos \
  --output_labels ./labels.txt \
  --output_graph ./output_graph.pb \
  --saved_model_dir ./flowers-tmp \
  --how_many_training_steps 10000
```

The following input argument are provided:

- `--image_dir`: the path to the photo library where each sub directory represent a new category
- `--output_labels`: a file where all the photo category will be saved
- `--output_graph`:  the output TensorFlow graph to be reused to adjust the input tensor
- `--saved_model_dir`:  the model save directory
- `--how_many_training_steps`:  the number training iterations

Once the model retrain is completed, you should get the following log message:

```
INFO:tensorflow:SavedModel written to: ./flowers-tmp/saved_model.pbtxt
```

You can check that your model was saved using the following utility:

```shell
cd ~/models/tutorials/image_retraining/
saved_model_cli show --dir ./flowers-tmp --tag_set serve --signature_def serving_default
```

It will display the `SavedModel SignatureDef` needed to configure the SAP HANA EML.

```
The given SavedModel SignatureDef contains the following input(s):
  inputs['image'] tensor_info:
      dtype: DT_FLOAT
      shape: (-1, 299, 299, 3)
      name: Placeholder:0
The given SavedModel SignatureDef contains the following output(s):
  outputs['prediction'] tensor_info:
      dtype: DT_FLOAT
      shape: (-1, 5)
      name: final_result:0
Method name is: tensorflow/serving/predict
```

As you can notice, the image input has a shape of rank 3 (`(-1, 299, 299, 3)`) which is not supported with the SAP HANA External Machine Learning integration.
This shape represent the image as 299 pixels by 299 pixels by the 3 RGB colors.

Therefore in the next step, you replace the input with a simple string containing the raw blob image that will be transformed by the model.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Adjust the model signature)]

You can create the following Python script `/home/tmsadm/export/flowers_export_savedmodel.py` using the content of the following [script](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-image-retraining/source/flowers_export_savedmodel.py):

```python
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import numpy as np
import os, shutil, time, urllib

import tensorflow as tf

tf.app.flags.DEFINE_string ('export_path' , os.path.expanduser("~") + '/export/flowers'                                , 'export path')
tf.app.flags.DEFINE_string ('labels_path' , os.path.expanduser("~") + '/models/tutorials/image_retraining/labels.txt'  , 'labels path')
tf.app.flags.DEFINE_string ('graph_path'  , os.path.expanduser("~") + '/models/tutorials/image_retraining/output_graph.pb'  , 'graph path')

args = tf.app.flags.FLAGS

def build_raw_image_node (raw_image_data):

    input_height   = 299
    input_width    = 299
    input_mean     = 0
    input_std      = 255
    input_channels = 3

    raw_image_data_squeezed = tf.squeeze(raw_image_data)
    image_reader = tf.image.decode_jpeg(raw_image_data_squeezed, channels=input_channels, name="jpeg_reader")

    float_caster = tf.cast(image_reader, tf.float32)
    dims_expander = tf.expand_dims(float_caster, 0)

    resized = tf.image.resize_bilinear(dims_expander, [input_height, input_width])
    normalized = tf.divide(tf.subtract(resized, [input_mean]), [input_std])
    return normalized

def main(unused_argv):
    session = tf.InteractiveSession()

    # create a new tf.string input placeholder that will be processed to produce the proper format
    raw_image_data = tf.placeholder(tf.string, shape = [None])
    new_input_placeholder = build_raw_image_node(raw_image_data)

    # reload the graph definition, and update the Placeholder input with the new definition
    with tf.gfile.FastGFile(args.graph_path, 'rb') as f:
        graph_def = tf.GraphDef()
        graph_def.ParseFromString(f.read())
        _, final_result = tf.import_graph_def(
            graph_def,
            name='',
            return_elements=['Placeholder:0', 'final_result:0'],
            input_map={'Placeholder:0':new_input_placeholder}
        )

    # extract the list of labels
    label_lines = [line.rstrip() for line in tf.gfile.GFile(args.labels_path)]
    # get a tensor with the top result
    values, indices = tf.nn.top_k(final_result, 1)

    # build the table lookup tensor with labels from the indices
    prediction_classes_descriptions = []
    for idx in range(len(label_lines)):
        prediction_classes_descriptions.append(label_lines[idx])
    prediction_classes_tensor = tf.constant(prediction_classes_descriptions)
    table = tf.contrib.lookup.index_to_string_table_from_tensor(prediction_classes_tensor, default_value="UNKNOWN")

    prediction_classes = table.lookup(tf.to_int64(indices))
    tf.tables_initializer().run(session=session)

    # build the signature def
    signature_def = (
        tf.saved_model.signature_def_utils.build_signature_def(
            inputs = {
                'image_blob'  : tf.saved_model.utils.build_tensor_info(raw_image_data),
            },
            outputs = {
                'class'       : tf.saved_model.utils.build_tensor_info(prediction_classes),
                'probability' : tf.saved_model.utils.build_tensor_info(values),
            },
            method_name = tf.saved_model.signature_constants.PREDICT_METHOD_NAME
        )
    )
    # re-create the model sub-directory
    shutil.rmtree(args.export_path)
    os.makedirs(args.export_path)

    # save the model with proper format
    legacy_init_op = tf.group(tf.tables_initializer(), name='legancy_init_op')
    builder = tf.saved_model.builder.SavedModelBuilder(args.export_path + "/" + str(int(round(time.time()))))
    builder.add_meta_graph_and_variables (
        session,
        [tf.saved_model.tag_constants.SERVING],
        signature_def_map = {
            tf.saved_model.signature_constants.DEFAULT_SERVING_SIGNATURE_DEF_KEY : signature_def
        },
        legacy_init_op = legacy_init_op,
    )
    builder.save(as_text=False)

if __name__ == '__main__':
    tf.app.run()
```

Once saved, you can run the following commands to save the model with the proper input tensor:

```shell
cd ~/export/
python flowers_export_savedmodel.py
```

You can check that your model was properly saved using the following utility:

```shell
cd ~/export/flowers
saved_model_cli show --dir ./* --tag_set serve --signature_def serving_default
```

It will display the `SavedModel SignatureDef` needed to configure the SAP HANA EML.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Import a Test Dataset in SAP HANA)]

In order to test your model, you will need to import the test data that you saved during the last step (5 images per label).

The use of this script assumes that you have installed the SAP HANA Client and imported the HDBCLI python package into your virtual environment:

```bash
pip install /usr/sap/hdbclient/hdbcli-2.2.36.tar.gz
```

> **Note:** the `hdbcli` tar file may be with a different version than the one displayed above

Now, you can create the following file `~/export/flowers_import_data.py` using the content of the following [script](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-image-retraining/source/flowers_import_data.py):

```python

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os, collections, re

from PIL import Image

import tensorflow as tf
from hdbcli import dbapi

tf.app.flags.DEFINE_string ('host', 'localhost' , 'HXE host')
tf.app.flags.DEFINE_integer('port', 39015       , 'HXE port')
tf.app.flags.DEFINE_string ('usr' , 'ML_USER'   , 'HXE user name')
tf.app.flags.DEFINE_string ('pwd' , '<< password >>'  , 'HXE password')

tf.app.flags.DEFINE_string ('image_dir' , os.path.expanduser("~") + '/models/tutorials/image_retraining/test/flower_photos' , 'image directory')

args = tf.app.flags.FLAGS

def create_image_lists(image_dir):
    if not tf.gfile.Exists(image_dir):
        tf.logging.error("Image directory '" + image_dir + "' not found.")
        return None
    result = collections.OrderedDict()

    sub_dir_paths = sorted(x[0] for x in tf.gfile.Walk(image_dir))

    # The root directory comes first, so skip it.
    is_root_dir = True
    for sub_dir_path in sub_dir_paths:
        if is_root_dir:
            is_root_dir = False
            continue
        extensions = ['jpg', 'jpeg', 'JPG', 'JPEG']
        file_list = []
        dir_name = os.path.basename(sub_dir_path)
        if dir_name == args.image_dir:
            continue
        tf.logging.info("Looking for images in '" + dir_name + "'")
        for extension in extensions:
            file_glob = os.path.join(image_dir, dir_name, '*.' + extension)
            file_list.extend(tf.gfile.Glob(file_glob))
        if not file_list:
            tf.logging.warning('No files found for label '+ dir_name)
            continue

        label_name = re.sub(r'[^a-z0-9]+', ' ', dir_name.lower())
        files = []
        for file_name in file_list:
            base_name = os.path.basename(file_name)
            files.append(base_name)
        result[label_name] = {
            'dir': sub_dir_path,
            'label': label_name,
            'files': files,
        }
    return result

def main(_):
    connection = dbapi.connect(address=args.host, port=args.port, user=args.usr, password=args.pwd)
    connection.setautocommit(False)
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
        cursor.execute('DROP TABLE TF_DATA.FLOWERS_DATA;')
    except (RuntimeError, TypeError, NameError, dbapi.Error):
        pass

    # Create the tables
    cursor.execute('CREATE TABLE TF_DATA.FLOWERS_DATA  (IMAGE_ID INTEGER, IMAGE_LABEL VARCHAR(50), IMAGE_FILE_PATH VARCHAR(255), IMAGE_FILE_NAME VARCHAR(255), IMAGE_RAW_DATA BLOB);')
    query_flowers = 'INSERT INTO TF_DATA.FLOWERS_DATA  (IMAGE_ID, IMAGE_LABEL, IMAGE_FILE_PATH, IMAGE_FILE_NAME, IMAGE_RAW_DATA) VALUES (:id, :label,:path,:file,:raw_data)'

    image_lists = create_image_lists(args.image_dir)

    imported_row = 0
    for image_label in image_lists.keys():
        image_file_list = image_lists[image_label]
        image_file_dir  = image_file_list["dir"]
        for image_file in image_file_list["files"]:
            with open(image_file_dir + "/" + image_file, 'rb') as f:
                imported_row +=1
                jpeg_data = f.read()
                img_blob = dbapi.LOB()
                cursor.execute(query_flowers, {"id": imported_row,"label": image_label, "path": image_file_dir, "file": image_file, "raw_data": img_blob })
                img_blob.write(data=jpeg_data)
                img_blob.close()
                connection.commit()

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
python flowers_import_data.py
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Start TensorFlow ModelServer)]


Now that our model is built and saved in the right format, you can adjust the TensorFlow Serving `ModelServer` configuration and start (or restart) it.

Update the model configuration file `~/export/config.cnf` like:

```js
model_config_list: {
    config: {
        name: "flowers",
        base_path: "/home/tmsadm/export/flowers",
        model_platform: "tensorflow"
    }
}
```

> **Note 1:** If you already have models defined, make sure to separate each `config` section with a comma.

> **Note 2:** Make sure to update the base path in case you not using the proposed one

You can now start the TensorFlow Serving `ModelServer` using the following command:

> **Note:** As of the publication of this tutorial, there is no ***graceful*** shutdown command for the TensorFlow Serving `ModelServer`. Therefore you will need to kill the process manually.

```shell
tensorflow_model_server --model_config_file=./export/config.cnf
```

You can use the following command if you prefer to run it as a background process with all outputs redirected to a log file:

```shell
nohup  tensorflow_model_server --model_config_file=./export/config.cnf > ./tensorflow_model_server.log 2>&1  </dev/null &
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test the model locally)]

You can now test your model with a local client program.

Create the following file `~/export/flowers_client.py` using the content of the following [script](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/mlb-hxe-tensorflow-image-retraining/source/flowers_test_client.py):

```python
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
```

> **Note:** Make sure to update the `<< password >>` in the script

Then, you can execute the following command:

```shell
cd ~/export
python iris_test_client.py
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Reload the SAP HANA EML Config)]

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
INSERT INTO CHECK_PARAMS VALUES ('Model', 'flowers');
CALL _SYS_AFL.EML_CHECKDESTINATION_PROC(CHECK_PARAMS, ?);
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test the Model from SAP HANA SQLScript)]

Since SAP HANA, express edition SPS03, when your model allows a single input to be scored at a time, you can still decide to run your score in batch. This is now possible using the **PREDICTM** function instead of **PREDICT**. This also require the input of a row identifier.

The following SQL script will allow you to test the deployed model using a **single** entry from the dataset uploaded before.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
SET SCHEMA EML_DATA;
-- Uncomment the following lines if you want to re-run the script
-- DROP TYPE TT_FLOWERS_PARAMS;
-- DROP TYPE TT_FLOWERS_FEATURES;
-- DROP TYPE TT_FLOWERS_RESULTS;

-- DROP TABLE FLOWERS_PROC_PARAM_TABLE;
-- DROP TABLE FLOWERS_PARAMS;
-- DROP VIEW FLOWERS_FEATURES;
-- DROP VIEW FLOWERS_FEATURES;  

-- Define table types for iris
CREATE TYPE TT_FLOWERS_PARAMS    AS TABLE ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
CREATE TYPE TT_FLOWERS_FEATURES  AS TABLE (IMAGE_BLOB   BLOB);
CREATE TYPE TT_FLOWERS_RESULTS   AS TABLE (
    CLASS VARCHAR(100), PROBABILITY FLOAT
);

-- Create description table for procedure creation
CREATE COLUMN TABLE FLOWERS_PROC_PARAM_TABLE (
    POSITION        INTEGER,
    SCHEMA_NAME     NVARCHAR(256),
    TYPE_NAME       NVARCHAR(256),
    PARAMETER_TYPE  VARCHAR(7)
);

-- Drop the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_DROP(CURRENT_SCHEMA, 'FLOWERS');
-- Populate the wrapper procedure parameter table
INSERT INTO FLOWERS_PROC_PARAM_TABLE VALUES (1, CURRENT_SCHEMA, 'TT_FLOWERS_PARAMS'   , 'in');
INSERT INTO FLOWERS_PROC_PARAM_TABLE VALUES (2, CURRENT_SCHEMA, 'TT_FLOWERS_FEATURES' , 'in');
INSERT INTO FLOWERS_PROC_PARAM_TABLE VALUES (6, CURRENT_SCHEMA, 'TT_FLOWERS_RESULTS'  , 'out');

-- Create the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_CREATE('EML', 'PREDICT', CURRENT_SCHEMA, 'FLOWERS', FLOWERS_PROC_PARAM_TABLE);
-- Create and populate the parameter table
CREATE TABLE FLOWERS_PARAMS   LIKE TT_FLOWERS_PARAMS;
INSERT INTO FLOWERS_PARAMS   VALUES ('Model', 'flowers');
INSERT INTO FLOWERS_PARAMS   VALUES ('RemoteSource', 'TensorFlow');
INSERT INTO FLOWERS_PARAMS   VALUES ('Deadline', '10000');

CREATE VIEW FLOWERS_FEATURES AS SELECT TOP 1 IMAGE_RAW_DATA AS IMAGE_BLOB FROM TF_DATA.FLOWERS_DATA WHERE IMAGE_LABEL = 'dandelion';
-- daisy, dandelion, roses, sunflowers, tulips

CREATE TABLE FLOWERS_RESULTS LIKE TT_FLOWERS_RESULTS;

CALL FLOWERS (FLOWERS_PARAMS, FLOWERS_FEATURES, FLOWERS_RESULTS) WITH OVERVIEW;

SELECT * FROM FLOWERS_RESULTS;
```

The following SQL script will allow you to test the deployed model using **multiple** entries from the dataset uploaded before.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
SET SCHEMA EML_DATA;
-- Uncomment the following lines if you want to re-run the script
-- DROP TYPE TT_FLOWERS_PARAMS;
-- DROP TYPE TT_FLOWERS_FEATURES;
-- DROP TYPE TT_FLOWERS_RESULTS;

-- DROP TABLE FLOWERS_PROC_PARAM_TABLE;
-- DROP TABLE FLOWERS_PARAMS;
-- DROP VIEW FLOWERS_FEATURES;
-- DROP VIEW FLOWERS_FEATURES;  

-- Define table types for iris
CREATE TYPE TT_FLOWERS_PARAMS    AS TABLE ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
CREATE TYPE TT_FLOWERS_FEATURES  AS TABLE (IMAGE_ID INTEGER, IMAGE_BLOB BLOB);
CREATE TYPE TT_FLOWERS_RESULTS   AS TABLE (
    IMAGE_ID INTEGER, CLASS VARCHAR(100), PROBABILITY FLOAT
);

-- Create description table for procedure creation
CREATE COLUMN TABLE FLOWERS_PROC_PARAM_TABLE (
    POSITION        INTEGER,
    SCHEMA_NAME     NVARCHAR(256),
    TYPE_NAME       NVARCHAR(256),
    PARAMETER_TYPE  VARCHAR(7)
);

-- Drop the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_DROP(CURRENT_SCHEMA, 'FLOWERS');
-- Populate the wrapper procedure parameter table
INSERT INTO FLOWERS_PROC_PARAM_TABLE VALUES (1, CURRENT_SCHEMA, 'TT_FLOWERS_PARAMS'   , 'in');
INSERT INTO FLOWERS_PROC_PARAM_TABLE VALUES (2, CURRENT_SCHEMA, 'TT_FLOWERS_FEATURES' , 'in');
INSERT INTO FLOWERS_PROC_PARAM_TABLE VALUES (6, CURRENT_SCHEMA, 'TT_FLOWERS_RESULTS'  , 'out');

-- Create the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_CREATE('EML', 'PREDICTM', CURRENT_SCHEMA, 'FLOWERS', FLOWERS_PROC_PARAM_TABLE);
-- Create and populate the parameter table
CREATE TABLE FLOWERS_PARAMS   LIKE TT_FLOWERS_PARAMS;
INSERT INTO FLOWERS_PARAMS   VALUES ('Model', 'flowers');
INSERT INTO FLOWERS_PARAMS   VALUES ('RemoteSource', 'TensorFlow');
INSERT INTO FLOWERS_PARAMS   VALUES ('Deadline', '10000');

CREATE VIEW FLOWERS_FEATURES AS SELECT IMAGE_ID, IMAGE_RAW_DATA AS IMAGE_BLOB FROM TF_DATA.FLOWERS_DATA;

CREATE TABLE FLOWERS_RESULTS LIKE TT_FLOWERS_RESULTS;

CALL FLOWERS (FLOWERS_PARAMS, FLOWERS_FEATURES, FLOWERS_RESULTS) WITH OVERVIEW;

SELECT D.IMAGE_ID, D.IMAGE_LABEL, D.IMAGE_FILE_NAME, R.CLASS AS PREDICTED_LABEL, R.PROBABILITY AS PREDICTED_PROBABILITY FROM TF_DATA.FLOWERS_DATA D LEFT OUTER JOIN FLOWERS_RESULTS R ON R.IMAGE_ID = D.IMAGE_ID;
```

[DONE]
[ACCORDION-END]
