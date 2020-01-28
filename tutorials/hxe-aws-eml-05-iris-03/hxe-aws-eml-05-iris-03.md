---
title: Train your Iris Model using Amazon SageMaker
description: Leverage the TensorFlow Python SDK for SageMaker to build your classification model
author_name: Josh Bentley
author_profile: https://github.com/jarjarbentley
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning ]
time: 30
---

## Details
### You will learn  
  - Use the SageMaker Python SDK for TensorFlow to build and train your model
  - Retrieve your model file locally from an Amazon S3 bucket
  - Check your model signature

Before proceeding with building your model with SageMaker, it is recommended to have some understanding how the amazon SageMaker works.

Amazon SageMaker provides the ability to build, train, and deploy machine learning models quickly by providing a fully-managed service that covers the entire machine learning workflow to label and prepare your data, choose an algorithm, train the algorithm, tune and optimize it for deployment, make predictions, and take action.

For more details, please check the following URL: <https://aws.amazon.com/sagemaker>

In this scenario, you will leverage Amazon SageMaker to only build & train your TensorFlow models. When deployed, a model is only available via REST API which is not compatible with the use of the `gRPC` protocol that the SAP HANA External Machine Learning library is using.

[ACCORDION-BEGIN [Step 1: ](Access the SageMaker Notebook instance)]

If you have your Jupyter Notebook instance already open in your browser, then you can move to the next step.

Else, access the <a href="https://console.aws.amazon.com/sagemaker" target="&#95;blank">Amazon SageMaker Console</a> (you also use the search for **SageMaker** in the Amazon Web Services Management Console).

![Amazon Web Services](sagemaker-01.png)

Click on **Open `JupyterLab`**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the Training Script)]

In order to train your model, you will need to provide a training script, which will define the following function:

- the estimator function (`estimator_fn`)
- the input function for training (`train_input_fn`) and evaluation (`eval_input_fn`)

These functions will allow you to adjust the training parameters like the batch size, the model signature etc..

For more details about the training, you can refer to the SageMaker Python SDK for TensorFlow documentation: <https://sagemaker.readthedocs.io/en/stable/using_tf.html#training-with-tensorflow-estimator>

On the menu bar, select **File > New > Text File**.

![Amazon Web Services](sagemaker-03.png)

Rename your notebook document **`iris_dnn_classifier.py`** using the menu bar with **File > Rename Notebook...**.

Paste the following code then save your **`iris_dnn_classifier.py`**  file:

```Python
import argparse, os, logging

import numpy as np
import tensorflow as tf
import pandas as pd

# Disable MKL to get a better performance for this model.
os.environ['TF_DISABLE_MKL'] = '1'
os.environ['TF_DISABLE_POOL_ALLOCATOR'] = '1'

col_features = ['SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth']
col_label = 'Species'
n_classes = 3
hidden_units = [10, 20, 10]

def estimator_fn(run_config, params):
    feature_columns = [tf.feature_column.numeric_column(key='x', default_value=None, dtype=tf.float32, shape = [len(col_features)])]
    return tf.estimator.DNNClassifier(
        feature_columns = feature_columns,
        hidden_units = hidden_units,
        n_classes = n_classes,
        config = run_config
    )

def serving_input_fn(params):
    """Returns the serving input function with raw tensor"""
    feature_spec = {'x': tf.placeholder(dtype=tf.float32, shape=[None,4], name='x')}
    return tf.estimator.export.build_raw_serving_input_receiver_fn(feature_spec)()

def train_input_fn(training_dir, params):
    """Returns input function that would feed the model during training"""
    return generate_input_fn(training_dir, params['input_train'], params['batch_size'])

def eval_input_fn(training_dir, params):
    """Returns input function that would feed the model during evaluation"""
    return generate_input_fn(training_dir, params['input_test'], params['batch_size'])

def generate_input_fn(training_dir, training_filename, batch_size):
    """Returns an estimator numpy input function that would feed the model"""
    input_set = pd.read_csv(
        os.path.join(training_dir, training_filename),
        names = (col_features + [col_label]),
        header = 0
    )

    features, labels = input_set, input_set.pop(col_label)

    # Convert the inputs to a Dataset.
    features = {'x': np.array(features.values [:,0:len(col_features)])}
    dataset = tf.data.Dataset.from_tensor_slices((dict(features), labels))        

    # Shuffle, repeat, and batch the examples.
    dataset = dataset.shuffle(batch_size).repeat().batch(batch_size)

    # Return the dataset.
    return dataset
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a Notebook document)]

Once open, you should have access to your Notebook instance.

On the menu bar, select **File > New > Notebook**.

![Amazon Web Services](sagemaker-03.png)

Select **`conda_tensorflow_p36`** as Kernel then click on **Select**.

![Amazon Web Services](sagemaker-04.png)

Rename your notebook document **`hxe-aws-eml-iris-03.ipynb`** using the menu bar with **File > Rename Notebook...**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Import Python packages)]

It is a command practice to group all import and generic configuration statements in the first notebook cell.

So, since you will be leveraging the SageMaker SDK for Python, you will need to import it.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
import datetime
from IPython.core.display import HTML

from sagemaker.session import Session
from sagemaker.tensorflow import TensorFlow
from sagemaker import get_execution_role
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Set the environment)]

It is a command practice to group all import and generic configuration statements in the first notebook cell.

So, since you will be leveraging the SageMaker SDK for Python, you will need to import it.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
# S3 bucket for saving code and model artifacts (will be created if it doesn't exists).
default_bucket = Session().default_bucket()

# IAM execution role that gives SageMaker access to resources in your AWS account.
role = get_execution_role()

# define the job_name that will also be used for the model file
job_name = 'my-model-iris-{}'.format(datetime.datetime.now().strftime("%Y-%m-%d-%H-%M-%S-%MS"))

# Location to save your custom code in tar.gz format in S3
custom_code_upload_location = 's3://{}/customcode/tensorflow_iris/'.format(default_bucket)

# Location where the model file is saved in S3
model_artifacts_location = 's3://{}/artifacts/'.format(default_bucket)
model_artifacts_path = 's3://{}/artifacts/{}'.format(default_bucket, job_name)

# the data location in S3
data_location = 's3://{}/data/'.format(default_bucket)

# the instance type to be used for training. using 'local' will not trigger a job on SageMaker
instance_type = 'ml.c4.xlarge'
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Build the Model object)]

Now, you can build the Model object using the SageMaker Python SDK for TensorFlow.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
# create the model object
model = TensorFlow (
    entry_point = './iris_dnn_classifier.py',
    role = role,
    hyperparameters = {
        'batch_size' : 100,
        'input_train' : 'iris_training_estimation.csv',
        'input_test' : 'iris_training_validation.csv',
    },
    output_path = model_artifacts_location,
    code_location = custom_code_upload_location,
    train_instance_count = 1,
    train_instance_type = instance_type,
    training_steps = 10000,
    evaluation_steps = 10000
)
```

You can adjust the training and evaluation step count setting to get better performance, but this might also increase the computation time and the billable seconds that will be charged to your account.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Start the Training Job)]

Now, you can start the training job for the model you just created.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
display(HTML("You can check the job logs at the following URL:  <a href='https://console.aws.amazon.com/sagemaker/home#/jobs/{}' target='_blank'>here</a>".format(job_name)))

# fit the model
model.fit(
    inputs = data_location,
    job_name = job_name
)
```

The job will take some time to complete, and you can click on the link in the output to monitor the training job directly from SageMaker.

Once completed, the full execution log file will be displayed for your information.
Additionally, you will be notified how many billable seconds will be charged to your account.

```
2019-03-19 22:43:31 Completed - Training job completed
Billable seconds: 51
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Download the model and check the signature)]

Now, you can download locally the model archive file and check to mode signature.

The model signature allows you to understand the expect input to be provided when consuming the model.

In the Iris model, you will need to provide an input tensor called `x` with the four attributes : ***`SepalLength`***, ***`SepalWidth`***, ***`PetalLength`***, ***`PetalWidth`***.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh -s "$model_artifacts_path"

# create folder and remove previous local version of the model if any
mkdir -p ./model
rm -Rf ./model/model.tar.gz

# download the model archive locally
aws s3 cp $1/output/model.tar.gz ./model/

# extract the archive
rm -Rf ./export
tar xf ./model/model.tar.gz

# display all model signatures
saved_model_cli show --all --dir ./export/Servo/*

```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]
