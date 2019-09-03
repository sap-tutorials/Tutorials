---
title: Build a TensorFlow Serving Docker image to host the Iris model
description: Leverage the pre-built TensorFlow Serving Docker image to build a new image that will serve the Iris Model, then deploy and test it locally
author_name: Josh Bentley
author_profile: https://github.com/jarjarbentley
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning ]
time: 30
---

## Details
### You will learn  
  - Pull the TensorFlow Serving Container locally
  - Start the container
  - Copy the model into the container
  - Commit the customized container image
  - Start the customized container
  - Test your model locally

Now that the Iris model has been trained, you are ready to create a TensorFlow Serving Container.

Because the SAP HANA EML library uses the `gRPC` protocol to communicate with TensorFlow Serving, it is not possible to use the SageMaker model endpoint. However, within the SageMaker platform, only REST API endpoint are made available for now.

For more details on the SageMaker TensorFlow Serving Container, please check the following URL: <https://github.com/aws/sagemaker-tensorflow-serving-container>

[ACCORDION-BEGIN [Step 1: ](Access the SageMaker Notebook instance)]

To create your TensorFlow Serving container for the Iris model, you will be using the existing SageMaker Notebook instance as Docker is installed and configured already. However, this can also be done in any environment where Docker is installed.

If you have your Jupyter Notebook instance already open in your browser, then you can move to the next step.

Else, access the <a href="https://console.aws.amazon.com/sagemaker" target="&#95;blank">Amazon SageMaker Console</a> (you also use the search for **SageMaker** in the Amazon Web Services Management Console).

![Amazon Web Services](sagemaker-01.png)

Click on **Open `JupyterLab`**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a Notebook document)]

Once open, you should have access to your Notebook instance.

On the menu bar, select **File > New > Notebook**.

![Amazon Web Services](sagemaker-03.png)

Select **`conda_tensorflow_p36`** as Kernel then click on **Select**.

![Amazon Web Services](sagemaker-04.png)

Rename your notebook document **`hxe-aws-eml-iris-04.ipynb`** using the menu bar with **File > Rename Notebook...**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Clean your Docker environment)]

Before getting started, let's clean your environment in case you made some experiments before and simply wants to start from scratch again.

In the first cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# check if there is already a serving_base container running
[ -z "$(docker ps | grep 'base' | cut -f 1 | cut -d ' ' -f 1)" ] || {
    container_id=$(docker ps | grep 'base' | cut -f 1 | cut -d ' ' -f 1)
    echo "killing base container id $container_id"
    docker kill $container_id > /dev/null
}
```

The above code will restore your environment by killing existing container instance named ***base***, so that you can run again the next steps again.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Pull and Run TensorFlow Serving Docker Container)]

You can now pull and run the TensorFlow Serving Docker Container.

You will use the latest version available (1.12.0) available on the Docker Hub.

For more details about TensorFlow Serving Docker Container on Docker Hub, you can check the following link: <https://hub.docker.com/r/tensorflow/serving>

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# pull the docker image for tensorflow/serving
docker pull tensorflow/serving:1.12.0

# run it using base as name
docker run -d --name base tensorflow/serving:1.12.0
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Import you Iris model in the Container)]

Now that the TensorFlow Serving Docker container is up and running, you can copy the Iris model into the container.

There are multiple approach to serve TensorFlow models in a Docker container.

One consist in having the model file in a persistent storage like an S3 bucket, then have the container use this location as the model folder.
This approach provides flexibility if you need to deploy new model versions for example, but requires the persistent storage to be available at all time and may  slow down the initial startup.

In this tutorial, you will a different approach where you will make a copy of the model into the container so that the container can be totally isolated and independent but also not depend on local/share filesystems where to look for the model files.

Keep in mind that these are approaches among others.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# copy the model files inside
docker cp export/Servo base:/models/iris
```

The above code will copy the local model files (from ***export/Servo***) into the container storage (from ***models/iris***).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Commit the Container)]

Now that the model is copied, you can commit (freeze) the container and assign it a name and a version.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# commit the change into a new image 'tensorflow/serving/iris' and set the MODEL_NAME env variable
docker commit --change "ENV MODEL_NAME iris" base tensorflow/serving/iris:1.0.0
```

You can now use this container using it's new name ***`tensorflow/serving/iris`*** and version number ***`1.0.0`***.
Additionally, you have set the ***`MODEL_NAME`*** environment variable is set to iris (the model folder name).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the available images & running containers)]

Now that the container is committed, you can check the local docker images and processes.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# check images repository
docker images
# check running containers
docker ps
```

As you can notice, the list of images includes:

 - ***`tensorflow/serving`***, version ***`1.12.0`***
 - ***`tensorflow/serving/iris`***, version ***`1.0.0`***
 - and the ***base*** image process

You can now stop and remove the unnecessary containers/images for the rest of this tutorial.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# stop & delete base instances if any
docker stop base
docker rm base

# delete tensorflow/serving image if any
docker rmi tensorflow/serving:1.12.0
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Start the Iris Container)]

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# create a log folders and clear existing content if any
mkdir -p ./logs
rm -Rf ./logs/docker.log

# run the docker container
docker run \
  -p 8500:8500 \
  -p 8501:8501 \
  tensorflow/serving/iris:1.0.0 \
  > ./logs/docker.log 2>&1 &
```

You can now check that the container is started using the following command:

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# check running containers
docker ps
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Test the Iris Container)]

As you could see that container was started to listen on port 8500, which is the `gRPC` port.

However, your Docker container will be running using its own IP address that you will need.

The following code will allow you to retrieve the Docker IP address and then use it to call the model via `gRPC` to score a new entry for `Versicolor` (class = 1):

 - SEPALLENGTH: 5.9
 - SEPALWIDTH: 3
 - PETALLENGTH: 4.2
 - PETALWIDTH: 1.5

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
"""
A client that talks to tensorflow_model_server serving the iris model.
"""
import os
from grpc.beta import implementations

import tensorflow as tf
from tensorflow_serving.apis import predict_pb2
from tensorflow_serving.apis import prediction_service_pb2_grpc

# get the docker container ip address
docker_ip = !echo $(ifconfig eth0 | grep "inet addr" | cut -d ':' -f 2 | cut -d ' ' -f 1)
docker_ip = docker_ip[0]

# set the TF Serving host and port based on the docker ip address
host = docker_ip
port = 8500

channel = implementations.insecure_channel(host, port)

request = predict_pb2.PredictRequest()
request.model_spec.name = 'iris'
request.model_spec.signature_name = 'predict'

stub = prediction_service_pb2_grpc.PredictionServiceStub(channel._channel)

x = [[5.9, 3, 4.2, 1.5]]

request.inputs['x'].CopyFrom(tf.contrib.util.make_tensor_proto(x))

response = stub.Predict(request, 100)
print("class : " + str(response.outputs['class_ids'].int64_val[0]))
```

You will get the detected class (from 0 to 2, where 1 is `versicolor`).

To get the probabilities for each class, you use the following code in the next cell, then press **SHIFT** + **ENTER** to execute the code:

```Python
print("probabilities : " + str(response.outputs['probabilities'].float_val))
```

To get full response displayed, you can use the following code in the next cell, then press **SHIFT** + **ENTER** to execute the code:

```Python
print("response : \n\n" + str(response))
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Stop the Iris Container)]

Before moving to the next tutorial, you can stop the current container instance to release the resources locally.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh
# check if there is already a serving_base container running
[ -z "$(docker ps | grep 'tensorflow/serving/iris:1.0.0' | cut -f 1 | cut -d ' ' -f 1)" ] || {
    container_id=$(docker ps | grep 'tensorflow/serving/iris:1.0.0' | cut -f 1 | cut -d ' ' -f 1)
    echo "killing tensorflow/serving/iris:1.0.0 container id $container_id"
    docker kill $container_id > /dev/null
}
```

[DONE]
[ACCORDION-END]
