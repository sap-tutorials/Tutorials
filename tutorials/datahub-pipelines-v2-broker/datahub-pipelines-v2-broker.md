---
title:  Use a Message Broker in SAP Data Hub, Developer Edition
description: Use a message broker to publish and subscribe to sensor data by using SAP Data Hub, developer edition.
auto_validation: true
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites

## Details
### You will learn  
During this tutorial, you will learn how to use a message broker within a pipeline. You will use a **Kafka Producer** as well as a **Kafka Consumer**.

### Time to Complete
**30 Min**

---

[ACCORDION-BEGIN [Step 1: ](Setup up Apache Kafka)]
During this tutorial, you will use Apache Kafka as message broker to stream sensor data (i.e. you will publish sensor data via a stream and also consume this stream).

>Apache Kafka is a distributed streaming platform. Simply spoken, it allows you to publish and subscribe to message streams. You can find more information on [https://kafka.apache.org](https://kafka.apache.org).


If you have not yet Apache Kafka (combined with Apache Zookeeper) set up, then an easy way to do is pulling an image from Docker Hub (https://hub.docker.com/).

Open a **command line** and search the Docker Hub for images by entering the following.

```sh
docker search kafka
```

You see a list of images related to Apache Kafka. The `spotify/kafka` image is suitable for the purpose of this tutorial. It runs Apache Kafka as well as Apache Zookeeper.

![picture_01](./datahub-pipelines-v2-broker_01.png)  

Pull the `spotify/kafka` image from Docker Hub by entering the following command.

```sh
docker pull spotify/kafka
```

Then verify that the image has been pulled by using the `docker images` command. You see a list of images on your local computer.

Run the image by entering the following command.

```sh
docker run --name kafka --hostname kafka --network dev-net spotify/kafka
```

The `hostname` and `network` options ensure that you can easily address Apache Kafka from the pipeline later on.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add and configure Kafka Producer)]
Open the pipeline which you have created during the previous tutorials (`test.myFirstPipeline`) in the modelling environment `http://localhost:8090`.

Remove the connection between the **Data Generator** operator and the **Terminal** operator.

Add a **Kafka Producer** operator to the pipeline by drag & drop. Then connect the `output` port of the **Data Generator** operator to the `message` port of the **Kafka Producer** operator.

![picture_02](./datahub-pipelines-v2-broker_02.png)  

Right click on the **Kafka Producer** and go to "Open Configuration" to configure the operator properties . You need to maintain the following properties:

| Property                       | Value                          |
| ------------------------------ | ------------------------------ |
| `brokers`                      | `kafka:9092`                   |
| `topic`                        | `sensordata`                   |

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add and configure Kafka Consumer)]
Add a **`Kafka Consumer2`** operator to the pipeline by drag & drop. Add a **`ToString` Converter** operator to the pipeline by drag & drop. Connect the `message` port of the **`Kafka Consumer2`** operator to the `ininterface` port of the **`ToString` Converter** operator. Connect the `outstring` port of the **`ToString` Converter** operator to the `in1` port of the **Terminal** operator.

![picture_03](./datahub-pipelines-v2-broker_03.png)  

Right click on the **`Kafka Consumer2`** and go to "Open Configuration" to configure the operator properties . You need to maintain the following properties:

| Property                       | Value                          |
| ------------------------------ | ------------------------------ |
| `brokers`                      | `kafka:9092`                   |
| `topics`                       | `sensordata`                   |

Afterwards press the **Save** button.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Execute the data pipeline)]
Press the **Run** button to execute the pipeline.

When the **Status** tab indicates that the pipeline is running, use the context menu **Open UI** of the **Terminal** operator to see the generated sensor data.

![picture_04](./datahub-pipelines-v2-broker_04.png)  

In contrast to the previous tutorial, this time the generated sensor data is not sent from the **Data Generator** operator to the **Terminal** operator directly, but via Apache Kafka.

Copy any row of the terminal output and paste it in the frame below and click on **Validate**.

[VALIDATE_1]

Stop the pipeline by pressing the **Stop** button.

[ACCORDION-END]

---
