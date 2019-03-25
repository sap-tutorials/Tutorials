---
title: Execute the Image Classification Model Retraining Job
description: Discover how to execute the retraining process for the SAP Leonardo Machine Learning foundation Image Classification Retraining scenario
auto_validation: true
time: 30
tags: [tutorial>beginner, topic>cloud, topic>machine-learning, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-leonardo-machine-learning-foundation
---

## Prerequisites
 - [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html)
 - [Install the Machine Learning foundation plugin for the Cloud Foundry CLI](https://developers.sap.com/tutorials/cp-mlf-install-sapmlcli.html)
 - [Create a Machine Learning Foundation service instance on the Cloud Foundry environment](https://developers.sap.com/tutorials/cp-mlf-create-instance.html)

## Details
### You will learn
  - The basics about the for the Job Retraining configuration
  - How to submit a new job
  - How to check the job status
  - How to get the job process logs

[ACCORDION-BEGIN [Step](Configure the Image Retraining Job)]

In order to submit a retraining job in general, you will need to provide the configuration details to execute the process.

From your terminal console using the following command:

```shell
cf sapml help retraining job_submit
```

This will return the details about the job submission command.

As you will notice, you will need to create a JSON file that includes the following entries:

Key                       | Description
--------------------------|--------------------------------------------------------------------------------------------
**`dataset`**             | the path to the retraining data in the file share, (the **flowers** folder in your case)
**`modelName`**           | the name of the retrained model
**`batchSize`**           | how many data samples in one batch (default = 64, min = 1, max = 1024)
**`learningRate`**        | learning rate of back propagation (default = 0.001, 0 < learning rate < 1)
**`maxEpochs`**           | maximum number of training epochs (default = 150, min = 1, max = 10000)
**`maxUnimprovedEpochs`** | number of unimproved epochs to stop training after (default = 15, min = 1, max = 10000)
**`completionTime`**      | maximum job run time (default = 24 h, min = 1 h, max = 168 h)
**`memory`**              | memory in MB provided to the retraining job (default = 8192, min = 8192, max = 32768)
**`generateTfSummaries`** | generate TensorFlow summaries if set to true, do not generate if set to false


Create a file named **`image_retrain.json`** with the following content:

```text
{
	"dataset": "flowers",
	"modelName": "flowers"
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Submit the Image Retraining Job)]

In your terminal console; execute the following command:

```shell
cf sapml retraining job_submit image_retrain.json -m image
```

In the console, you should see the following output:

```log
Job submitted successfully with id "flowers-xxxx-xx-xxxxxxxxxxxxxx".
```

Copy in your notepad the job id from the console (***`flowers-xxxx-xx-xxxxxxxxxxxxxx`***)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Check the Image Retraining Job Status)]

You can check the job status using the following command:

```shell
cf sapml retraining jobs -m image
```

The response should look like the following output where the start and finish time are not yet initialized:

```log
  ID                              STATUS   SUBMISSION TIME           START TIME                FINISH TIME
  flowers-xxxx-xx-xxxxxxxxxxxxxx  PENDING  2018-11-02T17:40:23.000Z  0001-01-01T00:00:00.000Z  0001-01-01T00:00:00.000Z
```

Repeat the operation every 5 minutes or so.

At some point you should receive the following output which informs you that the job was started and is currently running:

```log
  ID                              STATUS   SUBMISSION TIME           START TIME                FINISH TIME
  flowers-xxxx-xx-xxxxxxxxxxxxxx  RUNNING  2018-11-02T17:40:23.000Z  2018-11-02T17:47:52.000Z  0001-01-01T00:00:00.000Z
```

Repeat the operation every 5 minutes or so until the job is not longer listed, which implies that is was completed, or with a success status .

```log
  ID                              STATUS     SUBMISSION TIME           START TIME                FINISH TIME
  flowers-xxxx-xx-xxxxxxxxxxxxxx  SUCCEEDED  2018-11-02T17:40:23.000Z  2018-11-02T17:47:52.000Z   2018-11-02T17:57:52.000Z
```

You can also now check the retrain log file using the following command after replacing ***`flowers-xxxx-xx-xxxxxxxxxxxxxx`*** by the job id:

```shell
cf sapml fs get flowers-xxxx-xx-xxxxxxxxxxxxxx/retraining.log ./retrain.log
```

Open the ***`retrain.log`*** file in your favorite text editor.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]


---
