---
title: Upload your Dataset for Image Classification Retraining
description: Discover how to upload your dataset for the SAP Leonardo Machine Learning foundation Image Classification Retraining scenario
auto_validation: true
time: 20
tags: [tutorial>beginner, topic>cloud, topic>machine-learning, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-leonardo-machine-learning-foundation
---

## Prerequisites
 - [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html)
 - [Install the Machine Learning foundation plugin for the Cloud Foundry CLI](https://developers.sap.com/tutorials/cp-mlf-install-sapmlcli.html)
 - [Create a Machine Learning Foundation service instance on the Cloud Foundry environment](https://developers.sap.com/tutorials/cp-mlf-create-instance.html)

## Details
### You will learn
  - How to upload your dataset in the SAP Leonardo Machine Learning foundation storage area for the Image Classification Retraining scenario

[ACCORDION-BEGIN [Step](Login using the SAP Cloud Platform CLI)]

As described in the [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html), you should be able to successfully connect to your SAP Cloud Platform for Cloud Foundry trial environment.

Execute the following command:

```shell
cf login
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Get the Service Key details)]

When you created the service key for the Machine Learning Foundation service, you were provided with the following details:

 - the retraining API URL (under the **`IMAGE_RETRAIN_API_URL`** key)
 - the job submission API URL (under the **`JOB_SUBMISSION_API_URL`** key)
 - the authentication URL (under the **`url`** key)
 - the `client_id` & `client_secret` that allows you to get your OAuth token

In your terminal console, execute the following command:

```shell
cf service-key my-ml-foundation my-ml-foundation-key
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Set the Machine Learning foundation plugin Configuration)]

In your terminal console, execute the following command but make sure to update the ***authentication URL***, the ***job submission API URL***, and the ***retraining API URL*** first:

```shell
cf sapml config set auth_server <authentication URL>
cf sapml config set job_api <JOB_SUBMISSION_API_URL>
cf sapml config set retraining_image_api <IMAGE_RETRAIN_API_URL>
cf sapml config set ml_foundation_service_name  ml-foundation-trial-beta

cf sapml fs init
```

The output should look like this:

```
Attempting automatic login using Cloud Foundry service "ml-foundation-trial-beta" ...
Successfully logged in using "ml-foundation-trial-beta".
Initializing. Please wait.
Initialization finished successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Upload your dataset)]

Now, you can now transfer your dataset using the following commands:

```shell
cd flowers
cf sapml fs put test flowers/test/
cf sapml fs put training flowers/training/
cf sapml fs put validation flowers/validation/
```

[DONE]
[ACCORDION-END]
