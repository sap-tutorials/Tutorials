---
title: Prepare your environment for the SAP Leonardo Machine Learning foundation Image Classification Retraining scenario
description: Discover how to prepare your environment for the SAP Leonardo Machine Learning foundation Image Classification Retraining scenario
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>cloud, topic>machine-learning, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-leonardo-machine-learning-foundation
---

## Prerequisites
 - [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html)
 - [Install the Machine Learning foundation plugin for the Cloud Foundry CLI](https://developers.sap.com/tutorials/cp-mlf-install-sapmlcli.html)
 - [Create a Machine Learning Foundation service instance on the Cloud Foundry environment](https://developers.sap.com/tutorials/cp-mlf-create-instance.html)

## Details
### You will learn
  - How to connect to Cloud Foundry Command Line Interface to your SAP Cloud Platform for Cloud Foundry account
  - Get Your SAP Leonardo Machine Learning Foundation service instance Key
  - Configure SAP Leonardo Machine Learning Command Line Interface for your SAP Cloud Platform for Cloud Foundry account

[ACCORDION-BEGIN [Step](Login using the SAP Cloud Platform CLI)]

As described in the [Install the Cloud Foundry Command Line Interface (CLI)](https://developers.sap.com/tutorials/cp-cf-download-cli.html), you should be able to successfully connect to your SAP Cloud Platform for Cloud Foundry trial environment.

Execute the following command:

```shell
cf login
```

Make sure your target API endpoint, organization and space are set to the environment where you previously created your SAP Leonardo Machine Learning foundation service instance.

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

Before you can start using the SAP ML CLI, you will need to set a series of configuration setting.

In your terminal console, execute the following command but make sure to update the ***authentication URL***, the ***job submission API URL***, and the ***retraining API URL*** first:

```shell
cf sapml config set auth_server <authentication URL>
cf sapml config set job_api <JOB_SUBMISSION_API_URL>
cf sapml config set retraining_image_api <IMAGE_RETRAIN_API_URL>
cf sapml config set ml_foundation_service_name  ml-foundation-trial-beta

cf sapml fs init
```

> ### **Note:**
When using a productive account the ***`ml_foundation_service_name`*** might be different than the one using in trial. Make sure to adjust the value accordingly.

The output should look like this:

```
Attempting automatic login using Cloud Foundry service "ml-foundation-trial-beta" ...
Successfully logged in using "ml-foundation-trial-beta".
Initializing. Please wait.
Initialization finished successfully.
```

[DONE]
[ACCORDION-END]
