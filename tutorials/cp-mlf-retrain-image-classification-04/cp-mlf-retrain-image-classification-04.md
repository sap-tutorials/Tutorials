---
title: Deploy the Image Classification Retrained Model
description: Discover how to deploy the retraining model for the SAP Leonardo Machine Learning foundation Image Classification Retraining scenario
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
  - How to check the content of the model repository
  - How to request a new model deployment to be deployed
  - How to check the model deployment status

[ACCORDION-BEGIN [Step](Check the Model Repository Model)]

Once a job is completed, the model will be automatically stored in the model repository.

Only model store in the repository can then be deployed.

To check the list of model in the repository, you can execute the following command:

```shell
cf sapml retraining models -m image
```

Your model should be listed in the output.

```log
  NAME     VERSION  UPDATED AT                CREATED AT                CHECKSUM
  flowers  1        2018-11-03T15:53:09.418Z  2018-11-03T15:53:02.624Z
```

**Note:** You may have more than one version of the same model available in the repository

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Submit your Model for Deployment)]

Once the model is available in the repository, you can request the model to be deployed using the following command:

```shell
cf sapml retraining model_deploy flowers 1 -m image
```

Once the deployment request completed, you should get the following output:

```log
Deployment request submitted with id "xx-xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx".
```

Copy in your notepad the submission id from the console (***`xx-xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`***)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Check your Deployed Model)]

You can check that your model was properly deployed using the following command:

```shell
cf sapml retraining model_deployments -m image
```

Your model should be listed in the output with a status that will change during the deployment process:


```log
  ID         MODEL NAME MODEL VERSION  HOST    PORT  STATUS     DESCRIPTION
  xxxxxxxxx  flowers    1              zzzzzz  443   PENDING    Loading model...
```

Then:

```log
  ID         MODEL NAME MODEL VERSION  HOST    PORT  STATUS     DESCRIPTION
  xxxxxxxxx  flowers    1              zzzzzz  443   PENDING    Finalizing the deployment resources creation
```

And finally:

```log
  ID         MODEL NAME MODEL VERSION  HOST    PORT  STATUS     DESCRIPTION
  xxxxxxxxx  flowers    1              zzzzzz  443   SUCCEEDED  Model is deployed and ready to serve.
```

Copy in your notepad the host id from the console (***`HOST`***)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
