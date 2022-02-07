---
title: Train ML Model with Execution in SAP AI Core (Postman)
description: Train execution a ML model in SAP AI Core with help of Postman client.
auto_validation: true
time: 20
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---


## Details
### You will learn
  - How to create execution configuration for SAP AI Core
  - How to observe execution on SAP AI Core

---

[ACCORDION-BEGIN [Step 1: ](API to list available scenarios)]

Scenarios refers to the use case. It is created by an identifier within the workflows *(YAML files, see screenshots below)* that you synced with GitHub in previous tutorial. For all workflows of same use case, write the same scenario id.

!![scenario definition inside worflows](img/training/scenario.png)


> **COLLECTIONS** > *GET* List scenario

### Endpoint
**GET**
`{{apiurl}}/v2/lm/scenarios`

### HEADER

| KEY | VALUE |
| --- | --- |
| `AI-Resource-Group` | `tutorial` |

**SEND**

### Response

!![list scenarios](img/postman/call-scenario.png)

In the `RESPONSE`, you should see `id : text-clf-tutorial`

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 2: ](API to register train dataset as artifact)]

Every data entity *(dataset, model weights)* in SAP AI Core is considered **artifact**.

Previously you uploaded your data to AWS S3 and connected S3 to SAP AI Core.

Now you will specifically point to that dataset in S3.

> **COLLECTIONS** > *POST* Create artifact

### Endpoint

**POST**
`{{apiurl}}/v2/lm/artifacts`

### Header

| KEY | VALUE
| --- | ---
| `AI-Resource-Group` | `tutorial`

### Body
```
{
  "name": "tutorial-train-data",
  "kind": "dataset",
  "url": "ai://default/data",
  "description": "Text Classification Dataset",
  "scenarioId": "text-clf-tutorial"
}
```

**IMPORTANT:** DO NOT end `url` in the **Body** with `forward slash`.

**SEND**

!![artifact create](img/postman/call-artifact.png)

`RESPONSE`
```
{
    "id": "aefde425-231f-44a0-9abf-9228fc1b24c9",
    "message": "Artifact acknowledged",
    "url": "ai://default/data/"
}
```
</details>


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create training configuration)]

The training configuration specifies
  - the input as `artifactId`
  - the workflows to use  with `scenarioId` and `executableId` *(`name` mentioned in the training workflow)*.

!![executable id in workflow](img/training/executable_id.png)


| Key | Where to get value from |
| --- | --- |
| `artifactId` | from `RESPONSE` of API call `GET List artifacts`


Make the API call.

> **COLLECTIONS** > *POST* Create execution configuration

### Endpoint
**POST**
`{{apiurl}}/v2/lm/configurations`

### HEADER

| KEY | VALUE
| --- | ---
| `AI-Resource-Group` | `tutorial`

### BODY

*(change the highlighted line)*

```JSON[10]
{
  "name": "dev-tutorial-training-configuration",
  "executableId": "text-clf-train-tutorial",
  "scenarioId": "text-clf-tutorial",
  "versionId": "0.0.1",
  "parameterBindings": [],
  "inputArtifactBindings": [
      {
      "key": "text-data",
      "artifactId": "aefde425-231f-44a0-9abf-9228fc1b24c9"
    }
  ]
}
```

### Response

```
{
    'id': '1a10f5fd-27ed-486a-8f6e-4eed061b65d1',
    'message': 'Configuration created'
}    
```

!![ response of configuration creation](img/training/config-response.png)

**IMPORTANT:** This `id` from response needs to be added in the Postman Environment Variable.

| VARIABLE| CURRENT VALUE *(example value)* |
| --- | --- |
| `configurationid` | 1a10f5fd-27ed-486a-8f6e-4eed061b65d1

!![set configurationId to env](img/postman/env-configid.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Start training execution)]

After creating configuration for the execution, it is very simple to create an execution.

> **COLLECTIONS** > *POST* Start execution

### Endpoint

**POST**
`{{apiurl}}/v2/lm/configurations/{{configurationid}}/executions`

### Header

| Key | Value |
| --- | --- |
| AI-Resource-Group | tutorial |

!![start execution png](img/postman/call-start-execu.png)

### Response

```
{'id': ec4e592e024c22e3',
 'message': 'Execution scheduled',
 'status': <Status.UNKNOWN: 'UNKNOWN'>}
```

*(If the response results in 404, read the **Deprecation Notice below** )*

!![response of execution](img/training/execution-response.png)

**IMPORTANT:** This `id` from response needs to be added in the Postman Environment Variable.

| VARIABLE| CURRENT VALUE *(example value)* |
| --- | --- |
| `executionid` | ec4e592e024c22e3

!![set configurationId to env](img/postman/call-set-env.png)

### Deprecation Notice

If the previous call to create training execution does not work try with following endpoint, header, body as follows.

**ENDPOINT**

*POST*
`{{apiurl}}/v2/lm/executions`

**HEADER**

| KEY | VALUE |
| --- | --- |
| `AI-Resource-Group` | `tutorial`

**BODY**

*Change `configurationId` below*
```
{
  "configurationId": "1a10f5fd-27ed-486a-8f6e-4eed061b65d1"
}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](API to Observe Training Status)]

Since the training of the model takes some time, periodically check on the status.

Let's poll the status of the training and wait until it goes into one of the state - `COMPLETED` or `DEAD`.

> **COLLECTIONS** > *GET* Get execution

### Endpoint
**POST**
`{{apiurl}}/v2/lm/executions`

### HEADER

| Key | Value |
| --- | --- |
| `AI-Resource-Group` | `tutorial` |


!![training status](img/postman/call-execu-status-1.png)

**IMPORTANT:** Training the model takes time. Retry sending the request in intervals of 1 min.


**NOTE** In the same response you will get `outputArtifacts > id` deployment of the serving model. These artifacts represents the data (model weights).

!![training status](img/postman/call-execu-status-2.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Store training metrics)]

SAP AI Core, maintains a persistent data related to each execution *(model training)*. This data is custom decided and can be anything.

> **COLLECTIONS** > metrics > *PATCH* > Update/create metric resources (add/modify value of existing ones) against some execution

### Endpoint

*PATCH*
{{apiurl}}/v2/lm/metrics

### Header

| KEY | VALUE |
| --- | --- |
| `AI-Resource-Group` | `tutorial` |
| Content-Type | `application/merge-patch+json` |

### Body

*(example body, manually generated)*

```
{
  "executionId": "ec4e592e024c22e3",
  "metrics": [
    {
      "name": "Error Rate",
      "value": 0.98,
      "timestamp": "2021-06-28T07:50:24.589Z",
      "step": 2,
      "labels": [
        {
          "name": "group",
          "value": "tree-82"
        },
        {
            "name": "metrics.ai.sap.com/Artifact.name",
            "value": "text-model-tutorial"
        }
      ]
    }
  ],
  "tags": [
    {
      "name": "Artifact Group",
      "value": "RFC-1"
    }
  ],
  "customInfo": [
    {
      "name": "Confusion Matrix",
      "value": "[{'Predicted': 'False',  'Actual': 'False','value': 34},{'Predicted': 'False','Actual': 'True',  'value': 124}, {'Predicted': 'True','Actual': 'False','value': 165},{  'Predicted': 'True','Actual': 'True','value': 36}]"
    }
  ]
}
```

**SEND**

### Response

`204` :
*(No Response)*

!![reponse of patch metrics](img/training/metrics-create-response.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Retrive training metrics)]

1. Using SAP AI Launchpad.

    >**IMPORTANT** SAP AI Launchpad is a separate product of SAP. SAP AI Launchpad is not offered with subscription of SAP AI Core and vice versa. SAP AI Launchpad requires a separate subscription.

	!![SAP BTP metrics](img/training/metrics.png)

2. Using Postman API Call.

### Endpoint

**GET**
`{{apiurl}}/v2/lm/metrics?executionIds=ec4e592e024c22e3`

### `Params`

| Key | Value |
| --- | --- |
| `executionId` | `ec4e592e024c22e3` |

### Header

| KEY | VALUE |
| --- | --- |
| `AI-Resource-Group` | `tutorial`

**SEND**

### Response

!![response of get metrics](img/training/metrics-get-response.png)

[DONE]
[ACCORDION-END]
---
