---
title: Create Resource Group and Connect AWS S3 to SAP AI Core (Postman)
description: Learn creation of resource group in SAP AI Core to enable multi-tenancy. Store datasets to AWS S3 and connect to SAP AI Core through Postman client.
auto_validation: true
time: 15
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

## Details
### You will learn
  - How to enable multi-tenancy with resource group.
  - Create AWS S3 connection to SAP AI Core

---

[ACCORDION-BEGIN [Step 1: ](Create resource group)]


Resource groups represent a virtual collection of related resources within the scope of one SAP AI Core tenant.

> **COLLECTIONS** > admin > *POST* Create `resourcegroup`

### Endpoint
**POST**
`{{apiurl}}/v2/admin/resourceGroups`

### Body

```
{
  "resourceGroupId": "tutorial"
}
```

!![create resourcegroup](img/postman/call-resourcegroup.png)

**SEND**

### Response
```
{
  'resource_group_id': 'tutorial',
  'tenant_id': '1111-dddd-444-888-888888',
  'zone_id': ''
}
```
The value of `tenant_id` is equal to the value of `identityzoneid` from service key of SAP AI Core.

>**IMPORTANT:** The `create resource group` request results in `Response: 202`, which means the backend server will take time(~30 sec) to create the group. List the resource group(*see below*) to see the status of creation

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](API to list resource groups)]

> **COLLECTIONS** > admin > *GET* List `resourcegroup`

### Endpoint
**GET**
`{{apiurl}}/v2/admin/resourceGroups`

**SEND**

!![list resource groups](img/postman/list-resourcegroup.png)

### Response

```
{'count': 1,
 'resources': [{
  'resource_group_id': 'tutorial',
  'status': 'PROVISIONED',
  'status_message': 'All onboarding steps are completed.',
  'tenant_id': '1111-dddd-444-888-888888',
  'zone_id': ''
  ]}
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Manage AWS S3 Object Store)]

Use AWS S3 Object Store as a cloud storage for your datasets and models. You can get AWS S3 Bucket from either of two ways:

- Through SAP BTP Cockpit.

- Through AWS. Refer [AWS User Guide to S3](https://docs.aws.amazon.com/AmazonS3/latest/userguide/create-bucket-overview.html)

Follow the below steps for creating a path prefix through AWS CLI:

1. Install AWS CLI for your platform (Mac/ Linux/ Windows). [Instructions here](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html).

2. Check for `version` after installation completes, execute the following on terminal *(command prompt)*

    ```BASH
    aws --version
    ```

    !![aws version check](img/aws/aws-version.PNG)

3. Execute the following on the terminal *(command prompt)*

    ```BASH
    aws configure
    ```

4. Enter your the AWS S3 Object Store details. You can leave the `Default output format` entry as blank *(press enter)*.

    !![aws configure](img/aws/aws-configure.PNG)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Upload dataset to AWS S3 Object Store)]


| File   | Link |
|  :------------- | :------------- |
|  `travel.csv` | [Download Here](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-resource/travel.csv) |

 1. Replace `your-bucket-id` and execute the following on the terminal *(command prompt)* to create a path prefix(directory) and upload you datafile at the same time.

    ```BASH[1]
    aws s3 cp /local/path/to/travel.csv s3://your-bucket-id/tutorial/data/
    ```

2. Check your file. Replace `your-bucket-id` and execute the following on the terminal.

    ```BASH[1]
    aws s3 ls s3://your-bucket-id/tutorial/data/
    ```

    !![check dataset using aws cli](img/aws/check.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](API to connect S3 Object Store to SAP AI Core)]

Object Stores are connected to the resource groups, hence ensure you have the resource group created before proceeding.

Get your AWS S3 Credentials in the format below.

>  In case you are using the SAP Object Store, get the content from `SAP BTP cockpit > SAP BTP subaccount > Instances and Subscriptions > Instances > Credentials `.

```JSON
{
  "access_key_id": "ASDFASDFASDFASDF",
  "bucket": "asd-11111111-2222-3333-4444-55555555555",
  "secret_access_key": "asdfASDFqwerQWERasdfQWER",
  "host": "s3.amazonaws.com",
  "region": "us-east-1",
  "uri": "s3://ASDFASDFASDFASDF:asdfASDFqwerQWERasdfQWER@s3.amazonaws.com/asd-11111111-2222-3333-4444-55555555555",
  "username": "asd-s3-123456-78910-aaa3-dddd-asdf12345"
}
```

Make the API call.

> **COLLECTIONS** > admin > *POST* create `objectstoresecret`

### Endpoint
**POST**
`{{apiurl}}/v2/admin/objectStoreSecrets`

### Body
Create the body using above s3 credentials with following key-value, **(other key-value take from example body below)**

| S3 Key | BODY Key | *Example Value*|
| --- | -- | --- |
| bucket | bucket | asd-11111111-2222-3333-4444-55555555555
| `access_key_id` | `data` > `AWS_ACCESS_KEY_ID` | ASDFASDFASDFASDF
| `secret_access_key` | `data` > `AWS_SECRET_ACCESS_KEY` | `asdfASDFqwerQWERasdfQWER`
| `regiion` | region | us-east-1
| `host` | endpoint | s3.amazonaws.com

Example Body

Edit the highlighted lines.
```JSON[4, 5, 7, 9, 10]
{
  "name": "default",
  "type": "S3",
  "bucket": "asd-11111111-2222-3333-4444-55555555555",
  "endpoint": "s3.amazonaws.com",
  "pathPrefix": "tutorial",
  "region": "us-east-1",
  "data": {
  "AWS_ACCESS_KEY_ID": "ASDFASDFASDFASDF",
  "AWS_SECRET_ACCESS_KEY": "asdfASDFqwerQWERasdfQWER"
  }
}
```
**DONT SEND YET! You will require to set header as well**
!![s3 body](img/postman/s3-1.png)


**HEADER**

|KEY | VALUE |
| --- | --- |
| AI-Resource-Group | tutorial |


!![s3 header set](img/postman/S3-2.png)

**SEND**

### Response

```
{'message': 'secret has been created'}
```

This will connect your object store. The connection will be named `default`.

[VALIDATE_1]
[ACCORDION-END]

---
