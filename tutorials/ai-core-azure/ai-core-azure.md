---
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
keywords: Azure Bucket setup
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>artificial-intelligence, topic>machine-learning, software-product>sap-business-technology-platform, software-product>sap-ai-launchpad, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
parser: v2
---
# Setup Azure Data Bucket with AI Core
<!-- description -->Set up azure bucket (Azure Blob Storage) with SAP AI Core.

## Prerequisites
- An existing Azure account with Admin access to create and use Azure Data store.
- TODO establish the setup prerequisites for SAP AI Core 
![image](img/azure-dashboard.png)

>**IMPORTANT** Before you start this tutorial with SAP AI Launchpad, it is recommended that you should set up at least one other tool, either Postman or Python (SAP AI Core SDK) because some steps of this tutorial cannot be performed with SAP AI Launchpad.

## You will learn
- About Azure data store
- To setup Azure for use with AI core
- Setup Azure data buckets for use with AI Core.

 ## Intro
**What is Azure Data Store?**

Azure Data Store refers to a collection of data storage solutions offered by Microsoft Azure, including Azure Blob Storage, Azure Data Lake Storage, Azure Queue Storage, and Azure Table Storage. These solutions provide scalable, highly available, and secure data storage options for a variety of use cases, such as big data analytics, `NoSQL` databases, content delivery, and message queuing.


### System Setup
For Detailed docs refer : [AI Core Setup](https://developers.sap.com/tutorials/ai-core-setup.html)

- Downloading `config` for Postman

Go to [AI core API](https://api.sap.com/api/AI_CORE_API/overview) and download the JSON File.

![image](img/download-json.png)

- Open Postman and in the Top right corner click on Import.

![image](img/create-collection.png)

- Under file click on Choose file and Select the Json that you just downloaded.

![image](img/import-json.png)

- Once the Import is done you will be able to see the Postman `config`.

![image](img/postman.png)

###  Creating Object store Secret

At Postman at AI core API Go to admin > Object store secret > create object store secret.
And set the following json as body
```Python
{
	"name": "default",
	"type": "azure",
	"pathPrefix": "<path prefix to be appended>",
	"data": {
		"CONTAINER_URI": "https://account_name.blob.core.windows.net/container_name",  
		"REGION": "<region-name>",                  
		"CLIENT_ID": "<azure-client-id>",         
		"CLIENT_SECRET": "<azure-client-secret>",  
		"STORAGE_ACCESS_KEY": "sas_token",          
		"TENANT_ID": "azure tenant id",             
		"SUBSCRIPTION_ID": "subscription id",      
	}
}
```

Once Its done click on Send

![image](img/postman-call.png)

Hence Your Object Store secret for Azure Data buckets is created.
