---
title: Deploy Hyperledger Fabric Chaincode
description: As the fourth part of our "Hello World" example, you will understand how chaincode is managed and learn how to deploy and test it on SAP Cloud Platform
time: 10
auto_validation: true
tags: [ tutorial>beginner, topic>blockchain, products>sap-cloud-platform, topic>cloud]
primary_tag: topic>blockchain
author_name: Brian McKellar
---

## Details
### You will learn
  - An overview of Hyperledger Fabric chaincode, including how it is written and its main functions
  - How to deploy and test Hyperledger Fabric chaincode on SAP Cloud Platform

---

[ACCORDION-BEGIN [Step 1: ](Understanding Chaincode)]

Hyperledger Fabric Chaincode ('smart contracts') control all reading and writing to a Hyperledger Fabric channel and implements all relevant business logic.

![Image depicting overview of chaincode on SAP Cloud Platform](01--Chaincode-Overview.png)

Chaincode is written in GO code and after deployment executed directly on the Hyperledger Fabric peer node (in separate Docker containers). Usually, access to the chaincode is supported by the Hyperledger Fabric SDK via a `HTTP/2 gRPC` interface.

To describe the chaincode functions, a YAML file can be added, that uses `Swagger v2.0` semantics to describe a HTTP REST API onto the different chaincode functions. Based on this YAML description, a HTTP REST API is supported by the gateway on SAP Cloud Platform, allow applications to access chaincode functions via normal REST calls. The manifest file contains deployment information, such as the ID and version number of the chaincode.

![Image depicting overview of chaincode on SAP Cloud Platform](02--Chaincode-Introduction.png)

For this demo, we develop and deploy a simple "Hello World" chaincode that just supports reading and writing text strings to the blockchain against a defined ID

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Opening the Channel Node Dashboard)]

To deploy and test chaincode, click the **Dashboard** icon to navigate to your channel service instance dashboard:

![Image depicting node dashboard on SAP Cloud Platform](03--Node-Dashboard.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Accessing Chaincode Area)]

Once on your channel service instance dashboard, click **Chaincode** on the side menu:

![Image depicting channel service instance dashboard on SAP Cloud Platform](04--Chaincode-Tab.png)

Then, whilst in the chaincode area, click **Example Chaincode** - selecting 'Hello World' when prompted:

![Image depicting chaincode overview on SAP Cloud Platform](05--Chaincode-Area.png)

This opens the Example Chaincode window, giving you access to the manifest, API, and chaincode:

![Image depicting example chaincode window on SAP Cloud Platform](06--Chaincode-Deployed.png)

Enter the `id` of the `chaincode.yaml` file here and click **Submit Answer**

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploying Chaincode)]

With the Example Chaincode window open, click **Deploy Chaincode**:

![Image depicting deploying chaincode on SAP Cloud Platform](07--Chaincode-Tested.png)

The chaincode will now be deployed and displayed in your list of available chaincode:

![Image depicting available chaincode on SAP Cloud Platform](08--Available-Chaincode.png)


[DONE]
[ACCORDION-END]
