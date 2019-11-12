---
title: Create Instance of SAP Cloud Platform Enterprise Messaging Service
description:  Create an instance of SAP Cloud Platform Enterprise Messaging service from SAP Cloud Platform Cockpit and understand in detail about it's each property.  
time: 15
auto_validation: true
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-cloud-platform-enterprise-messaging
---

## Details
### You will learn
  - How to create an SAP Cloud Platform Enterprise Messaging service instance

>**IMPORTANT:** It is really important to learn the basics of messaging before going ahead with this tutorial. Check out [The Basics of Enterprise Messaging](cp-enterprisemessaging-learn-messaging-concepts).


## Prerequisites

  You must fulfill at least one of the following two points to be able to proceed with this tutorial.

  - An account in SAP Cloud Platform (Cloud Foundry) trial environment
  - SAP Cloud Platform (Cloud Foundry) environment with SAP Cloud Platform Enterprise Messaging entitlement  

---


[ACCORDION-BEGIN [Step 1: ](Create an instance)]

1. Open the SAP Cloud Platform cockpit(Cloud Foundry).

2. Navigate to a **Space** in your Cloud Foundry environment and select **Services >  Service Marketplace > Enterprise Messaging Service**.

3. Click **Instances > New Instance**.

4. Select **default** type service plan from the dropdown and click **Next**.

5. Specify parameters of the service descriptor using a JSON file.

    ```JSON
    {
    "emname": "em-instance-name",
    "namespace": "company/myscenario/1",
    "options": {
        "management": true,
        "messagingrest": true,
        "messaging": true
    },
    "rules": {
        "queueRules": {
            "inboundFilter": [
                "${namespace}/#"
            ],
            "outboundFilter": [
                "${namespace}/#"
            ]
        },
        "topicRules": {
            "inboundFilter": [
                "${namespace}/#"
            ],
            "outboundFilter": [
                "${namespace}/#"
            ]
        }
    }
}
    ```



    Service descriptor defines the message client. Each service instance is a message client that contains a name, namespace and a set of rules.

    The following section describes each parameter used in the service descriptor.

    - **emname** - It specifies the name of the message client. Should be unique for a subaccount.

    - **namespace** - The namespace ensures that every message client within a subaccount is unique. The namespace should be provided as a prefix and is not done automatically. The namespace contains exactly 3 segments (max length of 24 characters) with recommended approach as company/product/applicationName.

    - **options** -  It is used to define the access channels for the message client.

    - **rules** -  Defines the access privileges for the message client. In order to allow access to a queue or a topic, the namespace of the corresponding owner message client has to be added

    Click **Next**.

6. Opens another screen. Click **Next**.

7. Enter the instance name and click **Finish**. The instance name has to be the same as has been given against the **`emname`** parameter in the service descriptor.

    An instance of SAP Cloud Platform Enterprise Message is created.

    ![SAP Cloud Platform Enterprise Messaging Instance](em-instance-creation.PNG)

    >Each enterprise message instance represents a message client. Each message client has a set of queues and topics to which it is associated. All these queues and topics belonging to one message client are exposed to other message clients using its unique credentials. This entire set of queues and topics within different message clients in a subaccount can send and receive messages or events to each other using the service.  

Now you can proceed with the tutorial [Create Queues and Queue Subscriptions for Enterprise Messaging](cp-enterprisemessaging-queue-queuesubscription) to create queues and queue subscriptions so you can start working with SAP Cloud Platform Enterprise Messaging.

[VALIDATE_1]


[ACCORDION-END]



---
