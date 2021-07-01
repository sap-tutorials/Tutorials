---
title: Connect an ABAP System with SAP Cloud Platform Using a Secure Tunnel (Neo)
description: Set up your SAP Cloud Platform account (Neo) and the Cloud Connector to establish a secure tunnel between SAP Cloud Platform and the Application Server ABAP in your system landscape.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, products>sap-cloud-platform, products>sap-cloud-platform-connectivity, topic>abap-connectivity ]
time: 15
---

## Prerequisites  
  - [Install the Cloud Connector in your System Landscape](https://developers.sap.com/tutorials/cp-connectivity-install-cloud-connector.html)
  - [Get a Free Trial Account on SAP Cloud Platform (Neo)](https://developers.sap.com/tutorials/hcp-create-trial-account.html)

## Details
### You will learn  
- How to connect the Cloud Connector to your trial Neo account on SAP Cloud Platform
- How to connect the Cloud Connector to your ABAP system

---

[ACCORDION-BEGIN [Step 1: ](Connect Cloud Connector with trial subaccount)]

Before you can access data from the Cloud Connector in an application on SAP Cloud Platform (Neo), you must establish a trust between your SAP Cloud Platform subaccount and the Cloud Connector that is installed in your system landscape.

1. Go to [Your SAP Cloud Platform Trial](https://account.hanatrial.ondemand.com/cockpit) | **Neo Trial**, and navigate to your subaccount.

1. Copy your technical subaccount name from the title to a local text editor:

    ![Technical subaccount name](step-01-Find-Trial-ID-001.png)

1. Open your user information in the upper right corner:

    ![User Information](step-01-Find-Trial-ID-002.png)

2. Copy your user ID from your user information to a local text editor:

    ![User ID](step-01-Find-Trial-ID-003.png)

1. Log on to the Cloud Connector administration UI and choose **Connector** | **Define Subaccount** (if you already have defined a subaccount for another purpose, choose **Connector** | **Add Subaccount**). Enter the following information:

    |  Field Name            | Value                                                                   |
    |:-----------------------|:------------------------------------------------------------------------|
    |  **Region**            | `hanatrial.ondemand.com`                                                |
    |  **Subaccount**        | The technical subaccount name you copied earlier                        |
    |  **Display Name**      | This will be displayed in the Cloud Connector administration UI         |
    |  **Subaccount User**   | The user ID you copied earlier.                                         |
    |  **Password**          | Password of your subaccount user                                        |
    |  **Location ID**       | not required in this tutorial                                           |

1. Choose `Save`.

    ![Save cloud configuration](step-01-Configure-Cloud-001.png)

1. After a while you should get the following success message:

    ![Save cloud configuration](step-01-Configure-Cloud-002.png)
    > If your internal landscape is protected by a firewall that blocks any outgoing TCP traffic, the connection will only work using an HTTPS proxy. For more information see section [Set up Connection Parameters and HTTPS Proxy](https://help.sap.com/viewer/cca91383641e40ffbe03bdc78f00f681/Cloud/en-US/db9170a7d97610148537d5a84bf79ba2.html#loiodb9170a7d97610148537d5a84bf79ba2__configure_proxy) in the official documentation.

[VALIDATE_2]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Connect Cloud Connector with ABAP system)]
Access from any application on SAP Cloud Platform to resources on your ABAP system are provided by mapping the IP address of your ABAP system ( **Internal Host** ) to a **Virtual Host**. You provide this mapping and further attributes relevant for the connection in the **`ACCESS CONTROL`**. After the access control is set up you can use the virtual host on SAP Cloud platform to allow applications to connect to services on your ABAP system.

In this step we do not provide any password information. It will have to be provided later on SAP Cloud Platform when you create a destination for this host.

1. In the Cloud Connector Administration UI, expand the name of your subaccount and choose **`Cloud To On-Premise`**. Above table **`Mapping Virtual To Internal System`** choose the plus sign ( **`Add`** ).

    ![Access-Control-OP](step-03-Configure-OP-Connection-001.png)

1. Choose `ABAP System` as **`Back-end Type`** and choose **`Next`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-002.png)

1. Choose `HTTP` as **`Protocol`** and choose **`Next`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-003.png)

1. Enter the internal host and port of your ABAP system and choose **`Next`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-004.png)
    > To be sure that this information is correct, you could call your ABAP service in a Web browser first. If this works you can take the host address and port from the link you used.

1. Enter a **`Virtual Host`** and a **`Virtual Port`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-005.png)

    >These values can be arbitrary, simply choose something that makes sense for you but do not copy the     values of your internal host to hide this information outside your network.


1. Choose `None` as **`Principal Type`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-006.png)

1. Choose **`Next`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-007.png)

1. Choose **`Check Internal Host`**. This will check the connection to your ABAP system after you choose **`Finish`**.

    ![Access-Control-OP](step-03-Configure-OP-Connection-008.png)

1. If the ABAP system is not reachable check if your internal host is correct. Otherwise you should see something like this:

    ![Access-Control-OP](step-03-Configure-OP-Connection-009.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Allow access to ABAP resources)]

Before applications on SAP Cloud Platform can access any services of the ABAP system you must specify the corresponding paths in table **`Resources Accessible On <your virtual host>:<port>`**.

1. In your access control for your ABAP system you created in the last step, choose the plus sign ( **`Add`** ).

    ![Access-Control-OP](step-03-Configure-OP-Connection-010.png)

1. If your service is an OData service, add the following resource and choose **`Save`**.

    | Field Name                     | Value                              |
    |:-------------------------------|:-----------------------------------|
    | **`URL Path`**                 | `/sap/opu/odata`                   |
    | **`Enabled`**                  | (checked)                          |
    | **`Access Policy`**            | **`Path and all sub-paths`**       |

    ![Access-Control-OP](step-03-Configure-OP-Connection-011.png)
    > Path `/sap/opu/odata` is the basic resource path to be able to access the output of the OData service. You can add more paths later, for example, if you would like to allow access to related images stored in the ABAP system.

1. Your configuration should now look like this:

    ![Access-Control-OP](step-03-Configure-OP-Connection-012.png)

As a result, you now have configured a secure tunnel between your ABAP system and your Neo subaccount on SAP Cloud Platform.  

[VALIDATE_3]

[ACCORDION-END]




---
