---
title: Create Cards With Actions Like Approve Or Reject
description: Learn how to implement actions like approve or reject within a SAP Mobile Card.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards ]
time: 20
---

## Details

### You will learn

 - Learn how to implement actions within a SAP Mobile Card. Using the sample data service part of SAP Cloud Platform Mobile Services, you will connect to a system and learn how you can add an action to approve or reject a sales order.
 - Actions allow users to trigger a REST call from a card. This tutorial will change the status of a Sales Order from the Sample Service. At the end of this tutorial you can change the status from New to approve or reject of a sales order. This allows to build simple workflow solutions for the mobile device.

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A company wants to allow managers to approve sales order on the mobile device. The action card could be of type Automatic Instance Card to load all requests in a certain state. So, the query asks for all items of the sales order which need to be approved. Each item of the result set would show as a single card. The manager can now by selecting an action on the card approve or reject the sales order.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new action card in SAP Cloud Platform Mobile Services)]

Make sure you have logged in to SAP Cloud Platform Mobile Services cockpit. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_000.png)

Click on create icon to create a new card.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_001.png)

Provide the required information as per below.

| Field | Value |
|----|----|
| Name | `ActionCard` |
| Destination | `SAPCPMobileServices` |
| Template Source | choose `Template Manager` |
| HTML Template | choose `Sample Sales Order Template` |

> If you see a pop-up, click OK for the confirmation.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_009.png)

> **Destination** defines the root for the queries which is going to be used for this card.

Navigate to **Sample Data** tab to view the sample JSON response. Copy  `SalesOrderId` parameter.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_010.png)

Click on **Info** tab.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_011.png)

Click on plus icon to add a parameter to the card. This parameter will be used later to build the correct URL so that the action modifies the current `SalesOrder` status.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_012.png)

Provide the required information as per below.

| **Field** | **Value** |
|----|----|
| `sID` | `$.d.SalesOrderId` |


![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_013.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update the default title of this card)]

Click on **Editor**.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_014.png)

Replace `SalesOrder` text with **Action Card** to rename the title of card. This helps later to identify the right card on the device.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_015.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add actions to the card)]

Navigate to **Actions** tab to start adding actions to the card.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_016.png)

Enter below value for **XCSRF Token URL** :

```url
/SampleServices/ESPM.svc/
```
This way the client will know where to get the token from for the actions.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_018.png)

>Default value for behavior for action is **INACTIVE**, inactive action would not allow any new actions after a successful call.

Click on plus icon to add an action.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_019.png)

Provide the required information as per below.

| Field | Value |
|----|----|
| **Name** | `Accept` |
| **Label** | `accept` |
| **URL** | `/SampleServices/ESPM.svc/SalesOrderHeaders('${sID}')` |
| **HTTP Method** | `PATCH` |
| **Action Body** | `{"LifeCycleStatusName": "Accepted", "LifeCycleStatus": "A"}` |

>Here `URL` will is calling the current `SalesOrder` which the card represents. `${sID}` defines the placeholder where the current `SalesOrderID` will be put in from the OData JSON response.

>**Action Body** will patch the `SalesOrder` status from `New` to `Accepted` if the action is triggered.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_020.png)

Now, create a **Request Header** for `Accept` action, so click on  plus icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_021.png)

Provide the required information as per below.

| **Field** | **Value** |
|----|----|
| X-Requested-With | `XMLHttpRequest` |

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_022.png)

Click on plus icon to create one more new action.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_023.png)

Provide the required information as per below.

| Field | Value |
|----|----|
| **Name** | `Reject` |
| **Label** | `reject` |
| **URL** | `/SampleServices/ESPM.svc/UpdateSalesOrderStatus?id='${ID1}'&newStatus='R'`|
| **HTTP Method** | `POST` |

>This `URL` will is calling the OData Function `UpdateSalesOrderStatus` which is implemented on the Sample Service and is passing the current  `SalesOrderID`  in as well as the new Status. This is the second option to change a `SalesOrder` status.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_024.png)

Now, create a **Request Header** for Reject action, so click on plus icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_027.png)

Provide the required information as per below.

| **Field** | **Value** |
|----|----|
| X-Requested-With | `XMLHttpRequest` |

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_028.png)

Click on **Save**.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_029.png)

Click on **No** to allow editing of the card again.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_026.png)


[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Subscribe this card in your Mobile Cards application)]

In SAP Mobile Cards application, click on plus icon to open the subscriptions.

![card](Markdown_files/img_031.png)

Click on `ActionCard` under **All** tab.

![,Chrome Remote Desktop](Markdown_files/img_033.png)

Click **Subscribe** to activate the `ActionCard` subscription.

![,Chrome Remote Desktop](Markdown_files/img_034.png)

Click on any card to open it.

![,Chrome Remote Desktop](Markdown_files/img_035.png)

Here, you can see a preview of cards and then click on **Done**.

![,Chrome Remote Desktop](Markdown_files/img_036.png)

Click on action icon on left bottom to open the available actions on the card.

![,Chrome Remote Desktop](Markdown_files/img_037.png)

Click accept to **accept** the card and trigger a change in status. You will see a successful completion of the rest call.

![,Chrome Remote Desktop](Markdown_files/img_038.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Cross verify status update in the back end)]

In Mobile Services Cockpit, click on **Connectivity** under **Features** tab.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_040.png)

Click on **OData destination test** icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_041.png)

Enter below in relative path and click on **Next**.
```xml
/SampleServices/ESPM.svc
```

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_042.png)

Select `SalesOrderHeaders` entity set from dropdown.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_044.png)

See that the `SalesOrder` has changed the status accordingly.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_045.png)

[DONE]

[ACCORDION-END]
