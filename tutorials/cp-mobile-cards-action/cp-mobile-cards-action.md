---
title: Create Cards with Actions (Like Approve Or Reject)
description: Implement actions within an SAP Mobile Card, like approve or reject.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards ]
time: 20
---

## Details
### You will learn
 - How to implement actions within a SAP Mobile Card

 Using the sample data service that is part of SAP Cloud Platform Mobile Services, you will connect to a system and add an action to approve or reject a sales order.

 Actions allow users to trigger a REST call from a card. This tutorial will change the status of a sales order from the sample service. After completing the tutorial, you can change the sales order's status from **New** to **Approved** or **Rejected**. This allows you to build simple workflow solutions for the mobile device.

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A company wants to allow managers to approve sales order on a mobile device.

The action card could be of type Automatic Instance Card to load all requests in a certain state. So, the query asks for all items of the sales order that need to be approved. Each item of the result set would show as a single card. The manager can then select an action on the card and approve or reject the sales order.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new action card)]

Make sure you have logged into SAP Cloud Platform Mobile Services cockpit. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_000.png)

Click the **Create a New Card** icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_001.png)

Provide the required information as per below.

| Field | Value |
|----|----|
| **Name** | `ActionCard` |
| **Destination** | `SAPCPMobileServices` |
| **Template Source** | `Template Manager` |
| **HTML Template** | `Sample Sales Order Template` |

> If you see a pop-up, click OK.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_009.png)

> **Destination** defines the root for the queries that are going to be used for this card.

Navigate to the **Sample Data** tab to view the sample JSON response. Copy the `SalesOrderId` parameter.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_010.png)

Click the **Info** tab.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_011.png)

Click on the **+** icon to add a parameter to the card. This parameter will be used later to build the correct URL so that the action modifies the current `SalesOrder` status.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_012.png)

Provide the required information as per below.

| **Field** | **Value** |
|----|----|
| **`sID`** | `$.d.SalesOrderId` |


![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_013.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update the card's title)]

Click **Editor**.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_014.png)

Replace `SalesOrder` text with **Action Card** to rename the title of card. This helps later to identify the right card on the device.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_015.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add actions to the card)]

Navigate to the **Actions** tab to start adding actions to the card.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_016.png)

Enter the following value for **XCSRF Token URL**:

```URL
/SampleServices/ESPM.svc/
```
This way the client will know where to get the token from for the actions.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_018.png)

>The default value for behavior for an action is **INACTIVE**. An inactive action does not allow any new actions after a successful call.

Click the **+** icon to add an action.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_019.png)

Provide the required information:

| Field | Value |
|----|----|
| **Name** | `Accept` |
| **Label** | `accept` |
| **URL** | `/SampleServices/ESPM.svc/SalesOrderHeaders('${sID}')` |
| **HTTP Method** | `PATCH` |
| **Action Body** | `{"LifeCycleStatusName": "Accepted", "LifeCycleStatus": "A"}` |

>Here `URL` will call the current `SalesOrder` which the card represents. `${sID}` defines the placeholder where the current `SalesOrderID` will be put in from the OData JSON response.

>**Action Body** will patch the `SalesOrder` status from `New` to `Accepted` if the action is triggered.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_020.png)

Now, create a **Request Header** for the `Accept` action. CLick the **+** icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_021.png)

Provide the required information:

| **Field** | **Value** |
|----|----|
| **X-Requested-With** | `XMLHttpRequest` |

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_022.png)

Click the **+** icon to create another action.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_023.png)

Provide the required information:

| Field | Value |
|----|----|
| **Name** | `Reject` |
| **Label** | `reject` |
| **URL** | `/SampleServices/ESPM.svc/UpdateSalesOrderStatus?id='${ID1}'&newStatus='R'`|
| **HTTP Method** | `POST` |

>This `URL` will call the OData function `UpdateSalesOrderStatus`, which is implemented on the sample service and passes the current  `SalesOrderID` as well as the new status. This is the second option to change a `SalesOrder` status.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_024.png)

Now, create a **Request Header** for the `Reject` action. Click the **+** icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_027.png)

Provide the required information:

| Field | Value |
|----|----|
| **X-Requested-With** | `XMLHttpRequest` |

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_028.png)

Click **Save**.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_029.png)

Click **No** to allow editing of the card again.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_026.png)


[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Subscribe to the card in SAP Mobile Cards)]

In the SAP Mobile Cards application, click the **+** icon to open the subscriptions.

![Card](Markdown_files/img_031.png)

Click `ActionCard` under the **All** tab.

![Chrome Remote Desktop](Markdown_files/img_033.png)

Click **Subscribe** to activate the `ActionCard` subscription.

![Chrome Remote Desktop](Markdown_files/img_034.png)

Click any card to open it.

![Chrome Remote Desktop](Markdown_files/img_035.png)

Here, you can see a preview of the cards. Click **Done**.

![Chrome Remote Desktop](Markdown_files/img_036.png)

Click the action icon at the bottom-left to open the available actions on the card.

![Chrome Remote Desktop](Markdown_files/img_037.png)

Click **accept** to accept the card and trigger a change in status. You will see a successful completion of the REST call.

![Chrome Remote Desktop](Markdown_files/img_038.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Cross verify status update in the back end)]

In the Mobile Services cockpit, click **Connectivity** under the **Features** tab.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_040.png)

Click the **OData destination test** icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_041.png)

Enter the following for the relative path and click **Next**:

```XML
/SampleServices/ESPM.svc
```

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_042.png)

Select the `SalesOrderHeaders` entity set from the dropdown.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_044.png)

See that the `SalesOrder` has changed the status accordingly.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_045.png)

[DONE]

[ACCORDION-END]
