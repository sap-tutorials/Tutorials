---
title: Create Cards with Actions to Enable Workflows (Like Approve Or Reject)
description: Implement actions within an SAP Mobile Card to enable workflows like approve or reject.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services ]
time: 30
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---
## Prerequisites
 - **Tutorial:** [Create Cards Automatically with the Automatic Instance Generation Template](cp-mobile-cards-automatic-instance-generation)

## Details
### You will learn
 - How to deploy a new version of an SAP Mobile Card
 - How to implement actions within an SAP Mobile Card
 - How to take an end user's input within an SAP Mobile Card
 - How SAP Mobile Card can be used for Approval workflows

 Using the sample data service that is part of SAP Cloud Platform Mobile Services, you will connect to a system and add an action to approve or reject a sales order.

 Actions allow users to trigger a REST call from a card. This tutorial will change the status of a sales order. After completing the tutorial, you can change the sales order's status from **New** to **Approved** or **Rejected**. This allows you to build simple workflow solutions for the mobile device.

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A sales manager in a company usually needs to log into her computer to Approve or Reject Sales Order that her team has created. Since she is constantly on the move, she wants a way to perform these actions on her Mobile Device.

In this tutorial, you will see how you can quickly mobilize this use-case using SAP Mobile Cards. The Automatic Instance card will show three cards with the sales order information. Each card will have the Approve / Reject action. The sales manager can then select an action on a specific card to approve or reject the respective sales order.

!![MobileCardsImage](img_1.gif)
!![MobileCardsImage](img_2.gif)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new version of the card)]

Please ensure that you have completed the [Automatic Instance Generation tutorial](cp-mobile-cards-automatic-instance-generation) before you proceed with the following steps.

> At any given time, only one version of a card can be productive. You need to create a new version of the card to make any changes. While you are working on a new version, the end-users can keep using the productive version. Once you deploy the new version, the end-user's card will be updated.

Make sure you have logged in to SAP Cloud Platform Mobile Services cockpit. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

!![MobileCardsImage](img_3.png)

Click **Automatic Instance Card** in the Card Templates tab.

!![MobileCardsImage](img_4.png)

Click new **Version** button ![MobileCardsIcon](ico_copy.png) in the Versions table to create a new version.

!![MobileCardsImage](img_5.png)

Click **Yes** on the Confirmation dialog.

!![MobileCardsImage](img_6.png)

Click the **Edit** Button ![MobileCardsIcon](ico_edit.png) for the new version of the card.

!![MobileCardsImage](img_7.png)

Enter the version as **1.1** and click **Save**.

!![MobileCardsImage](img_8.png)

> You will be re-directed to the Card Templates Tab.

Click **Automatic Instance Card** in the Card Templates tab.

!![MobileCardsImage](img_9.png)

A new card with the version `1.1` is created, and the state of this card is Development.

!![MobileCardsImage](img_10.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update the card title)]

Click the **Edit** button for the development `1.1 version` card.

> To ensure you are editing the correct version, please notice the green vertical bar in the Versions Table. This bar indicates the version of the current view on SAP Cloud Platform Mobile Services Cockpit.

!![MobileCardsImage](img_11.png)

Click the **Editor** tab.

!![MobileCardsImage](img_12.png)

Replace `SalesOrder` text with **Action Card** to rename the title of the card.

!![MobileCardsImage](img_13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add Approve Action)]

Click the **Actions** tab.

!![MobileCardsImage](img_14.png)

Add `/` as the XCSRF Token URL.

!![MobileCardsImage](img_16.png)

Click the ![MobileCardsIcon](ico_add.png) icon to add an action and provide the required information:

| Field | Value |
|----|----|
| **Name** | `approve` |
| **Label** | `Approve` |
| **URL** | `/SalesOrderHeaders('${SalesOrderId}')` |
| **Behavior after Action** | `ACTIVE` |
| **HTTP Method** | `PATCH` |
| **Consider Action As** | `Positive` |
| **Action Body** | `{"LifeCycleStatusName": "Accepted", "LifeCycleStatus": "A"}` |

> Making an action active allows the user to perform the action multiple times.

!![MobileCardsImage](img_17.png)

>Here **URL** will call the current `SalesOrder` which the card represents.

>`${SalesOrderId}` defines the placeholder where the current `SalesOrderID` will be put in from the OData JSON response. !![MobileCardsImage](img_18.png)

>**Consider Action As** will assign an icon for the actions on Android.

>**Action Body** will patch the `SalesOrder` status from `New` to `Accepted` if the action is triggered.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add Reject Action)]

Click the ![MobileCardsIcon](ico_add.png) icon to create another action.

!![MobileCardsImage](img_19.png)

> For Reject, we want the Sales Manager to provide a reason. Thus, we will create an **Action Parameter** before we fill the details for the action.

Click ![MobileCardsIcon](ico_add.png) **Parameter** button to create an action parameter and provide the required information:

| Field | Value |
|----|----|
| **Name** | `reasonForRejection` |
| **Label** | `Please provide a reason for rejection` |
| **Data Type** | `Edm.String` |
| **Maximum Length** | `255` |
| **Is Nullable** | `Uncheck` |

!![MobileCardsImage](img_20.png)

> In SAP Mobile Cards, you can allow the user to provide an input while performing an action. This input is stored in an Action Parameter.

Click **Action 2** in the Actions table and provide the required information:

| Field | Value |
|----|----|
| **Name** | `reject` |
| **Label** | `Reject` |
| **URL** | `/SalesOrderHeaders('${SalesOrderId}')` |
| **Behavior after Action** | `ACTIVE` |
| **HTTP Method** | `PATCH` |
| **Consider Action As** | `Negative` |
| **Action Body** | `{"LifeCycleStatusName": "Rejected: ${reasonForRejection}", "LifeCycleStatus": "R"}` |

!![MobileCardsImage](img_21.png)

>We are storing the user's input in the Action Parameter `reasonForRejection`. We then send the value in this parameter as part of the body. Upon successful execution, the card will display the reason entered by the user.

Click **Save** to save the changes made.

!![MobileCardsImage](img_21_1.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy a new version of the Card)]

Click **Automatic Instance Card** in the Card Templates tab.

!![MobileCardsImage](img_22_1.png)

Click the **Info** tab.

!![MobileCardsImage](img_22.png)

In the Versions table, click the ![MobileCardsIcon](ico_check.png) icon to change the state to **Productive**.

!![MobileCardsImage](img_23.png)

Choose **Yes** to confirm.

![MobileCards](img_24.png)

Notice that the **State** of the versions has changed. The newly published version - `1.1` is now in **Productive** state, and the older version - `1.0` is now in *Backup* state.

!![MobileCardsImage](img_25.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Subscribe to the card in SAP Mobile Cards)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

> The SAP Mobile Services client syncs new versions periodically. In the interest of time, you can choose to un-subscribe and subscribe the card to **force the changes immediately**.

[OPTION BEGIN [Android]]

Do a pull refresh.

!![MobileCardsImage](img_26.png)

In the card properties view, notice that the version of the `Automatic Instance Card` is **1.1**.

!![MobileCardsImage](img_27.png)

Tap ![MobileCardsIcon](ico_android_back.png) 'Back' to see the latest cards downloaded on the device and tap ![MobileCardsIcon](ico_android_expand.png) button to expand the card action menu.

> The data is dynamically generated by the sample service on SAP Cloud Platform Mobile Service server. Thus, the data inside the card on your device may be different than what you see in the screenshot.

!![MobileCardsImage](img_29.png)

Tap **Reject** to reject the Sales Order.

!![MobileCardsImage](img_30.png)

> The icons seen here appear because of the `Consider Action As` property.

> Make a note of the `LifeCycleStatus` and Action Card Id.

Enter a reason for Rejection and Tap on **Reject**.

!![MobileCardsImage](img_31.png)

> The user is asked for a reason because we added an action parameter for the reject option.

You will now see 2 toast messages: `Performing Action` and `Performed Action Successfully`.

!![MobileCardsImage](img_32.png)
!![MobileCardsImage](img_33.png)

Tap on the card to open it. Notice, that the Life `LifeCycleStatus` has changed from `New` to `Rejected: Invalid Data`.

!![MobileCardsImage](img_34.png)

Now, let's assume that the necessary action has been taken, and this Sales Order can be approved.

Tap on the **Action Bar** button to view the actions and then tap on **Approve**.

!![MobileCardsImage](img_35.png)

You will now see 2 toast messages: `Performing Action` and `Performed Action Successfully`.

!![MobileCardsImage](img_36.png)
!![MobileCardsImage](img_37.png)

For the same card, `LifeCycleStatus` has changed from `Rejected: Invalid Data` to `Accepted`.

!![MobileCardsImage](img_38.png)

[OPTION END]

[OPTION BEGIN [iOS]]

Do a pull refresh.

!![MobileCardsImage](img_39.png)

In the card properties view, notice that the version of the `Automatic Instance Card` is **1.1**.

!![MobileCardsImage](img_40.png)

Tap New Cards to to see latest cards downloaded on the device.

!![MobileCardsImage](img_42.png)

> The data is dynamically generated by the sample service on SAP Cloud Platform Mobile Service server. Thus, the data inside the card on your device may be different than what you see in the screenshot.

Tap on a card to open it.

!![MobileCardsImage](img_43.png)

> Make a note of the `LifeCycleStatus` and Action Card Id.

Tap on the **Action Menu** button ![MobileCardsIconn](ico_ios_action.png) to view the actions.

!![MobileCardsImage](img_44.png)

Tap **Reject** to reject the Sales Order.

!![MobileCardsImage](img_45.png)

Enter a reason for Rejection and Tap on **Reject**.

!![MobileCardsImage](img_46.png)

> The user is asked for a reason because we added an action parameter for the reject option.

You will now see 2 toast messages: `Performing Action` and `Action Successfully Performed`.

!![MobileCardsImage](img_47.png)
!![MobileCardsImage](img_48.png)

Notice, that the Life `LifeCycleStatus` has changed from `New` to `Rejected: Incorrect Date`.

!![MobileCardsImage](img_49.png)

Now, let's assume that the necessary action has been taken, and this Sales Order can be approved.

Tap on the **Action Menu** button ![MobileCardsIconn](ico_ios_action.png) to view the actions.

!![MobileCardsImage](img_50.png)

Tap **Approve** to reject the Sales Order.

!![MobileCardsImage](img_51.png)

You will now see 2 toast messages: `Performing Action` and `Action Successfully Performed`.

!![MobileCardsImage](img_52.png)
!![MobileCardsImage](img_53.png)

Notice, that the Life `LifeCycleStatus` has changed from `Rejected: Incorrect Date` to `Aceepted`.

!![MobileCardsImage](img_54.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Cross verify status update in the back end)]

In the Mobile Services cockpit, click **Connectivity** under the **Features** tab.

!![MobileCardsImage](img_55.png)

For the `com.sap.edm.sampleservice.v2` sample service, click the **OData destination test** icon ![MobileCardsIconn](ico_odata_test.png).

!![MobileCardsImage](img_56.png)

Click **Next**.

!![MobileCardsImage](img_57.png)

Select `SalesOrderHeaders` as the Metadata Entity Set.

!![MobileCardsImage](img_58.png)

If you cannot find particular data set, click **Entity Properties** to select some properties, then click **OK**.

!![MobileCardsImage](img_59.png)

Verify the `LifeCycleStatus` for the Sales Orders on which you have performed an action.

!![MobileCardsImage](img_60.png)

> In the attached screenshot, Accept & Reject actions have been performed on two different cards.

Congratulations, you have built a work-flow capable SAP Mobile Cards. You can now use this knowledge to build other workflow applications.

[DONE]
[ACCORDION-END]
