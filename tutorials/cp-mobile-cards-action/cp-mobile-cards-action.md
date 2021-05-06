---
title: Create a Sales Order Approval Card
description: Implement actions within an SAP Mobile Card to enable workflows like approve, reject or reset.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>sap-mobile-cards, products>sap-mobile-services ]
time: 30
author_name: Sandeep T D S
author_profile: https://github.com/sandeep-tds
---

## Prerequisites
- [Completed the starter mission](mission.mobile-cards-get-started) or [Set up Mobile Services on your SAP Business Technology Platform account](cp-mobile-cards-setup)
- [Set up SAP Business Application Studio for Mobile Development](cp-mobile-bas-setup)
- [Created your first card in SAP Business Application Studio](cp-mobile-cards-bas-basic-card)
- **Install SAP Mobile Cards Application:**
   <table><tr><td align="center">!![Play Store QR Code](pre_qr_android.png)<br>Android</td><td align="center">!![App Store QR Code](pre_qr_ios.png)<br>iOS</td></tr></table>

## Details
### You will learn
 - How to create a Card template that shows one card instance for each record.
 - How to work with Query URLs.
 - How to implement actions within an SAP Mobile Card
 - How SAP Mobile Card can be used for Approval workflows

After completing the tutorial, you can reset the sales order's current status to **New** or **Approved** or **Rejected**. This allows you to build simple workflow solutions for the mobile device.

---

[ACCORDION-BEGIN [Step 1: ](Get Familiar With The Real World Use Case)]

A sales manager in a company usually needs to log into her computer to Approve or Reject Sales Order that her team has created. In some cases, she also wants to reset the status of the order from Rejected or Approved back to New. Since she is constantly on the move, she wants a way to perform these actions on her Mobile Device.

In this tutorial, you will see how you can quickly mobilise this use-case using SAP Mobile Cards. The Multi Instance card will show three cards with the sales order information. Each card will have the details of one sales order and the action to accept/reject the order or reset the order status.

>For this tutorial, you will use Mobile Services sample backend to retrieve top 3 sales orders.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create A Sales Order Card Using A Template)]

1. Open your Business Application Studio, and enter your Development space.

    !![Mobile Cards Image](img_2_1.png)

    > If your dev space is not running, click the Play Button - ![Mobile Cards Image](ico_dev_space_run.png) to start it.

    > !![Mobile Cards Image](img_2_1_note.png)

2. In the menu bar, go to View &rarr; Find Command, click Find Command.

    !![Mobile Cards Image](img_2_2.png)

    >  For faster development, you can use the shortcut key.

3. Type *Mobile Cards: New*, and select **Mobile Cards: New From Template**.

    !![Mobile Cards Image](img_2_3.png)

4. Select **Sales Order Approval Card - Multi Instance**.

    !![Mobile Cards Image](img_2_4.png)

5. Enter a name for the card; e.g. **Action Card**.

    !![Mobile Cards Image](img_2_5.png)

6. Open **metadata.json** file from the project explorer.

    !![Mobile Cards Image](img_2_6.png)

    > **Destination** defines the root for the queries to be used for this card.

7. Click on the **Actions** tab, to view the *Approve* and *Reject* actions.

    ![MobileCardsImage](img_2_7.png)

    > Actions allow users to trigger a REST call from a card. Whenever an action is performed, the changes are reflected in the backend and in the mobile card the lifecycle status of the order is changed (according to the action i.e. approve or reject).

    > For reject action, parameter is added through which the user is prompted to add a reason why he/she is rejecting the order.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Adding reset action in the card)]

1. Click the add icon (![MobileCardsIcon](ico_add.png)) to add an action and provide the following information:

    | Field | Value |
    |----|----|
    | **Name** | `reset` |
    | **Label** | `Reset` |
    | **URL** | `/SalesOrderHeaders('${SalesOrderId}')` |
    | **Behavior after Action** | `ACTIVE` |
    | **HTTP Method** | `PATCH` |
    | **Consider Action As** | `Neutral` |
    | **Action Body** | `{"LifeCycleStatusName": "New", "LifeCycleStatus": "N"}` |

    !![MobileCardsImage](img_3_1.png)

    > `${SalesOrderId}` in the URL is a parameter defined in the URLs tab..

    > !![MobileCardsImage](img_3_1_note.png)

    > Making an action active allows the user to perform another action. For details, [click here](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-development-features.html#behavior-after-action).

    > **Consider Action As** assigns an icon for the action seen on the Android device.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy and publish the card)]

1. Open Find Command, search for *Mobile Cards* and select **Mobile Cards: Deploy**.

    !![MobileCardsImage](img_4_1.png)

2. Select **Action Card**.

    !![MobileCardsImage](img_4_2.png)

    > If prompted to enter your username & password, enter the login details you use to login to the Mobile Service cockpit.

    > Your will receive a success message when your card is successfully deployed.

    > !![MobileCardsImage](img_4_2_note.png)

3. Open Find Command, search for *Mobile Cards* and select **Mobile Cards: Publish**.

    !![MobileCardsImage](img_4_3.png)

    > If prompted to enter your username & password, enter the login details you use to login to the Mobile Service cockpit.

    > Your will receive a success message when your card is successfully published.

    > !![MobileCardsImage](img_4_3_note.png)

    > By default, the status of newly created cards is **Development**, and thereby can't be seen on the mobile device. By publishing the card, the card becomes productive and accessible on the device. [Click here](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-managing-cards.html#card-life-cycle) to learn more about the card lifecycle.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Perform actions on your device)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

[OPTION BEGIN [Android]]

1. Perform **Pull Refresh** in the SAP Mobile Cards Android client.

    !![Android Pull Refresh](img_5_and_1.png)

    > The SAP Mobile Services client syncs new versions periodically. In the interest of time, you may choose to un-subscribe and subscribe the card to **force the changes immediately**.

    > !![Android Pull Refresh](img_5_and_1_note.png)

2. Tap expand actions button (![MobileCardsIcon](ico_android_expand.png)) to expand the actions menu.


    !![Android Pull Refresh](img_5_and_2.png)

    > The icons seen here appear because of the `Consider Action As` property.


3. Make a note of the `LifeCycleStatus` and the `SalesOrderId`, and tap **Approve Order**.

    !![Android Pull Refresh](img_5_and_3.png)

    > You will see a toast messages when the action is being performed ![MobileCardsImage](img_5_and_3_note_1.png)

    > and a message when it is completed successfully ![MobileCardsImage](img_5_and_3_note_2.png)

4. **Tap** on the card to open it, and notice the `LifeCycleStatus`.

    !![Android Pull Refresh](img_5_and_4.png)

    > The data is dynamically generated by the sample service on SAP Mobile Service server. Thus, the data inside the card on your device may be different than what you see in the screenshot.

5. Select another card, **tap** on it to open it, and make a note of the `Customer` and `LifeCycleStatus`.

    !![Android Pull Refresh](img_5_and_5.png)

6. **Tap** actions menu (![Android Action Button](ico_and_action.png)), and **tap** **Reject Order**.

    !![Android Pull Refresh](img_5_and_6.png)


7. Enter a reason for Rejection and Tap **Reject Order**.

    !![MobileCardsImage](img_5_and_7.png)

    > The user is asked for a reason because we added an action parameter for the reject option.

8. **Tap** the back icon **&larr;** until you return to the cards list view, and re-open the card and notice the `LifeCycleStatus` for the respective customer.

    !![MobileCardsImage](img_5_and_8.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Perform **Pull Refresh** in the SAP Mobile Cards iOS client.

    !![Card Templates Tab](img_5_ios_1.png)

    > The SAP Mobile Services client syncs new versions periodically. In the interest of time, you may choose to un-subscribe and subscribe the card to **force the changes immediately**.

    > !![Android Pull Refresh](img_5_ios_1_note.png)

2. **Tap** on a card to open it, and make a note of the `Customer` and `LifeCycleStatus`.

    !![Card Templates Tab](img_5_ios_2.png)

3. **Tap** the actions menu (![Android Action Button](ico_ios_action.png)), and **tap** **Approve Order**.

    !![Android Pull Refresh](img_5_ios_3.png)

    > You will see a toast messages when the action is being performed ![MobileCardsImage](img_5_ios_3_note_1.png)

    > and a message when it is completed successfully ![MobileCardsImage](img_5_ios_3_note_2.png)

    > If the lifecycle status of the card isn't updated, click done and perform a pull refresh.

4. Select another card, **tap** on it to open it, and make a note of the `Customer` and `LifeCycleStatus`.

    !![Android Pull Refresh](img_5_ios_4.png)

5. **Tap** the actions menu (![Android Action Button](ico_ios_action.png)), and **tap** **Reject Order**.

    !![Android Pull Refresh](img_5_ios_5.png)

6. Enter a reason for Rejection and Tap **Reject Order**.

    !![MobileCardsImage](img_5_ios_6.png)

    > The user is asked for a reason because we added an action parameter for the reject option.

    > If the lifecycle status of the card isn't updated, click done and perform a pull refresh.

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View changes on the back end)]

1. In the Mobile Services cockpit, click **Mobile Connectivity** under the **Features** tab.

    !![MobileCardsImage](img_6_1.png)

2. For the `com.sap.edm.sampleservice.v2` sample service, click the **Launch in Browser** button ![MobileCardsIconn](ico_odata_browser.png).

    !![MobileCardsImage](img_6_2.png)

3. In the browser window that opens, replace `?auth=uaa` with `/SalesOrderHeaders`.

    !![MobileCardsImage](img_6_3.png)

4. Scan the `LifeCycleStatusName` column to identify records that reflect the actions performed by you.

    !![MobileCardsImage](img_6_4.png)

> Keep this browser window open. It will be used in the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Reset the status for the rejected card)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

Now, let's assume that the necessary action has been taken for the rejected card. We will now reset the status of the card in this step.

[OPTION BEGIN [Android]]

1. Scroll through the cards, and **tap** on the card that was rejected.

    !![Android Pull Refresh](img_7_and_1.png)

2. **Tap** the actions menu (![Android Action Button](ico_and_action.png)), and **tap** **Reset**.

    !![Android Pull Refresh](img_7_and_2.png)

3. **Tap &larr;** until you return to the cards list view, and re-open the card and notice the `LifeCycleStatus`.

        !![MobileCardsImage](img_7_and_3.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Scroll through the cards, and **tap** on the card that was rejected.

    !![Android Pull Refresh](img_7_ios_1.png)

2. **Tap** the actions menu (![Android Action Button](ico_ios_action.png)), and **tap** **Reset**.

    !![Android Pull Refresh](img_7_ios_2.png)

3. Notice the `LifeCycleStatus` after the card is refreshed.

    !![MobileCardsImage](img_7_ios_3.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Verify status on the back end)]

1. Refresh the `com.sap.edm.sampleservice.v2` sample service browser window.

    !![Android Pull Refresh](img_8_1.png)

2. Notice the `LifeCycleStatus` for the record that was previously `Rejected: Incorrect Amount`.

    !![Android Pull Refresh](img_8_2.png)

    > It should now be **New**.

**Congratulations!** You have built an approval card with actions using SAP Mobile Cards.

[DONE]
[ACCORDION-END]
