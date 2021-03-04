---
title: Set Up SAP Web Analytics
description: Subscribe to the trial version of SAP Web Analytics on the SAP Business Technology Platform cockpit.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-web-analytics, products>sap-business-technology-platform]
primary_tag: products>sap-web-analytics
author_name: Vikram Kulkarni
author_profile: https://github.com/Vikramkulkarni01
---

## Prerequisites  
[Get a Free Trial Account on SAP BTP](hcp-create-trial-account)

## Details
### You will learn
  - How to subscribe to SAP Web Analytics
  - Who are customer admin and space admin
  - Create a customer admin who can set up SAP Web Analytics


SAP Web Analytics is a software as a service (SaaS) offering on the SAP Business Technology Platform (BTP).

The service lets you collect, report, and analyze the usage data of your website that helps you to identify meaningful patterns from various digital channels. These insights may lead you to implement improvements that can help you optimize the overall user experience of your websites.

A **customer admin** in SAP Web Analytics is an administrator for that particular subscription. For example, the IT department of an organization that subscribes to the service.

As a customer admin for Web Analytics service, within your subscription, you create unique workspaces. In SAP Web Analytics, these workspaces are called **Spaces**.

You can create a space for different departments in your organization. Each space is managed by one or more **space admins**. The space admins can register multiple websites in a space. A customer admin can be a space admin too.

For example, create a space named "E-commerce" to which you register your shopping website for usage tracking.


[ACCORDION-BEGIN [Step 1: ](Open the SAP BTP Cockpit)]

In the [home page](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial) of SAP BTP cockpit trial, select **Enter Your Trial Account**.

!![Cloud Foundry](Step_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a subaccount)]

Skip this step if you already have a trial subaccount.

1. Select **New Subaccount**.

2. Add the following details and select **Create**. If you want to get a flavor of the beta features, enable the checkbox.

    |  Field Name       | Value
    |  :-------------   | :-------------
    |  **Display Name**     | **`Display name of your choice`**
    |  **Provider**         | **`Amazon Web Services (AWS)`**
    |  **Region**           | **`Europe (Frankfurt)`** or **`US East (VA)`**
    |  **Subdomain**        | **`my-org-name`** (provide a generic value for your organization; this value will be prefixed to the URL that SAP Web Analytics provides for the application)

    ![Subaccount](Step_2.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add Entitlements to your Subaccount)]

1. Open your subaccount.
2. In the left pane, select **Entitlements**.
3. Select **Configure Entitlements** and then **Add Service Plans**.
4. From the list of entitlements, select **Web Analytics** and choose an available plan. Select **Add 1 Service Plan**.
5. Select **Save** to save your entitlements.

    ![Entitlements](Step_3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Subscribe to SAP Web Analytics)]

1. Open your subaccount.
2. In the left pane, select **Service Marketplace**.
3. Look out for **Web Analytics** in the list of services. Select the service to open it.
4. Select **Create** to subscribe to the service.

    !![Create](Step4_1.png)

5. Select the **Service** as **`Web Analytics`** and **Plan** as **`standard`** then choose **Create**.
    !![Subscribe](Step5.png)

6. Choose **View Subscription**. You'll see that you are now subscribed.
    !![View Subscription](Step6.png)

    !![Subscribed in Green](Step6.1_Subscribed.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create customer admin role)]

1. Select your subaccount to navigate back to subaccount overview page.
2. In the left pane, select **Security** and then **Role Collections**.
3. Select **Create New Role Collection**.
4. Add the following details and select **Create**.

    |  Field Name       | Value
    |  :-------------   | :-------------
    |  Name             | **My org's SAP Web Analytics customer admin**
    |  Description      | **Customer admin**

    ![New Role](Step_5.1.png)

5. Click the newly created role and select **Edit**.
6. In the **Roles** section, select **Add a Role**.
7. In the **Role Name** list, select **Web Analytics Admin**. If you've subscribed to multiple cloud services, you might see other entries as well in the list.

    ![Add Role](Step_5.2.png)

8. Select **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Assign scope to the customer admin)]

1. Select your subaccount to navigate back to subaccount overview page.
2. In the left pane, select **Security** and then **Trust Configuration**.
3. Select **Default identify provider**. If you've configured your own ID service with SAP BTP, select your custom ID service.
4. Enter your e-mail address (or the attribute that you've configured in the custom ID service) so that you can onboard yourself as the customer admin and select **Show Assignments**.
5. If your e-mail address is not part of the ID service, add the same.
6. Select **Assign Role Collection**.
7. In the **Role Collection** list, select the new role that you created for SAP Web Analytics and select **Assign Role Collection**.

    ![Create Trust Configuration](Step_6.png)


[VALIDATE_1]
[ACCORDION-END]






---
