---
title: Prepare Portal Environment for Creating Sites on SAP Cloud Platform on Cloud Foundry
description: To get started with building a Portal site, administrators must perform the required onboarding steps.
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-cloud-platform, products>cloud, products>sap-fiori]
primary_tag: products>sap-cloud-platform-portal
---

## Prerequisites
  - You've created a SAP Cloud Platform trial account in the Cloud Foundry environment.
  - You can also use a subaccount in your SAP Cloud Platform global account. If you don't have a subaccount in the Cloud Foundry environment, refer to this topic: [Onboarding](https://help.sap.com/viewer/ad4b9f0b14b0458cad9bd27bf435637d/Cloud/en-US/fd79b232967545569d1ae4d8f691016b.html).



## Details
### You will learn
  - How to subscribe to the Portal Service
  - How to assign users to the `Super_Admin` role so that they can design Portal sites
  - How to access the Portal service

In this group of tutorials our goal is to create an attractive Portal site using the SAP Cloud Platform Portal service. But before we can do this, there are some preparation steps that you need to do.


[ACCORDION-BEGIN [Step 1: ](Subscribe to the Portal service)]

Before you can access the Portal service, you need to first subscribe to it.

1. Click on your trial subaccount or the subaccount you created in the SAP Cloud Platform cockpit.

2. Click **Subscriptions** from the side menu.

    ![Click Subscriptions](01_click_subscriptions.png)

    The Portal service is unsubscribed by default.

3. Click the Portal service.

    ![Click Portal tile](02_unsubscribed_portal.png)

4. Click **Subscribe** and wait for the status to change to **Subscribed**.

    ![Subscribe to the Portal](03_subscribe.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Add yourself to the Super_Admin role)]

To be able to do administrative tasks in the Portal you must be assigned to the `Super_Admin` role. In this step, you will first create a role collection and then you'll assign yourself to the `Super_Admin` role.

1. Click on your subaccount again.

2. Click **Security > Role Collections** from the side menu.

    ![Open Role Collections](04_role_collections.png)

3.  Click **New Role Collection** and then name your role collection `Administrator`. Then click **Save**.

    ![Name role collection](05_create_role_collection.png)

4. Click your role collection to open the **Roles** screen and then click **Add Role**.

    ![Add role](06_add_role.png)

5. Select the following values and then click **Save**:

    >Note below that the **Application Identifier** has an ID at the end.  This ID may be different on your system -- just make sure that you choose the value with this format: **portal-cf-service!`<id>`**.

    |  Property     | Value
    |  :------------- | :-------------
    |  Application Identifier           | **`portal-cf-service!<id>`**
    |  Role Template           | **Super_Admin**
    |  Role    | **Super_Admin**

    >Note that the **Application Identifier** has an id at the end.  This id may be different on your system - just make sure that you choose the value with this format: **portal-cf-service!`<id>`**.

    ![Add role properties](07_add_role_properties.png)

6. Go back to your subaccount (you can use the breadcrumbs at the top of your screen).

7. Click **Security > Trust Configuration** from the side menu.

    ![Select Trust Configuration](08_trust_configuration.png)

8. Click the `SAP ID Service`.

9. Enter your email address and then click **Show Assignments**.

    ![Show Assignments](09_show_assignments.png)

10. Click **Assign Role Collection**.  In the dialog box that opens, select the `Administrator` role collection that you defined above and then click **Assign Role Collection**.

    ![Assign Role Collection](10_assign_role_collection.png)

You have now been assigned to the `Super_Admin` role and you can access the Portal service and carry out all your admin tasks.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access the Portal service)]

You are now ready to access the Portal service.  

1. Click on your subaccount.

2. Click **Subscriptions** from the side panel.

    ![Open Subscriptions](2_click_subscriptions.png)

    You'll see that you are now subscribed to the Portal service.

3. Click **Go to Application** on the **Portal** tile.

    ![Access Portal Service](3_access_portal_service.png)

4. Add your credentials if you are prompted to do so.

   The Portal service opens with the Site Directory in focus. This is where you create and manage the sites that you create for this subaccount.

  ![Open Site Directory](4_open_site_directory.png)


[VALIDATE_6]

[ACCORDION-END]
