---
title: Assign Process Visibility Roles to Users
description: Assign required roles of Process Visibility to the users.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud ]
primary_tag: products>sap-cloud-platform
---

## Prerequisites
 - You are assigned to the **User & Role Administrator** role in the **Security** section of your subaccount.

## Details
### You will learn
  - How to create role collections
  - How to assign role collections to your users

Add roles to one or more role collections and then assign these role collections to your users to use various applications of SAP Cloud Platform Process Visibility.

[ACCORDION-BEGIN [Step 1: ](Create a role collection)]

1. Log into the SAP Cloud Platform cockpit.

2. Navigate to your subaccount.

    ![Subaccount](Sub-Account-01.png)

3. From the navigation pane, choose the **Security** tab, then choose **Role Collections**.

    ![Role Collection](Role-Collections-02.png)

4. Click **New Role Collection** to create a role collection for your service.

    ![New Role Collection](New-Role-Collection-03.png)

5. Provide a name and optionally a description to your role collection, then click **Save**.

    ![Role Collection Name](Role-Collection-Name-04.png)

6. Choose the role collection created from the list and click **Add Role**.

    ![Add Role](Add-Role-05.png)

    ![Add Role](Add-Role-06.png)

7. Choose the entry that begins with **`pvreuseservice`** to access the APIs as an application identifier.

    ![Application Identifier](Application-Identifier-07.png)

8. Choose the role template and the corresponding role to be assigned to the users from the dropdown menu, and then click **Save**.

In this tutorial, assign the role templates **`PVAdmin`**, **`PVOperator`**, **`PVDeveloper`**, **`PVEventSender`** by following the steps from **Step 6**. For more information about these roles, refer to [Process Visibility Roles](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/e395bfade9c64d89922c561c4b92979f.html).

![Admin Role](PV-Admin-08.png)

![Operator Role](PVOperator-09.png)

![Developer Role](Developer-10.png)

![Event Sender Role](Event-Sender-11.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Assign role collections to users)]

1. Navigate to your subaccount in the SAP Cloud Platform cockpit.

    ![Subaccount](Sub-Account-01.png)

2. In the navigation pane, under **Security**, choose **Trust Configuration**.

    ![Trust Configuration](Trust-configuration-13.png)

3. Choose the name of your identity provider from the list of available identity providers.

    ![Identifier](Identifier-14.png)

4. Enter the email address of the user to wish you to assign the role collection into the **E-Mail Address** field, then choose **Show Assignments**.

    ![Show Assignments](Show-Assignments-15.png)

    If a user is not added to the respective identity provider, add the user by selecting **Add User** in the confirmation dialog box.

    ![Add User](Add-User-16.png)

5. Choose **Assign Role Collection**. In the new window, select the desired role collection, and click **Assign Role Collection**.

    ![Assign Role Collection](Assign-Role-Collection-17.png)

    ![Assign Role Collection](Assign-Role-Collection-18.png)

Keep editing the role assignments according to your requirements.

[VALIDATE_1]
[ACCORDION-END]


---
