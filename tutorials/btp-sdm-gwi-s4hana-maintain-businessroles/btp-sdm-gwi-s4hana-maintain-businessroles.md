---
title: Maintain Business Roles and Restrictions in SAP S/4HANA Cloud, public edition
description: In this tutorial, you create a business role to access the application.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud, software-product>sap-s-4hana-cloud, software-product>sap-s-4hana-cloud-front-end]
primary_tag: software-product>sap-s-4hana-cloud
author_name: Vikram Kulkarni
author_profile: https://github.com/Vikramkulkarni01

---

## Prerequisites
 - You've access to SAP S/4HANA Cloud, public edition with admin privileges.


### You will learn
  - How to create a new business role
  - How to maintain restrictions

---

[ACCORDION-BEGIN [Step 1: ](Create business role)]

1. Logon to your SAP S/4HANA Cloud, public edition. Search for **Maintain Business Roles** app.

    !![Selecting App](Select_Maintain_Business_Roles.png)

2. Click on **New** at the bottom of the screen.

    !![Create New Role](Click_New.png)

3. In the **New Business Role** window, enter the following details:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Business Role ID | **`FOR_TUTORIAL`**
    |  Business Role Description | **`For tutorial purposes`**

      !![Enter_Business_Role_Details](Enter_Business_Role_Details.jpg)

4. Choose **Create**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add business catalog)]

1. Navigate to **Assigned Business Catalogs** tab and select **Add**.

    !![Assigned_Business_Catalog](Assigned_Business_Catalog.png)

2. Search for `file management`, select `File Management - My Files` and click **Apply** and **OK**.

    !![Look for file management](Search for File management.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Maintain restrictions)]

1. In the same business role, select **Maintain Restrictions**.

    !![Maintain_Restrictions](Maintain_Restrictions.png)

2. Select **Access Categories** from the left-side navigation menu and maintain the following values:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Write, Read, Value Help | **`Restricted`**
    |  Read, Value Help | **`Restricted`**

    !![Accessing Categories from left side menu](Access_Categories.png)

3. Select **Assigned Restriction Types** and choose `File Share Content`.

    !![Access Restriction type](Assigned Restriction Type.png)

4. In the **Restrictions and Values** section, enter the following details.  

    |  Field Name     | Value
    |  :------------- | :-------------
    |  File Share Type | **`2`**
    |  File Share ID | **`ZTEST`**
    |  File Share Content Group | **`Unrestricted Access`**

    >**Remember**: During the process of creating a communication system, you maintained **File Share ID** value. Please make sure the same **File Share ID** is being used.

    File Share Type

      !![File_Share_Type](File_Share_Type.png)

    File Share ID

      !![File_Share_ID](File_Share_ID.png)

    File Share Content Group  

      !![File_Share_Content](File_Share_Content Group.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add business user)]

1. Select **Assigned Business Users** and click **Add**.

    !![AddbusinessUser](Add business user.png)

2. Search for your **User Name**, select it and click **Apply** and **OK**.
    >A username that you have maintained. Please select the valid username you have used in both SAP BTP and S/4HANA Cloud, public edition.

    !![Select Username](Select Username.png)

3. Choose **Save**.

    !![Save](Save.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Test yourself)]

  [VALIDATE_6]

[DONE]
[ACCORDION-END]

---
