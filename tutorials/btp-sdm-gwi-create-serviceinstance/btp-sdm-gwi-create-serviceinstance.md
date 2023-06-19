---
title: Create a Service Instance and Service Key for Document Management Service.
description: Use the free tier or the standard service plan to create a service instance and the associated service key for Document Management Service, Integration Option.
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-hana, software-product>sap-document-management-service ]
primary_tag: software-product>sap-business-technology-platform
author_name: Vikram Kulkarni
author_profile: https://github.com/Vikramkulkarni01
---

## Prerequisites
  - You have created an account on SAP BTP to try out either free tier or standard service plans: [Get an Account on SAP BTP to Try Out Free Tier Service Plans](https://bit.ly/3LYSOBr).
  - You are entitled to use the Document Management Service, Integration Option: [Manage Entitlements Using the Cockpit](https://bit.ly/41B95SU).

### You will learn
  - How to create a service instance of Document Management Service, Integration Option.
  - How to create a service key for your service instance.

### The Use Case:

You'll export table data from SAP S/4HANA Cloud, public edition to Google Sheets. You'll achieve this scenario by integrating SAP S/4HANA with Google Workspace using the SAP Document Management Service.

Here SAP S/4HANA acts as a source system and Google Drive acts as a target system. You'll also create trust between these systems via SAP BTP cockpit.

  ![Use Case](UseCase_Architecture.png)

---

[ACCORDION-BEGIN [Step 1: ](Create a service instance)]

1. Log on to your SAP BTP subaccount. Navigate to **Service Marketplace** and search for **Document Management Service, Integration Option**. Click **Create** to start the service instance creation dialog.

    !![Creation Dialog](CreateDialog.png)

2. In the dialog, choose the **Standard** plan and select the **Space** that you can use. Enter a name for your new instance, for example, `For Demo` and click **Create**.

    !![NewInstanceCreation](NewInstanceCreation.png)

3. In the next **Information** dialog, click on **View Instance** to navigate to the list of your service instances.

    !![ViewInstance](ViewInstance.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a service key)]
You are now able to create a service key for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

1. In the same subaccount, navigate to **Instances and Subscriptions** and choose the demo instance you created in the previous step, then click the **...** dots to open the menu and select **Create Service Key**.

  !![CreateServiceKey](CreateServiceKey.png)

2. In the dialog, enter `My tutorial` as the name of your service key. Click **Create** to create the service key.

    !![NewServiceKey](NewServiceKey.png)

3. You can now view the service key in the browser or download it. Click on the **Service Key** and click **View**.

    !![ViewingKey Image](ViewKey.png)

4. In the credentials view dialog, click on **Form**.

    ![Credential Form details](CredentialsForm.png)

5. Scroll down to copy the values from the following fields and paste them into the secure area (for example, notepad or any text editor).

    1.  **`clientid:`**

    2.  **`clientsecret:`**

    3.  **`url:`**

      !![Details of the Credentials](CredentialDetails.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a role collection)]
  Create a role collection manually. You can skip this step if you already created the role collection and can use it.

1. Navigate to your **Subaccount** in the **SAP BTP Cockpit**.

2. Choose **Security** &rarr; **Role Collections** on the left.

3. Choose the ( &#x2B; ) icon in the top right corner of the screen to create a new role collection.

4. Enter **Name** `SDM_roles`.

5. Choose **Create**.

    !![Create_rolecollections](Create_rolecollections_btp.png)

**The newly added role collection appears now in the list, but it doesn't contain any roles. To add a role:**

1. Choose the **`SDM_roles`** role collection and then choose **Edit**.

    !![Create_rolecollections](Edit_rolecollections.png)

2. Open the value help for **Role Name**.

    !![Role Name](RoleName_Valuehelp.png)

3. Select `SDM_Admin` in list of roles and then choose relevant **`Application Identifier`** from the dropdown.
    >**Note:** The **Application Identifier** should match the second part of your `clientid`.
    For example, if your `clientid` is `tt-d12345cb-6d78-901b-2345-6ab78901a2c3!b456789|xyz-id-abc-s4-test-tutorial-account!b012344` the your **Application Identifier** that you need to select is `xyz-id-abc-s4-test-tutorial-account!b012344`.

    !![Application Identifier](AddRole.png)

    Choose **Add**.

4. Choose **Save**.

    !![Saving Role](Save_Roles.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Test yourself)]

[VALIDATE_6]

[DONE]
[ACCORDION-END]


---
