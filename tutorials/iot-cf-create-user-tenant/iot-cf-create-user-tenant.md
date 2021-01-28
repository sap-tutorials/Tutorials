---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Create User and Tenant
description: Create a user and tenant in the Internet of Things Service Cockpit.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Enable SAP Cloud Platform Internet of Things for Cloud Foundry](iot-cf-enable-iot-service).


## Details
### You will learn
- How to create a user and a tenant in the Internet of Things Service Cockpit

### Time to Complete
10 min

## Next Steps
- **Tutorials:** [Install the Paho Client](iot-cf-install-paho-client)

---

[ACCORDION-BEGIN [Step 1: ](Create a User)]

>You need an instance of the SAP Cloud Platform Internet of Things Service. You need the `<HOST_NAME>` and the `<INSTANCE_ID`, which you can use to access the system. In addition, you need the instance owner credentials (`name`, `password`).

1.  Log on to the Internet of Things Service Cockpit with the instance owner credentials:

    `https://<HOST_NAME>/<INSTANCE_ID>/iot/cockpit/`

2.  Use the main menu to navigate to the **Users** section of the **User Management** category.

3.  Choose **+** above the **Users** list.

4.  In the **General Information** section, enter a **Name** and a **Password** for the user.

    >The user name must start with an alphanumeric character and should not contain more than 40 characters. Allowed characters are lowercase letters, decimal digits, hyphen, period, and underscore.
    >
    >The password must contain at least six characters, including numbers, uppercase, and lowercase letters.
    >
    >Optional: In the Custom Properties section, add custom properties to the user.

5.  Choose **Create**.

    You get a notification that the user was successfully created.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Tenant)]

The multi-tenancy feature of the Internet of Things Service allows you to create multiple tenants on one instance. Each tenant can have one tenant owner (user) that can assign users.

1.  Use the main menu to navigate to the **Tenants** section of the **User Management** category.

2.  Choose **+** above the **Tenants** list.

3.  In the **General Information** section, enter a **Name** for the tenant.

4.  (Optional) In the **Custom Properties** section, add custom properties for this tenant.

5.  Choose **Create**.

    You get a notification that the tenant was successfully created.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Assign Users to a Tenant)]

1.  Use the main menu to navigate to the **Tenants** section of the **User Management** category.

2.  Search and select the tenant you want to assign to a user.

3.  In the **Members** section, choose **+** above the table of the user you want to add.

4.  In the **Add Member** dialog, enter the **Name** of the user you want to add.

    >You can find the **Name** of the user you want to add by navigating to the **Users** section of the **User Management** category.
    Search and select the user you want to add. The details page displays the **Name** and the **ID** for the user selected. Copy the **Name**.

5.  Select an appropriate **Role** for the member.

6.  Confirm by choosing **Add**.

[VALIDATE_2]

[ACCORDION-END]
