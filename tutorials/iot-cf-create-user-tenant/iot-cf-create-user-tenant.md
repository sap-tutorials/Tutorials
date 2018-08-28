---
title: Create User and Tenant
description: Create a user and tenant in the Internet of Things Service Cockpit.
auto_validation: true
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things, topic>cloud ]
---

<!-- loio027ae38c64974f4ea914bdfe5b323702 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Enable SAP Cloud Platform Internet of Things for Cloud Foundry](https://www.sap.com/developer/tutorials/iot-cf-enable-iot-service.html).


## Details
### You will learn
- How to create a user and a tenant in the Internet of Things Service Cockpit

### Time to Complete
10 min

---

[ACCORDION-BEGIN [Step 1: ](Create a User)]

> Note:
> You need an instance of the SAP Cloud Platform Internet of Things Service. You need the host name (referred to as `<HOST_NAME>`, which you can use to access the Internet of Things Service Cockpit. In addition, you get the instance owner credentials (`name`, `password`).
>
>

1.  Log on to the Internet of Things Service Cockpit with the instance owner credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Users* section of the *User Management* category.

3.  Choose **+** above the *Users* list.

4.  In the *General Information* section, enter a *Name* and a *Password* for the user.

    > Note:
    > The user name must start with an alphanumeric character and should not contain more than 40 characters. Allowed characters are lowercase letters, decimal digits, hyphen, period, and underscore.
    >
    >
    > Note:
    > The password must contain at least six characters, including numbers, uppercase, and lowercase letters.
    >
    >

5.  Choose *Create*.

    You get a notification that the user was successfully created.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Tenant)]

The multi-tenancy feature of the Internet of Things Service allows you to create multiple tenants on one instance. Each tenant can have one tenant owner (user) that can assign users.

1.  Use the main menu to navigate to the *Tenants* section of the *User Management* category.

2.  Choose **+** above the *Tenants* list.

3.  In the *General Information* section, enter a *Name* for the tenant.

4.  (Optional) In the *Custom Properties* section, add custom properties for this tenant.

5.  Choose *Create*.

    You get a notification that the tenant was successfully created.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Assign Users to a Tenant)]

1.  Use the main menu to navigate to the *Tenants* section of the *User Management* category.

2.  Search and select the tenant you want to assign to a user.

3.  In the *Users* section, choose **+** above the table of the user you want to add.

4.  In the *Add User* dialog, enter the *User ID* of the user you want to add.

    > Note:
    > You can find the *ID* of the user you want to add by navigating to the *Users* section of the *User Management* category.
    Search and select the user you want to add. The details page displays the *Name* and the *ID* for the user selected. Copy the *ID*.
    >
    >

5.  Select an appropriate *Role* for the user from the dropdown box.

6.  Confirm the dialog by choosing *Add*.

[VALIDATE_2]

[ACCORDION-END]
