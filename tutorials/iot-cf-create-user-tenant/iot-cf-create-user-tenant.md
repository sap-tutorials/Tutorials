---
title: Create User and Tenant
description: Create a user and tenant in the Internet of Things Service Cockpit.
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things ]
---

<!-- loio027ae38c64974f4ea914bdfe5b323702 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Enable the SAP Cloud Platform Internet of Things Service for the Cloud Foundry Environment].


## Details
### You will learn
- How to create a user and a tenant in the Internet of Things Service Cockpit of the SAP Cloud Platform Internet of Things Service for the Cloud Foundry environment.

### Time to Complete
10 min

---

[ACCORDION-BEGIN [Step 1: ](Create a User)]

> Note:
> You need an instance of the SAP Cloud Platform Internet of Things Service. You need the host name (referred to as `<HOST_NAME>`, which you can use to access the Internet of Things Service Cockpit. In addition, you get the instance owner credentials (`name`, `password`).
>
>

1.  Log on to the SAP Cloud Platform Internet of Things Service with the instance owner credentials.

```bash
https://<HOST_NAME>/iot/cockpit/
```

2.  Use the main menu to navigate to the *Users* section of the *User Management* category.

3.  Choose **+** above the *Users* list.

4.  In the *General Information* section, enter a *Name* and a *Password* for the user.

    > Note:
    > The user name must start with an alphanumeric character and should not contain more than 40 characters. Allowed characters are lowercase letters, decimal digits, hyphen, period, and underscore.
    >
    > The password must contain at least six characters, including numbers, uppercase, and lowercase letters.
    >
    >

5.  Choose *Create*.

    You get a notification that the user was successfully created.

    > Note:
    > It is currently not possible to edit all properties of existing users in the Internet of Things Service Cockpit. Instead, you can use the API of your SAP Cloud Platform Internet of Things Service account. For more information, please refer to the [SAP Cloud Platform Internet of Things Service](https://help.sap.com/viewer/9a8cae62b9ab4278af1f39e188b11bc7/Cloud/en-US) documentation or the [Internet of Things API Service](https://help.sap.com/viewer/6040fec3f22e4f9b8bf495f3789d66b5/Cloud/en-US) documentation.
    >
    >


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Tenant)]

The multi-tenancy feature of the Internet of Things Service allows you to create multiple tenants on one instance. Each tenant can have one tenant owner (user) that can assign users.

1.  Log on to the SAP Cloud Platform Internet of Things Service with the instance owner credentials.

```bash
https://<HOST_NAME>/iot/cockpit/
```

2.  Use the main menu to navigate to the *Tenants* section of the *User Management* category.

3.  Choose **+** above the *Tenants* list.

4.  In the *General Information* section, enter a *Name* for the tenant.

5.  Choose *Create*.

    You get a notification that the tenant was successfully created.

    > Note:
    > It is currently not possible to edit all properties of existing tenants in the Internet of Things Service Cockpit. Instead, you can use the API of your SAP Cloud Platform Internet of Things Service account. For more information, please refer to the [Internet of Things Service Cockpit](https://help.sap.com/viewer/9a8cae62b9ab4278af1f39e188b11bc7/Cloud/en-US) documentation or the [Internet of Things API Service](https://help.sap.com/viewer/6040fec3f22e4f9b8bf495f3789d66b5/Cloud/en-US) documentation.
    >
    >


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Optional: Assign Multiple Users to a Tenant)]

1.  Log on to the Internet of Things Service Cockpit with the instance owner credentials.

```bash
https://<HOST_NAME>/iot/cockpit/
```

2.  Use the main menu to navigate to the *Tenants* section of the *User Management* category.

3.  Search and select the tenant.

4.  In the *Users* section, choose **+** above the table to assign a user.

5.  In the dialog *Add User* add the *User ID* of the user you want to add.

    > Note:
    > You can find the *ID* of the user you want to add by navigating to the *Users* section of the *User Management* category.
    >
    > -   Search and select the user you want to add.
    >
    > -   The details page displays the *Name* and the *ID* for the user selected.
    >
    > -   Copy the *ID*.
    >
    >

6.  Select an appropriate *Role* for the user from the dropdown box.

7.  Confirm the dialog by choosing *Add*.

    > Note:
    > It is currently not possible to edit assigned tenant users in the Internet of Things Service Cockpit. Instead, you can use the API of your SAP Cloud Platform Internet of Things Service account. For more information, please refer to the [Internet of Things API Service](https://help.sap.com/viewer/6040fec3f22e4f9b8bf495f3789d66b5/Cloud/en-US) documentation.
    >
    >


[ACCORDION-END]
