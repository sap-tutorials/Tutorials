---
title: Assign Roles to Users for Managing Business Rules
description: Assign the SAP Cloud Platform roles required to author, manage and deploy business rules.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud,products>sap-cloud-platform,products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-business-rules
---

## Details
### You will learn
  - How to create role collections and assign it to a user

---
Add the roles required to access the **Manage Rules Project** application to the role collection and assign the role collection to your users.

[ACCORDION-BEGIN [Step 1: ](Create a role collection)]

1. In [SAP Cloud Platform](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial) cockpit, navigate to your subaccount.

    ![Navigate to subaccount](subaccount.png)

2. In the navigation area, under **Security**, choose **Role Collections**.

    ![Role collections](assign-role01.png)

3. Choose **New Role Collection**.

    ![New role collections](assign_role02.png)

4. In the **New Role Collection** window, enter **business-rules** and then choose **Save**.

    ![New role collection1](assign_role03.png)

5. Choose **business-rules** role collection to add roles.

    ![Add Roles](assign-role04.png)

6. Choose **Add Role**.

    ![Add role](assign_role05.png)

7. In the **Add Role** window, choose the following values:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  **Application Identifier**           | **`bpmrulebroker!b2466`**
    |  **Role Template**           | **`RuleRepositorySuperUser`**
    |  **Role**           | **`RuleRepositorySuperUser`**

    Choose **Save**.

    ![Business Rules roles](assign_role06.png)

8. Similarly add the role **`RuleRuntimeSuperUser`** to the application identifier **`bpmrulebroker!b2466`** and then choose **Save**.

    ![Business Rules roles1](assign_role06_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Assign role collection)]

1. Navigate to the subaccount using the breadcrumb navigation.

2. In the navigation area, under **Security**, choose **Trust Configuration** and then choose **SAP ID Service**.

    ![Trust configuration](trust_config01.png)

3. Enter your e-mail address in the **User** input field and then choose **Show Assignments**.

    ![Show assignments](trust_config02.png)

    There is no role collection assigned at this point. If a user is not added to the respective identity provider, add the user by selecting **Add User** in the confirmation dialog box.

    ![Add User](trust_config02_1.png)

4. Choose **Assign Role Collection**.

    ![Assign role collections](trust_config03.png)

5. In the **Assign Role Collection** window, choose **`business-rules`** from the dropdown list and then choose **Assign Role Collection**.

    ![Assign role collections](trust_config04.png)


[VALIDATE_1]
[ACCORDION-END]
