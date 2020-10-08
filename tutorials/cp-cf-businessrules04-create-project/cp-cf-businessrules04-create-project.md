---
title: Create a Project to Author Rules
description: Create a business rules project to determine the equipment for a new employee based on employee details.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud, topic>cloud,products>sap-cloud-platform,products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-business-rules
author_name: Vandana Vasudevan
author_profile: https://github.com/VandanaVasudevan
---
## Prerequisites
 - Ensure that you setup the **Workflow Management** service to access the **Manage Rule Projects** application. For more information, see [Set Up Workflow Management in Cloud Cockpit](https://developers.sap.com/tutorials/cp-starter-ibpm-employeeonboarding-1-setup.html).

## Details
### You will learn
  - How to create a project to author rules

SAP Cloud Platform Business Rules is an Intelligent Business Process Management service that lets you digitize and automate decision making. You can author and manage your decisions using the **Manage Rule Projects** application of business rules service.

A project is used to configure and manage the entities of business rules. Entities of business rules include data objects, rules, rule sets, and rule services. You can create projects in the **Manage Rule Projects** application, which can be accessed from the **Workflow Management** home screen. For more information, see [SAP Cloud Platform Business Rules](https://help.sap.com/viewer/product/BUSINESS_RULES/Cloud/en-US).

[ACCORDION-BEGIN [Step 1: ](Open Manage Rules Project application)]

1. Log on to **Workflow Management** home screen, and choose **Manage Rule Projects** application tile.

    ![Workflow Management Home Screen](workflow_management_hs.png)

2. In the **Manage Projects** screen, choose +.

    ![Manage Rule Project Application](create_project0.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enter details of the project)]

Enter the following details of the project:

|  Field Name     | Value
|  :------------- | :-------------
|  Name           | **`DetermineEquipment01`**
|  Label           | **`DetermineEquipment01`**
|  Description    | **`Business rules to determine equipment for the new hire.`**
|  System            | **`SAP Cloud Platform`**
|  Expression Language    | **`2.0`**

**Label** and **Description** are a mandatory fields. **Description** field does not support special characters.

![Project Details](create_project1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Save your project)]

Choose **Save**.

![Save the project](create_project2.png)

[VALIDATE_1]

[ACCORDION-END]
