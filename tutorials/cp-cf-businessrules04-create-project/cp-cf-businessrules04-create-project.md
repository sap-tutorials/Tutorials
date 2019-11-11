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
 - You have accessed the **Manage Rules Project** application. For more information, see [Set up the Manage Rules Project Application](cp-cf-businessrules03-setup-mrp).

## Details
### You will learn
  - How to create a project to author rules

A project is used to configure and manage the entities of business rules. It contains data objects, rules, rule sets, and rule services. You can create projects in the **Manage Rules Project** application.

[ACCORDION-BEGIN [Step 1: ](Open Manage Rules Project application)]

 Open the **Manage Rules Project** application. In the **Manage Rules Project** screen, choose +.

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
