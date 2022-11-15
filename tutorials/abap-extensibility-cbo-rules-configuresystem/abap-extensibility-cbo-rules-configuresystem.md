---
parser: v2
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, topic>abap-extensibility, software-product>sap-s-4hana-cloud]
primary_tag: topic>abap-extensibility
---

# Configure System in SAP Cloud Platform Business Rules
<!-- description --> Add and configure the system based on the SAP Cloud Platform destination in SAP Cloud Platform Business Rules in order to create and deploy rules.

## Prerequisites
  - User must have enabled Business Rules service in SAP Cloud Platform account.
  - User must have **`RuleSuperUser`** role assigned to repository and runtime configurations.


## You will learn
  - How to configure system in SAP Cloud Platform Business Rules

## Intro
SAP Cloud Platform Business Rules is a platform re-use service that encapsulates high change decision logic from application logic. Integrating business rules with S/4HANA is one of the extensibility capability to facilitate customisation opportunities and business process adaptations. With **S/4HANA Cloud 1811** release, you can author, manage and deploy your rules directly to S/4HANA system from SAP Cloud Platform.  

---

### Add system


1. Open **Manage Business Rules** application of your SAP Cloud Platform Business Rules.

    ![open manage rules application](ruleeditor.png)

2. Click the **Configure Systems** view icon (![open configuration](eye.png)).

3. Click **+** button to configure S/4HANA cloud system.

    ![add new system](addsystem.png)


### Configure system


1. Enter the following configuration details:

    - **Name** of your choice

    - **Description** is optional

    - **Destination** is the name of the destination created in previous tutorial

2. **Save** the system configuration details.

    ![configure and save system](configuresystem.png)

3. Click the pencil icon (![open rules](pencil.png) ) on the vertical toolbar to switch back to **Manage Rules Application**.


### Test yourself



