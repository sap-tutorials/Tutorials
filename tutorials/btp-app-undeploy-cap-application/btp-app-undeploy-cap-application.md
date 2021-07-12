---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Undeploy Your Cloud Foundry Application
description: This tutorial shows you how to undeploy your Cloud Foundry Application.
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Local Development using VS Code](btp-app-set-up-local-development)
 - [Create a Directory for Development](btp-app-create-directory)
 - [Create a CAP-Based Application](btp-app-create-cap-application)
 - [Create an SAP Fiori Elements-Based UI](btp-app-create-ui-fiori-elements)
 - [Add Business Logic to Your Application](btp-app-cap-business-logic)
 - [Create a UI Using Freestyle SAPUI5](btp-app-create-ui-freestyle-sapui5)
 - [Add More Than One Application to the Launch Page](btp-app-launchpage)
 - [Implement Roles and Authorization Checks in CAP](btp-app-cap-roles)
 - [Prepare for SAP BTP Development](btp-app-prepare-btp)
 - [Set Up the SAP HANA Cloud Service](btp-app-hana-cloud-setup)
 - [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-prepare-xsuaa)
 - [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)
 - [Add the SAP Launchpad Service](btp-app-launchpad-service)
 - [Assign a Role Collection to a User](btp-app-role-assignment)
 - [Enable Logging Service for Your Application](btp-app-logging)

## Details
### You will learn

 - How to undeploy your Cloud Foundry application
 

---

[ACCORDION-BEGIN [Step 1: ](Undeploy your Cloud Foundry application)]

[OPTION BEGIN [Delete applications]]

To undeploy (delete) an `mtar`, you don't need to delete the apps and the services individually. The deploy service on Cloud Foundry keeps track on the deployed `mtar` and its resources.

You can either undeploy the applications or undeploy the applications and their service instances. Undeploying the applications and their service instances will also delete the data stored in these instances (for example, database content).

```Shell/Bash
cf undeploy cpapp
```


[OPTION END]
[OPTION BEGIN [Delete applications and service instances]]

To undeploy (delete) an `mtar`, you don't need to delete the apps and the services individually. The deploy service on Cloud Foundry keeps track on the deployed `mtar` and its resources.

You can either undeploy the applications or undeploy the applications and their service instances. Undeploying the applications and their service instances will also delete the data stored in these instances (for example, database content).

> Deletes all data stored in the application.

```Shell/Bash
cf undeploy cpapp --delete-service-keys --delete-services
```

[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Summary)]

Congratulations! You have completed all tutorials. 

[VALIDATE_1]

[ACCORDION-END]
---