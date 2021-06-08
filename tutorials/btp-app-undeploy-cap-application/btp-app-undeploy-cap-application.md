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
 - [Add the Application Logging Service](btp-app-logging)

## Details
### You will learn
 - How to undeploy your Cloud Foundry application


---

[ACCORDION-BEGIN [Step 1: ](Undeploy Your Cloud Foundry Application)]

[OPTION BEGIN [Delete applications]]

To undeploy (delete) an `mtar`, you don't need to delete the apps and the services individually. The deploy service on Cloud Foundry keeps track on the deployed `mtar` and its resources.

You can either undeploy the applications or undeploy the applications and their service instances. Undeploying the applications and their service instances will also delete the data stored in these instances (for example, database content).

```bash
cf undeploy cpapp
```


[OPTION END]
[OPTION BEGIN [Delete applications and service instances]]

To undeploy (delete) an `mtar`, you don't need to delete the apps and the services individually. The deploy service on Cloud Foundry keeps track on the deployed `mtar` and its resources.

You can either undeploy the applications or undeploy the applications and their service instances. Undeploying the applications and their service instances will also delete the data stored in these instances (for example, database content).

> _Deletes all data stored in the application._

```bash
cf undeploy cpapp --delete-service-keys --delete-services
```

[VALIDATE_1]

[OPTION END]


[ACCORDION-END]
---
