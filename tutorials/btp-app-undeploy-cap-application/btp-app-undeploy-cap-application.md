---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Undeploy Your Multi-Target Application (MTA)
description: This tutorial shows you how to undeploy your Multi-Target Application (MTA) from Cloud Foundry.
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Enable Logging Service for Your Application](btp-app-logging)

## Details
### You will learn
 - How to undeploy your Multi-Target Application (MTA)


---

[ACCORDION-BEGIN [Step 1: ](Undeploy your Multi-Target Application (MTA))]
To undeploy (delete) an `mtar`, you don't need to delete the apps and the services individually. The deploy service on Cloud Foundry keeps track on the deployed `mtar` and its resources.

You can undeploy the applications and their service instances. Undeploying the applications and their service instances will also delete the data stored in these instances (for example, database content).

Execute the following command to delete applications and service instances (keep in mind that this will delete all data stored in the application):

```Shell/Bash
cf undeploy cpapp --delete-service-keys --delete-services
```

> Delete applications only.

> Alternatively, you can delete only the applications with the following command:
> ```bash
> cf undeploy cpapp
> ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Summary)]
Congratulations! You have completed all tutorials.

[VALIDATE_1]
[ACCORDION-END]
---
