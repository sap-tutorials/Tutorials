---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Undeploy Your CAP Application from Kyma
description: This tutorial shows you how to undeploy your CAP application from Kyma.
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp-kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Local Development using VS Code](btp-app-set-up-local-development)
 - [Create a Directory for Development](btp-app-create-directory)
 - [Create a CAP-Based Application](btp-app-create-cap-application)
 - [Create an SAP Fiori Elements-Based UI](btp-app-create-ui-fiori-elements)
 - [Add Business Logic to Your Application](btp-app-cap-business-logic)
 - [Create a UI Using Freestyle SAPUI5](btp-app-create-ui-freestyle-sapui5)
 - [Use a Local Launch Page](btp-app-launchpage)
 - [Implement Roles and Authorization Checks in CAP](btp-app-cap-roles)
 - [Prepare for SAP BTP Development](btp-app-kyma-prepare-btp)
 - [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment)
 - [Set Up SAP HANA Cloud for Kyma](btp-app-kyma-hana-cloud-setup)
 - [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-kyma-prepare-xsuaa)
 - [Add Helm Chart](btp-app-kyma-add-helm-chart)
 - [Deploy Your Application to Kyma](btp-app-kyma-deploy-application)
 - [Add the SAP Launchpad Service](btp-app-kyma-launchpad-service)
 - [Assign a Role Collection to a User](btp-app-kyma-role-assignment)

## Details
### You will learn
 - How to undeploy your CAP application from Kyma


---

[ACCORDION-BEGIN [Step 1: ](Undeploy your CAP application from Kyma)]
1. Uninstall the CAP application:

    ```Shell/Bash
    helm uninstall cpapp
    ```

2. Delete the database secret:

    ```Shell/Bash
    kubectl delete secret cpapp-db
    ```

3. Delete the container registry secret:

    ```Shell/Bash
    kubectl delete secret container-registry
    ```

4. Delete the namespace:

    ```Shell/Bash
    kubectl delete namespace  risk-management
    ```

[VALIDATE_1]
[ACCORDION-END]
---
