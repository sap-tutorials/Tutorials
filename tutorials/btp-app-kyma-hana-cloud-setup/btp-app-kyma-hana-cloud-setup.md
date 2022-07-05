---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Setup SAP HANA Cloud for Kyma
description: Learn how to add SAP HANA client and configuration, create an HDI container for a SAP HANA Cloud instance on Cloud Foundry, and create credentials for the Cloud Foundry SAP HANA Cloud instance in your Kyma cluster.
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori]
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
 - You have a SAP HANA Cloud instance on Cloud Foundry in your SAP BTP subaccount. You can follow the steps in [Create an SAP HANA Cloud service instance](btp-app-hana-cloud-setup-#create-an-sap-hana-cloud-service-instance) to create one if you haven't already.
 - If you'd like to use an existing SAP HANA Cloud instance from a different SAP BTP subaccount, follow the steps in [Use an existing SAP HANA Cloud service instance](btp-app-#use-an-existing-sap-hana-cloud-service-instance) to map the HANA Cloud instance to your Cloud Foundry space.

## Details
### You will learn
 - How to create an HDI container for a SAP HANA Cloud instance on Cloud Foundry and create credentials for the SAP HANA Cloud instance in your Kyma cluster.


---

[ACCORDION-BEGIN [Step 1: ](Overview)]
Setting up a SAP HANA Cloud instance directly on Kyma is not possible. Hence, in this tutorial, we will setup a SAP HANA Cloud instance on Cloud Foundry (or use an existing one). Afterwards, we'll create an HDI container on Cloud Foundry and will use a script in the templates to create credentials for the Cloud Foundry SAP HANA Cloud instance in your Kyma cluster.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Add SAP HANA client and configuration to your project)]
1. Add SAP HANA support to your project by running:

    ```Shell/Bash
    cds add hana --for production
    ```

    This adds the `db` module for SAP HANA access to your `package.json` file.

2. Check the changes in the `package.json` file done by `cds add hana`:

    <!-- cpes-file package.json:$.cds -->
    ```JSON[4-15]
    {
        "name": "cpapp",
        ...
        "cds": {
            "requires": {
                "[production]": {
                    "db": {
                        "kind": "hana-cloud"
                    }
                },
                "db": {
                    "kind": "sql"
                }
            }
        }
    }
    ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Setup HANA Cloud)]
1. Make sure you're logged in both your Cloud Foundry space and your Kyma cluster.

    - Cloud Foundry: run `cf login` and provide credentials. As an alternative, you can directly run `cf login --sso`. You'll have to follow a link to get a temporary authentication code that you can provide instead of manually entering credentials.
    - Kyma: you should be already logged in, if you've followed the instructions in [Login to your Kyma cluster](btp-app-#login-to-your-kyma-cluster). If you want to double check, run a simple command to see if you get any output. For example, if you run `kubectl get secret`, you should get an output similar to the one in Step 3 of [Create container registry secret](btp-app-#create-container-registry-secret).

2. Copy the folder `scripts` from `templates/kyma-add-helm-chart` to your project root folder.

2. In your project root folder, execute:

    ```
    ./scripts/create-db-secret.sh cpapp-db
    ```

    What happens here? The script completes several tasks:

    - Creates a HDI container on your already existing SAP HANA database instance in your Cloud Foundry space. This is represented by a SAP HANA service instance with plan `hdi-shared`. If you use the name `cpapp` for your project, then this service should be called `cpapp-db`.
    - Creates a service key (`cpapp-db-key` if your HDI container service is called `cpapp-bd`) on Cloud Foundry.
    - Creates a secret with the service key's credentials on your Kyma cluster.

> If you get an error, make sure you've added all required entitlements to your subaccount as described in [Create a Live Account](btp-app-#create-a-live-account).

[VALIDATE_1]
[ACCORDION-END]
---
