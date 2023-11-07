---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Set Up SAP HANA Cloud for Kyma
description: Learn how to add SAP HANA client and configuration, create an HDI container for an SAP HANA Cloud instance on Cloud Foundry, and create credentials for the Cloud Foundry SAP HANA Cloud instance in your Kyma cluster.
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp\\, kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment)
 - You have a SAP HANA Cloud service instance on Cloud Foundry in your SAP BTP subaccount. You can follow instructions in **Step 3 Create an SAP HANA Cloud service instance** of [Set Up the SAP HANA Cloud Service](btp-app-hana-cloud-setup) to create one if you haven't already.
 - If you'd like to use an existing SAP HANA Cloud service instance from a different SAP BTP subaccount, follow the steps in **Step 2 Use an existing SAP HANA Cloud service instance** of  [Set Up the SAP HANA Cloud Service](btp-app-hana-cloud-setup) to map the HANA Cloud instance to your Cloud Foundry space.


## Details
### You will learn
 - How to create an HDI container for an SAP HANA Cloud service instance on Cloud Foundry and create credentials for the SAP HANA cloud service instance in your Kyma cluster.


---

[ACCORDION-BEGIN [Step 1: ](Overview)]
> ### To earn your badge for the whole mission, you will need to mark all steps in a tutorial as done, including any optional ones that you may have skipped because they are not relevant for you.

Setting up an SAP HANA Cloud service instance directly on Kyma is not possible for trial accounts. Hence, in this tutorial, you will set up an SAP HANA Cloud service instance on Cloud Foundry (or use an existing one). Afterwards, we'll create an HDI container on Cloud Foundry and will use a script in the templates to create credentials for the SAP HANA Cloud service instance in your Kyma cluster.

> For non-trial accounts you can create a mapping from the Kyma Dashboard:

> [Create Service Instances for SAP HANA Cloud](https://cap.cloud.sap/docs/guides/deployment/deploy-to-kyma?q=kyma+hana#hana-cloud-instance) in Capire

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
[ACCORDION-BEGIN [Step 3: ](Set Up SAP HANA Cloud)]
1. Make sure that your HANA instance described in the pre-requisites is running.
2. Make sure you're logged in both your Cloud Foundry space and your Kyma cluster.

    - Cloud Foundry: run `cf login` and provide credentials. As an alternative, you can directly run `cf login --sso`. You'll have to follow a link to get a temporary authentication code that you can provide instead of manually entering credentials.
    - Kyma: you should be already logged in, if you've followed the instructions in `Step 4: Login to your Kyma cluster` of [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment). If you want to double check, run a simple command to see if you get any output. For example, if you run `kubectl get secret`, you should get an output similar to the one in `Step 6: Create container registry secret` of [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment).

3. In the root folder of your project, execute:

    ```Shell/Bash
    ./scripts/create-db-secret.sh cpapp-db
    ```

    What happens here? The script completes several tasks:

    - Creates an HDI container on your already existing SAP HANA Cloud service instance in your Cloud Foundry space. This is represented by an SAP HANA Cloud service instance with plan `hdi-shared`. If you use the name `cpapp` for your project, then this service should be called `cpapp-db`.
    - Creates a service key (`cpapp-db-key` if your HDI container service is called `cpapp-db`) on Cloud Foundry.
    - Creates a secret with the service key's credentials on your Kyma cluster.

> If you get an error, make sure you've added all required entitlements to your subaccount as described in `Step 3: Create a Live Account` of [Prepare for SAP BTP Development](btp-app-kyma-prepare-btp).

[VALIDATE_1]
[ACCORDION-END]
---