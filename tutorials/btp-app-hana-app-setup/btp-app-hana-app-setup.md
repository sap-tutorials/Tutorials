---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Add SAP HANA Support to Your Project
description: This tutorial shows you how to set up the SAP HANA Cloud service instance.
keywords: cap
auto_validation: true
time: 25
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-hana-cloud]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Prepare for SAP BTP Development](btp-app-prepare-btp)


## Details
### You will learn
 - How to add SAP HANA client to your project


---

[ACCORDION-BEGIN [Step 1: ](Add SAP HANA client and configuration to your project)]
1. Run the following command in your project root folder:

    ```Shell/Bash
    cds add hana --for production
    ```

    This adds the `hdb` module for SAP HANA access to your `package.json` file and will configure kind `sql` as the database service.

    Changes in the `package.json` file done by `cds add hana`:

    <!-- cpes-file package.json:$.cds -->
    ```JSON[4-13]
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

    Different to CAP default behavior, kind `sql` uses the SQLite in-memory database for local testing and the SAP HANA database for productive usage in the cloud.

    > Additional Documentation:

    > - [Using Databases](https://cap.cloud.sap/docs/guides/databases#get-hana)
    > - [CAP Configuration](https://cap.cloud.sap/docs/node.js/cds-env)

2. Run `cds watch` in your project folder and check that your app still works locally as it used to work before at <http://localhost:4004/>.

[VALIDATE_1]
[ACCORDION-END]
---