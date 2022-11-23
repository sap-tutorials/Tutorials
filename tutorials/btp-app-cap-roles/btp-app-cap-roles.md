---
parser: v2
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
keywords: cap
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Implement Roles and Authorization Checks In CAP
<!-- description --> This tutorial shows you how to enable authentication and authorization for your CAP application.

## Prerequisites
 - Before you start with this tutorial, you have two options:
  - Follow the instructions in **Step 16: Start from an example branch** of [Prepare Your Development Environment for CAP](btp-app-prepare-dev-environment-cap) to checkout the [`launchpage`](btp-app-launchpage) branch.
    - Complete the previous tutorial [Use a Local Launch Page](btp-app-launchpage) with all its prerequisites.


## You will learn
 - How to add role restrictions to entities
 - How to add a local user for testing
 - How to access the application with a user and password

---

### Adding CAP role restrictions to entities

1. Open the file `srv/risk-service.cds`.

2. Add the following restrictions block (`@(...)`) to your `Risks` and `Mitigations` entities.

<!-- cpes-file srv/risk-service.cds -->
```[4-13,15-24]
using { sap.ui.riskmanagement as my } from '../db/schema';
@path: 'service/risk'
service RiskService {
  entity Risks @(restrict : [
            {
                grant : [ 'READ' ],
                to : [ 'RiskViewer' ]
            },
            {
                grant : [ '*' ],
                to : [ 'RiskManager' ]
            }
        ]) as projection on my.Risks;
    annotate Risks with @odata.draft.enabled;
  entity Mitigations @(restrict : [
            {
                grant : [ 'READ' ],
                to : [ 'RiskViewer' ]
            },
            {
                grant : [ '*' ],
                to : [ 'RiskManager' ]
            }
        ]) as projection on my.Mitigations;
    annotate Mitigations with @odata.draft.enabled;
}
```

With this change, a user with the role `RiskViewer` can view risks and mitigations, and a user with role `RiskManager` can view and change risks and mitigations.

---
### Add Users for local testing

Since the authorization checks have been added to the CAP model, they apply not only when deployed to the cloud but also for local testing. Therefore, we need a way to log in to the application locally.

CAP offers a possibility to add local users for testing as part of the `cds` configuration. In this tutorial, we use the `.cdsrc.json` file to add the users.

1. Copy the file `templates/cap-roles/.cdsrc.json` to your project directory `cpapp`. If you are asked to replace an existing file with the same name, confirm.

    > You have to make hidden files visible in your operating system in order to see the file.

    The file defines two users `risk.viewer@tester.sap.com` and `risk.manager@tester.sap.com`.

2. Let's look at the `risk.manager@tester.sap.com` example.

    <!-- cpes-file .cdsrc.json:$.*.*.*.users[?(@.ID=="risk.manager@tester.sap.com")] -->
    ```JSON[7-14]
    {
      "[development]": {
        "auth": {
          "passport": {
            ...
            "users": {
              "risk.viewer@tester.sap.com": "...",
              "risk.manager@tester.sap.com": {
                "password": "initial",
                "ID": "risk.manager@tester.sap.com",
                "roles": [
                  "RiskManager"
                ]
              }
            }
          }
        }
      }
    }
    ```

    The user is defined by their `ID`, which happens to be the email address here, but it could also be a user ID. The user has an `email`, a `password` parameter, and a `roles` parameter. Keep in mind that the CAP roles and the Cloud Foundry roles and scopes are not the same thing.

---
### Access the Risks application with password

When accessing the `Risks` service in your browser, you get a basic auth popup now, asking for your user and password. You can use the two users to log in and see that it works.

1. With `cds watch` running, go to <http://localhost:4004/launchpage.html>.

2. Choose **Risks** and choose **Go**.

3. Enter **Username**: `risk.manager@tester.sap.com`.

4. Enter **Password**: `initial`.

<!-- border -->![Sign In Risk Application](role_risks_management.png)

  You can now access the `Risks` application.

<!-- border -->![Access Risk Application](risks_management_application.png)

> Currently there's no logout functionality. You can clear your browser's cache or simply close all browser windows to get rid of the login data in your browser. For Google Chrome, restart your browser (complete shutdown and restart) by entering `chrome://restart` in the address line.

The result of this tutorial can be found in the [`cap-roles`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/cap-roles) branch.
---
