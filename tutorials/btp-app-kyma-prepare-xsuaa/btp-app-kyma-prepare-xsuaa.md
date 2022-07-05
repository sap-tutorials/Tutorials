---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Prepare User Authentication and Authorization (XSUAA) Setup
description: This tutorial shows you how to set up User Authentication and Authorization (XSUAA).
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model

---

[ACCORDION-BEGIN [Step 1: ](Prerequisites)]
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
 - [Setup HANA Cloud for Kyma](btp-app-kyma-hana-cloud-setup)

## Details
### You will learn

 - How to set up user authentication and authorization (XSUAA)

---


## Setup XSUAA

Run the following command in your project folder:

```Shell/Bash
cds add xsuaa --for production
```

What happens here? Running `cds add xsuaa` does two things:

- Adds the XSUAA service to the `package.json` file of your project
- Creates the XSUAA security configuration for your project

### Adding the XSUAA service

Check if the following lines have been added to the `package.json` in your `cpapp` project:

<!-- cpes-file package.json:$.cds.requires -->
```JSON[7-9]
{
  ...
  "cds": {
    "requires": {
      "[production]": {
        ...
        "auth": {
          "kind": "xsuaa"
        }
      }
    }
  }
}
```

### Roles and scopes

A scope represents a single authorization to perform an action. For example, there could be a scope "Read" and a scope "Write". The scope allows a user to read or write a certain business object. Scopes can't be assigned to users directly. They're packaged into roles. For example, there could a role "Editor" consisting of the "Read" and "Write" scopes, while the role "Viewer" consists only of the "Read" scope.

### XSUAA security configuration

Check the file `xs-security.json` that was created in your `cpapp` project. The file contains the configuration of the XSUAA (XS User Authentication and Authorization service). The CAP server takes the authorization parts `@(restrict ... )` from our service definition form and creates scopes and role templates from it. For example, it found the roles `RiskViewer` and `RiskManager` in the `srv/risk-service.cds` file:

```JavaScript[4,8]
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
```

And created scopes and roles for both in the `xs-security.json` file:

```JSON
{
  "scopes": [
    {
      "name": "$XSAPPNAME.RiskViewer",
      "description": "Risk Viewer"
    },
    {
      "name": "$XSAPPNAME.RiskManager",
      "description": "Risk Manager"
    }
  ],
  "role-templates": [
    {
      "name": "RiskViewer",
      "description": "Risk Viewer",
      "scope-references": [
        "$XSAPPNAME.RiskViewer"
      ],
      "attribute-references": []
    },
    {
      "name": "RiskManager",
      "description": "Risk Manager",
      "scope-references": [
        "$XSAPPNAME.RiskManager"
      ],
      "attribute-references": []
    }
  ]
}
```

[VALIDATE_1]

[VALIDATE_1]
[ACCORDION-END]
---
