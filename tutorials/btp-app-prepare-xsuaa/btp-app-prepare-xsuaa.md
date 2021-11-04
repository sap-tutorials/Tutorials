---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Prepare User Authentication and Authorization (XSUAA) Setup
description: This tutorial shows you how to set up User Authentication and Authorization (XSUAA).
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
 - [Add More Than One Application to the Launch Page](btp-app-launchpage)
 - [Implement Roles and Authorization Checks in CAP](btp-app-cap-roles)
 - [Prepare for SAP BTP Development](btp-app-prepare-btp)
 - [Set Up the SAP HANA Cloud Service](btp-app-hana-cloud-setup)

## Details
### You will learn
 - How to enable authentication support
 - How to set up user authentication and authorization (XSUAA)


To start with this tutorial use the result in the [`hana-cloud-setup`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/hana-cloud-setup) branch.

---

[ACCORDION-BEGIN [Step 1: ](Enable authentication support)]
To enable authentication support in CAP for SAP BTP, the `xssec` module needs to be installed. If `cds watch`is still running stop it with <kbd>Ctrl</kbd> + <kbd>C</kbd>. In your project folder execute:

```Shell/Bash
npm i --save  @sap/xssec
```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Add UAA service)]
We need to tell CAP that XSUAA is used. For this open the `package.json` in your `cpapp` project and add the following lines.

<!-- cpes-file package.json:$.cds.requires -->
```JSON[7-10]
{
  ...
  "cds": {
    "requires": {
      "db": {
        "kind": "sql"
      },
      "uaa": {
        "kind": "xsuaa",
        "credentials": {}
      }
    }
  }
}
```

Make sure you have pasted the new lines within the `"requires": {` section and not outside of it. This can cause some errors in the next steps.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Roles and scopes)]
In the context of Cloud Foundry, a single authorization is called scope. For example, there could be a scope "Read" and a scope "Write". The scope allows a user to read or write a certain business object. Scopes can't be assigned to users directly. They're packaged into roles. For example, there could a role "Editor" consisting of the "Read" and "Write" scopes, while the role "Viewer" consists only of the "Read" scope.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](XSUAA security configuration)]
Create the file `xs-security.json` in your `cpapp` project by executing the following command.

```Shell/Bash
cds compile srv --to xsuaa >xs-security.json
```

The file contains the configuration of the XSUAA (XS User Authentication and Authorization service).

The CAP server takes the authorization parts `@(restrict ... )` from our service definition form and creates scopes and role templates from it.

For example, it found the roles `RiskViewer` and `RiskManager` in the `srv/risk-service.cds` file:

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
  "xsappname": "cpapp",
  ...
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
The result of this tutorial can be found in the [`prepare-xsuaa`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/prepare-xsuaa) branch.


[ACCORDION-END]
---