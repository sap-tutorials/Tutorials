---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Prepare User Authentication and Authorization (XSUAA) Setup
description: This tutorial shows you how to set up User Authentication and Authorization (XSUAA).
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Implement Roles and Authorization Checks In CAP](btp-app-#adding-cap-role-cf-scope-restrictions-to-entities)

## Details
### You will learn
 - How to enable authentication support
 - How to set up user authentication and authorization (XSUAA)

### See Also
 - [Maintenance of Application Security](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/b69c3392aab741e0b0ec4d5c53b2cad5.html)
 - [Application Security Descriptor Configuration Syntax](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/517895a9612241259d6941dbf9ad81cb.html)



To continue with this tutorial you can find the result of the previous tutorial in the [`cp/hana`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/cp/hana) branch.

---

[ACCORDION-BEGIN [Step 1: ](Enable Authentication Support)]

The enable authentication support in CAP for SAP BTP, the `xssec` and `xsenv` modules need to be installed. If `cds watch`is still running stop it with `Ctrl+C`. In your project folder execute:

```bash
npm i --save  @sap/xssec  @sap/xsenv
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Add UAA Service)]

We need to tell CAP that XSUAA is used. For this open the `package.json` in your `cpapp` project and add the following lines:

<!-- cpes-file package.json:$.cds.requires -->
```json [8-11]
{
  "name": "cpapp",
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

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Roles and Scopes)]

In the context of Cloud Foundry, a single authorization is called scope. For example, there could be a scope "Read" and a scope "Write". The scope allows a user to read or write a certain business object. Scopes can't be assigned to users directly. They're packaged into roles. For example, there could a role "Editor" consisting of the "Read" and "Write" scopes, while the role "Viewer" consists only of the "Read" scope.

However, CAP recommends using roles only, and creating one-to-one mappings between roles and scopes. We defined two roles like in section [Authorization](https://cap.cloud.sap/docs/guides/authorization#user-claims) in the CAP documentation.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](XSUAA Security Configuration)]

Create the file `xs-security.json` in your `cpapp` project by executing:

```bash
cds compile srv --to xsuaa >xs-security.json
```

The file contains the configuration of the XSUAA (XS User Authentication and Authorization service).

The CAP server takes the authorization parts `@(restrict ... )` from our service definition form ([Adding CAP Role Restrictions to Entities](btp-app-#adding-cap-role-cf-scope-restrictions-to-entities)) and creates scopes and role templates from it.

For example, it found the roles `RiskViewer` and `RiskManager` in the `srv/risk-service.cds` file:

```javascript [4,8]
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

```json
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


The result of this tutorial can be found in the [`cp/roles`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/cp/roles) branch.

[ACCORDION-END]
---
