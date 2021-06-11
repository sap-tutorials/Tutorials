---
title: Deploy Your Application with Access Control (SAP HANA Cloud)
description: Define security and enable user authentication and authorization for your SAP HANA Cloud CAP application before deploying it to SAP BTP.
time: 30
auto_validation: true
tags: [ tutorial>intermediate, products>sap-hana, software-product-function>sap-cloud-application-programming-model, products>sap-business-application-studio]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- This tutorial is designed for SAP HANA Cloud. It is not intended for SAP HANA on-premise or SAP HANA, express edition.
- You have created database artifacts and loaded data, as explained in [the previous tutorial](hana-cloud-cap-add-list-report).

## Details
### You will learn
  - How to incorporate security into the routing endpoint of your application
  - How to configure Cloud Application Programming (CAP) service authentication
  - How to make the current project deployable to SAP BTP

We will set up production level security using the SAP Authorization and Trust Management service for SAP BTP in the Cloud Foundry environment and, more specifically, the User Account and Authorization or UAA Service.

---

[ACCORDION-BEGIN [Step 1: ](Extend the .gitignore file)]

In the first step, we need to extend the ignore list of `git`. This will help us to keep the version control clean of any build artifacts that we'll create in this tutorial.

```gitignore
dist/
resources/
```

!![gitignore](gitignore.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 1: ](Turn authentication on)]

Open the `srv/interaction_srv.cds` file. You need to add `@requires: 'authenticated-user'` to the service definition. Authentication and scopes can also be applied at the individual entity level.

```CDS [3]
using app.interactions from '../db/interactions';

@requires: 'authenticated-user'
service CatalogService {

 entity Interactions_Header
	as projection on interactions.Interactions_Header;

 entity Interactions_Items
	as projection on  interactions.Interactions_Items;

}
```

!![auth_turn_on](auth_turn_on.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 1: ](Make the CAP app deployment-ready)]

The `svr` is not deployable now as it doesn't include any information about the needed runtime environment. Let's add this information with a simple `package.json` file that will tell the Cloud Foundry runtime that this is a Node.js module. The file also declared the needed dependencies and the configuration parameters that the app requires when run in production.

```JSON
{
    "name": "MyHANAApp",
    "version": "1.0.0",
    "description": "A simple CAP project.",
    "license": "UNLICENSED",
    "private": true,
    "dependencies": {
        "@sap/audit-logging": "^5.0.0",
        "@sap/cds": "^5",
        "@sap/hana-client": "^2.6.61",
        "@sap/xsenv": "^3.1.0",
        "@sap/xssec": "^3.2.0",
        "express": "^4",
        "passport": "^0.4.1"
    },
    "scripts": {
        "start": "cds serve gen/csn.json"
    },
    "engines": {
        "node": "14.X"
    },
    "cds": {
        "build": {
            "target": "."
        },
        "hana": {
            "deploy-format": "hdbtable"
        },
        "requires": {
            "db": {
                "kind": "hana"
            },
            "uaa": {
                "kind": "xsuaa"
            }
        }
    }
}
```

!![srv_package](srv_package.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 1: ](Make the Fiori app deployment-ready)]

Not only the backend component was missing its final touch. Do you remember when we select `None` in the destination field in the previous tutorial? Now it's time to add the route that will redirect traffic to this destination.

```JSON [5-10]
{
  "welcomeFile": "/index.html",
  "authenticationMethod": "route",
  "routes": [
      {
        "source": "/catalog/(.*)",
        "destination": "hana-app-api",
        "csrfProtection": true,
        "authenticationType": "xsuaa"
    },
    ...
```

!![new_route](new_route.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 5: ](Adjust mta.yaml)]

We also need to make a few changes to the `mta.yaml` to create a destination **for** the web application that points **to** the backend server.

1. Create a binding from the `MyHANAApp-srv` to the existing `uaa_MyHANAApp` so that the access control can be performed. The change in line 13 helps us fix the URL that this module will later connect to. The remaining lines enable us to keep the resulting deployment archive as small as possible to allow a faster upload.

    !![mta_1](mta_1.png)


3. In the second part, we define the destination that points to the backend server. Note that we make use of the exact URL patterns as we used for the `host` parameter. Both lines (14 and 107) use the variable `appname` - which hasn't been defined yet. Let's do this in line 130. Make sure you use a unique suffix for this name. It might be a good idea to reuse the ID of your subaccount for this.

    !![mta_2](mta_2.png)


> This file defines four service-instance-level destinations. Once the project has been deployed, you can find them in the service dashboard of the destination services
> !![destinations_in_action](destinations_in_action.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Build the project)]

**Right-click** on the `mta.yaml` file and select **Build MTA Project**. This will trigger a process that generated the `.mtar` deployment artifact.

!![mbt_build](mbt_build.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 1: ](Deploy the project)]

Once the build process has been completed, look for the newly generated file in the project tree (you'll find it in the `mta_archives` folder). **Right-click** on this file and select **Deploy MTA Archive**.
!![cf_deploy](cf_deploy.png)




[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 1: ](Test the HTML5 app)]

1. You should see the URL of the deployed backend server once the operation completes. **Open** this URL in a new tab.

    !![deploy_finished](deploy_finished.png)


2. If you try to access one of the service endpoints or metadata, you should receive an Unauthorized error.

    !![Unauthorized](unauthorized_cap.png)

    This means your security setup is working. Accessing the URL of the CAP service will always produce an error now as there is no authentication token present.  We need to run via the managed application router (short: approuter) to generate and forward the authentication token.

3. Go to the SAP BTP Cockpit and click on the **HTML5 Applications** menu on the subaccount level. You should see at least one entry there; click on **frontend** to access the web application via the managed approuter.

    !![running_lr](running_lr.png)

1. You should only see one tile **List Report** in the Launchpad sandbox. Select this application.

    !![flp](flp.png)


2. Note that you need to select the visible columns manually before you can see any records.

    !![select_columns](select_columns.png)

7.  Once all needed columns are selected, hit the **Go** button to display the data. This should show the **NO DATA** this time.

    > The reason why you don't see data, is that you created a **new HDI container** during the deployment. You can use the *SAP HANA Projects* panel of the SAP Business Application Studio to open the container in the SAP HANA DB Explorer. In there, you can repeat the same steps to display the data that you saw at the end of the previous tutorial.

    !![fe_lr](fe_lr.png)


Congratulations! You have successfully configured and tested the SAP HANA Cloud and Cloud Business Application-based project with production level authentication and authorization.


[VALIDATE_1]
[ACCORDION-END]

---
