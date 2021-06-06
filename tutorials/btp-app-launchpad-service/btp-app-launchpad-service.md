---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Add the SAP Launchpad Service
description: This tutorial shows you how to add the SAP Launchpad application.
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - Your subaccount has quota for the services `SAP Launchpad service` and `SAP HTML5 Applications Repository service` as described in [Prepare for SAP BTP Development](btp-app-prepare-btp)

## Details
### You will learn
 - How to add Navigation Targets
 - How to add SAP Cloud Service
 - How to add the Destination Service and destinations
 - How to subscribe to SAP Launchpad service
 - How to test your SAP Launchpad site

In this tutorial, you will use the SAP Launchpad service to access your CAP service and its UI. Additionally, the SAP Launchpad service provides features like personalization, role-based visibility, theming, and more. You can add multiple applications to one launchpad, including subscribed ones and applications from SAP S/4HANA or SAP BTP.

### See Also
 - [What is SAP Launchpad service?](https://help.sap.com/viewer/8c8e1958338140699bd4811b37b82ece/Cloud/en-US/9db48fa44f7e4c62a01bc74c82e74e07.html)
 - Further tutorials, for example [Deliver Your First SAP Launchpad Site (with integrated apps)](mission.launchpad-cf)



To continue with this tutorial you can find the result of the previous tutorial in the [`launchpad-service-prep`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/launchpad-service-prep) branch.

---

[ACCORDION-BEGIN [Step 1: ](Add Navigation Targets)]

In this step, you add the navigation targets `Risks-display` and `Mitigations-display` to the application manifest (`manifest.json`) file. Navigation targets are required to navigate between applications, but also to start the applications from SAP Launchpad service.

[VALIDATE_1]

[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Add Navigation Target for Risks UI)]

1. Open the file `app/risks/webapp/manifest.json`.

2. Add the external navigation target to the `sap.app` JSON object. You can add it right behind the `sourceTemplate` object:

<!-- cpes-file app/risks/webapp/manifest.json:$["sap.app"].crossNavigation -->
```json [8-19]
{
  ...
  "sap.app": {
    "id": "ns.risks",
    ...
    "sourceTemplate": {
      ...
    },
    "crossNavigation": {
      "inbounds": {
        "Risks-display": {
          "signature": {
            "parameters": {},
            "additionalParameters": "allowed"
          },
          "semanticObject": "Risks",
          "action": "display"
        }
      }
    }
  }
}
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Add Navigation Target for Mitigations UI)]

Do the same with the mitigations manifest file `app/mitigations/webapp/manifest.json`, but with the `semanticObject` name `Mitigations`:

<!-- cpes-file app/mitigations/webapp/manifest.json:$["sap.app"].crossNavigation -->
```json [8-19]
{
  ...
  "sap.app": {
    "id": "ns.mitigations",
    ...
    "dataSources": {
      ...
    },
    "crossNavigation": {
      "inbounds": {
        "Mitigations-display": {
          "signature": {
            "parameters": {},
            "additionalParameters": "allowed"
          },
          "semanticObject": "Mitigations",
          "action": "display"
        }
      }
    }
  }
}
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](Add SAP Cloud Service)]

Add your SAP Cloud service at the end of `app/risks/webapp/manifest.json` and `app/mitigations/webapp/manifest.json` files:

<!-- cpes-file app/risks/webapp/manifest.json:$["sap.cloud"] -->
```json [6-9]
{
  "_version": "",
  ...
  "sap.fiori": {
    ...
  },
  "sap.cloud": {
    "public": true,
    "service": "cpapp.service"
  }
}
```

The name of your SAP Cloud service (`cpapp` in this case) should be unique within an SAP BTP region. It is used to identify the resources that belong to one UI in the SAP Launchpad service.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 5: ](Add the SAP Destination Service)]

Add the following lines to the `resources` section of the `mta.yaml` file:

<!-- snippet mta.yaml resources: cpapp-destination -->
```yaml [2-9]
resources:
   ...
 - name: cpapp-destination
   type: org.cloudfoundry.managed-service
   parameters:
     service: destination
     service-plan: lite
     config:
       HTML5Runtime_enabled: true
```

The SAP Destination service stores URLs and credentials (so called "destinations") to access applications and services.

The service configuration option `HTML5Runtime_enabled: true` is required to make the destinations available to the SAP Launchpad service.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 6: ](Add SAP HTML5 Application Repository Service)]

Add the following lines to the `resources` section of the `mta.yaml` file:

<!-- snippet mta.yaml resources: cpapp-html5-repo-host -->
```yaml [2-7]
resources:
   ...
 - name: cpapp-html5-repo-host
   type: org.cloudfoundry.managed-service
   parameters:
     service: html5-apps-repo
     service-plan: app-host
```

The SAP HTML5 Application Repository service stores static UI files. In the deployment, the SAP Fiori UI applications are uploaded to this service and the SAP Launchpad service serves the UIs from there.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 7: ](Add Destinations)]

You add three destinations for the SAP Destination service that are used by the SAP Launchpad service:

1. `cpapp-app-srv` - The destination to the CAP service. It is required by your UIs running in SAP Launchpad service to access your service.
2. `cpapp-html5-repo-host` - The destination to your SAP HTML5 Application Repository service instance. It allows the SAP Launchpad service to access your UI applications.
3. `cpapp-uaa` - The destination to your XSUAA service instance. The SAP Launchpad service needs it to convert OAuth tokens for use with your CAP service.

Add the following lines to the `mta.yaml` file:

<!-- snippet mta.yaml modules: cpapp-destinations -->
```yaml [2-39]
modules:
   ...
 - name: cpapp-destinations
   type: com.sap.application.content
   requires:
     - name: cpapp-uaa
       parameters:
         service-key:
           name: cpapp-uaa-key
     - name: cpapp-html5-repo-host
       parameters:
         service-key:
           name: cpapp-html5-repo-host-key
     - name: srv-api
     - name: cpapp-destination
       parameters:
         content-target: true
   parameters:
     content:
       instance:
         destinations:
           - Authentication: OAuth2UserTokenExchange
             Name: cpapp-app-srv
             TokenServiceInstanceName: cpapp-uaa
             TokenServiceKeyName: cpapp-uaa-key
             URL: '~{srv-api/srv-url}'
             sap.cloud.service: cpapp.service
           - Name: cpapp-html5-repo-host
             ServiceInstanceName: cpapp-html5-repo-host
             ServiceKeyName: cpapp-html5-repo-host-key
             sap.cloud.service: cpapp.service
           - Authentication: OAuth2UserTokenExchange
             Name: cpapp-uaa
             ServiceInstanceName: cpapp-uaa
             ServiceKeyName: cpapp-uaa-key
             sap.cloud.service: cpapp.service
         existing_destinations_policy: update
   build-parameters:
     no-source: true
```

What happens now? The `cpapp-app-srv` destination uses the URL exported from the `cpapp-srv` module. Besides exporting the CAP service's URL as `srv-url` property, the XSUAA service instance that is required for authentication and authorization checks is added here.

<!-- snippet mta.yaml cpapp-srv srv-api -->
```yaml
modules:
   ...
   provides:
    - name: srv-api      # required by consumers of CAP services (for example, approuter)
      properties:
        srv-url: ${default-url}
 # -------------------- SIDECAR MODULE ------------------------
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 8: ](Install Required UI Tools and MTA)]

1. Install [SAPUI5 tooling](https://www.npmjs.com/package/@sap/ux-ui5-tooling) package as global module:

    ```bash
    npm install -g @sap/ux-ui5-tooling
    ```
2. Install [SAP Fiori application generator](https://www.npmjs.com/package/@sap/generator-fiori) package as global module:

    ```bash
    npm install -g @sap/generator-fiori
    ```

3. Install [MTA](https://www.npmjs.com/package/mta) package as global module:

    ```bash
    npm i -g mta
    ```

4. Delete the file `app/mitigations/.yo-rc.json`, because this makes the SAP Fiori application generator fail.

    ```bash
    rm app/mitigations/.yo-rc.json
    ```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 9: ](Remove Broken Dependency from Risks Application)]

Remove the following two lines from `app/risks/package.json`, because they produce build issues and are not needed:

```json [3-4]
    "devDependencies": {
      ...
      "@sap/ux-ui5-tooling": "1",
      "@sap/ux-specification": "latest",
    }
```

Make sure the last entry in `"devDependencies"` doesn't end with a comma (`,`) character.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 10: ](Add SAP Fiori Elements Risks Application)]

1. Switch to `app/risks` directory:

    ```bash
    cd app/risks
    ```

2. Add deployment configuration:

    ```bash
    fiori add deploy-config cf
    ```


3. It will ask for the destination name.

4. Enter `cpapp-app-srv`.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 11: ](Add SAPUI5 Freestyle Mitigations Application)]

1. Repeat the procedure with the `app/mitigations` folder:

    ```bash
    cd ../../app/mitigations
    fiori add deploy-config cf
    ```

2. Enter `cpapp-app-srv` for the destination name again.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 12: ](Check the Results in mta.yaml)]

The newly added modules `nsrisks` and `nsmitigations` do the build of the SAP Fiori application. Each build result is a ZIP file that contains optimized UI resources and a ZIP file `manifest-bundle.zip` with the `manifest.json` and the `i18n` files. The latter is required by the SAP Launchpad service.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 13: ](Risks Application Module nsrisks)]

<!-- snippet mta.yaml --branch launchpad-service modules: "- name: nsrisks" -->
```yaml
modules:
  ...
- name: nsrisks
  type: html5
  path: app/risks
  build-parameters:
    build-result: dist
    builder: custom
    commands:
    - npm install
    - npm run build:cf
    supported-platforms: []
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 14: ](Mitigations Application Module nsmitigations)]

<!-- snippet mta.yaml --branch launchpad-service modules: "- name: nsmitigations" -->
```yaml
modules:
  ...
- name: nsmitigations
  type: html5
  path: app/mitigations
  build-parameters:
    build-result: dist
    builder: custom
    commands:
    - npm install
    - npm run build:cf
    supported-platforms: []
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 15: ](HTML5 Application Deployer)]

The module `cpapp-app-content` deploys the ZIP files from the `nsrisks.zip` and `nsmitigations.zip` to the SAP HTML5 Application Repository service, where it can be accessed by the SAP Launchpad service using the previously added destinations.

<!-- snippet mta.yaml --branch launchpad-service modules: cpapp-app-content -->
```yaml
modules:
  ...
- name: cpapp-app-content
  type: com.sap.application.content
  path: .
  requires:
  - name: cpapp-html5-repo-host
    parameters:
      content-target: true
  build-parameters:
    build-result: resources
    requires:
    - artifacts:
      - nsrisks.zip
      name: nsrisks
      target-path: resources/
    - artifacts:
      - nsmitigations.zip
      name: nsmitigations
      target-path: resources/
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 16: ](Add UI Build Files to .gitignore)]

The SAP Fiori build files do not need to be stored in Git. You can add it to your `.gitignore` file:

```bash
dist/
resources/
app/*/package-lock.json
```

> _Can't see `.gitignore`?_

> `.gitignore` files are excluded by default from the file types shown in the VS Code workspace. In VS Code, go to **File** **&rarr;** **Preferences** **&rarr;** **Settings**, search for *`fileexclude`*, and delete the setting for `.gitignore` files under the **Workspace** tab.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 17: ](Re-Build and Re-Deploy the .mtar File)]

1. Build your project with the MTA Build Tool (MBT):

    ```bash
    mbt build -t ./
    ```

    > _Make sure you're in the root folder of your project_

2. Deploy your project to SAP BTP:

    ```bash
    cf deploy cpapp_1.0.0.mtar
    ```

> _MBT build options_

> [How to build an MTA archive from the project sources](https://sap.github.io/cloud-mta-build-tool/usage/#how-to-build-an-mta-archive-from-the-project-sources)

3. Go to SAP BTP cockpit to check the deployed content.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 18: ](Subscribe to SAP Launchpad Service)]

1. Log on to your **Global Account** and navigate to the **Subaccount** where you have deployed your service and application.
2. Choose **Services** **&rarr;** **Service Marketplace** on the left.
3. Search for the **Launchpad Service** tile and choose **Create**.


    !![Create Launchpad Service](create_launchpad_service.png)

4. Keep the default settings for **Service** and **Plan** and choose **Create**.


    !![New Instance or Subscription](new_instance_dialog.png)

You have now subscribed to the SAP Launchpad service.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 19: ](Create Your SAP Launchpad Site)]

1. Choose **Services** **&rarr;** **Instances and Subscriptions** on the left.
2. Locate the **Launchpad Service** under **Subscriptions** and choose **Go to Application**.


    !![Instances and Subscriptions](instances_and_subscriptions.png)



    > _In case you get the error: `Sorry, we couldn't find  the site`._

    > If you get the error `Sorry, we couldn't find the site. Please contact your site administrator for assistance.` while opening the application, you have to assign your user to the `Launchpad_Admin` role collection.

    > ![Couldn't find the site](cant_find_site_error.png)

    > 1. Choose **Security** **&rarr;** **Trust Configuration** on the left.
    > 2. Choose your identity provider from the list.
    > 3. Enter your e-mail address and choose **Show Assignments**.
    > 4. Choose **Assign Role Collection** and assign the `Launchpad Admin` role collection to your user.

    > See section [Initial Setup](https://help.sap.com/viewer/8c8e1958338140699bd4811b37b82ece/Cloud/en-US/fd79b232967545569d1ae4d8f691016b.html) in the SAP Launchpad service's documentation for more details.


3. Choose **Provider Manager** on the left and refresh the `HTML5 Apps` entry there.


    !![Refresh HTML5 Apps](refresh_html5_apps.png)

    > _Content providers aren't reloaded automatically when you push an app, so it's important to manually refresh._

4. Choose **Content Manager** **&rarr;** **Content Explorer** and open the content provider `HTML5 Apps`.

    !![HTML5 Apps Content Provider](html5_apps_content_provider.png)


5. Add the `Risks` and `Mitigations` to **My Content**.

    !![Add Apps to My Content](add_apps_to_my_content.png)


4. Choose **Content Manager** **&rarr;** **My Content**.
5. In the item list, choose the item `Everyone`.

    !![Role Everyone](role_everyone.png)

    > _`Everyone` is a role that has to be assigned to the `Risks` and `Mitigations` apps so all users can access them._

6. Choose **Edit** and assign the `Risks` and `Mitigations` apps to the role.

    !![Add Apps to Role](apps_to_role_everyone.png)

7. Choose **New** **&rarr;** **Group**.

    !![New Group](new_group.png)

8. Type in `Risk Management` as the title of the group and assign the `Risks` and `Mitigations` apps to it.

    !![Create Group](create_group.png)

    > _This way, you're telling the SAP Launchpad service to display the `Risks` and `Mitigations` apps in a group called `Risk Management`._

9. Choose **Site Directory** **&rarr;** **Create Site**.

    !![Create Site](create_site.png)

10. Type in `Risk Management Site` for the site name and choose **Create**.

    > _The new site gets the `Everyone` role by default, so you don't have to assign it explicitly. The default site properties are sufficient for the purposes of this tutorial._



[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 20: ](Test Your SAP Launchpad Site)]

1. Choose **Go to site**.

    !![Go to site](go_to_site.png)

    You can see the `Risk Management` group that includes the `Mitigations` and `Risks` apps.

2. Open the `Risks` app.

    !![Risk Management Site](risk_management_site.png)

You have launched your `Risks` app through the SAP Launchpad service.


  !![Risks App](risks.png)

> _If you choose ***Go***, you'll get an error because you haven't assigned a role collection to your user yet. We'll do it in the next step._

[DONE]

The result of this tutorial can be found in the [`launchpad-service`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/launchpad-service) branch.

[ACCORDION-END]
