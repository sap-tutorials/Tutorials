---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Prepare SAP Build Work Zone, Standard Edition Setup
description: This tutorial shows you how to add the SAP Launchpad application.
keywords: cap
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)

## Details
### You will learn
 - How to add navigation targets and prepare your UI applications
 - How to add SAP Cloud service
 - How to add the Destination Service and destinations

---

[ACCORDION-BEGIN [Step 1: ](Prepapre UI applications)]
In this tutorial, you will use the SAP Build Work Zone, standard edition to access your CAP service and its UI. Additionally, the SAP Build Work Zone, standard edition provides features like personalization, role-based visibility, theming, and more. You can add multiple applications to one launchpad, including subscribed ones and applications from SAP S/4HANA or SAP BTP.

Navigation targets are required to navigate between applications, but also to start the applications from SAP Build Work Zone, standard edition. In the next steps, you add the navigation targets `Risks-display` and `Mitigations-display` to the application manifest (`manifest.json`) file.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Add navigation target for Risks UI)]
1. Open the file `app/risks/webapp/manifest.json`.

2. Add the external navigation target to the `sap.app` JSON object. You can add it right behind the `sourceTemplate` object:

<!-- cpes-file app/risks/webapp/manifest.json:$["sap.app"].crossNavigation -->
```JSON[8-19]
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

[VALIDATE_1]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Add navigation target for Mitigations UI)]
Do the same with the mitigations manifest file `app/mitigations/webapp/manifest.json`, but with the `semanticObject` name `Mitigations`.

<!-- cpes-file app/mitigations/webapp/manifest.json:$["sap.app"].crossNavigation -->
```JSON[8-19]
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
---
[ACCORDION-BEGIN [Step 4: ](Add SAP Cloud service)]
Add your SAP Cloud service at the end of `app/risks/webapp/manifest.json` and `app/mitigations/webapp/manifest.json` files:

<!-- cpes-file app/risks/webapp/manifest.json:$["sap.cloud"] -->
```JSON[6-9]
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

The name of your SAP Cloud service (`cpapp` in this case) should be unique within an SAP BTP region. It is used to identify the resources that belong to one UI in the SAP Build Work Zone, standard edition.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Add the SAP Destination service)]
Add the following lines to the `resources` section of the `mta.yaml` file:

<!-- snippet mta.yaml resources: cpapp-destination -->
```YAML[3-9]
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

The service configuration option `HTML5Runtime_enabled: true` is required to make the destinations available to the SAP Build Work Zone, standard edition.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 6: ](Add SAP HTML5 Application Repository service)]
Add the following lines to the `resources` section of the `mta.yaml` file:

<!-- snippet mta.yaml resources: cpapp-html5-repo-host -->
```YAML[3-7]
resources:
   ...
 - name: cpapp-html5-repo-host
   type: org.cloudfoundry.managed-service
   parameters:
     service: html5-apps-repo
     service-plan: app-host
```

The SAP HTML5 Application Repository service stores static UI files. In the deployment, the SAP Fiori UI applications are uploaded to this service and the SAP Build Work Zone, standard edition serves the UIs from there.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 7: ](Add destinations)]
You add three destinations for the SAP Destination service that are used by the SAP Build Work Zone, standard edition:

1. `cpapp-app-srv` - The destination to the CAP service. It is required by your UIs running in SAP Build Work Zone, standard edition to access your service.

2. `cpapp-html5-repo-host` - The destination to your SAP HTML5 Application Repository service instance. It allows the SAP Build Work Zone, standard edition to access your UI applications.

3. `cpapp-uaa` - The destination to your XSUAA service instance. The SAP Build Work Zone, standard edition needs it to convert OAuth tokens for use with your CAP service.

Add the following lines to the `modules` section of the `mta.yaml` file:

<!-- snippet mta.yaml modules: cpapp-destinations -->
```YAML[3-39]
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
```YAML
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
---
[ACCORDION-BEGIN [Step 8: ](Install required UI tools and MTA)]
1. Install [SAPUI5 tooling](https://www.npmjs.com/package/@sap/ux-ui5-tooling) package as global module in the root folder of your project:

    ```Shell/Bash
    npm install --global @sap/ux-ui5-tooling
    ```

2. Install [SAP Fiori application generator](https://www.npmjs.com/package/@sap/generator-fiori) package as global module:

    ```Shell/Bash
    npm install --global @sap/generator-fiori
    ```

3. Install [MTA](https://www.npmjs.com/package/mta) package as global module:

    ```Shell/Bash
    npm install --global mta
    ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 9: ](Add deployment configuration for the SAP Fiori elements Risks application)]
1. Switch to `app/risks` folder:

    ```Shell/Bash
    cd app/risks
    ```

2. Add deployment configuration:

    ```Shell/Bash
    fiori add deploy-config cf
    ```

    > If the SAP Fiori generator fails, make sure to remove other `.yo-rc.json` files you might have in any of your project's directories and try again.

3. It will ask for the destination name.

4. Enter `cpapp-app-srv` for the destination name.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 10: ](Add deployment configuration for the SAPUI5 freestyle Mitigations application)]
Repeat the procedure from the previous step for the `app/mitigation` folder.

1. Switch to `app/mitigations` folder:

    ```Shell/Bash
    cd ../../app/mitigations
    ```

2. Add deployment configuration:

    ```Shell/Bash
    fiori add deploy-config cf
    ```

    It will ask for the destination name.

3. Enter `cpapp-app-srv` for the destination name again.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 11: ](Check the results in mta.yaml)]
The newly added modules `nsrisks` and `nsmitigations` do the build of the SAP Fiori application. Each build result is a ZIP file that contains optimized UI resources and a ZIP file `manifest-bundle.zip` with the `manifest.json` and the `i18n` files. The latter is required by the SAP Build Work Zone, standard edition.

1. Check the `risks` application module `nsrisks`:

    <!-- snippet mta.yaml --branch launchpad-service modules: "- name: nsrisks" -->
    ```YAML
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

2. Check the `mitigations` application module `nsmitigations`:

    <!-- snippet mta.yaml --branch launchpad-service modules: "- name: nsmitigations" -->
    ```YAML
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

3. Check the HTML5 application deployer.

    The module `cpapp-app-content` deploys the ZIP files from the `nsrisks.zip` and `nsmitigations.zip` to the SAP HTML5 Application Repository service, where it can be accessed by the SAP Build Work Zone, standard edition using the previously added destinations.

    <!-- snippet mta.yaml --branch launchpad-service modules: cpapp-app-content -->
    ```YAML
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
---
[ACCORDION-BEGIN [Step 12: ](Add UI build files to .gitignore)]
The SAP Fiori build files do not need to be stored in GitHub. You can add it to your `.gitignore` file:

```gitignore
dist/
resources/
app/*/package-lock.json
```

> Can't see `.gitignore`?

> `.gitignore` files are excluded by default from the file types shown in the VS Code workspace. In VS Code, go to **File** (for macOS: **Code**) &rarr; **Preferences** &rarr; **Settings**, search for `fileexclude`, and delete the setting for `.gitignore` files under the **Workspace** tab.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 13: ](Re-build and re-deploy the .mtar file)]
1. Build your project with the MTA Build Tool (MBT) in the root folder of your project:

    ```Shell/Bash
    mbt build -t ./
    ```

2. Deploy your project to SAP BTP:

    ```Shell/Bash
    cf deploy cpapp_1.0.0.mtar
    ```

    > Your SAP HANA Cloud instance will be automatically stopped overnight, according to the server region time zone. That means you need to restart your instance every day before you start working with it.

    > Additional Documentation:

    > [How to build an MTA archive from the project sources](https://sap.github.io/cloud-mta-build-tool/usage/#how-to-build-an-mta-archive-from-the-project-sources)

3. Go to [SAP BTP Cockpit](https://account.hana.ondemand.com/) to check the deployed content.

[DONE]
[ACCORDION-END]
---