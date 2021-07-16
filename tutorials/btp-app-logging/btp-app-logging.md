---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Enable Logging Service for Your Application
description: This tutorial shows you how to enable Logging Service for your application.
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori]
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
 - [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-prepare-xsuaa)
 - [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)
 - [Add the SAP Launchpad Service](btp-app-launchpad-service)
 - [Assign a Role Collection to a User](btp-app-role-assignment)

## Details
### You will learn
 - How to access logs from terminal and SAP BTP cockpit
 - How to analyze logs using Kibana dashboard
 - How to enable Logging Service for your CAP application
 - How to test the Logging Service


To continue with this tutorial you can find the result of the previous tutorial in the [`launchpad-service`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/launchpad-service) branch.

---

[ACCORDION-BEGIN [Step 1: ](Access logs from terminal)]
1. Display recent logs:

    ```Shell/Bash
    cf logs --recent <appname>
    ```

2. Follow logs live:

    ```Shell/Bash
    cf logs <appname>
    ```

> Choose **Ctrl** + **C** to quit.

[VALIDATE_1]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Access logs from SAP BTP cockpit)]
1. Go to your subaccount in **SAP BTP cockpit**.

2. Choose **Cloud Foundry** **&rarr;** **Spaces**.

3. Choose your space.

4. Choose on the application whose logs you want to access.

5. Choose **Logs**.

    !![App Logs](cpapp-srv-logs.png)

    > Only the recent logs are displayed.



[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Analyze logs using Kibana dashboard)]
1. Go to your subaccount in **SAP BTP cockpit**.

2. Choose **Cloud Foundry** **&rarr;** **Spaces**.

3. Choose your space.

4. Choose the application whose logs you want to access.

5. Choose **Logs**.

6. Choose **Open Kibana Dashboard**.

    > Kibana shows all logs.

    > Although the **Open Kibana Dashboard** link is placed on the logs view of a particular application, it will give you access to the logs of all spaces where you have the required permission.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Enable Logging Service)]
[OPTION BEGIN [Trial]]

Logs are only kept for a short period and won't be visible in Kibana without the Logging Service. By binding the Logging Service to your application, logs will be kept for longer and will be available for further analysis via Kibana.

It's suggested to enable the Logging Service for all applications, so that error analysis will be possible even some hours after the incident.

In our experience, the `development` plan wasn't sufficient for test scenarios. Probably, its okay for personal development spaces. However, this tutorial uses the `standard` plan to be on the safe side.

1. Add an instance for the logging service to the `resources` section of your `mta.yaml`:


```YAML[4-9]
...
resources:
...
- name: cpapp-logs
  type: org.cloudfoundry.managed-service
  parameters:
    service: application-logs
    service-plan: lite
```

=== "Live"

```YAML[4-9]
...
resources:
...
- name: cpapp-logs
  type: org.cloudfoundry.managed-service
  parameters:
    service: application-logs
    service-plan: standard
```

2. Bind the logging service instance to all `modules` of the `mta.yaml`:

<!-- cpes-file mta.yaml:$.modules[?(@.name=="cpapp-srv")].requires[?(@.name=="cpapp-logs")] -->
```YAML[9-9]
_schema-version: '3.1'
...
modules:
  ...
  - name: cpapp-srv
    ...
    requires:
      ...
      - name: cpapp-logs
```

<!-- cpes-file mta.yaml:$.modules[?(@.name=="cpapp-db-deployer")].requires[?(@.name=="cpapp-logs")] -->
```YAML[9-9]
_schema-version: '3.1'
...
modules:
  ...
  - name: cpapp-db-deployer
    ...
    requires:
      ...
      - name: cpapp-logs
```
[OPTION END]
[OPTION BEGIN [Live]]

Logs are only kept for a short period and won't be visible in Kibana without the Logging Service. By binding the Logging Service to your application, logs will be kept for longer and will be available for further analysis via Kibana.

It's suggested to enable the Logging Service for all applications, so that error analysis will be possible even some hours after the incident.

In our experience, the `development` plan wasn't sufficient for test scenarios. Probably, its okay for personal development spaces. However, this tutorial uses the `standard` plan to be on the safe side.

1. Add an instance for the logging service to the `resources` section of your `mta.yaml`:

=== "Trial"

```YAML[4-9]
...
resources:
...
- name: cpapp-logs
  type: org.cloudfoundry.managed-service
  parameters:
    service: application-logs
    service-plan: lite
```


```YAML[4-9]
...
resources:
...
- name: cpapp-logs
  type: org.cloudfoundry.managed-service
  parameters:
    service: application-logs
    service-plan: standard
```

2. Bind the logging service instance to all `modules` of the `mta.yaml`:

<!-- cpes-file mta.yaml:$.modules[?(@.name=="cpapp-srv")].requires[?(@.name=="cpapp-logs")] -->
```YAML[9-9]
_schema-version: '3.1'
...
modules:
  ...
  - name: cpapp-srv
    ...
    requires:
      ...
      - name: cpapp-logs
```

<!-- cpes-file mta.yaml:$.modules[?(@.name=="cpapp-db-deployer")].requires[?(@.name=="cpapp-logs")] -->
```YAML[9-9]
_schema-version: '3.1'
...
modules:
  ...
  - name: cpapp-db-deployer
    ...
    requires:
      ...
      - name: cpapp-logs
```
[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Test it)]
1. Build the MTAR file and deploy it to your Cloud Foundry space:

    ```
    mbt build -t ./
    cf deploy cpapp_1.0.0.mtar
    ```

2. Open Kibana after successful deployment. Your org should now be visible in the Kibana dashboard:

    !![Kibana: Filter Org](kibana_filter_org.png)

3. Choose the ( &#x2B; ) icon to filter for it.

4. The filter is added to the filter bar on the top of the screen and gets applied on the dashboard.

5. You should see your newly applied applications:

    !![Kibana: Components](kibana_components.png)

You can also browse all logs using the *Discover* button (compass icon) on the left navigation bar.
The available fields are displayed on the left side of the screen. You can add fields to the message display or quickly filter for any of the top values.

The time filter is on the right top of the screen. Don't forget to choose *Refresh*.



[DONE]
The result of this tutorial can be found in the [`cp/logging`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/cp/logging) branch.

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=btp-app-logging" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
[ACCORDION-END]
---
