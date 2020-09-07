---
title: Add Navigation to Your Custom-Developed SAPUI5 App to Integrate It Into Your SAP Cloud Platform Launchpad Site
description: Add navigation properties to your custom-developed app.
auto_validation: true
time: 20
tags: [ tutorial>beginner, topic>sapui5, products>sap-business-application-studio]
primary_tag: products>sap-cloud-platform-for-the-cloud-foundry-environment
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

## Prerequisites
 - You have completed tutorials 1, 2, and 3 in this group.


## Details
### You will learn
  - How to configure the navigation properties for your custom app.

---
Adding navigation properties to custom-developed apps is mandatory in order to enable adding them to an SAP Cloud Platform Launchpad site. To add navigation properties to an app, edit its `manifest.json` file and add an intent to it. An intent is a unique combination of a semantic object and an action.

[ACCORDION-BEGIN [Step 1: ](Add an intent to the manifest.json file)]


1. Open the **manifest.json** file:

    a. In your SAP Business Application Studio explorer, expand your app folder and then expand the `webapp` subfolder and select it.

    b. Click on the `manifest.json` file to open it.

    ![Open manifest.json file](1-open-manifestjson.png)

    Now you'll add the intent navigation parameters to the `sap.app` descriptor in the `manifes.json` file.

2. Under the **sap.app** section of the `manifest.json` file, put your cursor on the line before the closing bracket of the `sap.app` section. In our example, we'll put it after the closing bracket of the `datasources` section.

    ![Find entry for intent](2-find-entry-point.png)

3. Add the intent descriptor as follows:

    a. Add a comma after the closing bracket of the `datasources` section to add a new section.

    ![Add comma](3-add-comma.png)

    b. Click enter to add a new row and then backspace to make sure you're at the beginning of the newly added row.

    c. Copy the following intent information (note the copy button on the right of this code).

    ```JSON
    "crossNavigation": {
        "inbounds": {
            "intent1": {
                "signature": {
                    "parameters": {},
                    "additionalParameters": "allowed"
                },
                "semanticObject": "Object",
                "action": "display",
                "title": "{{appTitle}}",
                "info": "{{appTitle}}",
                "subTitle": "{{appSubTitle}}",
                "icon": "sap-icon://account"
            }
        }
    }
    ```
     d. Paste this code in your `manifest.json` file. It should look like this:

    ![Paste intent navigation](4-paste-intent-navigation.png)

4. Click **File** -> **Save**.

In the next tutorial, you'll build and deploy your app to SAP Cloud Platform.


[VALIDATE_6]
[ACCORDION-END]
