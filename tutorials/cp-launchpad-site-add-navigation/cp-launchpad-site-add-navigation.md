---
title: Add Navigation Manually to Integrate an SAPUI5 App Into a Launchpad Site
description: Add navigation properties to your custom-developed SAPUI5 app so it can be integrated into your launchpad site in the SAP Launchpad service.
auto_validation: true
time: 20
tags: [ tutorial>beginner, topic>sapui5, products>sap-business-application-studio, products>sap-business-technology-platform, products>sap-launchpad-service]
primary_tag: products>sap-launchpad-service
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

## Details
### You will learn
  - How to configure the navigation properties for your custom app

Adding navigation properties to custom-developed apps is mandatory if you want to add them to a launchpad site. To add navigation properties to an app, edit its `manifest.json` file and add an intent to it. An intent is a unique combination of a semantic object and an action.

---

[ACCORDION-BEGIN [Step 1: ](Open manifest.json file)]
1. In your SAP Business Application Studio explorer, expand your app folder and then expand the `webapp` subfolder and select it.

2. Click the `manifest.json` file to open it.

    ![Open manifest.json file](1-open-manifestjson.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Add intent)]
Now you'll add the intent navigation parameters to the `sap.app` descriptor in the `manifest.json` file.

1. Under the **sap.app** section of the `manifest.json` file, put your cursor on the line before the closing bracket of the `sap.app` section. In our example, we'll put it after the closing bracket of the `datasources` section.

    ![Find entry for intent](2-find-entry-point.png)

2. Add the intent descriptor as follows:

    - Add a comma after the closing bracket of the `datasources` section to add a new section.

        ![Add comma](3-add-comma.png)

    - Click enter to add a new row and then backspace to make sure you're at the beginning of the newly added row.

    - Copy the following intent information (note the copy button on the right of this code).

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

    - Paste this code in your `manifest.json` file. It should look like this:

        ![Paste intent navigation](4-paste-intent-navigation.png)

3. Click **File > Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test yourself)]

[VALIDATE_6]
[ACCORDION-END]
