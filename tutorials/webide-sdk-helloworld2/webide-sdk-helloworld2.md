---
title: Changing Code in a Plugin
description: In this tutorial, you will change some of the code in a plugin, and see how to configure an API service.
tags: [  tutorial>beginner, topic>cloud, products>sap-hana-cloud-platform, products>sap-web-ide ]
---

## Prerequisites  
 - [Create a New Plugin Project](http://www.sap.com/developer/tutorials/webide-sdk-helloworld1.html)


## Next Steps
- [Deploying a Feature to SAP Cloud Platform](http://www.sap.com/developer/tutorials/webide-sdk-helloworld3.html)

## Details
### You will learn  
  - How to modify i18n strings
  - How to configure an API service
  - How to test a plugin  

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Modify i18n strings)]
1. In the workspace, go to **`coolproject`** | **client** | **`coolplugin`** | **i18n** and double-click the `i18n.properties` file.
2. Change the following strings:<table><tbody><tr><th>Key</th><th>Value</th></tr>
<tr><td>`command_helloworld`</td><td>`Welcome`</td></tr>
<tr><td>`commandgroup_sample`</td><td>`Greetings`</td></tr></tbody></table>  
3. Click **Save**.
  ![Modify i18n strings](Step1-i18n.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change menu by configuring an API service)]
1. In the workspace, go to **`coolproject`** | **client** | **`coolplugin`** and double-click the `plugin.json` file.
![Modify i18n strings](Step2-pluginjson.png)
2. Change all occurrences of `tools` to `edit`.                
3. Click **Save**.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Test the plugin)]
In the Workspace, right-click the **`plugin.json`** file and choose **Run** | **Run As Plugin**.
SAP Web IDE opens in a new browser tab called **Debug Mode**.

You can open the same dialog as in the previous tutorial, but now the menu item is located in the **Edit** menu and is called **Greetings** | **Welcome**.


[DONE]
[ACCORDION-END]

## Next Steps
- [Deploying a Feature to SAP Cloud Platform](http://www.sap.com/developer/tutorials/webide-sdk-helloworld3.html)
