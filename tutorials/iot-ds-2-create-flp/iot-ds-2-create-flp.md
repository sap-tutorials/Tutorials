---
title: Set up Fiori Launchpad in Cloud Foundry to receive Notifications
description: Create and configure a Fiori Portal Site to receive notifications from Leonardo IoT using SAP Web IDE

auto_validation: true
time: 40
tags: [ tutorial>beginner, products>sap-leonardo-iot, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform-portal, products>sap-web-ide, products>sap-web-ide-plug-ins]
primary_tag: topic>internet-of-things
---

## Prerequisites
 - **Tutorial**: If you don't have access to a Web IDE, follow the tutorial [Prepare SAP Web IDE for Cloud Foundry Development](sapui5-webide-open-webide) in your Neo sub-account.

## Details
### You will learn
  - How to create a new MTA project
  - How to add a Fiori Portal Site to a MTA project
---

Fiori Portal Site provides a user with access to their notifications and applications deployed on the cloud.

[ACCORDION-BEGIN [Step 1: ](Create a new MTA project)]

  1. Launch SAP Web IDE in a browser tab.

  2. Enable Leonardo IoT Web IDE feature by completing Step 2 of [Build an IoT Condition Monitoring App](iot-express-5-use-webide-template).

  3. In the development perspective,  right-click on your workspace and create a new project from a template (there are other ways to do this, for example, from the home page).

    ![WebIde New Project](/images/webide_create_mta_1.png)

  4. Choose **`Multi-Target Application`** from the **`Cloud Foundry`** Environment and **`Featured`** Category, click **Next**.

    ![WebIde New Project](/images/webide_create_mta_2.png)

  5. Enter `iot-ds` as a project name, click **Next**.

    ![WebIde New Project](/images/webide_create_mta_3.png)

  6. Check the **`Use HTML5 Application Repository`** checkbox, click **Finish**.

    ![WebIde New Project](/images/webide_create_mta_4.png)

    SAP Web IDE will generate the code and will open the new application in the Code Editor.  Click the `Show Hidden Files` button to see   all the files in the application.

    ![WebIde New Project](/images/webide_create_mta_5.png)

  7. Create a new file by right clicking on `iot-ds` project.

    ![WebIde New Project](/images/webide_create_mta_6.png)

  8. Enter `xs-security.json` as **File Name** and click **OK**.

    ![WebIde New Project](/images/webide_create_mta_7.png)

  9. A blank `xs-security.json` file should be opened in the Code Editor.  

  10. Paste the following content into the file.

    ```JSON
    {
      "xsappname": "iot-ds-app",
      "tenant-mode": "dedicated",
      "description": "Security profile of called application",
      "scopes": [
        {
          "name": "uaa.user",
          "description": "UAA"
        }
      ],
      "role-templates": [
        {
          "name": "Token_Exchange",
          "description": "UAA",
          "scope-references": [
            "uaa.user"
          ]
        }
      ]
    }  
    ```

  11. Click **Save** to save the changes.

    ![WebIde New Project](/images/webide_create_mta_8.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Fiori Portal Site)]

  1. Enable SAP Fiori Launchpad Site Web IDE feature.  Go to Web IDE **`Preference`**, search for `portal` in **`Extensions`**. Turn On this feature.  Click **Save** to save the change.

    ![Web IDE Portal Feature](/images/create_portal_2.png)

  2. In Development perspective, right click on MTA project and add new **`SAP Fiori Launchpad Site Module`**.

    ![Web IDE Portal Feature](/images/create_portal_3_a.png)

  3. Enter `PortalSite` as **Module** name.

  4. Click **Finish**.

    ![Web IDE Portal Feature](/images/create_portal_4.png)

  5. Fiori Portal Site has been added to the MTA project.

    ![Web IDE Portal Feature](/images/create_portal_5_a.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure Leonardo IoT Notification Service)]

  1. Open `CommonDataModel.json` in **Code Editor**..

    ![Web IDE Portal Feature](/images/create_portal_9_2_a.png)

  2. Find the location of the JSON key **`payload.sites.payload.config`** in the file.

    ![Web IDE Portal Feature](/images/create_portal_9_1_a.png)

  3. Update the value as follows:

    ```JSON
    "config": {
    	"ushellConfig": {
    		"renderers": {
    			"fiori2": {
    				"componentData": {
    					"config": {
    						"enableRecentActivity": false,
    						"enableNotificationsUI": true,
    						"applications": {
    							"Shell-home": {
    								"enableNotificationsPreview": true
    							}
    						}
    					}
    				}
    			}
    		},
    		"services": {
    			"Notifications": {
    				"config": {
    					"enabled": true,
    					"serviceUrl": "/ns/Consumer.svc",
    					"webSocketUrl": "/ns/WebSocket.svc",
    					"pollingIntervalInSeconds": 10
    				}
    			}
    		}
    	}
    }	  
    ```  

    ![Web IDE Portal Feature](/images/create_portal_9_a.png)

  4. **Save** the changes.

  5. Double click `xs-app.json` file in the folder `iot-ds_appRouter` to open it in the Code Editor.

  6. Add the following route for Notification Service:

    ```JSON
    {
      "source": "^/ns/(.*)",
      "target": "$1",
      "service": "com.sap.leonardo.iot",
      "endpoint": "notifications"
    }  	  
    ```

    ![Web IDE Portal Feature](/images/create_portal_10_a.png)

  7. **Save** the file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure Cloud Foundry Settings in Web IDE)]

  * Please configure the Cloud Foundry settings in your Web IDE preference before proceeding to the next step.  You can proceed to the next step if they are already configured.  For more information on how to configure the Cloud Foundry settings in Web IDE, see Step 6 and Step 7 of [Prepare SAP Web IDE for Cloud Foundry Development](sapui5-webide-open-webide).


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add IoT Service Resource)]

  1. Right click on the `iot-ds` project and add new SAP Cloud Platform Service.

    ![Service Binding 1](/images/service_binding_1.png)

  2. If you were prompted to login, enter your user name and password for the Cloud Foundry space which you have configured in the Web IDE Cloud Foundry Preferences.

  3. Enter `iotae` in the **Search** field.

  4. Select the entry in the search result.

  5. Click **Next**.

    ![Service Binding 1](/images/service_binding_2.png)

  6. In the field **Add or Reuse an Existing Instance**, select **Reuse Service**.

  7. In the field **Service Name (Instance)**, select your service instance from the dropdown.  The service instance name is customer specific.

  8. In the field **Resource Name**, enter the same service instance name selected in the previous step.

  9. The field  **Plan** should be automatically filled.   

  10. Click **Next**.

    ![Service Binding 1](/images/service_binding_3.png)

  11. Click **Finish**.

    ![Service Binding 1](/images/service_binding_4.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add UAA Service Resource)]

  1. Open `mta.yaml` in Code Editor.

    ![Uaa MTA 1](/images/uaa_mta_1.png)

  2. Add the following UAA resource under **`resources`**. Please use the *Copy* button to copy the formatted configuration and paste it in the specified location in the `yaml` file.

    ```yaml
      - name: uaa_iot-ds
        parameters:
          path: ./xs-security.json
          service-plan: application
          service: xsuaa
        type: org.cloudfoundry.managed-service
    ```  

    ![Uaa MTA 2](/images/uaa_mta_2.png)

    If you see a red exclamation point on a line in the Code Editor, it usually indicates a formatting issue with the line.  Please check the indentation of the indicated line as well as the line above or below it.  In this example, the entry **`-name: uaa-iot-ds`** is misaligned with the entry **`-name: iot-ds-html5_repo_host`** .

    ![Uaa MTA 2](/images/yaml_format_error_1.png)

    **Note:** Please **use spaces** and **not tabs** when applying indentations in the mta.yaml file

  3. **Save** the changes.

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Configure Service Bindings)]

  1. Open `mta.yaml` file in **MTA Editor**.

  2. Select `iot-ds_appRouter` in **Modules**.

    ![Service Binding 1](/images/service_binding_5.png)

  3. Scroll down to the **Requires** section.

  4. Click the **+** button.

  5. A new entry will be created in the table. Select the same **`iotae`** service instance name selected in Step 4.

    ![Service Binding 1](/images/service_binding_6.png)

  6. Click the **+** button.

  7. A new entry will be created in the table. Select the **`uaa`** instance name created in Step 5.

    ![Service Binding 1](/images/service_binding_8.png)

  8. Select **`PortalSite`** in **Modules**.

  9. Scroll down to the **Requires** section.

  10. Click the **+** button.

  11. A new entry will be created in the table. Select the **`iotae`** service instance name selected in Step 4.

    ![Service Binding 1](/images/service_binding_7.png)

  12. **Save** the file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test Notification in Fiori Portal Site (Optional))]

If you want to test notifications in Fiori Portal Site at this point, you'll need to comment out the references to `UI Deployer` in `mta.yaml`. The `UI Deployer` is used for deploying UI components but we have not added any UI components to our project yet and it will cause the deployment to fail. We'll add UI components to our project in the next tutorial [Build a Decision Support UI in the Web IDE](iot-ds-3-create-ui) and we will be able to test notifications without having to make these changes.

To comment out the references to `UI Deployer`:

  * open `mta.yaml` in Code Editor.

  * Type a `#` character in front of the lines shown below.

    ![Disable UI Deployer](/images/disable_uideployer.png)

  * **Save** the file.

Please complete step 1 to 4 in tutorial [Build, Deploy and test Notification and Decision Support application](iot-ds-4-build-test).

[DONE]
[ACCORDION-END]

---
