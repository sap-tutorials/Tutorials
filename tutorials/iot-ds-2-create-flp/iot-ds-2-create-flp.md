---
title: Set Up Fiori Launchpad in Cloud Foundry to Receive Notifications
description: Create and configure an SAP Fiori Launchpad Site to receive notifications from Leonardo IoT using SAP Web IDE.

auto_validation: true
time: 40
tags: [ tutorial>beginner, products>sap-leonardo-iot, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform-portal, products>sap-web-ide, products>sap-web-ide-plug-ins]
primary_tag: topic>internet-of-things
---

## Details
### You will learn
  - How to create a new MTA project
  - How to add a Fiori Launchpad Site to a MTA project
---

Fiori Launchpad Site provides a user with access to their notifications and applications deployed on the cloud.

[ACCORDION-BEGIN [Step 1: ](Create MTA project)]

  1. Launch SAP Web IDE in a browser tab.  

    * If you have a SAP Cloud Platform trial account, click [here](https://account.hanatrial.ondemand.com/) to open home page and click on *Launch SAP Web IDE*.

        ![WebIde SCP Trial](/images/hanatrial-open-webide.png)

    * To access SAP Web IDE from your Neo sub-account, open SAP Cloud Platform Cockpit and navigate to your Neo sub-account.

        ![WebIde SCP Trial](/images/scp-neo-subaccount.png)

        Select SAP Web IDE Full-Stack Service.

        ![WebIde SCP Trial](/images/scp-neo-webide-1.png)

        Select *Go to Service* to launch SAP Web IDE.

        ![WebIde SCP Trial](/images/scp-neo-webide-2.png)

  2. In the development perspective,  right-click on your workspace and create a new project from a template (there are other ways to do this, for example, from the home page).

    ![WebIde New Project](/images/webide_create_mta_1.png)

  3. Choose **`Multi-Target Application`** from the **`Cloud Foundry`** Environment and **`Featured`** Category, click **Next**.

    ![WebIde New Project](/images/webide_create_mta_2.png)

  4. Enter `iot-ds` as a project name, click **Next**.

    ![WebIde New Project](/images/webide_create_mta_3.png)

  5. Check the **`Use HTML5 Application Repository`** checkbox, click **Finish**.

    ![WebIde New Project](/images/webide_create_mta_4.png)

  6. SAP Web IDE will generate the code and will open the new application in the Code Editor.  Click the **Show Hidden Files** button to see   all the files in the application.

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

[ACCORDION-BEGIN [Step 2: ](Create Fiori Launchpad site)]

  1. Enable SAP Fiori Launchpad Site Web IDE feature.  Go to Web IDE **`Preference`**, search for `portal` in **`Extensions`**. Turn On this feature.  Click **Save** to save the change.

    ![Web IDE Portal Feature](/images/create_portal_2.png)

  2. In Development perspective, right click on MTA project and add new **`SAP Fiori Launchpad Site Module`**.

    ![Web IDE Portal Feature](/images/create_portal_3_a.png)

  3. Enter `PortalSite` as **Module** name.

  4. Click **Finish**.

    ![Web IDE Portal Feature](/images/create_portal_4.png)

  5. Fiori Launchpad Site has been added to the MTA project.

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

[ACCORDION-BEGIN [Step 4: ](Add IoT Service resource)]

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

[ACCORDION-BEGIN [Step 5: ](Add UAA service resource)]

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


[ACCORDION-BEGIN [Step 6: ](Configure service bindings)]

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

  12. **Save** the file.

    ![Service Binding 1](/images/service_binding_7.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test notification in Fiori Launchpad site)]

If you want to test notifications in Fiori Launchpad Site at this point, you'll need to comment out the references to `UI Deployer` in `mta.yaml`. The `UI Deployer` is used for deploying UI components but we have not added any UI components to our project yet and it will cause the deployment to fail. We'll add UI components to our project in the next tutorial [Build a Decision Support UI in the Web IDE](iot-ds-3-create-ui) and we will be able to test notifications without having to make these changes.

> Testing notification is optional at this point. To continue, please following the instructions below and complete Steps 8 to 12.
Otherwise, you can continue with the next tutorial [Build a Decision Support UI in SAP Web IDE](iot-ds-3-create-ui).

To comment out the references to `UI Deployer`:

  * open `mta.yaml` in Code Editor.

  * Type a `#` character in front of the lines shown below.

    ![Disable UI Deployer](/images/disable_uideployer.png)

  * **Save** the file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Build MTA project)]

  1. In Web IDE, right click on the iot-ds project and click **Build** to start the build of the project.

    ![WebIDE Build](/images/shared/webide_build_0_1.png)

  2. When the build is completed, a notification will be displayed at the top right corner of the Web IDE.  

  3. You can also check the status of the build by viewing the logs in the Web IDE console.

    ![WebIDE Build](/images/shared/webide_build_1.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Deploy MTA project)]

  1. You need to have the following Entitlements in your Cloud Foundry sub-account to **deploy** and **run** our application.  You can find more information on how to configure Entitlements in [Configure Entitlements and Quotas for Subaccounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html).

    Entitlement | Unit
    ------------ | -------------
    Portal|1 (or unlimited)
    HTML5 Application|2
    Application Runtime Memory| 1 Gib (Minimum)
    Destination|1

  2. To start a deployment, expand the `mta_archives` folder in your project, right click on the `mtar` file inside the folder and click **Deploy**.

    ![WebIDE Build](/images/shared/webide_build_3.png)

  3. Select the appropriate **Cloud Foundry API Endpoint**, **Organization** and **Space** for your deployment.  If you cannot find your selections, please check your Cloud Foundry settings in Web IDE preference.  For more information, please see Step 6 and Step 7 of [Prepare SAP Web IDE for Cloud Foundry Development](sapui5-webide-open-webide).

  4. Click **Deploy**.

    ![WebIDE Build](/images/shared/webide_build_4.png)

  5. When the deployment is completed, a notification will be displayed at the top right corner of the Web IDE.

  6. You can also check the status of the deployment by viewing the logs in the Web IDE console.

    ![WebIDE Build](/images/shared/webide_build_5.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Retrieve thing ID)]

  We will need to retrieve the Thing Id for the next Step. If you already have the Thing Id, please proceed to the next step.

  1. From Leonardo IoT Launchpad, select the Thing Modeler tile.

    ![Thing Modeler Tile 1](/images/shared/thing_modeler_tile.png)  

  2. Select the `greenhouse` package and select **Things** on the left panel.

  3. Click on the **Connectivity Information** icon on the top right corner.

    ![Thing Modeler 1](/images/shared/thing_modeler_1.png)  

  4. Copy the Thing Id.  We'll use it in the next step.

    ![Thing Id](/images/shared/thing_id.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Generate test notification)]

  1. From Leonardo IoT Launchpad, select the **Actions** tile.

    ![Actions Tile](/images/shared/launchpad_tile_actions.png)

  2. Select **Greenhouse Action**.

    ![Actions Tile](/images/shared/test_portal_1_1.png)

  3. Click **Test** button.

    ![Actions Tile](/images/shared/test_portal_2.png)

  4. Enter the **Thing Id** in the Test Action dialog. Click **Test**.

    ![Actions Tile](/images/shared/test_portal_3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Check notification in Fiori Portal site)]

  1. To find the URL of the Fiori Launchpad Site, go to the SAP Cloud Platform Cockpit, navigate to the space of the sub-account where the MTA project is deployed.

  2. Navigate to the **Applications** screen.

  3. Click on `iot-ds_appRouter`.

    ![Find Portal Url 1](/images/shared/find_portal_url_1.png)

  4. You should see the route displayed under **Application Routes**.

    ![Find Portal Url 1](/images/shared/find_portal_url_2.png)

  5. Append `/cp.portal/site` at the end of the Application Route to create the URL of the Fiori Launchpad Site.

    For example:

      Application Route:
      mytenant-space1-approuter1.cfapps.eu10.hana.ondemand.com

      The Fiori Launchpad Site URL will be
      `https://mytenant-space1-approuter1.cfapps.eu10.hana.ondemand.com/cp.portal/site`

  6. Launch Fiori Launchpad Site in a browser.  You should see a Fiori notification alert.

    ![Find Portal Url 1](/images/shared/test_notif_1.png)

  7. Click on the alert icon and the Fiori Notification Panel will be opened.

    ![Find Portal Url 2](/images/shared/test_notif_2.png)

[DONE]
[ACCORDION-END]
---
