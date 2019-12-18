---
title: Installing the streaming studio plugin
description: Install the streaming analytics plugin for SAP HANA studio, add a connection to the streaming analytics server, and create a HANA service.
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>beginner, products>sap-hana-streaming-analytics ]
---

## Prerequisites  
 **Proficiency:** Beginner

## Next Steps
 [Create a Streaming Project with SAP HANA streaming analytics](https://developers.sap.com/tutorials/sds-create-streaming-project.html)

## Details
Install the streaming analytics plugin for SAP HANA studio, add a connection to the streaming analytics server, and create a HANA service.
### You will learn  


### Time to Complete
**10 Min**

---

[ACCORDION-BEGIN [Step 1: ](Install the plugin)]
1. Use the Download Manager to download the streaming studio plugin file.

2. Extract the contents of the streaming studio plugin file.

3. Open studio and select _Help > Install New Software_:

    ![Install New Software](install-new-software-hdbstudio.png)

4. In the Install dialog, click **Add**:

    ![Click Add](click-add-hdbstudio.png)

5. In the Add Repository dialog, click **Local**, select the `<extracted_path>/<platform>/SAP_HANA_STREAMING/repository` folder, and click **OK**:

    ![Click Local](click-local-hdbstudio.png)

    ![Select the repository folder](select-repository-folder.png)

6. In the Install dialog, check SAP HANA streaming analytics, then click **Next**:

    ![Check SAP HANA streaming analytics](select-streaming-hdbstudio.png)

7. Review the items to be installed, then click **Next**:

    ![Review the items to be installed](review-items-hdbstudio.png)

8. Review the license, accept the terms and conditions to continue, then click **Finish**:

    ![Review and accept the license](accept-license-hdbstudio.png)

9. For any security warnings, click **OK**.

10. At the prompt to restart studio, select **Yes**.

11. If you are installing the plugin into studio for Linux, log off of the machine and log back in.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the streaming perspectives)]

1. In studio, select _Window > Perspective > Open Perspective > Other_, then select the **SAP HANA Streaming Development** perspective and click **OK**:

    ![SAP HANA Streaming Development perspective](open-development-hdbstudio.png)

2. Select _Window > Perspective > Open Perspective > SAP HANA Streaming Run-Test_:

    ![SAP HANA Streaming Run-Test perspective](open-run-test.png)

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add a streaming analytics connection)]

Here we will connect studio to the SAP HANA streaming analytics server.

1. In the **SAP HANA Streaming Run-Test** perspective, in the **Server** view, select **New Server URL**:

    ![New Server URL](new-server-url.png)

2. Enter the following connection details:
    - Host Name: The hostname or IP address for your SAP HANA installation
    - Port: 3`<instance-number>`26

    Enable SSL, then click **OK**:

      ![Enter connection details](enter-connection-details-hdbstudio.png)

3. Right-click on the new server and select **Change User Name and Password**:

    ![Change User Name and Password](change-credentials-hdbstudio.png)

4. Enter the credentials for `SYSTEM` and click **OK**:

    ![Enter the credentials for SYSTEM](enter-credentials-hdbstudio.png)

    >**Note:**
    > To automatically connect to the streaming server when starting studio, check **Use Secure Storage for Streaming Credentials**.

5. Right-click on the server and select **Connect Server**:

    ![Connect Server](connect-server-hdbstudio.png)

6. Open _Window > Preferences_ and select **SAP HANA streaming analytics** from the list:

    ![Open Preferences](open-preferences-hdbstudio.png)

    ![SAP HANA streaming analytics preferences](streaming-preferences-hdbstudio.png)

7. Set **Default Server URL** to the new server and click **OK**:

    ![Default Server URL](set-default-server-url-hdbstudio.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add a HANA service)]

In this step we will create a named **Data Service** on the SAP HANA streaming analytics server.  In this case, the data service will connect to a particular HANA database. Streaming projects use named data services to connect to database tables.

1. Switch to the **SAP HANA Streaming Development** perspective and select the **Data Services** tab:

    ![Data Services tab](data-services-tab-hdbstudio.png)

2. Right-click on the new server and select **Load Workspaces**:

    ![Load Workspaces](load-workspaces-hdbstudio.png)

3. Right-click on the **Server-wide** folder and select **Add HANA Service**:

    ![Add HANA Service](add-hana-service-hdbstudio.png)

4. Select **`newservice1`** and, in the **Properties** view:
    - Enter the credentials for `SYSTEM`.
    - Check **Use Default HANA Server**.

    ![Edit HANA service properties](hana-service-hdbstudio.png)

5. Right-click on **`newservice1`** and select **Rename Service**. Name the new service `hanadb`:
    ![Name the service](name-service-hdbstudio.png)

6. To confirm that the HANA service is configured properly, right-click on it and select **Discover**:

    ![Discover](discover-hdbstudio.png)

[DONE]

[ACCORDION-END]
