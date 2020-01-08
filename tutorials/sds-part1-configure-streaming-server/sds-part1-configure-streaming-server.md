---
title: Configure SAP HANA Smart Data Streaming Server Connection
description: Part 1 of 9. Configure SAP HANA streaming connection and add SAP HANA data service in SAP HANA Studio.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [ tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana-studio ]
author_name: Bill Jiang
author_profile: https://github.com/BillJiangSAP
time: 20
---
## Prerequisites  
 - **System:** You must have an SAP HANA SPS10 system or higher with smart data streaming installed that you can connect to and use.
 - **Integrated Development Environment:** You must have SAP HANA Studio 2 installed with the streaming plug-in installed and configured.
 - This tutorial assumes you will be using the HANA SYSTEM user to connect to your HANA system and will refer to it as "SYSTEM".

## Details
### You will learn
 - Connecting to a SAP HANA system.
 - Setting up the SAP HANA Streaming Development and Run perspective.
 - Connecting to a streaming server in SAP HANA Studio.
 - Adding and configuring a SAP HANA data service.


---

[ACCORDION-BEGIN [Step 1: ](Connect to a SAP HANA System)]    

1. Open HANA Studio. In the **SAP HANA Administration Console** perspective, right click in the white space within the **Systems** view. Next select the **Add System...** menu item to execute it. You can also press **s**.

    ![Add System](connect-to-hana-system-1-add-system.png)

2. Enter the Host Name and the Instance Number of the HANA server you will connect to.

    ![Hana Server Info](connect-to-hana-system-2-hana-server-info.png)

3. Click **Next**. You can also press **Alt+n**.

    ![Click Next](connect-to-hana-system-3-click-next.png)

4. Enter `SYSTEM` as the **User Name** and enter your password.

    ![Username and Password](connect-to-hana-system-4-user-and-pass.png)

5. Click the **Store user name and password in secure storage** checkbox so you won't have to re-enter
your credentials when re-connecting to the system. Next click **Finish** or press **Alt+f**.

    ![Click Finish](connect-to-hana-system-5-click-finish.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open Streaming Perspective)]    

1. Go to **Windows** > **Perspective** > **Open Perspective** and click the **Other...** menu item to execute it. You can also press **o**.

    ![Open Perspective](open-perspective-1-add-streaming-perspective.png)

2. Select the **SAP HANA Streaming Development** entry by clicking it.

    ![Select Streaming Dev](open-perspective-2-hana-dev.png)

3. Click **OK**.

4. Repeat for the **SAP HANA Streaming Run-Test** perspective entry.

    ![Select Streaming Run-Test](open-perspective-4-hana-run-test.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create Server Connection in SAP HANA Streaming Development Perspective)]    

1. Go to the **SAP HANA Streaming Run-Test** perspective.

    ![Open Streaming Perspective](add-streaming-server-1-open-streaming-perspective.png)

2. In the **Server** view, select the **New Server URL** tool.

    ![Add Server](add-streaming-server-2-add-server-button.png)

3. Now you will connect to the streaming server (Not the HANA database server). Enter the host name in the **Host Name** field. The **Port** number has the format `3XX26` (XX = Instance Number. e.g. If your Instance Number is "10" then you would enter "31026" for port). Obtain these from your system administrator.

    ![Server Info](add-streaming-server-3-server-info.png)

    NOTE: This is the host name and port for the streaming server, not the HANA database server.
    Make sure to check the **SSL** checkbox as the server uses SSL for encryption.

4. You should now see the streaming server listed in the **Server** view. Right click it and select **Change User Name and Password**.

    ![Change Username and Password](add-streaming-server-4-change-username-and-password.png)

5. Enter the streaming server's credentials and check **Use Secure Storage for Streaming Credentials**. Press **OK** to continue.

    ![Enter Username and Password](add-streaming-server-5-enter-username-and-password.png)

6. Right click your streaming server listed in the **Server** view again. Then select **Connect Server**.

    ![Connect Server](add-streaming-server-6-connect-to-server.png)

7. Now go to HANA Studio's **Preferences** under the **Windows** menu. Then go to the **SAP HANA Smart Data Streaming** tab.

    ![Preferences](add-streaming-server-7-preferences.png)

8. Set the **Default Server URL** to your server `<server.address>:<port>`. The server address is the Smart Data Streaming server node, not the HANA server node. The port number should be in the format of "3XX26", where XX is the Instance Number.

    ![Default Server](add-streaming-server-8-default-server.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure HANA Data Service)]    

1. Click **SAP HANA Streaming Development** tab to open this perspective.

    ![Dev perspective](configure-data-service-1-dev-perspective.png)

2. Click the **Data Services** tab to open this view. This is normally tucked behind the **Projects** view.

    ![Data Services](configure-data-service-2-data-services.png)

3. Go to your connected streaming host by double clicking it and right click on your **Server-wide** folder.

    ![Server Wide](configure-data-service-3-server-wide.png)

4. Click the **Add HANA Service** menu item to execute it.

    ![Add HANA Service](configure-data-service-4-add-hana-service.png)

5. Click on **`newservice1`** to select it.

    ![New Service](configure-data-service-5-new-service.png)

6. Go to the **Properties** view, which should be located on the bottom half of the studio by default. Enter the username and password corresponding to the HANA database you wish to connect to and check **Use Default HANA Server**.

    ![Service Info](configure-data-service-6-service-info.png)

7. Rename the service. Right click on **`newservice1`** in the **Data Services** view and select **Rename Service**. Change the name to `freezermon_service`.

    ![rename Service](configure-data-service-7-rename-service.png)

8. To verify that the service is properly configured, right click again on the **`freezermon_service`** service, and select **Discover**. When the service is correctly configured, executing the **Discover** operation will display the database schema that is accessible through the service.

    ![Discover Schema](configure-data-service-8-discover-schema.png)

[DONE]

[ACCORDION-END]
