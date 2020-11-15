---
title: Design and Test Your First Integration Flow
description: Design an integration flow to integrate an online webshop that exposes data via OData service and fetch the product details. Your request will be sent via Postman client.
auto_validation: true
time: 45
tags: [ tutorial>beginner, products>sap-cloud-platform, products>sap-cloud-platform-connectivity, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-integration-for-process-services
author_name: Vikram Kulkarni
author_profile: https://github.com/Vikramkulkarni01
---

## Prerequisites
 - You have provisioned your SAP Cloud Platform Integration Suite tenant. For more information, see [Set Up Integration Suite Trial](cp-starter-isuite-onboard-subscribe).
 - You have [downloaded](https://www.getpostman.com/downloads/) and installed the Postman client.

## Details
### You will learn
  - How to assign role collection
  - How to design and deploy an integration flow using the web-based integration flow designer
  - How to design an integration flow to query data from an online web shop that is available as an OData service



[ACCORDION-BEGIN [Step 1: ](Assign Role Collection)]
 In this step, you create a required role collection for executing this scenario.

 1. In your subaccount, go to **Security > Role Collections** and click **+** button to add new role collections.
  !![Add Role Collections](St1.1Add Role Collections.png)

 2. Enter the name **`EsbMessagingSend`** and click **Create**.
  !![Create Role Collection](St1.2Create Role Collection.png)

 3. In the **Role Collections** page, search for the role **`EsbMessagingSend`** created in the previous step and click it.
  !![Search Role Collection](St1.3Search Role Collection.png)

 4. Choose **Edit**.
    !![Edit Role Collection](St1.4 Edit Role Collection.png)

 5. In the **Role Name** field, click **Search** button to search for the role name.
    !![Search Role Name](St1.5 Search Role Name.png)

 6. Search for the role name **`MessagingSend` > Select Role** and click **Add**.
    !![Add Role name](St1.6 ADD Role Name.png)

 7. Choose **Save**.
    !![Save Role Name](St1.7 Save Role Name.png)

 8. Go to **Security > Trust Configuration** and click **sap.default**.
    !![Trust Configuration page 1](St1.8 Trust Configuration.png)

 9. Enter the user's e-mail address and click **Show Assignments** to see the role collections that are currently assigned to this user. Then click **Assign Role Collection**.
    !![Trust Configuration Initial Screen](St1.9 Trust Configuration_part1.png)

 10. Select role **`EsbMessagingSend`** and Click **Assign Role Collection**.
    !![Assigning Role ](St1.10 Assigning Role Collection.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access your workspace)]

  Click the **Design** tab (pencil icon) to access your workspace. This is where you will create your integration package and integration flow.

  ![Access workspace](1.1.access-workspace.png)

  This is the space where you will work on your integration flows and where you can access all your integration projects.

[DONE]
[ACCORDION-END]

  [ACCORDION-BEGIN [Step 3: ](Create an integration package and integration flow)]

  1. Choose **Create** to create a new integration package.

    >An integration flow should be associated with an integration package.

      ![Create integration package](2.1.create-integration-package.png)

      In the **Header** tab, provide a **Name** and **Short Description** for your integration package.

    >The **Technical Name** gets populated automatically based on the name that you provide.

      Choose **Save** and then choose **Artifacts** to navigate to the artifacts tab. In this tab, you will create your first integration flow.

      ![Provide package details and navigate to artifacts](2.2.enter-integration-package.details.png)

  2. Choose **Add** > **Integration Flow**.

      !![Add integration flow artifact](2.3.add_integration_flow_new.png)

      Enter the **Name** for integration flow and choose **OK**.

      !![Enter integration flow details and confirm](2.4.enter-iflow-details.png)

  3. Save the integration package by choosing **Save** and open the integration flow by selecting it.

      You can then edit the integration flow to add the required steps to create your integration scenario.

      ![Save integration package and open integration flow](2.5.save-open-iflow.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Edit the integration flow)]
1. Access your integration package by choosing **Design** > **(Integration package name)**.

    !![Access integration package](1.1.access-integration-package.png)


2. Access your integration flow by choosing **Artifacts** > **(Integration flow name)**.

    !![Access integration flow](1.2.access-integration-flow.png)

3. Start editing the integration flow by choosing **Edit**.

    !![Start editing the integration flow](1.3.edit-iflow.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Connect sender channel with HTTPS adapter)]

1. Create the sender channel by clicking the arrow icon on **Sender** and dragging it to the **Start Message** step.

    !![Create the sender channel](2.1.connect-sender-channel.png)


2. In the **Adapter Type** prompt, select the **HTTPS** adapter.

    !![Select HTTPS adapter](2.2.-select-https-adapter.png)

3. Select the **Connection** tab. In the **Address** field, enter **`/CloudIntegrationTrial`**.

    Optionally, you can enter any value of your choice, but ensure that you use **"/"** symbol before specifying the endpoint name. Deselect the **CSRF Protected** checkbox (this will be selected by default).

    !![Link text e.g., Destination screen](2.3.configure-http-connection.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Add JSON to XML converter)]

1. From the palette (the grey sidebar containing integration flow steps), choose **Message Transformers > Converter > JSON to XML Converter**.

    You add this converter as the input to the integration flow is sent in JSON format. After it is converted into XML, the message is sent as header information to the OData service to fetch the required product details.

    !![Select JSON to XML converter from palette](3.1.select-xmljson-converter.png)

2. Connect the converter to the message path by clicking on the message path.
    >**TIP:** When you place your cursor on the message processing path, you see it change to green color.

    !![Connect JSON to XML Converter to message path](3.2.connect-jsonxml-converter.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add and configure content modifier)]

1. Choose **Message Transformers** > **Content Modifier** and add it to the message path, as you did for the **JSON to XML Converter**.

    !![Add Content Modifier](4.1.connect-content-modifier.png)

2. Configure the **Content Modifier** by selecting the **Message Header** tab and entering the following parameters:


    |  Field Name     | Description
    |  :------------- | :-------------
    |  **Name**           | **`productIdentifier`**
    |  **Type**           | Select **`XPath`** from the dropdown list
    |  **Data Type**    | **`java.lang.String`**
    |  **Value**          | **`//productIdentifier`**

    !![Configure Content Modifier](4.2.configure-content-modifier.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add request reply step)]

From the palette, choose **Call** > **External Call** > **Request Reply**. Connect it to the message path, similar to the previous steps.

!![Connect request reply step](5.1.connect-request-reply.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Connect request reply to receiver)]

1. Move the **Receiver** step below the **Request Reply** step by selecting it and dragging it to the desired position on the palette. You do this to ensure that your integration flow is elegantly designed.

    >**TIP:** Use the pan and zoom controls (highlighted in yellow) for resizing the canvas. Use the guidelines to ensure that **Receiver** is aligned with the **Request Reply** step.

    !![Move Receiver below Request Reply](6.1.move-receiver.png)

2. Connect the **Request Reply** to **Receiver** by dragging the arrow icon on **Request Reply** to the **Receiver**.

    !![Connect Request Reply to Receiver](6.2.connect-requestreply-receiver.png)

3. In the **Adapter Type** prompt, select **OData**. In the **Message Protocol** prompt, select **OData V2**.

    !![Select adapter and message protocol](6.3.select-odata-adapter.png)

4. Select the **Connection** tab. In the **Address** field, enter **`https://espmrefapps.hana.ondemand.com/espm-cloud-web/espm.svc`**. This is the URL of the online web shop from which you will fetch the product details.

    !![Enter connection details for OData adapter](6.4.odata-connection-details.png)

5. Select the **Processing** tab and choose **Select** in the **Resource Path** field.

    !![OData processing resource path selection - 1](6.5.odata-processing-1.png)

6. Ensure the connection details are the same and choose **Step 2**.

    !![OData processing resource path selection - 2](6.6.odata-processing-2.png)

7. Click the **Select Entity** field and choose **Products** from the dropdown list.

    !![OData processing resource path selection - 3](6.7.odata-processing-3.png)

8. Enable the **Select All Fields** checkbox and choose **Step 3**.

    !![OData processing resource path selection - 4](6.8.odata-processing-4.png)

9. Choose the **Select Field** icon.

    !![OData processing resource path selection - 5](6.9.odata-processing-5.png)

10. Select **Product ID** and choose **OK**.

    !![OData processing resource path selection - 6](6.10.odata-processing-6.png)

11. In the dropdown list, select **Equal**. In the value field, enter **`${header.productIdentifier}`**. Choose **Finish**.

    !![OData processing resource path selection - 7](6.11.odata-processing-7.png)

In this step, you have configured the OData adapter to fetch the details of the product based on the product ID that you send as input while making the HTTP call.

[VALIDATE_6]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Deploy the integration flow)]

1. Choose **Deploy** to deploy the integration flow. Choose **Yes** in the confirmation dialog for deployment. Once you see the deployment confirmation, choose the **Monitor** tab to access the monitoring view.

    !![Deploy iflow and access monitoring view](7.1.deploy-iflow.png)

2. In the **Monitor** view, under the **Manage Integration Content** section, choose **All** to access all the artifacts that you have deployed. You will also see the integration flow that you have deployed here.

    !![Access deployed integration content in monitoring view](7.2.acess-deployed-artifacts.png)

3. Please wait till the integration flow is in status **Started**. Then, select the integration flow and in the **Endpoints** tab, choose the **Copy** icon.

![Copy Endpoints](7.3.copy-endpoint-url.png)

You will use this endpoint in the Postman client to test your integration flow.  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Test integration flow using Postman)]

1. Launch the Postman client that you have installed on your local machine. In the **Enter request URL** field, enter the integration flow endpoint that you copied after deploying the integration flow. Then, select the **POST** operation from the dropdown list.

    ![Create new postman request](8.1.create-new-postman-request.png)

2. Select the **Authorization** tab and choose **Basic Auth** in the **Type** dropdown list. In the **Username** and **Password** field, enter the **`EmailID`** and **`Password`** that you used to access the SAP Cloud Platform Integration Suite tenant.

    ![Provide Authorization details](8.2.provide-auth-details.png)

3. Select the **Body** tab and choose **raw** radio button. In the form below, enter

    ```JSON
    {
      "productIdentifier": "HT-2000"
    }
    ```

    Choose **Send**. This is the ID of the product that you are requesting from the web shop.

    >**TIP:** If you would like to query a different product, access <https://espmrefapps.hana.ondemand.com/espm-cloud-web/webshop/> and select a product of your choice. Copy its **Product ID** and use that ID in the Postman request.

    ![Enter payload details and send request.](8.3.enter-payload-send.png)

4. Observe the response that you receive in the **Postman** client. You can see the product details fetched from the web shop. You can also double-check these details by accessing the web shop directly and searching for the product ID that you sent as input in the Postman request.

    ![Observe response in Postman](8.4.observe-response.png)

[DONE]
[ACCORDION-END]

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=cp-starter-integration-cpi-design-iflow" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
