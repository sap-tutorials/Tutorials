---
parser: v2
auto_validation: true
time: 40
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, software-product>sap-btp--cloud-foundry-environment]
primary_tag: software-product>sap-integration-suite
author_name: Karunaharan V
author_profile: https://github.com/Karunaharan
---

# Design and Deploy Your First Integration Flow
<!-- description --> Design an integration flow to integrate an online webshop that exposes data via OData service and fetch the product details.

## Prerequisites
 - You have provisioned your SAP Integration Suite tenant. For more information, see [Set Up Integration Suite Trial](cp-starter-isuite-onboard-subscribe).


## You will learn
  - How to design and deploy an integration flow using the web-based integration flow designer
  - How to design an integration flow to fetch data from an online web shop that is available as an OData service


  ### Create an integration package and integration flow


1. Choose **Design** > **Integrations and APIs** > **Create** to create an integration package.

    >An integration flow must be associated with an integration package.

      In the **Header** tab, provide a **Name** and **Short Description** for your integration package.

    >The **Technical Name** gets populated automatically based on the name that you provide.

      Choose **Save** and then choose **Artifacts** to navigate to the artifacts tab. In this tab, you will create your first integration flow.

      <!-- border -->![Provide package details and navigate to artifacts](2-1-enter-integration-package-details.png)

2. Choose **Add** > **Integration Flow**.

      <!-- border -->![Add integration flow artifact](2-2-add-integration-flow-new.png)

      Enter a **Name** for the integration flow and choose **OK**.

      <!-- border -->![Enter integration flow details and confirm](2-2-enter-iflow-details.png)

3. Choose **Save** and open the integration flow by selecting it.

      <!-- border -->![Save integration package and open integration flow](2-3-save-open-iflow.png)

4. Choose **Edit** to start editing the integration flow.

    Choose **Restore** at the bottom right corner to bring up the **Property Sheet**. Property sheet is the place where you define the parameters for every step in the integration flow.

      <!-- border -->![Start editing the integration flow](3-1-edit-iflow.png)


### Connect sender channel with HTTPS adapter

In this step, you define your sender channel and sender adapter. In this case, we use a HTTPS sender adapter.

1. Choose the **Sender** step. Create the sender channel by clicking the arrow icon on **Sender** and dragging it to the **Start** step.

    <!-- border -->![Create the sender channel](4-1-connect-sender-channel.png)

2. In the **Adapter Type** prompt, select the **HTTPS** adapter.

    <!-- border -->![Select HTTPS adapter](4-2-select-https-adapter.png)

3. In the property sheet, select the **Connection** tab. In the **Address** field, enter **`/products/details`**.

    Optionally, you can enter any value of your choice, but ensure that you use **"/"** symbol before specifying the endpoint name. Deselect the **CSRF Protected** checkbox (this will be selected by default).

    <!-- border -->![Configure HTTPS connection](4-3-configure-https-connection.png)



### Add JSON to XML converter

Input to the integration flow is sent in JSON format. At a later point, you communicate to a web shop, which is a OData service. You use the converter for this JSON to XML conversion. After the input is converted into XML, the message is sent as header information to the OData service to fetch the required product details.

1. From the palette (the grey bar on the top containing integration flow steps), choose **Message Transformers > Converter > JSON to XML Converter**.

    <!-- border -->![JSON to XML Converter](5-1-json-to-xml-converter.png)

2. Connect the converter to the message path by clicking on the message path.

    <!-- border -->![Connect JSON to XML Converter to message path](5-2-connect-jsonxml-converter.png)


### Add and configure content modifier


1. Choose **Message Transformers > Content Modifier** and add it to the message path, as you did for the **JSON to XML Converter**.

    <!-- border -->![Add Content Modifier](6-1-connect-content-modifier.png)

2. In the property sheet, choose **Message Header > Add** to configure the **Content Modifier**. Enter the following parameters:

    |  Field Name     | Description
    |  :------------- | :-------------
    |  **Action**           | **`Create`**
    |  **Name**           | **`productIdentifier`**
    |  **Source Type**        | Select **`XPath`** from the dropdown list
    |  **Source Value**       | **`//productIdentifier`**
    |  **Data Type**    | **`java.lang.String`**

    <!-- border -->![Configure Content Modifier](6-2-configure-content-modifier.png)


### Add request reply step


From the palette, choose **Call** > **External Call** > **Request Reply**. Connect it to the message path, similar to the previous steps.

  <!-- border -->![Connect request reply step](7-1-connect-request-reply-1.png)

  <!-- border -->![Connect request reply step](7-1-connect-request-reply.png)



### Connect request reply to receiver


1. Move the **Receiver** step below the **Request Reply** step by selecting it and dragging it to the desired position on the editor. You do this to ensure that your integration flow is elegantly designed.

    >**TIP:** Use the pan and zoom controls (highlighted in yellow) for resizing the canvas. Use the guidelines to ensure that **Receiver** is aligned with the **Request Reply** step.

    <!-- border -->![Move Receiver below Request Reply](8-1-move-receiver.png)

2. Connect the **Request Reply** to **Receiver** by dragging the arrow icon on **Request Reply** to the **Receiver**.

    <!-- border -->![Connect Request Reply to Receiver](8-2-connect-requestreply-receiver.png)

3. In the **Adapter Type** prompt, select **OData**. In the **Message Protocol** prompt, select **OData V2**.

    <!-- border -->![Select adapter](8-3-select-odata-adapter.png)

    <!-- border -->![Select message protocol](8-3-select-odata-adapter-msg-protocol.png)

4. Select the **Connection** tab. In the **Address** field, enter **`https://refapp-espm-ui-cf.cfapps.eu10.hana.ondemand.com/espm-cloud-web/espm.svc`**. This is the URL of the online web shop from which you will fetch the product details.

    <!-- border -->![Enter connection details for OData adapter](8-4-odata-connection-details.png)

5. Select the **Processing** tab and choose **Select** in the **Resource Path** field.

    <!-- border -->![OData processing resource path selection - 1](8-5-odata-processing-1.png)

6. Ensure the connection details are the same and choose **Step 2**.

    <!-- border -->![OData processing resource path selection - 2](8-6-odata-processing-2.png)

7. Choose the **Select Entity** field and choose **Products** from the dropdown list.

    <!-- border -->![OData processing resource path selection - 3](8-7-odata-processing-3.png)

8. Enable the **Select All Fields** checkbox and choose **Step 3**.

    <!-- border -->![OData processing resource path selection - 4](8-8-odata-processing-4.png)

9. Choose the **Select Field** icon.

    <!-- border -->![OData processing resource path selection - 5](8-9-odata-processing-5.png)

10. Choose **Product ID** and then **OK**.

    <!-- border -->![OData processing resource path selection - 6](8-10-odata-processing-6.png)

11. In the dropdown list, select **Equal**. In the value field, enter **`${header.productIdentifier}`**. Choose **Finish**.

    <!-- border -->![OData processing resource path selection - 7](8-11-odata-processing-7.png)

12. Choose **Save**.

Now you have configured the OData adapter to fetch the details of the product based on the product ID that you send as input while making the HTTP call.



### Deploy the integration flow


1. Choose **Deploy** to deploy the integration flow. Choose **Yes** in the confirmation dialog for deployment. Upon deployment confirmation, choose the **Deployment Status** tab in the property sheet.

    <!-- border -->![Deploy integration flow and access deployment status](9-1-deploy-iflow.png)

2. In the **Deployment Status** tab, you can see details about the deployment for the integration flow. The expected deployment status is **Deployed** and runtime status is **Started**.

    <!-- border -->![View deployment status](9-2-view-deployment-status.png)


Later in the mission, you will use the endpoint of the deployed integration flow in a subsequent step. You will define the integration flow endpoint as application programming interface (API). And finally, call the integration flow API using API Management.

