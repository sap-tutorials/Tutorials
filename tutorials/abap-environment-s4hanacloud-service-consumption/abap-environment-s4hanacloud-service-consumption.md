---
auto_validation: true
title: Create Service Consumption Model for Business Partner and Sales Order Item Cube
description: Create Service Consumption Model for Business Partner and Sales Order Item Cube.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>intermediate, topic>abap-development, products>sap-business-technology-platform, tutorial>license ]
time: 45
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
 - Create a developer user in a SAP BTP, ABAP Environment system.
 - Download the latest Eclipse and install ADT <https://tools.hana.ondemand.com/#abap>.

## Details
### You will learn  
  - How to create destination
  - How to create service consumption model for business partner and sales order item cube

---

[ACCORDION-BEGIN [Step 1: ](Copying the inbound service URL)]
  1. Log in to SAP S/4HANA Cloud system as administrator and select **Communication Arrangement** in the **Communication Management** section.

      ![Copying the inbound service URL](administrator.png)

  2. Select the communication arrangement that you have created.

      ![Copying the inbound service URL](arrangement2.png)

  3. Copy the business partner (A2X) URL for later use.

      ![Copying the inbound service URL](arrangement4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create destination with basic authentication)]
  1. Log in to the [SAP BTP cockpit](https://account.hana.ondemand.com/) and navigate to your **subaccount**. Select **Destinations** under **Connectivity** and create a new destination.

      ![Create destination with basic authentication](cockpit.png)

  2. Enter destination configuration:

      -  Name: your destination name
      -  Type: HTTP
      -  Description: your description
      -  URL: Service URL from communication arrangement ( Business Partner (A2CX) ODataV2 service URL)
      -  (without `/sap/opu/odata/sap/API_BUSINESS_PARTNER`)
      -  Proxy Type: Internet
      -  Authentication: `BasicAuthentication`
      -  User: <your_communication_user>
      -  Password: <password_of_communication_user>

      ![Create destination with basic authentication](destination3.png)

   3. Check connection.

      ![Create destination with basic authentication](connection.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create Custom CDS View in SAP S/4HANA Cloud)]
  1. Log in to your SAP S/4HANA Cloud system, navigate to Extensibility and open the Custom CDS Views tile.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds.png)

  2. Click Create to create your custom CDS view.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds2.png)

  3. Name your CDS view `YY1_SALESORDERITEMCUBEXXX` and label. Select **External API** as scenario and click **Create**.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds3.png)

  4. Search for **`I_SalesOrderItemCube`** and select it.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds4.png)

  5. Click **Add** and select **Associated Data Source**.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds5.png)

  6. Search for **`I_BusinessUser`**, select it and click **OK**.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds6.png)

  7. Select the butterfly symbol to add a join condition.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds7.png)

  8. Click **Add**.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds8.png)

  9. Search for `userid` and select it.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cdsx.png)

 10. Add a value to your join condition, therefore select the **field symbol** under Value.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds10.png)

 11. Search for `CreatedByUser` under `I_SalesOrderItemCube` and select it.

       ![Create Custom CDS View in SAP S/4HANA Cloud](cds11.png)

 12. Now your join condition is complete. Click **Close**.

       ![Create Custom CDS View in SAP S/4HANA Cloud](cds12.png)

 13. Navigate to **Parameters** and add manual parameters for the exchange rate type and display currency.

       ![Create Custom CDS View in SAP S/4HANA Cloud](cds16.png)

 14. Navigate to **Elements**, select **Add** > **Elements**.

       ![Create Custom CDS View in SAP S/4HANA Cloud](cds13.png)

 15. Search for `BusinessPartner`, select it and click **OK**.

       ![Create Custom CDS View in SAP S/4HANA Cloud](cds14.png)

 16. Add also the following elements to your CDS view:
     - `SalesOrder`
     - `SalesOrderItem`
     - `CreationDate`
     - `NetAmountInDisplayCurrency`
     - `DisplayCurrency`

  17. Check your result and click **Publish**.

      ![Create Custom CDS View in SAP S/4HANA Cloud](cds15.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create custom communication scenarios)]
  1. Navigate to **Extensibility** and select **Custom Communication Scenarios**.

      ![Create custom communication scenarios](extensibility.png)

  2. Click **New**.

      ![Create custom communication scenarios](new2.png)

  3. Create a communication scenario:
     - Communication Scenario ID: `Salesorderitemcubexxx`
     - Description: Sales order item cube

     Click **New**.

      ![Create custom communication scenarios](new3.png)

  4. Click `+` to add an inbound service.

      ![Create custom communication scenarios](new.png)

  5. Search for `YY1_SalesOrderItemCubeXXX`, select it and click **OK**.

      ![Create custom communication scenarios](newscenario.png)

  6. Select your inbound service and click **Publish**.

      ![Create custom communication scenarios](publishx.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Download metadata for business partner)]
  1. Go to the SAP API Business Hub and select the OData service for business partner: (https://api.sap.com/api/API_BUSINESS_PARTNER/overview).

     Copy the production URL.

      ![Download metadata for business partner](ok.png)

  2. Now open a browser of your choice and past the production URL with your hostname and port.
     Add also `$metadata` at the end, like following:

     ```URL
       https://{host}:{port}/sap/opu/odata/sap/API_BUSINESS_PARTNER/$metadata
     ```

     Log in with you communication user and password to see your metadata.

      ![Download metadata for business partner](logon.png)

  3. You can see the metadata. Save your metadata as an `edmx` file.

      ![Create communication system](edmx.png)

  3. Use `bupa.edmx` and select all files as file type.

      ![Download metadata for business partner](edmx2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create service consumption model for business partner)]
  1. Switch to Eclipse an create a package:
     - Name: `Z_Package_XXX`
     - Description: My Package

     Click **Next**.

      ![Create service consumption model for business partner](package.png)

  2. Click **Next**.

      ![Create service consumption model for business partner](package2.png)

  3. Click **Finish**.

      ![Create service consumption model for business partner](package4.png)

  4. Select **File** > **New** > **Other…**

      ![Create service consumption model for business partner](consumption.png)

  5. Search **Service Consumption Model**, select it and click **Next**.

      ![Create service consumption model for business partner](consumption2.png)

  6. Create a service consumption model.
     - Name: `Z_BUSINESSPARTNER_XXX`
     - Description: Service Consumption Model for business partner

     Select your metadata `bupa.edmx` and click **Next**.

      ![Create service consumption model for business partner](consumption3.png)

  7. Click **Next**.

      ![Create service consumption model for business partner](consumption4.png)

  8. Click **Next**.

      ![Create service consumption model for business partner](consumption5.png)

  9. Click **Finish**.

      ![Create service consumption model for business partner](consumption6.png)

  10. Check your result.

      ![Create service consumption model for business partner](consumption7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create service consumption model for sales order item)]
  1. Open your SAP S/4HANA Cloud system and navigate to **Communication Management** and select **Communication Arrangements**.

      ![Create service consumption model for sales order item](com.png)

  2. Search for your communication arrangement `yy1_salesorderitemcubexxx` and select it.

      ![Create service consumption model for sales order item](comm2.png)

  3. Copy the service URL of your communication arrangement.

      ![Create service consumption model for sales order item](comm3.png)

  4. Paste the copied service URL in your browser, add `/$metadata` at the end of your URL and log in with you communication user and password to see your metadata.

      ![Create service consumption model for sales order item](consumption12.png)

  5. You can see the metadata. Save your metadata as an `edmx` file.
     Use `salesorder.edmx` and select all files as file type.

      ![Create service consumption model for sales order item](consumption13.png)

      ![Create service consumption model for sales order item](consumption14.png)

  6. Go to Eclipse and select **File** > **New** > **Other…**

      ![Create service consumption model for sales order item](consumption15.png)

  7. Search **Service Consumption Model**, select it and click **Next**.

      ![Create service consumption model for sales order item](consumption16.png)

  8. Create a service consumption model.
     - Name: `Z_SALESORDERITEMCUBE_XXX`
     - Description: `Service Consumption Model for SalesOrderItemCube`

     Select your metadata `salesorder.edmx` and click **Next**.

      ![Create service consumption model for sales order item](consumption17.png)

  9. Click **Next**.

      ![Create service consumption model for sales order item](consumption18.png)

  10. Click **Next**.

      ![Create service consumption model for sales order item](consumption19.png)

  11. Click **Finish**.

      ![Create service consumption model for sales order item](consumption20.png)

  12. Check your result.

      ![Create service consumption model for sales order item](consumption21.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
