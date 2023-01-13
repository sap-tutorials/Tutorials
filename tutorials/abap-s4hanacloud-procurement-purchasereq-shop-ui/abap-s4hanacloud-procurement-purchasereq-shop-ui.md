---
parser: v2
auto_validation: true
primary_tag: software-product-function>s-4hana-cloud-abap-environment
tags:  [ tutorial>beginner, software-product>sap-btp--abap-environment, software-product-function>s-4hana-cloud-abap-environment, programming-tool>abap-development, programming-tool>abap-extensibility]
time: 25
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Create a SAP Fiori App and Deploy it to SAP S/4HANA Cloud, ABAP Environment
<!-- description --> Create a SAP Fiori app for a RAP business object in SAP Business Application Studio and deploy it to SAP S/4HANA Cloud, ABAP Environment.

## Prerequisites  
- You have a license for SAP S/4HANA Cloud and have a developer user in it
- You have installed the latest [Eclipse with ADT](abap-install-adt).
- **Trial:** You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.
- Business Catalog `SAP_CORE_BC_COM` must be assigned to business user
- The user must have the same email address as the user from trial

## You will learn  
- How to assign role collections
- How to create dev spaces
- How to set up organization and space
- How to create list report object pages
- How to run SAP Fiori applications
- How to deploy applications
- How to check BSP library in Eclipse
- How to create IAM apps and business catalogs

## Intro
>**HINT**: The administrator receives an welcome e-mail after provisioning. This e-mail includes the system URL. By removing `/ui` you can log into the SAP S/4HANA Cloud ABAP Environment system. Further information can be found [here](https://help.sap.com/docs/SAP_S4HANA_CLOUD/6aa39f1ac05441e5a23f484f31e477e7/4b962c243a3342189f8af460cc444883.html?locale=en-US&state=DRAFT).

---
### Assign role collection to user


  1. Login to [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/) and click **Enter Your Trial Account**.

      ![assign role collection](bas1.png)

  2. Select your subaccount **trial**.

      ![assign role collection](bas2.png)

  3. Now you are in the trial overview page. Click **Trust Configuration**.

      ![assign role collection](basxnew.png)

  4. Click **Default identity provider** to set up your trust.

      ![assign role collection](bas3.png)

      **HINT:** If you are using a licensed system, make sure you have the trust administrator role assigned to your user.

  5. Enter your e-mail address and click **Show Assignments**.

      ![assign role collection](bas5.png)

  5. Click **Assign Role Collection** .

      ![assign role collection](bas6.png)

  6. Select **`Business_Application_Studio_Developer`** and click **Assign Role Collection**.

      ![assign role collection](bas7.png)

  7. Check your result. Now your user should have the **`Business_Application_Studio_Developer`** role collection assigned.

      ![assign role collection](bas8.png)

      You are now able to develop on SAP Business Application Studio.



### Configure destination


  1. Select your subaccount **trial**.

    ![assign role collection](bas2.png)

  2. In the navigation pane expand the **Connectivity** section and select **Destinations**. Click **New Destination**.

    ![assign role collection](destinations.png)

    Configure the new destination with the following standard field values.

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | **`System_XXX_SAML_ASSERTION`**
    |  Type           | **`HTTP`**
    |  Description    | **`SAML Assertion Destination to SAP S/4HANA ABAP Environment system_xxx`**
    |  URL          | In the SAP S/4HANA Cloud tenant, navigate to the **Communication Systems** app and copy the **Host Name** from **Own System** = `Yes`<div><!-- border -->![Own System Host Name in Communication Systems App](s4hc-cs-own-system-host-name.png)</div> and paste it with prefix `https://` for example `https://my12345-api.s4hana.ondemand.com.`
    |  Proxy Type   | **`Internet`**
    |  Authentication | **`SAMLAssertion`**
    |  Audience   | Enter the URL of your system and remove `-api`, for example `https://my12345.s4hana.ondemand.com`.
    |  `AuthnContextClassRef` | **`urn:oasis:names:tc:SAML:2.0:ac:classes:PreviousSession`**

    Select **New Property** and maintain the following **Additional Properties** and values.

    |  Field Name     | Value          | Remark
    |  :------------- | :------------- | :-------------
    |  HTML5.DynamicDestination           | **`true`**   |&nbsp;
    |  HTML5.Timeout           | **`60000`**   | value stated in milliseconds. 60000 equals 1 minute. Required as deployment needs longer than the standard of 30 seconds.
    |  `WebIDEEnabled`    | **`true`**   |&nbsp;
    |  `WebIDEUsage`          | **`odata_abap,dev_abap`**   |&nbsp;
    |  `nameIDFormat`     | **`urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress`**  | Required in case your subaccount sends mail address as SAML Name ID for authentication ( **Subject Name Identifier** in Identity Authentication tenant ), although SAP S/4HANA Cloud tenant expects user login by default. **That is the case with a trial Account.** This also requires the mail address to be maintained for SAP S/4HANA Cloud tenant business users.

    Make sure that the **Use default JDK truststore** checkbox is ticked.

      ![Configure Destination](destinations2.png)

    Click **Save**.

  3. Click **Download Trust**.

    ![assign role collection](trust.png)



### Create communication system


  1.  Select your system and right-click **Properties**.

      ![dev](properties.png)

  2.   Select **ABAP Development** and copy the **System URL** without the `-api`. Paste in a browser and log in to the system.

      ![dev](properties2.png)

  3.  In your SAP Fiori launchpad select **Communication Systems** under Communication Management.

      ![dev](system.png)

  4. Click **New**.

      ![dev](system2.png)

  5. Create a new communication system:
       - System ID: **``BAS_TRIAL_XXX``**
       - System Name: **``BAS_TRIAL_XXX``**

       Click **Create**.

     ![dev](system3.png)

  6. Click the arrow and select **Technical Data**.

      ![dev](system4.png)

  7. Check **Inbound Only** in the general section. Set `SAML Bearer Assertion Provider` **ON** and click **Upload Signing Certificate**.

      ![dev](system5.png)

  8. Click **Browse** and select your trust configuration, then click **Upload**.

      ![dev](system6.png)

  9. Copy everything after **`CN=` of your signing certificate subject** and past it in **`SAML Bearer Issuer`**. Click **Save**.

      ![dev](signing.png)

      Now your communication system is set up.





### Create dev space


  1.  Select **trial**.

      ![dev](destination3.png)

  2.   Select **Service Marketplace** and search for **SAP Business Application Studio**. Select actions and click **Go to Application**.

      ![dev](studio212.png)

  3.  Check the privacy statement and click **OK**.

      ![dev](studio2.png)

  4. Now the SAP Business Application Studio has started. Click **Create Dev Space**.

      ![dev](studio3.png)

  5. Create a new dev space:
       - Name: **Fiori**
       - Type: **SAP Fiori**

       Click **Create Dev Space**.

     ![dev](studio4.png)

  6. When your status is **Running**, select your dev space **Fiori**.

      ![dev](studio5.png)


  ### Open workspace


  1. Now you are in your **Fiori** dev space in SAP Business Application Studio.
     Select **Open Folder** to set your workspace.

      ![organization](studio6.png)

  2. Select **projects** and click **Open**.

      ![organization](studio7.png)



### Create list report object page


  1. Select **View** > **Find Command**.

    ![object](view.png)

  2. Select **Fiori: Open Application Generator**.  

    ![object](neu9.png)

  3. Select **List Report Object Page** and click **Next >**.

    ![object](neu10.png)

  4. Configure data source, system and service:
     - Data source: **Connect to a System**
     - System: **`System_XXX_SAML_ASSERTION`**
     - Service: **`ZSB_SHOP_XXX(1) - OData V2`**

     ![object](list.png)

     Click **Next >**.

  5. Select your main entity **`online_shop`** and click **Next >**.

    ![object](list2.png)

  6. Configure project attributes:  
     - Name: **`z_onlineshop_xxx`**
     - Title: **Online Shop XXX**
     - Description: **A Fiori application.**
     - Add deployment configuration: Yes
     - Add FLP configuration: Yes
     - Configure advanced options: No

     Click **Next >**.

    ![object](list3.png)

    **HINT:** Your **application name must** begin with a `z letter` and **must** be in **lowercase letters**.

  7. Configure deployment:

       - Target: ABAP
       - Destination: `System_XXX_SAML_ASSERTION <your_abap_system_url>`
       - Name: `z_purchase_xxx`
       - Package: `z_purchase_req_xxx`
       - Transport Request: `<your_transport_request>`
       - Deployment description: `z_onlineshop_xxx`

      <!-- border -->![app](list4.png)

      Click **Next >**.

    >**HINT: If you want to copy your transport request, please do following:**  Open Eclipse, search your package **`Z_PURCHASE_REQ_XXX`** and open it. Open your transport organizer to see your transport request. The transport request of the superior folder needs to be chosen. Copy your transport request for later use. You can find your **transport request** underneath the **Modifiable** folder.
        <!-- border -->![app](transportrequest.png)

  8. Configure Fiori Launchpad:

       - Semantic Object: `z_purchase_req_xxx`
       - Action: display
       - Title: Online Shop XXX

      <!-- border -->![app](list5.png)

      Click **Finish**.

  9. Now all files have been generated.



### Run SAP Fiori application for data preview


  1. Press the run button on the left side and select the **`Start z_onlineshop_xxx`** run button to start your SAP Fiori application.

      ![run](list6.png)

      **HINT**: An alternative to run the application is to right-click your project folder, open the terminal and enter: `npm start`.

  2. Your default browser will open. Click **Go** to see your result.

      ![run](list7.png)

  3. Check your result.

     ![run](list8.png)


### Deploy your application


1. Click **Files**, right-click your project and select **Open in Terminal**.

    ![deploy](terminal.png)

2. Enter **`npm run deploy`** to deploy your application. When prompted, check deployment configuration and press y. Open the URL at the end of the deployment log in browser to preview the application.

    ![deploy](terminal2.png)

    When the deployment is successful, you will get these two information back as a result: **UIAD details** and **deployment successful**.


### Check BSP library and SAP Fiori Launchpad app descriptor item in Eclipse


  1. Open Eclipse and check the **BSP library** and **SAP Fiori Launchpad app descriptor item folder** in your package **`Z_PURCHASE_REQ_XXX`**. If you are not able to see BSP applications and SAP Fiori Launchpad app description items, refresh your package `Z_PURCHASE_REQ_XXX` by pressing `F5`.

    ![library](bsp.png)



### Create IAM App and business catalog


  1. In Eclipse right-click your package **`Z_PURCHASE_REQ_XXX`** and select **New** > **Other Repository Object**.

      ![iam](iam0.png)

  2. Search for **IAM App**, select it and click **Next >**.

      ![iam](iam.png)

  3. Create a new IAM App:
     - Name: **`ZSHOP_IAM_XXX`**
     - Description: IAM App for online shop

      ![iam](iam2.png)

      Click **Next >**.

  4. Click **Finish**.

      ![iam](iam3.png)

  5. Select **Services** and add a new one.

      ![iam](iam4.png)

  6. Select following:
      - Service Type: `OData V2 (deprecated)`
      - Service Name: `ZSB_SHOP_XXX_0001`    

      ![iam](iam5.png)

      Click **OK**.

      **Save** and **activate** your IAM app.

      ![iam](activate.png)

  7. Right-click your package **`Z_PURCHASE_REQ_XXX`** and select  **New** > **Other Repository Object**.

      ![catalog](iam0.png)

  8. Search for **Business Catalog**, select it and click **Next >**.

      ![catalog](catalog.png)

  9. Create a new business catalog:
     - Name: **`ZSHOP_BC_XXX`**
     - Description: Business catalog for online shop

      ![catalog](catalog2.png)

      Click **Next >**.

 10. Click **Finish**.

      ![catalog](catalog3.png)

 11. Select **Apps** and add a new one.

      ![catalog](catalog4.png)

 12. Create a new business catalog:
     - IAM App: `ZSHOP_IAM_XXX_EXT`
     - Name: `ZSHOP_BC_XXX_0001`
     - Description: Business Catalog to IAM App assignment

      ![catalog](catalog5.png)

      Click **Next >**.

 13. Click **Finish**.

       ![catalog](catalog6.png)

 14. Click **Publish Locally** to publish your business catalog.

       ![catalog](catalog7.png)




### Run SAP Fiori application


  1. Go back to SAP Business Application Studio and deploy your changes. Therefore right-click your project **`z_onlineshop_xxx`** and select **Open in Terminal**.

    ![url](terminal5.png)


  2. Enter **`npm run deploy`**. When prompted, check deployment configuration and press y.

    ![url](terminal6.png)


  3. Press **`CTRL and click the following link`** to open the URL in a browser.

     ![url](terminal8.png)

  4. Log in to your system.

      ![url](login.png)

  5. Click **Go** and check your result.

      ![url](list8.png)



### Test yourself
