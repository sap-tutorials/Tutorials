---
parser: v2
auto_validation: true
primary_tag: software-product>sap-btp--abap-environment
tags: [  tutorial>beginner, programming-tool>abap-development, software-product>sap-business-technology-platform, software-products>sap-business-application-studio ]
time: 25
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Add SAP BTP ABAP environment App to Portal Site
<!-- description --> Add your SAP BTP ABAP environment app to portal site

## Prerequisites  
- **Trial:** You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.
- You completed [Develop a Fiori App](group.abap-env-restful-managed) group

## You will learn  
- How to configure destinations
- How to run launchpad service
- How to create launchpad service
- How to assign role collection
- How to run launchpad site



---
### Configure destination


  1. Login to [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/) and click **Enter Your Trial Account**.

      ![Configure destination](destination.png)

  2. Select your subaccount **trial**.

      ![Configure destination](destination2.png)

  3. Select **Destinations** and click **New Destination**.

      ![Configure destination](destination3.png)

  4. Create a new destination with following entries:
      - Name: `Destination_XXX`
      - Type: HTTP
      - URL: <your_sap_btp_abap_environment_system_url>
      - Proxy Type: Internet
      - Authentication: `NoAuthentication`

     Additional Properties:

      - `sap-client`: 100
      - `sap-platform`: ABAP

     ![Configure destination](destination4.png)

     Click **Save**.


### Run launchpad service


  1.  In your trial subaccount select **Service Marketplace** and search for **Launchpad Service**, select it and click **Create**.

      ![Run launchpad service](service.png)

  2.  Select **Launchpad Service** as service and **standard** as plan.

      ![Run launchpad service](servicex.png)

      Click **Create**.

  3.  Navigate in your trial subaccount to **Users** and select your own user.

      ![Create launchpad site](service16.png)

  4.  Select **Assign Role Collection**

      ![Create launchpad site](service18.png)

  5. Assign `Launchpad_Admin`, `Launchpad_Advanced_Theming` and `Launchpad_External_User` to your user.

      ![Create launchpad site](servicexx.png)

  6.  Select **Instances and Subscriptions** and open your launchpad service.

      ![Run launchpad service](service2.png)


### Create launchpad site


  1. Click **Create Site**.

      ![Create launchpad site](service3.png)

  2. Create a new site:
       - Name: **`Travel_App_XXX`**

     ![Create launchpad site](service4.png)

      Click **Create**.

  3. Select the **cube** on the left side and select **New** > **App**.

      ![Create launchpad site](service5.png)

  4. Enter your properties:
     - Title: Travel App XXX
     - ID: ID from `manifest.json`(project `ztravel_app_xxx` in SAP Business Application Studio)
     - System: `<your_destination>` from SAP BTP trial cockpit
     - App UI Technology: SAPUI5
     - SAPUI5 Component Name: semantic object from `manifest.json` (project `ztravel_app_xxx` in SAP Business Application Studio).


      ![Create launchpad site](service6.png)


      `Manifest.json` file:

      Search for `id`:

      ![Create launchpad site](service7.png)

      Search for `semantic object`:

      ![Create launchpad site](service8.png)

  5. Select **NAVIGATION** and enter your semantic object and action:
     - Semantic Object: `ztravel_app_xxx`
     - Action: display

      ![Create launchpad site](service9.png)

      Click **Save**.

  6.  Select the **cube** on the left side and select **New** > **Catalog**.

      ![Create launchpad site](service10.png)

  7. Enter your general data:
     - Title: Travel App XXX

      ![Create launchpad site](service11.png)

     Click on the search field, select **Travel App XXX** as an app, click **Save** and return.

  8.  Select the **cube** on the left side and select **New** > **Group**.

      ![Create launchpad site](service12.png)

  9.  Enter your general data:
     - Title: Travel App XXX

      ![Create launchpad site](service13.png)

      Click on the search field, select **Travel App XXX** as an app, click **Save** and return.

 10. Select the **cube** on the left side and select **New** > **Role**.

     ![Create launchpad site](service14.png)

  11.  Enter your general data:
     - Title: Travel App XXX

      ![Create launchpad site](service15.png)

      Click on the search field, select **Travel App XXX** as an app, click **Save** and return.


### Assign role collection


  1. Switch to SAP BTP trial cockpit. Navigate to your trial subaccount, select Users and select your own user.

      ![Create launchpad site](service16.png)

  2.  Select **Assign Role Collection**

      ![Create launchpad site](service18.png)

  3. Search `Travel_App_XXX`, select it and click **Assign Role Collection**.

      ![Create launchpad site](service19.png)
    


### Run launchpad site


  1. Switch to Site Manager, select the settings of your launchpad site.

      ![Create launchpad site](service21.png)

  2.  Click **Edit**.

      ![Create launchpad site](service22.png)

  3. Search `Travel_App_XXX`, select it and click **Save**.

      ![Create launchpad site](service23.png)

  4. Open your launchpad site.

      ![Create launchpad site](service24.png)

  5. Check your result.

      ![Create launchpad site](service25.png)
  

### Test yourself



