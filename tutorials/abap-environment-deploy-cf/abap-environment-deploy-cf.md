---
parser: v2
auto_validation: true
primary_tag: software-product>sap-btp--abap-environment
tags: [  tutorial>beginner, programming-tool>abap-development, software-product>sap-business-technology-platform, software-products>sap-business-application-studio ]
time: 45
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Create a SAP Fiori App and Deploy it to SAP BTP, Cloud Foundry environment
<!-- description --> Create a SAP Fiori app using SAP-managed app router for a RAP business object from SAP BTP, ABAP Environment in SAP Business Application Studio and deploy it to SAP BTP, Cloud Foundry environment.

## Prerequisites  
- **Trial:** You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.
- User has authorization to execute the OData service​
- The user is Space Developer in the targeted Cloud Foundry Space​
- Business Application Studio is subscribed and the SAP Business Application Studio Developer role collection is assigned​
- SAP Launchpad Service is subscribed and the Launchpad Admin role collection is assigned
- **Limitation:** SAP Business Application Studio must be subscribed in same subaccount as the ABAP instance



## You will learn  
- How to create MTA projects
- How to create SAP Fiori applications
- How to run applications
- How to build MTA projects
- How to deploy MTA archive files
- How to check deployment
- How to open SAP launchpad service


---

### Open SAP Business Application Studio


  1. Log in to [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/) and click **Enter Your Trial Account**.

      ![SAP Business Application Studio](bas1.png)

  2. Select your subaccount **trial**.

      ![SAP Business Application Studio](bas2.png)

  3. Select **Service Marketplace** and search for **SAP Business Application Studio** and select it.

      ![SAP Business Application Studio](basnew.png)



### Create dev space


  1. Create a new dev space:
       - Name: **`Managed_App_Router`**
       - Type: **SAP Fiori**

       Click **Create Dev Space**.

      ![dev](bass.png)

  2. Click **`Managed_App_Router`**.

      ![dev](bas22.png)

  3. Select **Open Folder**.

     ![dev](bas33.png)

  4. Select **projects** and click **Open**.

      ![dev](bas44.png)


### Create MTA project


  1. Select **View** > **Find Command**.

      ![MTA project](mta.png)

  2. Search for **Open CF: Application Router Generator** and select it.

      ![MTA project](mta2.png)

  4. Create MTA project:
      - MTA ID:  `ztravel_app_xxx`
      - MTA description: Travel App XXX
      - Add router module: `Managed Approuter`

    Click **Finish**.

      ![MTA project](mta3.png)

  5. Check your result.

      ![MTA project](mta4.png)



### Login to Cloud Foundry


  1.  Select **View** > **Find Command**.

      ![login](cf.png)

  2. Search for **Login to Cloud Foundry** and select it.
      ![login](cf2.png)

  3. Press **Enter** to set your API endpoint.

     ![login](cf3.png)

  4. Enter your email address and press **Enter**.

      ![login](cf4.png)

  5. Enter your password and press **Enter**.

      ![login](cf5.png)


  6. Select your global account.

      ![login](cf6.png)


  7. Select your dev space.

      ![login](cf7.png)


  8. Now you are logged in and your organization and space have been set.

      ![login](cf8.png)


### Create SAP Fiori application​


  1. Select **View** > **Find Command**.

      ![Fiori application](fiori.png)

  2.  Search for **Open Application Generator** and select it.

      ![Fiori application](fiori2.png)

  3. Select **SAP Fiori elements** as application type and **List Report Object Page** as `floorplan`.

    Click **Next >**.

      ![Fiori application](fiori3.png)

  4. Configure data source, system and service:
     - Data source: **Connect to a SAP System**
     - System: **`ABAP Environment on SAP Business Technology Platform`**
     - ABAP Environment: **`default_abap-trial`**
     - Service: **`ZUI_C_TRAVEL_M_XXX(1) - OData V2`**

     Click **Next >**.

      ![organization](fiori4.png)

  5. Select **`TravelProcessor`** and click **Next >**.

      ![Fiori application](fiori5.png)

  6. Configure project attributes:  
     - Name: **`ztravel_app_xxx`**
     - Title: **Travel App XXX**
     - Description: **A Fiori application.**
     - Project folder path:
     **`/home/user/projects/ztravel_app_xxx`**
     - Add deployment configuration: Yes
     - Add FLP configuration: Yes
     - Configure advanced options: No

     Click **Next >**.
      ![Fiori application](fiori6.png)

    >**HINT**: Make sure, you have selected your **MTA project** in the **folder path**.

  7. Configure deployment:

       - Target: Cloud Foundry
       - Destination Name:
       `<your_destination_system_url>`

       Click **Next >**.

      ![Fiori application](fiori7.png)

  8. Configure SAP Fiori Launchpad:

       - Semantic Object: `ztravel_app_xxx`
       - Action: maintain
       - Title: Travel App XXX

       Click **Finish**.

      ![Fiori application](fiori8.png)




### Check result


Check your result

  ![result](result.png)


### Run application


1. Right-click your SAP Fiori Application `ztravel_app_xxx` and select **Open in Terminal**.

    ![run](run.png)

2. Enter **`npm run start`** to run your application.

    ![run](run2.png)

3. Click **Go**.

    ![run](run3.png)

4. Check your result.

    ![run](run4.png)



### Build MTA project​


  1. Right-click your  `mta.yaml`file inside your SAP Fiori Application `ztravel_app_xxx` and select **Build MTA Project**.

    ![build](build.png)

  2. Check your result. A new `mta_archives` folder has been created.

    ![build](build2.png)

    >**HINT:** You can also create the `mta_archives` by right-click on the **project** > **open terminal**, and **check** where the `mta_archive` is present, and then run the command `npm run build`. The `mta_archives` file will then get generated.



### Deploy MTA archive file​


  1. Right-click your  `ztravel_app_xxx_0.0.1.mtar`file inside your `mta_archives` folder and select **Deploy MTA Archive**.

      ![deploy](deploy.png)

  2. Check your result.

      ![deploy](deploy2.png)

    >**HINT:** You can also deploy the application from command line by **opening the terminal**, **check** where the **`mta_archive`** is present, and then run the **command `npm run deploy`**.





### Check deployment result


  1. Go back to SAP BTP trial cockpit, select **HTML5 Applications** on the left menu and select your application **`ztravelappxxx`**.

    ![deployment](deployment.png)


  2. Click **Go**.

      ![deployment](deployment2.png)

  3. Check your result.

      ![deployment](deployment3.png)


### Open SAP Launchpad Service


  1. Go back to SAP BTP trial cockpit, select **Service Marketplace** on the left menu, search for **Launchpad Service** and select **Go to Application**.

    ![service](service.png)


  2. Click **Content Providers** on the left menu. Now you can see your HTML5 application. Click the refresh button.

      ![service](service3.png)


  3. Go to your content explorer and select your HTML5 app.

    ![service](service4.png)


  4.  Check your result. Your **Travel App XXX** should appear here.

      ![service](service5.png)



### Test yourself



