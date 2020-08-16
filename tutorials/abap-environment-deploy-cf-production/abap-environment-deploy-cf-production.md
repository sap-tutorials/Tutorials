---
auto_validation: true
title: Run SAP Fiori Application With SAP Business Application Studio
description: Run your SAP Fiori application with SAP Business Application Studio
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
---

## Prerequisites  
- You need a SAP Cloud Platform ABAP Environment trial user or a license.


## Details
### You will learn  
- How to assign role collections
- How to create dev spaces
- How to set up organization and space
- How to create list report object pages
- How to run SAP Fiori applications

---
[ACCORDION-BEGIN [Step 1: ](Assign role collection to user)]

  1. Login to [SAP Cloud Platform trial cockpit](https://cockpit.hanatrial.ondemand.com/) and click **Enter Your Trial Account**.

      ![assign role collection](bas1.png)

  2. Select your subaccount **trial**.

      ![assign role collection](bas2.png)

  3. Click **Trust Configuration** to set up your trust.

      ![assign role collection](bas3.png)

      HINT: If you are using a licensed system, make sure you have the trust administrator role assigned to your user.

  4. Select **sap.default**.

      ![assign role collection](bas4.png)

  5. Enter your e-mail address and click **Show Assignments**.

      ![assign role collection](bas5.png)

  6. Click **Assign Role Collection** .

      ![assign role collection](bas6.png)

  7. Select **`Business_Application_Studio_Developer`** and click **Assign Role Collection**.

      ![assign role collection](bas7.png)

  8. Check your result. Now your user should have the **`Business_Application_Studio_Developer`** role collection assigned.

      ![assign role collection](bas8.png)

      You are now able to develop on SAP Business Application Studio.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create dev space)]

  1.  Select **trial** > **Subscriptions** > **SAP Business Application Studio** and click **Go to Application**.

      ![dev](studio.png)

  2.  Check the privacy statement and click **OK**.

      ![dev](studio2.png)

  3. Now the SAP Business Application Studio has started. Click **Create Dev Space**.

      ![dev](studio3.png)

  4. Create a new dev space:
       - Name: **Fiori**
       - Type: **SAP Fiori**
       - Additional SAP Extensions: **Launchpad Module**

       Click **Create Dev Space**.

     ![dev](studio4.png)

  5. Select your dev space **Fiori**.

      ![dev](studio5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set up organization and space)]

  1. Now you are in your **Fiori** dev space in SAP Business Application Studio.
     Select **Open Workspace** to set your workspace.

      ![organization](studio6.png)

  2. Select **projects** and click **Open**.

      ![organization](studio7.png)

  3. Select on the button **The organization and space in Cloud Foundry have not been set.**

      ![organization](studio8.png)

  4. Press enter to set your Cloud Foundry endpoint.

      ![organization](studio9.png)

  5. Enter the same e-mail address you entered in your trial instance and press enter.
      ![organization](studio10.png)

  6. Enter your password and press enter.

      ![organization](studio11.png)

  7. Select your global account and press enter.

      ![organization](studio12.png)

  8. Select dev as your space and press enter.

      ![organization](studio13.png)

  9. Check your result. Now your organization and space have been set.

     ![organization](studio14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create list report object page)]

  1. Select **View** > **Find Command**.

    ![object](studio15.png)

  2. Search for **Yeoman UI Generators** and select it.

    ![object](studio16.png)

  3. Select **SAP Fiori elements application** and click **Next >**.

    ![object](studio17.png)

  4. Select **List Report Object Page V2** and click **Next >**.

    ![object](studio18.png)

  5. Configure data source, system and service:
     - Data source: **Connect to SAP System**
     - System: **`your_abap_trial_instance`**
     - Service: **`ZUI_C_TRAVEL_M_XXX`**

     Click **Next >**.

    ![object](studio19.png)

  6. Select your main entity **`TravelProcessor`** and click **Next**.

    ![object](studio20.png)

  7. Configure project attributes:
     - Name: **`ztravel_app_xxx`**
     - Title: **Travel App XXX**
     - Description: **A Fiori application.**

     Click **Next >**.

    ![object](studio21.png)

    HINT: Your **application name must** be in **lowercase letters**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run SAP Fiori application)]

  1. Close the wizard.

      ![run](studio22.png)

  2. Press the run button.

      ![run](studio23.png)

  3. Select **`Start ztravel_app_xxx`** and press the run button to run your SAP Fiori application.

      ![run](studio24.png)

  4. Click **Expose and Open**.

      ![run](studio25.png)

  5. Enter **travel** and press enter.

      ![run](studio26.png)

  6. Select **`test/`**.

      ![run](studio27.png)

  7. Select **`flpSandbox.html`**.

      ![run](studio28.png)

  8. Now your SAP Fiori application runs. Select your application **Travel App XXX**.

      ![run](studio29.png)

  9. Click **Go** to see your result.

      ![run](studio30.png)

 10. Check your result.

     ![run](studio31.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=abap-environment-deploy-cf-production" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
