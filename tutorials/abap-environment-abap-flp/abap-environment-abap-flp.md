---
auto_validation: true
title: Integrate List Report into ABAP Fiori Launchpad
description: Integrate your list report application into ABAP Fiori launchpad.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-business-technology-platform, tutorial>license]
time: 15
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
- You need an SAP BTP, ABAP environment license.
- You have created the [Travel App Group](group.abap-env-restful-managed) in a licensed system.
- You need to have following business role assigned for your business user `SAP_BR_DEVELOPER`.
- You need to have business catalog `SAP_CORE_BC_UI_FLD` assigned for the usage of manage launchpad space.
- You need to have business catalog `SAP_CORE_BC_UI` assigned for the usage of manage launchpad settings.

Hint: Your Business Application Studio subscription needs to be in the same subaccount as the ABAP system.


## Details
### You will learn  
- How to make use of IAM App and business catalog
- How to create business roles
- How to pin business catalogs
- How to create spaces and pages

---
[ACCORDION-BEGIN [Step 1: ](Make use of existing IAM App and business catalog)]

  1. Open ADT, select your package `ZTRAVEL_APP_XXX` and open your IAM App `ZTRAVEL_IAM_XXX`. Add the UI5 application ID to your IAM app and publish it.

    ![iamapp](iamapp.png)

      If you don't have any IAM App created yet. Please create an IAM App and add the UI5 application to it.

  2. Make use of your business catalog `ZTRAVEL_BC_XXX`.

    ![businesscatalog](businesscatalog.png)

      If you don't have created a business catalog yet, please create one and publish it locally.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create business role)]

  1.  Right-click on your **ABAP system** in the project explorer of ADT and select **Properties**. Click **ABAP Development** and copy the **system URL**.

     ![url](url.png)

  2. Log in to your ABAP system as an administrator.

     ![login](login.png)

  3. Select **Maintain Business Roles**.

      ![role](role.png)

  4. Click **New**.

      ![role2](role2.png)

  5. Create new business role:
      - Business Role ID: `BR_Z_TRAVEL_XXX`
      - Business Role Description: Business role for travel

       ![role3](role3.png)

      Click **Create**.


  6. Click **Add**.

       ![role4](role4.png)

  7. Search for business catalog `ZTRAVEL_BC_XXX`, select it and click **Apply**.

       ![role5](role5.png)

  8. Click **Assign Business Users** and click **Add**.

       ![role6](role6.png)

  9. Search your business user, select it and click **Apply** and **OK**.

       ![role7](role7.png)

 10. Click **Save**. Now your business catalog and business user is assigned to your business role.

       ![role8](role8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Alternative 1: Pin business catalog)]

This step is alternative 1, you can also use alternative 2 in step 4.

  1. Log in with your business user. Select your user on the top right corner and click **App Finder**.

      ![pin](pin.png)

  2. Search for `Travel App XXX` and pin it to My Home.

      ![pin](pin2.png)

  3. Go to My Home and you will be able to see your tile.

      ![pin](pin3.png)

HINT: This is an alternative way to add the app to spaces/pages. This is only possible via personalization. If you want the app to appear directly on the homepage, you need to switch to space/page.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Alternative 2: Create space and page)]

  1. Log in to your licensed ABAP system as an administrator and select **Maintain Business Roles**.

      ![module](module.png)

  2. Search your business role and select it.

      ![module](module2.png)

  3. Click **Edit**.

      ![module](module3.png)

  4. Select **Manage Launchpad Space**.

      ![module](module4.png)

  5. Click **Create and Assign Space**.

      ![module](module5.png)

  6. Save your changes.

      ![module](module6.png)

  7. Click `Z_TRAVEL_APP_XXX` to go to space details.

      ![module](module7.png)

  8. Click `Z_TRAVEL_APP_XXX` to go to page details.

      ![module](module8.png)

  9. Click **Edit**.

      ![module](module9.png)

 10. Add your business catalog to your page.

      ![module](module10.png)

 11. Save your changes.

      ![module](module11.png)

 12. Go to your homepage and select **Manage Launchpad Settings** .      

      ![module](homepage.png)

 13. Click **Edit**.

     ![module](edit.png)

 14. Set the `SPACES_ENABLE_USER` settings to **ON** and click **Save**.

      ![module](on.png)

 15. Login with business user and select your user. Select Settings.

      ![module](module12.png)

 16. Select Spaces, click Use Spaces and save your changes.

      ![module](module13.png)

 17. Check your result.

      ![module](module14.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

---

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=abap-environment-deploy-cf-production" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
