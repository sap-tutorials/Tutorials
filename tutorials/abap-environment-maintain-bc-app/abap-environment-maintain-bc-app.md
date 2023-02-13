---
parser: v2
auto_validation: true
primary_tag: software-product>sap-btp--abap-environment
tags: [  tutorial>beginner, programming-tool>abap-development, software-product>sap-business-technology-platform ]
time: 30
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Use Custom Business Configurations app
<!-- description --> Learn how to use the Custom Business Configurations app to maintain configurations.

## Prerequisites  
- You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.
- This is the third tutorial of group [Create a SAP Fiori based Table Maintenance app](group.abap-env-factory). You must complete the tutorials in the given order.
- Install [ABAP Development Tools](https://tools.hana.ondemand.com/#abap). You can also follow **step 1** of this [tutorial](abap-install-adt) to install ADT.


## You will learn  
- How to use the Custom Business Configurations app to maintain configurations


## Intro
The [**Custom Business Configurations**](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/76384d8e68e646d6ae5ce8977412cbb4.html) app serves as an entry point to the configuration objects provided by different applications or partners. You can use the app to adjust these configuration objects to change and influence the system behavior.

The required business catalog is contained in business role template `SAP_BR_BPC_EXPERT (Configuration Expert - Business Process Configuration)`. Ensure the user responsible for maintaining the error codes has this role assigned.

>**Hint:** The trial user in SAP BTP ABAP trial system already has the required catalog.

---
### Custom Business Configurations


  1. Start the SAP Build Work Zone. You can start the SAP Build Work Zone from ADT if you right-click on your ABAP system in the project explorer and select **Properties**, select **ABAP Development** and click on the system URL.

  2. Logon with the user responsible for maintaining the error codes.

  3. Select **Custom Business Configurations** tile.

      ![Start Custom Business Configurations app](m.png)

  4. Select your business configuration.

      ![Select business configuration](m2.png)

  5. Click **Edit**.

      ![Click edit](m3.png)

  6. Enter the following:
     - Error Code: **`401`**
     - Text: **`Unauthorized`**

     ![Enter new row](m4.png)

     Click **Add Row**.

  7. Click **Save**.

     ![Click save](m5.png)

  8. If you only have a trial account and performed the adjustments mentioned in the first tutorial, the save operation is successful and you can skip the rest of this tutorial and continue with the next one. If not, you get an error message that the transport request is missing. Click **Close**.

      ![Error message](m6.png)

9. Click on **Select Transport**.

    ![Click Select transport](m7.png)

10. If a Task of a modifiable Transport Request is assigned to your user, you can select the Transport Request and proceed with saving. If not you need to create a new one first.

    ![Select transport](m8.png)

11. To create a transport request, return to the SAP Build Work Zone Home Page and select the [**Export Customizing Transports**](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/fa7366c3888848bd94566104ac52e627.html) tile.

     ![Start Export Customizing Transports app](m9.png)

12. Click **Create**.

     ![Create new transport request](m10.png)

13. Create a transport request:
    - Description: **`New Error Codes ###`**
    - Technical Type: **`Customizing Request`**

    Click **Create**.

14. If you get an error message that there is already a default transport request, click **Cancel** and go back and search for the default customizing request.

    ![Search default customizing request](m12.png)

15. Select the default transport request and create a new task for your user.

    ![Select default request](m13.png)

16. Create new task:
    - Description: **`New Error Codes`**
    - Owner: **`<User responsible for maintaining error codes>`**

    ![Create new task](m15.png)

    Click **Create**.

17. Back in the **Custom Business Configurations** app click again on the **Select Transport** action. Use the input help to select a Transport Request and click **OK** and then click **Select Transport**. The selected Transport Request is now shown in the section **Transport**. If you miss a Transport Request, try to refresh the **Custom Business Configurations** app.

    ![Select transport request in Custom Business Configurations app](m16.png)

18. Click **Save**. The data has been recorded on the transport request.

19. If you have activated `Log Changes` for the database table, you can navigate to the **Business Configuration Change Logs** by clicking on **Display Change Logs** in the **Custom Business Configurations** app.


    ![Business Configuration Change Logs app](m17.png)
     
>You can manage your customizing transports with the [**Export Customizing Transport**](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/fa7366c3888848bd94566104ac52e627.html) app

>Business Configuration content can be recorded on both software components of type `Business Configuration` or `Development`. The former is recommended, see also [Business Configuration for SAP Cloud Platform ABAP Environment | SAP Blogs](https://blogs.sap.com/2019/12/20/business-configuration-for-sap-cloud-platform-abap-environment/). The transport request must have the attribute `SAP_ATO_TRANSPORT_TYPE` set to `BC` and the attribute `SAP_CUS_TRANSPORT_CATEGORY` set to `DEFAULT_CUST` or `MANUAL_CUST`.



### Test yourself



---
