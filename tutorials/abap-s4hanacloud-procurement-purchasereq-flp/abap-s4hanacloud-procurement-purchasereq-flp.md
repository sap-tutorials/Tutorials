---
parser: v2
auto_validation: true
primary_tag: software-product-function>s-4hana-cloud-abap-environment
tags:  [ tutorial>beginner, software-product>sap-btp--abap-environment, software-product-function>s-4hana-cloud-abap-environment, programming-tool>abap-development, programming-tool>abap-extensibility]
time: 25
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Integrate List Report Into SAP Fiori Launchpad
<!-- description --> Integrate your list report application into SAP Fiori launchpad.

## Prerequisites  
- You have a license for SAP S/4HANA Cloud and have a developer user in it
- You have installed the latest [Eclipse with ADT](abap-install-adt).
- You need to have following business role assigned for your business user `SAP_BR_DEVELOPER` and `SAP_BR_BPC_EXPERT`.
- You need to have business catalog `SAP_CORE_BC_UI_FLD` assigned for the usage of manage launchpad space.
- You need to have business catalog `SAP_CORE_BC_UI` assigned for the usage of manage launchpad settings.
- You need to have business catalog `SAP_CORE_BC_BCT_TRN_MNG_PC` and `SAP_CORE_BC_BCT_TRN_REL_PC` assigned to your user to see the app.
- You need to [enable spaces layout](https://help.sap.com/docs/SAP_S4HANA_CLOUD/4fc8d03390c342da8a60f8ee387bca1a/64a5e1675ce7413791a654d2228a90be.html?locale=en-US&state=TEST&version=2208.502) in SAP Fiori launchpad to see the app tiles, that are added to spaces or pages.


## You will learn    
- How to make use of IAM apps and business catalogs
- How to create business roles
- How to manage launchpad spaces and assign business role
- How to assign and manage launchpad spaces

## Intro
>**HINT**: The administrator receives an welcome e-mail after provisioning. This e-mail includes the system URL. By removing `/ui` you can log into the SAP S/4HANA Cloud ABAP Environment system. Further information can be found [here](https://help.sap.com/docs/SAP_S4HANA_CLOUD/6aa39f1ac05441e5a23f484f31e477e7/4b962c243a3342189f8af460cc444883.html?locale=en-US&state=DRAFT).

---

### Make use of existing IAM App and business catalog


  1. Open ADT, select your package `Z_PURCHASE_REQ_XXX` and open your IAM App `ZSHOP_IAM_XXX`. Add the UI5 application ID to your IAM app and activate it.

     ![iamapp](iamapp.png)

      If you don't have any IAM App created yet. Please create an IAM App and add the UI5 application to it.

  2. **Publish** your IAM App.

     ![iamapp](iamapp2.png)

  3. Make use of your business catalog `ZSHOP_BC_XXX`.

     ![businesscatalog](businesscatalog.png)

      If you don't have created a business catalog yet, please create one and publish it locally.



### Create business role


  1. Log in to your **S/4HANA Cloud ABAP environment customizing system**.

     ![login](login.png)

  2. Select **Maintain Business Roles**.

      ![role](role.png)

  3. Click **New**.

      ![role2](role2.png)

  4. Create new business role:
      - Business Role ID: `BR_Z_SHOP_XXX`
      - Business Role Description: Business role for online shop

       ![role3](role33.png)

      Click **Create**.


  5. Go to **Assigned Business Catalogs** and click **Add**.

       ![role4](role4.png)

  6. Search for business catalog `ZSHOP_BC_XXX`, select it and click **Apply** and **OK**.

       ![role5](role5.png)

  7. Click **Assign Business Users** and click **Add**.

       ![role6](role6.png)

  8. Search your business user, select it and click **Apply** and **OK**.

       ![role7](role7.png)

  9. Select **Maintain Restrictions**.

       ![role7](role8.png)

 10. Adjust Maintain Restrictions for **Write, Read, Value Help** to **Unrestricted**. Go back.

      ![role7](role9.png)

 11. Click **Save**. Now your business catalog and business user is assigned to your business role.

       ![role8](role10.png)



### Alternative 1: Manage launchpad spaces and assign business role


Option 1:

  1.  Select the **Manage Launchpad Spaces** tile.

      ![transport](space.png)

  2. Click **Create**.

      ![transport](space2.png)

  3. Create new space and page:
       - Space ID: `Z_Online_Shop_XXX`
       - Space description: Space for online shop
       - Space title: Online Shop XXX
       - Page ID: `Z_Online_Shop_XXX`
       - Page description: Page for online shop
       - Page title: Online Shop XXX

      ![transport](brole45.png)

      Click **Create**.

  4. Check your result and click **Save**.

      ![transport](space4.png)

  5. Go back to the homepage.

      ![transport](space5.png)

  6. Select the **Maintain Business Roles** tile.

      ![transport](space6.png)

  7. Search for your business role **`BR_Z_SHOP_XXX`** and select it.

      ![transport](space7.png)

  8. Click **Edit** and select **Assigned Launchpad Spaces**.

      ![transport](space8.png)

  9. Click **Add**.

      ![transport](space9.png)

 10. Select **Use Existing Space**, select your space `Z_Online_Shop_XXX` and click **Assign Space**.

      ![transport](space100.png)


 11. Check your result and click **Save**.

      ![transport](space11.png)

 12. Open the Mange Launchpad Pages tile.

      ![transport](managespaces.png)

 13. Select your page `Z_Online_Shop_XXX`.

      ![transport](managespaces2.png)

 14. Click **Edit**.    

      ![transport](managespaces3.png)

 15. Add a description to your section tile, select your business catalog and click **Add**.

      ![transport](managespaces4.png)

 16. Save your changes.

      ![transport](managespaces5.png)

 17. Check your result.

      ![transport](managespaces6.png)

 18. Click **Page Preview**.

      ![transport](managespaces7.png)

 19. Check the result in the preview.

      ![transport](managespaces8.png)



### Alternative 2: Assign and manage launchpad spaces


Option 2:

  1. Select the **Maintain Business Roles** tile.

      ![transport](brole.png)

  2. Search and select your business role.

      ![transport](brolenew.png)

  3. Click **Edit** and select **Assigned Launchpad Spaces**.

      ![transport](brole2.png)

  4. Click **Add**.

      ![transport](brole3.png)

  5. Select **Create New Space**. Add Launchpad Space:
      - Assign Space to Business Role: Create New Space
      - Space ID: `Z_TRAVEL_APP_XXX`
      - Space description: Space for travel app
      - Space title: Travel App XXX
      - Check **Create Page from Business Catalogs and Groups**.

     ![transport](brole44.png)

      Click **Create and Assign Space**.

  7. Click **Save**.

       ![transport](brole5.png)

  8. Open the Mange Launchpad Pages tile.

      ![transport](managespaces.png)

  9. Select your page `Z_Online_Shop_XXX`.

      ![transport](managespaces2.png)

 10. Click **Edit**.    

      ![transport](managespaces3.png)

 11. Add a description to your section tile, select your business catalog and click **Add**.

      ![transport](managespaces4.png)

 12. Save your changes.

      ![transport](managespaces5.png)

 13. Check your result.

      ![transport](managespaces6.png)

 14. Click **Page Preview**.      

      ![transport](managespaces7.png)

 15. Check the result in the preview.

      ![transport](managespaces8.png)


### Export software collection (optional)

If you want to transport your software collection to your production system, follow the steps below.

  1. Open the **Export Software Collection** tile.

     ![export](export.png)

  2. Select **+** to create a new software collection.

      ![export](export2.png)

  3. Create new software collection:
      - Name: `Z_SC_XXX`

       ![role3](export3.png)

      Click **Create**.


  5. Click **Add Items**

       ![role4](export4.png)

  6. Select your **business role**, **SAP Fiori Launchpad page**, **SAP Fiori launchpad space** and click **OK**.

       ![role5](export5.png)

  7. Now you are able to select each item and export it.

       ![role6](export6.png)


### Test yourself

---
