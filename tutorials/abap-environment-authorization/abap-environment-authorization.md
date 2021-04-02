---
auto_validation: true
title: Create Authorization Model and App in SAP BTP, ABAP Environment
description: Create IAM Apps, services and catalogs for authorization model in the SAP BTP, ABAP environment.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-business-technology-platform, tutorial>license]
time: 20
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
  - You need a SAP BTP, ABAP environment license.
  - ADT version 2.96 or higher

## Details
### You will learn
  - How to create authorization fields
  - How to create access controls
  - How to edit authorization default values
  - How to create IAM Apps and services
  - How to create restriction fields and restriction types
  - How to create business catalogs
  - How to create restriction types

In this tutorial, wherever `XXX` appears, use a number (e.g. `000`).

---


[ACCORDION-BEGIN [Step 1: ](Create authorization field)]
1. Right-click on **`Z_ROOM_XXX`**, select the menu path **New** > **Other ABAP Repository Object**.

      ![Create authorization field](field.png)

2. Search for **Authorization Field**, select it and click **Next>**.

    ![Create authorization field](field2.png)

2. Create your **authorization field**:
     - Name: **`Z_LOCAFXXX`**

     Click **Next>**.

    ![Create authorization field](field3.png)

3. Click **Finish**.

    ![Create authorization field](field4.png)

4. Edit your authorization field:
      - Data Element: **`Z_LOCA_DTEL_XXX`**

    Save and activate.

    ![Create authorization field](field5.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create authorization object)]
1. Right-click on **`Z_ROOM_XXX`**, select the menu path **New** > **Other ABAP Repository Object**.

      ![Create authorization object](object.png)

2. Search for **Authorization Object**, select it and click **Next>**.

    ![Create authorization object](object2.png)

3.  Create your **authorization object**:
       - Name: **`Z_LOCAOXXX`**
       - Description: **`Location`**

       Click **Next>**.

       ![Create authorization object](object3.png)

4. Click **Finish**.

      ![Create authorization object](object4.png)

5. Edit your authorization object and save it. The description and access category will appear then.

      ![Create authorization object](object5.png)

      Save and activate.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create access control)]
  1. Right-click on **`Z_ROOM_XXX`**, select the menu path **New** > **Other ABAP Repository Object**.

      ![Create Access Control](access.png)

  2. Search for **Access Control**, select it and click **Next>**.

      ![Create Access Control](access2.png)

  3.  Create your **access control:**
     - Name: **`Z_I_ROOM_XXX`**
     - Description: **`Room`**

     Click **Next>**.

      ![Create Access Control](access3.png)

  4. Click **Next>**.

      ![Create Access Control](access4.png)

  5. Select **Define Role with PFCG Aspect** and click **Finish**.

      ![Create Access Control](access5.png)

  6. Edit your service definition:
    ```ABAP
    @EndUserText.label: 'Room'
    @MappingRole: true
    define role Z_I_Room_XXX
    {
      grant
        select
            on
                Z_I_ROOM_XXX
                    where
                        (location) = aspect pfcg_auth(Z_LOCAOXXX, Z_LOCAFXXX, ACTVT = '03');  
    }
    ```
    Save and activate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enhance behavior)]
Switch to your behavior implementation, click `CTRL + F` and search for method validate. Edit following as your validate method.
```ABAP
     METHOD validate.
        AUTHORITY-CHECK OBJECT 'Z_LOCAOXXX' ID 'ACTVT' FIELD iv_action ID 'Z_LOCAFXXX' FIELD is_room-location.
        IF sy-subrc <> 0.
          rv_message = 'Not authorized'.
        ENDIF.
    ENDMETHOD.
```

Save and activate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Edit authorization default values)]
  1. Select your service binding`Z_I_ROOM_BND_XXX` and click **Default Authorization Values**.

      ![Edit authorization default values](default.png)

  2. Define following objects:

      ![Edit authorization default values](default2.png)

      Save and activate.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create IAM app & add service)]
  1. Right-click on **`Z_ROOM_XXX`**, select the menu path **New** > **Other ABAP Repository Object**.

      ![Create Access Control](app.png)

  2. Search for **IAM App**, select it and click **Next>**.

      ![Create Access Control](app2.png)

  3.  Create your **IAM App:**
     - Name: **`Z_ROOM_XXX`**
     - Description: **`Room`**

     Click **Next>**.

      ![Create Access Control](app3.png)

  4. Click **Finish**.

      ![Create Access Control](app4.png)

  5. Select **Services**.

      ![Create Access Control](app5.png)

  6. Add new services.

      ![Create Access Control](app6.png)

  7. Find your service:
    - Service Type: `OData V2`
    - Service Name: `Z_I_ROOM_BND_XXX_0001`

    Add `_0001` to your service name to find it.
    Click **OK**.

      ![Create Access Control](app7.png)

  8. Click **Authorizations**.

      ![Create Access Control](app8.png)

  9. Select following activity:

      ![Create Access Control](app14.png)

      Save and activate.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Create restriction field and restriction type)]
  1. Right-click on your package **`Z_ROOM_XXX`** and select **New** > **Other ABAP Repository Object**.

    ![Create restriction field and restriction type](restriction6.png)

  2. Search for **restriction field**, select it and click **Next >**.

    ![Create restriction field and restriction type](restriction2.png)

  3. Create your restriction field:
    - Name: **`Z_LOC_RF_XXX`**
    - Description: Restriction field

      ![Create restriction field and restriction type](restriction3.png)

    Click **Next >**.

  4. Click **Finish**.  

    ![Create restriction field and restriction type](restriction4.png)

  5. Add **`Z_LOCAFXXX`** as authorization field, save and activate.

    ![Create restriction field and restriction type](restriction5.png)

  6. Right-click on your package **`Z_ROOM_XXX`** and select **New** > **Other ABAP Repository Object**.

    ![Create restriction field and restriction type](restriction6.png)

  7. Search for **restriction type**, select it and click **Next >**.

    ![Create restriction field and restriction type](restriction7.png)

  8. Create your restriction field:
    - Name: **`Z_LOC_RT_XXX`**
    - Description: Restriction type for location

    ![Create restriction field and restriction type](restriction8.png)

    Click **Next >**.

  9. Click **Finish**.   

    ![Create restriction field and restriction type](restriction9.png)

  10. Add **`Z_LOC_RF_XXX`** as restriction field AND **`Z_LOCAOXXX`** as restriction object.

    ![Create restriction field and restriction type](restriction10.png)

    Save and activate.


[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 8: ](Create business catalog & add IAM app)]
  1. Right-click on **`Z_ROOM_XXX`**, select the menu path **New** > **Other ABAP Repository Object**.

      ![Create Access Control](catalog.png)

  2. Search for **Business Catalog**, select it and click **Next>**.

      ![Create Access Control](catalog2.png)

  3.  Create your **business catalog:**
     - Name: **`Z_ROOM_BC_XXX`**
     - Description: **`Room`**

     Click **Next>**.

      ![Create Access Control](catalog3.png)

  4. Click **Finish**.

      ![Create Access Control](catalog4.png)

  5. Select **Apps**.

      ![Create Access Control](catalog5.png)

  6. Add new Apps.

      ![Create Access Control](catalog6.png)

  7. Add your App:
    - App ID: `Z_ROOM_XXX_EXT`
    - Assignment ID: `Z_ROOM_BC_XXX_0001`

    Click **Next>**.

      ![Create Access Control](catalog7.png)

  8.  Click **Finish**.

      ![Create Access Control](catalog8.png)

  9. Click **Publish Locally**

      ![Create Access Control](catalog9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create restriction type)]

Open your business catalog **`Z_ROOM_BC_XXX`**, add **`Z_LOC_RT_XXX`** as a restriction type, select write and click **Publish Locally**.

![Create restriction type](restriction.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
