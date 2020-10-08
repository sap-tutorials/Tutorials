---
auto_validation: true
title: Implement the Logical Deletion
description: Implement the logical deletion of your factory calendar.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform, tutorial>license ]
time: 20
author_name: Merve Temel
author_profile: https://github.com/mervey45
---
 
## Prerequisites  
- You need an SAP Cloud Platform ABAP Environment [trial user](abap-environment-trial-onboarding) or a license.


## Details
### You will learn  
- How to add delete flag to CDS view
- How to add UI annotation



---
[ACCORDION-BEGIN [Step 1: ](Add delete flag to CDS view)]

  1. Open your CDS view **`ZCAL_HOLIDAY_XXX`** and add following UI annotation:

    ```ABAP
    @UI.hidden: true
    ```

      ![userinterface](ui1.png)

  2. Add the where condition at the end of your CDS View **`ZCAL_HOLIDAY_XXX`**:

    ```ABAP
    where configdeprecationcode = ''
    ```

      ![deprecation](deprecation.png)

      Save and activate.

  3. Open your behavior definition **`ZCAL_I_HOLIDAY_XXX`** and disable the physical deletion.
     Remove the **delete** property from the root node.

      ![delete](delete.png)

  4. Add **action delete** to your behavior definition.

    ```ABAP
    action delete;
    ```

  5. Save and activate your behavior definition.
  Set the cursor on the action delete definition and press **CTRL+1**. Click on **Add missing method for action delete in new local handler class** to create your action method.

      ![delete](delete2.png)

  5. Insert following code as your delete method in **`ZBP_CAL_I_HOLIDAY_XXX`**:

    ```ABAP
    METHOD delete.
      CONSTANTS no_longer_valid TYPE zconfig_deprecation_code_xxx VALUE 'E'.
      DATA holidayroot_table TYPE TABLE FOR UPDATE zcal_i_holiday_xxx.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).
        APPEND
          VALUE #( holiday_id            = <key>-holiday_id
                   configdeprecationcode = no_longer_valid
                   %control              = VALUE #( configdeprecationcode = cl_abap_behv=>flag_changed ) )
             TO holidayroot_table.
      ENDLOOP.

      MODIFY ENTITIES OF zcal_i_holiday_xxx IN LOCAL MODE
        ENTITY HolidayRoot UPDATE FROM holidayroot_table
            MAPPED   DATA(ls_mapped)
            FAILED   DATA(ls_failed)
            REPORTED DATA(ls_reported).
    ENDMETHOD.
    ```

    ![delete](delete3.png)

  6. Save and activate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add UI annotation)]
  1. Go to **`ZCAL_I_HOLIDAY_XXX`** and search for `configdeprecationcode` and add following UI annotations:

    ```ABAP
    @UI.hidden: true
       @UI.lineItem:
         [ { type: #FOR_ACTION, dataAction: 'delete', label: 'Delete' } ]
       @UI.identification:
         [ { type: #FOR_ACTION, dataAction: 'delete', label: 'Delete' } ]
       configdeprecationcode         as ConfigDeprecationCode,
    ```

    ![userinterface](ui.png)

  2.  Check your result.

      ![result](result.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
