---
auto_validation: true
title: Implement Authorization Check for Modify Operations
description: Provide authorizations for a business configuration and implement an authorization check for modify operations.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-business-technology-platform, tutorial>license ]
time: 30
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
- You need a SAP BTP, ABAP environment license.

## Details
### You will learn  
- How to define authorization check for read operations
- How to provide authorizations for a business configuration
- How to implement authorization check for modify operations

---

[ACCORDION-BEGIN [Step 1: ](Define authorization check for read operations)]

[Authorization control in RAP](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/375a8124b22948688ac1c55297868d06.html) protects your business object against unauthorized access to data.

To protect data from unauthorized read access, the ABAP CDS provides its own authorization concept based on a data control language (DCL). To restrict read access to RAP business objects, it is sufficient to model DCL for the CDS entities used in RAP business objects.

  1. Edit Data Definition `ZCAL_I_MCAL_XXX`. Set `@AccessControl.authorizationCheck` to `#CHECK`.

    ```ABAP
    @EndUserText.label: 'Public Holiday'
    @AccessControl.authorizationCheck: #CHECK
    @ObjectModel.semanticKey: ['Holiday']
    define view entity zcal_i_mcal_xxx
      as select from zcal_holiday_xxx
      composition [0..*] of zcal_i_mcal_txt_xxx as _HolidayTxt
      association to parent ZCAL_I_MCAL_ALL_XXX as _HolidayAll on $projection.HolidayAllID = _HolidayAll.HolidayAllID
    {
      key holiday_id       as Holiday,
          1                as HolidayAllID,
          month_of_holiday as HolidayMonth,
          day_of_holiday   as HolidayDay,
          _HolidayTxt,
          _HolidayAll
    }
    ```

  2. Save and activate.

  3. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

  4. Create a new access control:
     - Name: `ZCAL_I_MCAL_XXX`
     - Description: `Access Control for ZCAL_I_MCAL_XXX`
     - Protected Entity: `ZCAL_I_MCAL_XXX`

     ![userinterface](access2.png)

      Click **Next >**.

  5.  Click **Finish**.

    ![userinterface](access3.png)


  6. Define the check on authorization object `S_TABU_NAM` with activity **03** (read) and **table** equal to the name of your root CDS View.   

    ```ABAP
    @EndUserText.label: 'Access Control for ZCAL_I_MCAL_XXX'
    @MappingRole: true
    define role ZCAL_I_MCAL_XXX {
      grant
        select
          on
            ZCAL_I_MCAL_XXX
              where
                ( ) = aspect pfcg_auth( S_TABU_NAM, TABLE = 'ZCAL_I_MCAL_XXX', ACTVT = '03' );         
    }
    ```

  7. Save and activate.

  8. Edit Data Definition `ZCAL_I_MCAL_TXT_XXX`. Set `@AccessControl.authorizationCheck` to `#CHECK`.

    ```ABAP
    @EndUserText.label: 'Holiday text'
    @AccessControl.authorizationCheck: #CHECK
    @ObjectModel.dataCategory: #TEXT
    define view entity zcal_i_mcal_txt_xxx
      as select from zcal_holitxt_xxx
      association        to parent zcal_i_mcal_xxx as _Public_Holiday on $projection.Holiday = _Public_Holiday.Holiday
      association [0..*] to I_LanguageText         as _LanguageText   on $projection.Language = _LanguageText.LanguageCode
      association [1]    to ZCAL_I_MCAL_ALL_XXX    as _HolidayAll     on $projection.HolidayAllID = _HolidayAll.HolidayAllID
    {
          @Semantics.language: true
      key spras            as Language,
      key holiday_id       as Holiday,
          1                as HolidayAllID,
          @Semantics.text: true
          fcal_description as HolidayDescription,
          _Public_Holiday,
          _LanguageText,
          _HolidayAll
    }
    ```

  9. Save and activate.

 10. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

 11. Create a new access control:
     - Name: `ZCAL_I_MCAL_TXT_XXX`
     - Description: `Access Control for ZCAL_I_MCAL_TXT_XXX`
     - Protected Entity: `ZCAL_I_MCAL_TXT_XXX`

     ![userinterface](accessc.png)

      Click **Next >**.

 12. Click **Finish**.

    ![userinterface](accessc2.png)

 13. Your code should look like this:

    ```ABAP
    @EndUserText.label: 'Access Control for ZCAL_I_MCAL_TXT_XXX'
    @MappingRole: true
    define role ZCAL_I_MCAL_TXT_XXX {
      grant
        select
          on
            ZCAL_I_MCAL_TXT_XXX
              where
                inheriting conditions from entity ZCAL_I_MCAL_XXX;           
    }
    ```

  14. Save and activate.

  15. Edit Data Definition `ZCAL_C_MCAL_XXX`. Set `@AccessControl.authorizationCheck` to `#CHECK`.

    ```ABAP
    @EndUserText.label: 'Projection Holiday'
    @AccessControl.authorizationCheck: #CHECK
    @Metadata.allowExtensions: true
    define view entity ZCAL_C_MCAL_XXX
      as projection on zcal_i_mcal_xxx
    {
      key Holiday,
          @Consumption.hidden: true
          HolidayAllID,
          HolidayMonth,
          HolidayDay,
          _HolidayTxt.HolidayDescription as HolidayDescription : localized,     
          _HolidayTxt : redirected to composition child ZCAL_C_MCAL_TXT_XXX,
          _HolidayAll : redirected to parent ZCAL_C_MCAL_ALL_XXX
    }
    ```

  16. Save and activate.

  17. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

  18. Create a new access control:
     - Name: `ZCAL_C_MCAL_XXX`
     - Description: `Access control for CDS view ZCAL_C_MCAL_XXX`
     - Protected Entity: `ZCAL_C_MCAL_XXX`

     ![userinterface](access11.png)

      Click **Next >**.

  19. Click **Finish**.

    ![userinterface](access12.png)

  20. Your code should look like this:

    ```ABAP
    @EndUserText.label: 'Access Control for ZCAL_C_MCAL_XXX'
    @MappingRole: true
    define role ZCAL_C_MCAL_XXX {
      grant
        select
          on
            ZCAL_C_MCAL_XXX
              where
                inheriting conditions from entity ZCAL_I_MCAL_XXX;          
    }
    ```

  21. Save and activate.

  22. Edit Data Definition `ZCAL_C_MCAL_TXT_XXX`. Set `@AccessControl.authorizationCheck` to `#CHECK`.

    ```ABAP
    @EndUserText.label: 'Holiday text'
    @AccessControl.authorizationCheck: #CHECK
    @Metadata.allowExtensions: true
    define view entity ZCAL_C_MCAL_TXT_XXX
     as projection on zcal_i_mcal_txt_xxx
    {
          @Consumption.valueHelpDefinition: [ {entity: {name: 'I_Language', element: 'Language' }} ]
          @ObjectModel.text.element:['LanguageDescription']
      key Language,
      key Holiday,
          @Consumption.hidden: true
          HolidayAllID,
          HolidayDescription,
          _LanguageText.LanguageName as LanguageDescription : localized,
          _Public_Holiday : redirected to parent ZCAL_C_MCAL_XXX,
          _HolidayAll     : redirected to ZCAL_C_MCAL_ALL_XXX
    }
    ```

  23. Save and activate.

  24. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

  25. Create a new access control:
     - Name: `ZCAL_C_MCAL_TXT_XXX`
     - Description: `Access Control for ZCAL_C_MCAL_TXT_XXX`
     - Protected Entity: `ZCAL_C_MCAL_TXT_XXX`

     ![userinterface](access8.png)

      Click **Next >**.

  26.  Click **Finish**.

    ![userinterface](access9.png)

  27. Replace your code with following.  

    ```ABAP
    @EndUserText.label: 'Access Control for ZCAL_C_MCAL_TXT_XXX'
    @MappingRole: true
    define role ZCAL_C_MCAL_TXT_XXX {
      grant
        select
          on
            ZCAL_C_MCAL_TXT_XXX
              where
                inheriting conditions from entity ZCAL_I_MCAL_TXT_XXX;           
    }
    ```

  28. Save and activate.

  29. Start the **Maintain Business Configuration** app and select your Business Configuration.

      ![userinterface](mbc1.png)

  30. The list is empty as you have introduced a check on read operations but yet have to assign the required authorization to your user.

      ![userinterface](empty.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Provide authorizations for a business configuration)]

In RAP business objects, modifying operations, such as standard operations and actions can be checked against unauthorized access during runtime.

  1. Right-click on **`Z_CALENDAR_XXX`** and select **New** > **Other ABAP Repository Object**.

    ![authorizations](iam.png)

  2. Search for **IAM App**, select it and click **Next >**.

    ![authorizations](iam2.png)

  3. Create a new IAM app:
      - Name:`ZCAL_UI_HOLIDAY_O4_XXX`
      - Description: IAM App for factory calendar
      - Application Type: `MBC - Business Configuration App`

      ![authorizations](iam3.png)

      Click **Next >**.

  4. Click **Finish**.

      ![authorizations](iam4.png)

  5. Select **Services** and add a new service to the IAM App.

      ![authorizations](iam5.png)

  6. Add service to IAM app:
     - Service Type: `OData V4`
     - Service Name: `ZCAL_UI_MCAL_04_XXX`

      ![authorizations](iam6.png)

      Click **OK**.

  7. Select **Authorization** and add an authorization object to the IAM App.

     ![authorizations](iam7.png)

  8. Search for **`S_TABU_NAM`** and click **OK** to add the authorization object.

     ![authorizations](iam8.png)

  9. Click **`S_TABU_NAM`**, select `ACTVT` and check **Change** and **Display**.

    ![authorizations](authorization.png)

 10. Select **`TABLE`** and add `ZCAL_I_MCAL_XXX` to `TABLE`.

    ![authorizations](iam99.png)

 11. Save, activate and publish the IAM app.

    ![authorizations](iam10.png)

 12. Right-click on **`Z_CALENDAR_XXX`** and select **New** > **Other ABAP Repository Object**.

    ![authorizations](iam.png)

 13. Search for **Business Catalog**, select it and click **Next >**.

    ![authorizations](catalog.png)

 14. Create a new business catalog:
      - Name: `Z_BC_HOLIDAY_XXX`
      - Description: Business catalog for holiday

      ![authorizations](catalog2.png)

      Click **Next >**.

 15. Click **Finish**.

      ![delete](catalog3.png)

 16. Select **Apps** and add a new app to the business catalog.

      ![authorizations](catalog4.png)

 17. Add app to business catalog:
     - IAM App: `ZCAL_UI_HOLIDAY_O4_XXX_MBC`
     - Name: `Z_BC_HOLIDAY_XXX_0001`
     - Description: Business Catalog to IAM App assignment

      ![authorizations](catalog5.png)

      Click **Next >**.

 18. Click **Finish**.

     ![authorizations](catalog6.png)

 19. Click **Publish Locally**.

     ![authorizations](catalog7.png)

     Once you have created the IAM app and business catalog, assign the catalog to a business role on the SAP Fiori Launchpad by following [this user guide](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/8980ad05330b4585ab96a8e09cef4688.html). Finally, follow [this user guide](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/e40e710321c74f28916affa9ae984bce.html) to assign the business role to business users to grant the rights to use your business configuration inside the maintain business configurations SAP Fiori app.

    >**HINT:** If you activated the **Log changes** function in the database table settings, you can add additional instances of authorization object `S_TABU_NAM` with **Display Change Documents** for `ACTVT` and the name of the table for `TABLE`. This allows the user to see the changes in the **[Business Configuration Change Logs](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5c6cf20499894f1083e80dba7c5963d4.html)** app.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Implement authorization check for modify operations)]
In RAP business objects, modifying operations, such as standard operations and actions can be checked against unauthorized access during runtime.

  1. Open ABAP class **`ZBP_CAL_I_MCAL_ALL_XXX`** and edit method `get_global_authorizations`.

    ```ABAP
    METHOD get_global_authorizations.
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZCAL_I_MCAL_XXX' ID 'ACTVT' FIELD '02'.
    IF sy-subrc = 0.
      result = VALUE #( %update = if_abap_behv=>auth-allowed
                        %action-Edit = if_abap_behv=>auth-allowed
                        %action-selectTransportRequest = if_abap_behv=>auth-allowed ).
    ELSE.
      result = VALUE #( %update = if_abap_behv=>auth-unauthorized
                        %action-Edit = if_abap_behv=>auth-unauthorized
                        %action-selectTransportRequest = if_abap_behv=>auth-unauthorized ).
    ENDIF.

    ENDMETHOD.
    ```

  2. Save and activate.

  3. Your ABAP class should look like this:

    ```ABAP
    CLASS lhc_mbc_cts DEFINITION.
      PUBLIC SECTION.
        METHODS get_mbc_cts RETURNING VALUE(result) TYPE REF TO if_mbc_cp_rap_table_cts.
    ENDCLASS.

    CLASS lhc_mbc_cts IMPLEMENTATION.
      METHOD get_mbc_cts.
        result = mbc_cp_api=>rap_table_cts( table_entity_relations = VALUE #( ( entity = 'HolidayRoot' table = 'ZCAL_HOLIDAY_XXX' )
                                                                              ( entity = 'HolidayText' table = 'ZCAL_HOLITXT_XXX' ) ) ).
      ENDMETHOD.
    ENDCLASS.

    CLASS lhc_holidayroot DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.
        METHODS validateChanges FOR VALIDATE ON SAVE
          IMPORTING keys FOR HolidayRoot~validateChanges.
    ENDCLASS.

    CLASS lhc_holidayroot IMPLEMENTATION.
      METHOD validateChanges.
        DATA change TYPE REQUEST FOR CHANGE zcal_i_mcal_all_xxx.
        SELECT SINGLE request FROM zcal_x_cal_a_xxx INTO @DATA(request).
        NEW lhc_mbc_cts( )->get_mbc_cts( )->validate_changes(
            transport_request = request
            table             = 'ZCAL_HOLIDAY_XXX'
            keys              = REF #( keys )
            reported          = REF #( reported )
            failed            = REF #( failed )
            change            = REF #( change-holidayroot ) ).
      ENDMETHOD.
    ENDCLASS.

    CLASS lhc_holidaytext DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.
        METHODS validateChanges FOR VALIDATE ON SAVE
          IMPORTING keys FOR HolidayText~validateChanges.
    ENDCLASS.

    CLASS lhc_holidaytext IMPLEMENTATION.
      METHOD validatechanges.
        DATA change TYPE REQUEST FOR CHANGE zcal_i_mcal_all_xxx.
        SELECT SINGLE request FROM zcal_x_cal_a_xxx INTO @DATA(request).
        NEW lhc_mbc_cts( )->get_mbc_cts( )->validate_changes(
            transport_request = request
            table             = 'ZCAL_HOLITXT_XXX'
            keys              = REF #( keys )
            reported          = REF #( reported )
            failed            = REF #( failed )
            change            = REF #( change-holidaytext ) ).
      ENDMETHOD.
    ENDCLASS.

    CLASS lhc_holidayall DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.
        METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
          IMPORTING REQUEST requested_authorizations FOR holidayall RESULT result.
        METHODS get_instance_features FOR INSTANCE FEATURES
          IMPORTING keys REQUEST requested_features FOR holidayall RESULT result.
        METHODS selecttransportrequest FOR MODIFY
          IMPORTING keys FOR ACTION holidayall~selecttransportrequest RESULT result.
    ENDCLASS.

    CLASS lhc_holidayall IMPLEMENTATION.
      METHOD get_global_authorizations.
      AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZCAL_I_MCAL_XXX' ID 'ACTVT' FIELD '02'.
      IF sy-subrc = 0.
        result = VALUE #( %update = if_abap_behv=>auth-allowed
                          %action-Edit = if_abap_behv=>auth-allowed
                          %action-selectTransportRequest = if_abap_behv=>auth-allowed ).
      ELSE.
        result = VALUE #( %update = if_abap_behv=>auth-unauthorized
                          %action-Edit = if_abap_behv=>auth-unauthorized
                          %action-selectTransportRequest = if_abap_behv=>auth-unauthorized ).
      ENDIF.

      ENDMETHOD.

      METHOD get_instance_features.
        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
             ENTITY holidayall
             ALL FIELDS WITH CORRESPONDING #( keys )
             RESULT DATA(all).

        result = VALUE #( ( %tky = all[ 1 ]-%tky
                            %action-selecttransportrequest = COND #( WHEN all[ 1 ]-%is_draft = if_abap_behv=>mk-on THEN if_abap_behv=>mk-off
                                                                     ELSE if_abap_behv=>mk-on  )   ) ).
      ENDMETHOD.

      METHOD selecttransportrequest.
        MODIFY ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
        ENTITY holidayall
        UPDATE FIELDS ( request hidetransport )
        WITH VALUE #( FOR key IN keys
                     ( %tky         = key-%tky
                       request = key-%param-transportrequestid
                       hidetransport = abap_false ) ).
        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
          ENTITY holidayall
            ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(holidays).
        result = VALUE #( FOR holiday IN holidays
                            ( %tky   = holiday-%tky
                              %param = holiday ) ).
      ENDMETHOD.

    ENDCLASS.

    CLASS lsc_zcal_i_mcal_all_xxx DEFINITION INHERITING FROM cl_abap_behavior_saver.
      PROTECTED SECTION.
        METHODS save_modified REDEFINITION.
        METHODS cleanup_finalize REDEFINITION.
    ENDCLASS.

    CLASS lsc_zcal_i_mcal_all_xxx IMPLEMENTATION.
      METHOD save_modified.
        READ TABLE update-holidayall INDEX 1 INTO DATA(all).
        IF all-request IS NOT INITIAL.
          NEW lhc_mbc_cts( )->get_mbc_cts( )->record_changes(
            EXPORTING
              transport_request = all-request
              create            = REF #( create )
              update            = REF #( update )
              delete            = REF #( delete ) ).
        ENDIF.
      ENDMETHOD.

      METHOD cleanup_finalize.
      ENDMETHOD.
    ENDCLASS.
    ```

  4. Save and activate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Assign Business Catalog to Business Role and assign Business Role to Business User)]

1. Open **Maintain Business Roles** in SAP Fiori Launchpad as an administrator.

    ![authorizations](rolex.png)

2. Create a new business role:
     - Business Role ID: `ZCAL_HOLIDAY_XXX`
     - Business Role Description: Business Role Holiday Calendar

     ![authorizations](role.png)

3. Click **Add**,

      ![authorizations](role2.png)

4. Search `Z_BC_HOLIDAY_XXX`, select it and click **Apply and OK**.

      ![authorizations](role3.png)

5. Select **Assigned Business Users** and click **Add**.

    ![authorizations](role9.png)

6. Search for your user, select it and click **Apply and OK**.

    ![authorizations](role10.png)

7. Save your changes.

    ![authorizations](role11.png)


8. Edit your business role and click **Maintain Restrictions**.

    ![authorizations](role5.png)

9. Ensure that for the business role you created for this business configuration, the access category **Write, Read, Value Help** is set to **No Access**.

    ![authorizations](no.png)

10. Start the **Maintain Business Configuration** app and select your business configuration. The **Edit** action is disabled.

    ![authorizations](edit.png)

11. Ensure that for the business role you created for this business configuration, the access category **Write, Read, Value Help** is set to **Unrestricted**. Save your changes.

    ![authorizations](unrestricted.png)

12. Start the **Maintain Business Configuration** app and select your business configuration. The **Edit** action is enabled. You can change, update or delete configuration entries.

    ![authorizations](edit2.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add business configuration deprecation)]
Some configuration entries cannot or may not be physically deleted from a database but only marked as deprecated.

  1. Open database table `ZCAL_HOLIDAY_XXX` and add field `configdeprecationcode` of type `config_deprecation_code`. Your code should look like this:

    ```ABAP
    @EndUserText.label : 'Public Holiday Table'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    @AbapCatalog.tableCategory : #TRANSPARENT
    @AbapCatalog.deliveryClass : #C
    @AbapCatalog.dataMaintenance : #RESTRICTED
    define table zcal_holiday_xxx {
      key client            : mandt not null;
      key holiday_id        : zcal_holiday_id_xxx not null;
      month_of_holiday      : zcal_month_xxx;
      day_of_holiday        : zcal_day_xxx;
      last_changed_at       : timestampl;
      local_last_changed_at : timestampl;
      configdeprecationcode : config_deprecation_code;

    }
    ```

  2. Save and activate.

  3. Edit data definition `ZCAL_I_MCAL_XXX`.

    ```ABAP
    @EndUserText.label: 'Public Holiday'
    @AccessControl.authorizationCheck: #CHECK
    @ObjectModel.semanticKey: ['Holiday']
    define view entity zcal_i_mcal_xxx
      as select from zcal_holiday_xxx
      composition [0..*] of zcal_i_mcal_txt_xxx as _HolidayTxt
      association to parent ZCAL_I_MCAL_ALL_XXX as _HolidayAll on $projection.HolidayAllID = _HolidayAll.HolidayAllID
      association [0..*] to I_ConfignDeprecationCodeText as _DeprecationText on _DeprecationText.ConfigurationDeprecationCode = $projection.configdeprecationcode
    {
      key holiday_id       as Holiday,
          1                as HolidayAllID,
          month_of_holiday as HolidayMonth,
          day_of_holiday   as HolidayDay,
          configdeprecationcode,
          case when configdeprecationcode = 'W' then 2
               when configdeprecationcode = 'E' then 1
               else 3
          end              as criticality,
          _HolidayTxt,
          _HolidayAll,
          _DeprecationText
    }
    ```

  4. Save and activate.

  5. Edit data definition `ZCAL_C_MCAL_XXX`.

    ```ABAP
    @EndUserText.label: 'Projection Holiday'
    @AccessControl.authorizationCheck: #CHECK
    @Metadata.allowExtensions: true
    define view entity ZCAL_c_MCAL_XXX
      as projection on zcal_i_mcal_xxx
    {
      key Holiday,
          @Consumption.hidden: true
          HolidayAllID,
          HolidayMonth,
          HolidayDay,
          _HolidayTxt.HolidayDescription as HolidayDescription : localized,    
          _HolidayTxt : redirected to composition child ZCAL_C_MCAL_TXT_XXX,
          _HolidayAll : redirected to parent ZCAL_C_MCAL_ALL_XXX,
          _DeprecationText.ConfignDeprecationCodeName as DeprecationDescription : localized,
          @ObjectModel.text.element:['DeprecationDescription']
          configdeprecationcode,
          criticality
    }
    ```

  6. Save and activate.

  7. Edit metadata extension `ZCAL_C_MCAL_XXX`.

    ```ABAP
    @Metadata.layer: #CORE
    @Search.searchable: true
    @UI: {
      headerInfo: { typeName: 'Public Holiday',
                    typeNamePlural: 'Public Holidays',
                    title: { type: #STANDARD, label: 'Public Holiday', value: 'Holiday' } },
      presentationVariant: [{ sortOrder: [{ by: 'Holiday', direction:  #ASC }] }] }
    annotate view ZCAL_c_MCAL_XXX with
    {
      @UI.facet: [
        {
          id: 'PublicHoliday',
          purpose: #STANDARD,
          label: 'Public Holiday',
          type: #IDENTIFICATION_REFERENCE,
          position: 1
        },
        {
          id: 'Description',
          label: 'Description',
          type: #LINEITEM_REFERENCE,
          position: 2,
          targetElement: '_HolidayTxt'
         }
        ]

      @UI: {  identification: [ { position: 1 } ],
              lineItem:       [ { position: 1 } ] }
      @Search.defaultSearchElement: true
      Holiday;
      @UI: {  identification: [ { position: 3 } ],
              lineItem:       [ { position: 3 } ] }
      HolidayMonth;
      @UI: {  identification: [ { position: 4 } ],
              lineItem:       [ { position: 4 } ] }
      HolidayDay;
      @UI: {  identification: [ { position: 2, hidden: true } ],
              lineItem:       [ { position: 2 } ] }
      HolidayDescription;
      @UI: {  identification: [ { position: 5, criticality: 'criticality' } ],
            lineItem:       [ { position: 5, criticality: 'criticality' } ] }
      configdeprecationcode;
      @UI.hidden: true
      criticality;
      @UI.hidden: true
      DeprecationDescription;

    }
    ```

  8. Save and activate.

  9. Edit behavior definition `ZCAL_I_MCAL_ALL_XXX`. Set field `configdeprecationcode` to read only for entity `HolidayRoot`.

    ```ABAP
    managed with additional save implementation in class zbp_cal_i_mcal_all_xxx unique;
    strict;
    with draft;

    define behavior for zcal_i_mcal_all_xxx alias HolidayAll
    with unmanaged save
    lock master total etag LastChangedAtMax
    authorization master ( global )
    draft table zcal_x_cal_a_xxx
    {
      update;
      draft action Activate;
      draft action Discard;
      draft action Edit;
      draft action Resume;
      draft determine action Prepare;

      action ( features : instance ) selectTransportRequest parameter D_SelectCustomizingTransptReqP result [1] $self;

      association _Holiday { create; with draft; }
    }
    define behavior for zcal_i_mcal_xxx alias HolidayRoot
    lock dependent by _HolidayAll
    authorization dependent by _HolidayAll
    persistent table zcal_holiday_xxx
    draft table zcal_d_cal_xxx
    {
      update;
      delete;
      field ( readonly ) HolidayAllID;
      field ( readonly : update ) Holiday;
      field ( readonly ) configdeprecationcode;
      association _HolidayAll { with draft; }
      association _HolidayTxt { create; with draft; }

      mapping for ZCAL_HOLIDAY_XXX corresponding
      {
        Holiday = holiday_id;
        HolidayDay = day_of_holiday;
        HolidayMonth = month_of_holiday;
      }

      validation validateChanges on save { create; update; delete; }
    }

    define behavior for zcal_i_mcal_txt_xxx alias HolidayText
    lock dependent by _HolidayAll
    authorization dependent by _HolidayAll
    persistent table zcal_holitxt_xxx
    draft table zcal_d_txt_xxx

    {
      update;
      delete;
      field ( readonly : update ) Holiday;
      field ( readonly : update ) Language;

      field ( readonly ) HolidayAllID;

      association _HolidayAll { with draft; }
      association _Public_Holiday { with draft; }


      mapping for ZCAL_HOLITXT_XXX corresponding
      {
        Language = spras;
        Holiday = holiday_id;
        HolidayDescription = fcal_description;
      }


      validation validateChanges on save { create; update; delete; }
    }
    ```


  10. Place the cursor on draft table `ZCAL_D_MCAL_XXX` and use the quick assist `(CTRL+1)` to adjust the draft table.

      ![draft](draft.png)

  11. Save and activate the draft table.

  12. Save and activate behavior definition `ZCAL_I_MCAL_ALL_XXX`.

  13. Our app now shows the deprecation status under label **Validity** alongside a semaphore icon plus description in the logon language. We want to [create UI actions](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/2553c7347f43431baa68efec360fe062.html) to change the validity status.

      ![draft](validity.png)

  14. Edit Behavior Definition `ZCAL_I_MCAL_ALL_XXX` and add `action ( features : instance ) deprecate result [1] $self;` to entity `HolidayRoot`.

    ```ABAP
    managed with additional save implementation in class zbp_cal_i_mcal_all_xxx unique;
    strict;
    with draft;

    define behavior for zcal_i_mcal_all_xxx alias HolidayAll
    with unmanaged save
    lock master total etag LastChangedAtMax
    authorization master ( global )
    draft table zcal_x_cal_a_xxx
    {
      update;
      draft action Activate;
      draft action Discard;
      draft action Edit;
      draft action Resume;
      draft determine action Prepare;

      action ( features : instance ) selectTransportRequest parameter D_SelectCustomizingTransptReqP result [1] $self;

      association _Holiday { create; with draft; }
    }
    define behavior for zcal_i_mcal_xxx alias HolidayRoot
    lock dependent by _HolidayAll
    authorization dependent by _HolidayAll
    persistent table zcal_holiday_xxx
    draft table zcal_d_cal_xxx
    {
      update;
      delete;
      field ( readonly ) HolidayAllID;
      field ( readonly : update ) Holiday;
      field ( readonly ) configdeprecationcode;
      association _HolidayAll { with draft; }
      association _HolidayTxt { create; with draft; }
      action ( features : instance ) deprecate result [1] $self;

      mapping for ZCAL_HOLIDAY_XXX corresponding
      {
        Holiday = holiday_id;
        HolidayDay = day_of_holiday;
        HolidayMonth = month_of_holiday;
      }

      validation validateChanges on save { create; update; delete; }
    }

    define behavior for zcal_i_mcal_txt_xxx alias HolidayText
    lock dependent by _HolidayAll
    authorization dependent by _HolidayAll
    persistent table zcal_holitxt_xxx
    draft table zcal_d_txt_xxx

    {
      update;
      delete;
      field ( readonly : update ) Holiday;
      field ( readonly : update ) Language;

      field ( readonly ) HolidayAllID;

      association _HolidayAll { with draft; }
      association _Public_Holiday { with draft; }


      mapping for ZCAL_HOLITXT_XXX corresponding
      {
        Language = spras;
        Holiday = holiday_id;
        HolidayDescription = fcal_description;
      }


      validation validateChanges on save { create; update; delete; }
    }
    ```

  15. Save and activate.

  16. Place the cursor on action `deprecate` and use the quick assist `(CTRL+1)` to create the missing methods.

      ![draft](deprecate.png)

  17. Edit class `ZBP_CAL_I_MCAL_ALL_XXX`.

    ```ABAP
    CLASS lhc_mbc_cts DEFINITION.
      PUBLIC SECTION.
        METHODS get_mbc_cts RETURNING VALUE(result) TYPE REF TO if_mbc_cp_rap_table_cts.
    ENDCLASS.

    CLASS lhc_mbc_cts IMPLEMENTATION.
      METHOD get_mbc_cts.
        result = mbc_cp_api=>rap_table_cts( table_entity_relations = VALUE #( ( entity = 'HolidayRoot' table = 'ZCAL_HOLIDAY_XXX' )
                                                                              ( entity = 'HolidayText' table = 'ZCAL_HOLITXT_XXX' ) ) ).
      ENDMETHOD.
    ENDCLASS.

    CLASS lhc_holidayroot DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.
        CONSTANTS:
          BEGIN OF config_code,
            valid    TYPE config_deprecation_code VALUE '',
            obsolete TYPE config_deprecation_code VALUE 'W',
            not_valid TYPE config_deprecation_code VALUE 'E',
          END OF config_code.
        METHODS validateChanges FOR VALIDATE ON SAVE
          IMPORTING keys FOR HolidayRoot~validateChanges.
        METHODS get_instance_features FOR INSTANCE FEATURES
          IMPORTING keys REQUEST requested_features FOR HolidayRoot RESULT result.

        METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
          IMPORTING REQUEST requested_authorizations FOR HolidayRoot RESULT result.

        METHODS deprecate FOR MODIFY
          IMPORTING keys FOR ACTION HolidayRoot~deprecate RESULT result.
    ENDCLASS.

    CLASS lhc_holidayroot IMPLEMENTATION.
      METHOD validateChanges.
        DATA change TYPE REQUEST FOR CHANGE zcal_i_mcal_all_xxx.
        SELECT SINGLE request FROM zcal_x_cal_a_xxx INTO @DATA(request).
        NEW lhc_mbc_cts( )->get_mbc_cts( )->validate_changes(
            transport_request = request
            table             = 'ZCAL_HOLIDAY_XXX'
            keys              = REF #( keys )
            reported          = REF #( reported )
            failed            = REF #( failed )
            change            = REF #( change-holidayroot ) ).
      ENDMETHOD.
      METHOD get_instance_features.
        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
          ENTITY holidayall
          FROM VALUE #( ( %tky-HolidayAllID = 1
                          %is_draft = if_abap_behv=>mk-on ) )
          RESULT DATA(all).

        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
          ENTITY holidayroot
          FIELDS ( configdeprecationcode ) WITH CORRESPONDING #( keys )
          RESULT DATA(holidays)
          FAILED failed.

        result =
          VALUE #(
            FOR holiday IN holidays
              LET deprecate = COND #( WHEN all IS INITIAL THEN if_abap_behv=>fc-o-disabled
                                        WHEN holiday-configdeprecationcode = config_code-valid THEN if_abap_behv=>fc-o-enabled
                                        ELSE if_abap_behv=>fc-o-disabled  )
              IN
                ( %tky                 = holiday-%tky
                  %action-deprecate    = deprecate
                 ) ).
      ENDMETHOD.

      METHOD get_global_authorizations.
        AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZCAL_I_MCAL_XXX' ID 'ACTVT' FIELD '02'.
        IF sy-subrc = 0.
          result-%action-deprecate = if_abap_behv=>auth-allowed.
        ELSE.
          result-%action-deprecate = if_abap_behv=>auth-unauthorized.
        ENDIF.
      ENDMETHOD.

      METHOD deprecate.
        MODIFY ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
        ENTITY HolidayRoot
           UPDATE
             FIELDS ( configdeprecationcode criticality )
             WITH VALUE #( FOR key IN keys
                             ( %tky         = key-%tky
                               configdeprecationcode = config_code-obsolete
                               criticality = 2 ) )
        FAILED failed
        REPORTED reported.

        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
          ENTITY HolidayRoot
            ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(holidays).

        result = VALUE #( FOR holiday IN holidays
                            ( %tky   = holiday-%tky
                              %param = holiday ) ).
      ENDMETHOD.

    ENDCLASS.

    CLASS lhc_holidaytext DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.
        METHODS validateChanges FOR VALIDATE ON SAVE
          IMPORTING keys FOR HolidayText~validateChanges.
    ENDCLASS.

    CLASS lhc_holidaytext IMPLEMENTATION.
      METHOD validatechanges.
        DATA change TYPE REQUEST FOR CHANGE zcal_i_mcal_all_xxx.
        SELECT SINGLE request FROM zcal_x_cal_a_xxx INTO @DATA(request).
        NEW lhc_mbc_cts( )->get_mbc_cts( )->validate_changes(
            transport_request = request
            table             = 'ZCAL_HOLITXT_XXX'
            keys              = REF #( keys )
            reported          = REF #( reported )
            failed            = REF #( failed )
            change            = REF #( change-holidaytext ) ).
      ENDMETHOD.
    ENDCLASS.

    CLASS lhc_holidayall DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.
        METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
          IMPORTING REQUEST requested_authorizations FOR holidayall RESULT result.
        METHODS get_instance_features FOR INSTANCE FEATURES
          IMPORTING keys REQUEST requested_features FOR holidayall RESULT result.
        METHODS selecttransportrequest FOR MODIFY
          IMPORTING keys FOR ACTION holidayall~selecttransportrequest RESULT result.
    ENDCLASS.

    CLASS lhc_holidayall IMPLEMENTATION.
      METHOD get_global_authorizations.
        AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZCAL_I_MCAL_XXX' ID 'ACTVT' FIELD '02'.
        IF sy-subrc = 0.
          result = VALUE #( %update = if_abap_behv=>auth-allowed
                            %action-Edit = if_abap_behv=>auth-allowed
                            %action-selectTransportRequest = if_abap_behv=>auth-allowed ).
        ELSE.
          result = VALUE #( %update = if_abap_behv=>auth-unauthorized
                            %action-Edit = if_abap_behv=>auth-unauthorized
                            %action-selectTransportRequest = if_abap_behv=>auth-unauthorized ).
        ENDIF.
      ENDMETHOD.

      METHOD get_instance_features.
        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
             ENTITY holidayall
             ALL FIELDS WITH CORRESPONDING #( keys )
             RESULT DATA(all).

        result = VALUE #( ( %tky = all[ 1 ]-%tky
                            %action-selecttransportrequest = COND #( WHEN all[ 1 ]-%is_draft = if_abap_behv=>mk-on THEN if_abap_behv=>mk-off
                                                                     ELSE if_abap_behv=>mk-on  )   ) ).
      ENDMETHOD.

      METHOD selecttransportrequest.
        MODIFY ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
        ENTITY holidayall
        UPDATE FIELDS ( request hidetransport )
        WITH VALUE #( FOR key IN keys
                     ( %tky         = key-%tky
                       request = key-%param-transportrequestid
                       hidetransport = abap_false ) ).
        READ ENTITIES OF zcal_i_mcal_all_xxx IN LOCAL MODE
          ENTITY holidayall
            ALL FIELDS WITH CORRESPONDING #( keys )
          RESULT DATA(holidays).
        result = VALUE #( FOR holiday IN holidays
                            ( %tky   = holiday-%tky
                              %param = holiday ) ).
      ENDMETHOD.

    ENDCLASS.

    CLASS lsc_zcal_i_mcal_all DEFINITION INHERITING FROM cl_abap_behavior_saver.
      PROTECTED SECTION.
        METHODS save_modified REDEFINITION.
        METHODS cleanup_finalize REDEFINITION.
    ENDCLASS.

    CLASS lsc_zcal_i_mcal_all IMPLEMENTATION.
      METHOD save_modified.
        READ TABLE update-holidayall INDEX 1 INTO DATA(all).
        IF all-request IS NOT INITIAL.
          NEW lhc_mbc_cts( )->get_mbc_cts( )->record_changes(
            EXPORTING
              transport_request = all-request
              create            = REF #( create )
              update            = REF #( update )
              delete            = REF #( delete ) ).
        ENDIF.
      ENDMETHOD.

      METHOD cleanup_finalize.
      ENDMETHOD.
    ENDCLASS.
    ```

  18. Save and activate.

  19. Edit behavior projection `ZCAL_C_MCAL_ALL_XXX` and add action `deprecate` to entity `HolidayRoot`.

    ```ABAP
    projection implementation in class zbp_cal_c_mcal_all_xxx unique;
    strict;
    use draft;

    define behavior for ZCAL_C_MCAL_ALL_XXX alias HolidayAll
    {
      use action Activate;
      use action Discard;
      use action Edit;
      use action Prepare;
      use action Resume;

      use action selectTransportRequest;

      use association _Holiday { create ( augment ); with draft; }
    }
    define behavior for ZCAL_c_MCAL_XXX alias HolidayRoot
    {
      use update ( augment );
      use delete;
      use action deprecate;

      field ( modify ) HolidayDescription;

      use association _HolidayAll { with draft; }
      use association _HolidayTxt { create; with draft; }
    }

    define behavior for ZCAL_c_MCAL_TXT_XXX alias HolidayText
    {
      use update;
      use delete;

      use association _HolidayAll { with draft; }
      use association _Public_Holiday { with draft; }
    }
    ```

  20. Edit metadata extension `ZCAL_C_MCAL_XXX` and add the `#FOR_ACTION` annotation for field `configdeprecationcode`.

    ```ABAP
    @Metadata.layer: #CORE
    @Search.searchable: true
    @UI: {
      headerInfo: { typeName: 'Public Holiday',
                    typeNamePlural: 'Public Holidays',
                    title: { type: #STANDARD, label: 'Public Holiday', value: 'Holiday' } },
      presentationVariant: [{ sortOrder: [{ by: 'Holiday', direction:  #ASC }] }] }
    annotate view ZCAL_c_MCAL_XXX with
    {
      @UI.facet: [
        {
          id: 'PublicHoliday',
          purpose: #STANDARD,
          label: 'Public Holiday',
          type: #IDENTIFICATION_REFERENCE,
          position: 1
        },
        {
          id: 'Description',
          label: 'Description',
          type: #LINEITEM_REFERENCE,
          position: 2,
          targetElement: '_HolidayTxt'
         }
        ]

      @UI: {  identification: [ { position: 1 } ],
              lineItem:       [ { position: 1 } ] }
      @Search.defaultSearchElement: true
      Holiday;
      @UI: {  identification: [ { position: 3 } ],
              lineItem:       [ { position: 3 } ] }
      HolidayMonth;
      @UI: {  identification: [ { position: 4 } ],
              lineItem:       [ { position: 4 } ] }
      HolidayDay;
      @UI: {  identification: [ { position: 2, hidden: true } ],
              lineItem:       [ { position: 2 } ] }
      HolidayDescription;
      @UI: {  identification: [ { position: 5, criticality: 'criticality' },
                              { type: #FOR_ACTION, dataAction: 'deprecate', label: 'Deprecate' } ],
            lineItem:       [ { position: 5, criticality: 'criticality' },
                              { type: #FOR_ACTION, dataAction: 'deprecate', label: 'Deprecate' } ] }   
      configdeprecationcode;
      @UI.hidden: true
      criticality;
      @UI.hidden: true
      DeprecationDescription;

    }
    ```

  21. Save and activate.

  22. Our app now has a new action: `Deprecate`. The action is active/inactive according to the selected item. Via this action you can switch the `Validity` value from `Valid` to `Obsolete`.

    ![draft](deprecate2.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
