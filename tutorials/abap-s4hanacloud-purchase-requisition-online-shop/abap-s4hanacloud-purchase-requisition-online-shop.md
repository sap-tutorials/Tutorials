---
parser: v2
auto_validation: true
primary_tag: software-product-function>s-4hana-cloud-abap-environment
tags: [  tutorial>beginner, software-product>abap-development, software-products>sap-business-technology-platform, programming-tool>abap-development ]
time: 25
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Create a Custom RAP Business Object to Trigger Purchase Requisitions API
<!-- description --> Create a custom RAP business object to trigger purchase requisitions API with SAP S/4HANA Cloud ABAP Environment.

In the online shop, customers can order various items. Once an item is ordered, a new purchase requisition is created via purchase requisitions API.

[EML](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/af7782de6b9140e29a24eae607bf4138.html) can be used to trigger purchase requisitions API, which is released for cloud development.


## Prerequisites  
- You have a developer user in a SAP S/4HANA Cloud System.
- You have installed SAP ABAP Development Tools (ADT), version 3.16 or later

## You will learn  
- How to logon to SAP S/4HANA Cloud ABAP Environment
- How to create an ABAP package
- How to create a database table
- How to create a CDS model and projection view
- How to create behavior definition & implementation
- How to create service definition & service binding
- How to run SAP Fiori Elements Preview

---
### Logon to SAP S/4HANA Cloud ABAP Environment


  1. Open Eclipse, select **File** > **New** > **Other**.

      ![logon](logon.png)

  2. Search **ABAP Cloud Project**, select it and click **Next >**.

      ![logon](logon2.png)

  3. Select **SAP S/4HANA Cloud ABAP Environment**, enter the ABAP service instance URL and click **Next >**.

      ![logon](logon3.png)

  4. Click **Open Logon Page in Browser** to log in.

      ![logon](logon4.png)

  5. Click **Next >**.

      ![logon](logon5.png)

  6. Check your ABAP service instance connection and click **Finish**.

      ![logon](logon6.png)

  7. Now your project will be available in the project explorer.

      ![logon](logon7.png)


### Create ABAP package


  1.  Select **ZLOCAL** > **New** > **ABAP Package**.

      ![package](package.png)

  2.  Create new ABAP package:
       - Name: `Z_PURCHASE_REQ_XXX`
       - Description: Package XXX
       - Check **Add to favorite packages**

      ![package](package2.png)

       Click **Next >**.

  3.  Create a new request:
      -  Request Description: TR12345

      ![package](package3.png)

       Click **Finish**.


### Create database table


  1. Right-click your package `Z_PURCHASE_REQ_XXX` and select **New** > **Other ABAP Repository Object**.

      ![table](table.png)

  2. Search for **database table**, select it and click **Next >**.

      ![table](table2.png)

  3. Create new database table:
     - Name: `ZONLINESHOP_XXX`
     - Description: Shop to purchase electronics

      ![table](table3.png)

       Click **Next >**.

  4. Click **Finish**.

      ![table](table4.png)

  5. Replace your code with following:

    ```ABAP
    @EndUserText.label : 'Shop to purchase electronics'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    @AbapCatalog.tableCategory : #TRANSPARENT
    @AbapCatalog.deliveryClass : #A
    @AbapCatalog.dataMaintenance : #RESTRICTED
    define table zonlineshop_xxx {
      key client     : abap.clnt not null;
      key order_uuid : sysuuid_x16 not null;
      order_id       : abap.char(10) not null;
      ordereditem    : abap.char(10) not null;
      deliverydate   : abap.dats;
      creationdate   : abap.dats;

    }
    ```

   6. Save and activate.


### Create CDS data model


  1. Right-click your package `Z_PURCHASE_REQ_XXX` and select **New** > **Other ABAP Repository Object**.

      ![cds](cds.png)

  2. Search for **data definition**, select it and click **Next >**.

      ![cds](cds2.png)

  3. Create new data definition:
     - Name: `ZI_ONLINE_SHOP_XXX`
     - Description: Data model for online shop

      ![cds](cds3.png)

       Click **Next >**.

  4. Click **Finish**.

      ![cds](cds4.png)

  5. Replace your code with following:

    ```ABAP
    @EndUserText.label: 'Data model for online shop'
    @AccessControl.authorizationCheck: #CHECK
    define root view entity ZI_ONLINE_SHOP_XXX as select from zonlineshop_xxx {
       key order_uuid as Order_Uuid,
       order_id as Order_Id,
       ordereditem as Ordereditem,
       deliverydate as Deliverydate,
       creationdate as Creationdate  
    }
    ```

   6. Save and activate.



### Create projection view


  1. Right-click **Data Definitions** and select **New Data Definition**.

      ![projection](projection.png)

  2. Create new projection view:
     - Name: `ZC_ONLINE_SHOP_XXX`
     - Description: Projection view for online shop
     - Referenced Object: `ZI_ONLINE_SHOP_XXX`

      ![projection](projection2.png)

       Click **Next >**.

  3. Click **Finish**.

      ![projection](projection3.png)

  4. Replace your code with following:

    ```ABAP
    @EndUserText.label: 'shop projection'
    @AccessControl.authorizationCheck: #CHECK
    @Search.searchable: true
    @UI: { headerInfo: { typeName: 'Online Shop',
                         typeNamePlural: 'Online Shop',
                         title: { type: #STANDARD, label: 'Online Shop', value: 'order_id' } },

           presentationVariant: [{ sortOrder: [{ by: 'Creationdate',
                                                 direction: #DESC }] }] }
    define root view entity ZC_ONLINE_SHOP_XXX
      as projection on ZI_ONLINE_SHOP_XXX
    {
          @UI.facet: [          { id:                  'Orders',
                                       purpose:         #STANDARD,
                                       type:            #IDENTIFICATION_REFERENCE,
                                       label:           'Order',
                                       position:        10 }      ]
      key Order_Uuid,
          @UI: { lineItem:       [ { position: 10,label: 'order uuid', importance: #HIGH } ],
                   identification: [ { position: 10, label: 'order uuid' } ] }
          @Search.defaultSearchElement: true
          Order_Id,
          @UI: { lineItem:       [ { position: 20,label: 'Ordered Id', importance: #HIGH } ],
                   identification: [ { position: 20, label: 'Ordered Id' } ] }
          @Search.defaultSearchElement: true
          Ordereditem,
          @UI: { lineItem:       [ { position: 30,label: 'ordered Item', importance: #HIGH } ],
                   identification: [ { position: 30, label: 'ordered Item' } ] }

          Deliverydate as Deliverydate,
          @UI: { lineItem:       [ { position: 50,label: 'Creation date', importance: #HIGH } ],
                   identification: [ { position: 50, label: 'Creation date' } ] }
          Creationdate as Creationdate
    }
    ```

   5. Save and activate.


### Create behavior definition for CDS data model


  1. Right-click your data definition `ZI_ONLINE_SHOP_XXX` and select **New Behavior Definition**.

      ![behavior](behavior.png)

  2. Create new behavior definition:

      ![behavior](behavior2.png)

       Click **Next >**.

  3. Click **Finish**.

      ![behavior](behavior3.png)

  4. Replace your code with following:

    ```ABAP
    managed implementation in class zbp_i_online_shop_xxx unique;

    define behavior for ZI_ONLINE_SHOP_XXX alias Online_Shop
    persistent table zonlineshop_xxx
    lock master
    authorization master ( instance )
    //etag master <field_name>
    {
      field ( numbering : managed, readonly ) order_Uuid;
      field ( mandatory ) Ordereditem;
      field ( readonly ) Creationdate, order_id;
      determination calculate_order_id on modify { create; }
      internal action create_pr;
      internal action set_inforecord;
      create;
      update;
      delete;
    }
    ```

   5. Save and activate.


### Create behavior definition for projection view


  1. Right-click your projection view `ZC_ONLINE_SHOP_XXX` and select **New Behavior Definition**.

      ![behavior](behavior4.png)

  2. Create new behavior definition:

      ![behavior](behavior5.png)

       Click **Next >**.

  3. Click **Finish**.

      ![behavior](behavior6.png)

  4. Replace your code with following:

    ```ABAP
    projection;
    //strict; //Comment this line in to enable strict mode. The strict mode is prerequisite to be future proof regarding syntax and to be able to release your BO.

    define behavior for ZC_ONLINE_SHOP_XXX //alias <alias_name>
    {
      use create;
      use update;
      use delete;

    }
    ```

   5. Save and activate.



### Create behavior implementation


  1. In your behavior definition **`ZI_ONLINE_SHOP_XXX`** set the cursor before the implementation class `zbp_i_online_shop_xxx` and click **CTRL + 1**. Double-click on **Create behavior implementation class `zbp_i_online_shop_xxx`** to create your implementation class.

    ![implementation](implementation.png)


  2. Create new implementation class:

      ![implementation](implementation2.png)

      Click **Next >**.

  3.  Click **Finish**.

      ![implementation](implementation3.png)

  4. Replace your code with following:

    ```ABAP
    CLASS lhc_zbp_i_online_shop_xxx DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.

        METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
          IMPORTING keys REQUEST requested_authorizations FOR zi_online_shop_xxx RESULT result.

    * create a new purchase requisition via internal action. This action is triggered via determination calculate_order_id
    * creation of a new order triggers a new purchase requisition
        METHODS create_pr FOR MODIFY
          IMPORTING keys FOR ACTION zi_online_shop_xxx~create_pr.

    * update an existing/ info record via internal action. This action is triggered via internal action create_pr
    * creation of a new purchase requisition triggers a update of a existing info record

        METHODS set_inforecord FOR MODIFY
          IMPORTING keys FOR ACTION zi_online_shop_xxx~set_inforecord.
    * create a new order_id
        METHODS calculate_order_id FOR DETERMINE ON MODIFY
          IMPORTING keys FOR zi_online_shop_xxx~calculate_order_id.

    ENDCLASS.

    CLASS lhc_zbp_i_online_shop_xxx IMPLEMENTATION.

      METHOD get_instance_authorizations.
      ENDMETHOD.

      METHOD create_pr.
        IF keys IS NOT INITIAL.
          MODIFY ENTITIES OF i_purchaserequisitiontp
     ENTITY purchaserequisition
        CREATE FIELDS ( purchaserequisitiontype )
        WITH VALUE #(  ( %cid                    = 'My%CID_1'
                         purchaserequisitiontype = 'NB' ) )

       CREATE BY \_purchaserequisitionitem
       FIELDS ( plant
                purchaserequisitionitemtext
                accountassignmentcategory
                requestedquantity
                baseunit
                purchaserequisitionprice
                purreqnitemcurrency
                materialgroup
                purchasinggroup
                purchasingorganization
    *               MultipleAcctAssgmtDistribution
                    )
       WITH VALUE #(
                     (    %cid_ref = 'My%CID_1'
                          %target = VALUE #(
                                           (  %cid                            = 'My%ItemCID_1'
                                              plant                           = '1010'
    *                                            plant                          = '0001'
                                              purchaserequisitionitemtext     = 'created from PAAS API XXX'
    *                                            accountassignmentcategory      = 'K'
                                                accountassignmentcategory     = 'U'
                                              requestedquantity               = '8.00'
                                              baseunit                        = 'EA'
                                              purchaserequisitionprice        = '10.00'
                                              purreqnitemcurrency             = 'EUR'
                                              materialgroup                   = 'A001'
                                              purchasinggroup                 = '001'
                                              purchasingorganization          = '1010'
    *                                           purchasingorganization          = '0001'
    *                                           MultipleAcctAssgmtDistribution  = '1'
                                              )
                                           )
                      )
                    )
    ENTITY purchaserequisitionitem

    CREATE BY \_purchasereqnacctassgmt
        FIELDS ( CostCenter
                 GLAccount
                 Quantity
                 BaseUnit )
        WITH VALUE #(
                       ( %cid_ref = 'My%ItemCID_1'
                         %target  = VALUE #( ( CostCenter  = 'JMW-COST'
                                               GLAccount   = '0000400000' ) ) ) )
    *                                            Quantity    = '3.00'
    *                                            BaseUnit    = 'EA' )

    *                                           ( CostCenter  = 'SVC_AUNIT_'
    *                                             GLAccount   = '0000400000'
    *                                             Quantity    = '7.00'
    *                                             BaseUnit    = 'EA' )   ) ) )
    CREATE BY \_purchasereqnitemtext
       FIELDS ( plainlongtext )
       WITH VALUE #(
                     (    %cid_ref = 'My%ItemCID_1'
                          %target  = VALUE #(
                                            (
                                              textobjecttype              = 'B01'
                                              language                    = 'E'
                                              plainlongtext               = 'item text created from PAAS API XXX'
                                             )
                                            (
                                              textobjecttype              = 'B02'
                                              language                    = 'E'
                                              plainlongtext               = 'item2 text created from PAAS API XXX'
                                             )

                                             )
                      )
                    )

     REPORTED DATA(ls_reported)
              MAPPED DATA(ls_mapped)
              FAILED DATA(ls_failed).


        ENDIF.
    *      info records creation
        MODIFY ENTITIES OF zi_online_shop_xxx IN LOCAL MODE
     ENTITY zi_online_shop_xxx EXECUTE set_inforecord FROM CORRESPONDING #( keys )
     FAILED DATA(lt_ir_failed)
     REPORTED DATA(lt_ir_reported).
      ENDMETHOD.


      METHOD set_inforecord.
        READ ENTITIES OF zi_online_shop_xxx IN LOCAL MODE
             ENTITY zi_online_shop_xxx
              ALL FIELDS
                WITH CORRESPONDING #( keys )
                RESULT DATA(lt_shop_read_result)
            FAILED    DATA(lt_failed)
            REPORTED  DATA(lt_reported).

        SELECT * FROM i_purchasinginforecordtp  WHERE PurchasingInfoRecord = '5500000219' INTO @DATA(ls_data).
        ENDSELECT.
    *update remainder for nodays-no delivery remainder
    *fi
        MODIFY ENTITIES OF i_purchasinginforecordtp
                                                ENTITY purchasinginforecord
                                                UPDATE FROM
                                                VALUE #( ( %key-PurchasingInfoRecord = '5500000219'

                                                             Supplier = ls_data-supplier
                                                             MaterialGroup = ls_data-MaterialGroup
                                                             SupplierMaterialGroup = ls_data-SupplierMaterialGroup
                                                             NoDaysReminder1           = '11'
                                                             PurchasingInfoRecordDesc = 'noDays remainder updated'
                                                             %control-supplier = 1
                                                             %control-MaterialGroup = 1
                                                             %control-SupplierMaterialGroup = 1
                                                             %control-NoDaysReminder1 = 1
                                                             %control-PurchasingInfoRecordDesc = 1
                                                              ) )


                                                         FAILED DATA(ls_failed_update)
                                                         REPORTED DATA(ls_reported_update)
                                                         MAPPED DATA(ls_mapped_update).

      ENDMETHOD.

      METHOD calculate_order_id.
        DATA:
          online_shops TYPE TABLE FOR UPDATE zi_online_shop_xxx,
          online_shop  TYPE STRUCTURE FOR UPDATE zi_online_shop_xxx.
    *      delete from zonlineshop_xxx UP TO 15 ROWS.
        SELECT MAX( order_id ) FROM zonlineshop_xxx INTO @DATA(max_order_id).
        READ ENTITIES OF zi_online_shop_xxx IN LOCAL MODE
           ENTITY zi_online_shop_xxx
            ALL FIELDS
              WITH CORRESPONDING #( keys )
              RESULT DATA(lt_online_shop_result)
          FAILED    DATA(lt_failed)
          REPORTED  DATA(lt_reported).
        DATA(today) = cl_abap_context_info=>get_system_date( ).

        LOOP AT lt_online_shop_result INTO DATA(online_shop_read).
          max_order_id += 1.

          online_shop = CORRESPONDING #( online_shop_read ).
          online_shop-order_id = max_order_id.
          online_shop-Creationdate = today.
          online_shop-Deliverydate = today + 10.
          APPEND online_shop TO online_shops.
        ENDLOOP.
        MODIFY ENTITIES OF zi_online_shop_xxx IN LOCAL MODE
       ENTITY zi_online_shop_xxx UPDATE SET FIELDS WITH online_shops
       MAPPED   DATA(lt_mapped_modify)
       FAILED   DATA(failed_modify)
       REPORTED DATA(reported_modify).
        MODIFY ENTITIES OF zi_online_shop_xxx IN LOCAL MODE
        ENTITY zi_online_shop_xxx EXECUTE create_pr FROM CORRESPONDING #( keys )
        FAILED DATA(lt_pr_failed)
        REPORTED DATA(lt_pr_reported).
      ENDMETHOD.

    ENDCLASS.
    ```

   5. Save and activate.

    >**HINT:** The option **internal** can be set before the action name to only provide an action for the same BO. An internal action can only be accessed from the business logic inside the business object implementation such as from a determination or from another action.



### Open documentation


You have 2 options to open the documentation inside ADT.


> **Option 1**:

>  1. Open your ABAP class **`zbp_i_online_shop_xxx`**, search for `i_purchaserequisitiontp`, press CTRL and click on it.
      ![service](docu.png)
>  2. Now you are in the released object `i_purchaserequisitiontp`.
     Click **Open Documentation** to open it.
      ![service](docu2.png)
>  3. Now you are able to read the documentation.
      ![service](docu3.png)

>     >**HINT:** You can also open the Element Info by clicking `i_purchaserequisitiontp` and pressing **`F2`**.
      ![service](docuhint.png)

>     >You can also switch to different layers inside the Element Info.
      ![service](docugif.gif)

> **Option 2**:

> 1. Go back to tab `i_purchaserequisitiontp`. You are now able to see the behavior definition folder of the released object `i_purchaserequisitiontp`  in the project explorer. Now navigate to the documentation `i_purchaserequisitiontp` and open it.
      ![service](docu4.png)
>>**HINT**: You can also check the API State of released object and see its visibility by selecting the properties.
> 2. Now you can see the documentation.
      ![service](docu5.png)



### Create service definition


  1. Right-click your projection view `ZC_ONLINE_SHOP_XXX` and select **New Service Definition**.

      ![service](service.png)

  2. Create new service definition:
     - Name: `ZSD_SHOP_XXX`
     - Description: Service definition for online shop

      ![service](service2.png)

       Click **Next >**.

  3. Click **Finish**.

      ![service](service3.png)

  4. Replace your code with following:

    ```ABAP
    @EndUserText.label: 'Service definition for online shop'
    define service ZSD_SHOP_XXX {
      expose ZC_ONLINE_SHOP_XXX as online_shop;
    }
    ```

   5. Save and activate.




### Create service binding


  1. Right-click your service binding `ZSD_SHOP_XXX` and select **New Service Binding**.

      ![binding](binding.png)

  2. Create new service binding:
     - Name: `ZSB_SHOP_XXX`
     - Description: Service binding for online shop
     - Binding Type: `OData V2 - UI`
     - Service Definition: `ZSD_SHOP_XXX`

      ![binding](binding2.png)

       Click **Next >**.

  3. Click **Finish**.

      ![binding](binding3.png)

  4. Right-click your service binding `ZSB_SHOP_XXX` and select **Activate**.

      ![binding](binding4.png)

   5. Click **Publish** to publish your service binding.

      ![binding](binding5.png)



### Run SAP Fiori Elements preview


 1. Select `online_shop` in your service binding and click **Preview** to open SAP Fiori Elements preview.

 2. Click **Create** to create a new entry.

     ![preview](create.png)

 3. Enter an id and date, click **Create**.

     ![preview](create2.png)

 4. Check your result

     ![preview](create3.png)



### Check purchase requisition


 1. Copy the system URL without `-api`, paste it in a browser and **log in**.

     ![preview](logonflp2.png)

 2. Select the **Manage Purchase Requisitions** tile.

     ![preview](logonflp4.png)

 3. Click **Go**.

     ![preview](purchase.png)

 4. Select your purchase requisition.

     ![preview](purchase2.png)

 5. Check your purchase requisition item and select it. Your ID should appear here instead of XXX.

     ![preview](purchase3.png)

 6. Select **Notes** and check your item text. Your ID should appear here instead of XXX.
     ![preview](purchase4.png)

 7. Check your item note. Your ID should appear here instead of XXX.

     ![preview](purchase5.png)




### Test yourself



