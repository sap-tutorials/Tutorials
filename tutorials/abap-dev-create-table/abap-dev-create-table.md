---
title: Create a Database Table
description: Create a database table from scratch using the ABAP Development Tools (ADT)
auto_validation: true
primary_tag: topic>abap-development
tags: [  tutorial>beginner, products>sap-netweaver-7.5  ]
time: 60
---

## Prerequisites  
-	You have a valid instance of an AS ABAP server, version 7.52 or higher. (The text-based Table Editor is not available for earlier ABAP server versions)
- You have downloaded and installed SAP ABAP Development Tools. We recommend the latest version, available from [ABAP Development Tools](https://tools.hana.ondemand.com/#abap)

## Details

### You will learn  
- How to create a table in ABAP, representing a table in your database
- How to create a reusable **domain**, which provides technical attributes for data elements
- How to create an elementary data type, or **data element**
- How to add an input check to a field. You can use this, for example, to check that the user is working in the correct client

Tables are defined independently of the database in the ABAP Dictionary. When you activate the table in the Data Dictionary, the table is created in the underlying database.

The table in this tutorial will store bank account details for customers. The table will have the following columns (or **fields**):

- `client`
- `bank_customer_id`
- `customer_first_name`
- `customer_last_name`
- `account_number`
- `city`
- `bank_name`
- `balance`
- `account_category`
- `lastchangedat`


---

[ACCORDION-BEGIN [Step 1: ](Create table)]
Create a table in your package:

1. Select (right-click) the package and choose **New > Other ABAP Repository Object** from the context menu:

    !![Image depicting step1-new-repo-object](step1-new-repo-object.png)

2. Enter the filter text **Table**, choose **Database table**, then choose Next.

3. Enter a name such as `ZDB_ACCOUNT_001` - always replacing `XXX` with your initials - and a description, then choose **Next**.

    !![step1b-table-name](step1b-table-name.png)

4. Accept the proposed transport request and choose Finish.

The code for the table appears in a new editor. Ignore the annotations at the top for now.

!![Image depicting step1d-table-editor](step1d-table-editor.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Understand table fields)]
In the next step, you will define the table fields. First you need to understand your options:

**Built-in ABAP types and new types**

There are 3 ways to create a field for a database table:

  - **Built-in type**: The quickest: You specify a (pre-defined) primitive type, length, and description, but no more. You cannot then reuse this field. For more information, see [ABAP Keyword Documentation: Predefined Dictionary Types](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenddic_builtin_types.htm).

  - Use an **existing data element**: The most powerful: A data element describes both the technical and semantic attributes of a field, such as a currency, or a customer name. You can define properties such as search help and (translatable) column header, and then use the same data element in many contexts. You often define the technical attributes of the data element in a domain, so they can be reused.

  - Create a **new data element**: If you want to reuse the benefits of data elements - i.e. semantic attributes such as reuse of translatable column headers or a check table, but a suitable one does not exist yet.

    !![overview-domain-dtel](overview-domain-dtel.png)

In this tutorial, you will create one domain and one data element. For the other fields, you will use a built-in type or existing data element.

**System clients**

One key field has been added automatically:

`client : abap.clnt;`

This specifies that the table is client-specific.
Tables can be cross-client or client-specific. Each client is a self-contained unit within an organization, such as a subsidiary. For client-specific tables, the client is the first key field in the table.

The field is also specified as `not null x`. This means that the field cannot be left blank. In this case, `abap.clnt` is automatically filled with the value of the current client (such as 001).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add field account number using primitive type)]
Now you will add the field **`account_number`**, based on a primitive type.

1. Enter the following (including the period), then choose **Code completion (Ctrl+Space)**:

    ```ABAP
      key account_number : abap.
    ```

    !![Image depicting step3-create-accnum-field](step3-create-accnum-field.png)

2. From the dropdown list, choose `numc(len)` and specify `len` as 8. Also, specify this key field as not null:
  `key account_number : abap.numc(8) not null;`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add field city using existing data element)]
Add a field based on a built-in domain.

1. In the editor, enter the name for your field, followed by a colon:  **city:**. Ignore the error for now.

2. Enter the type, by entering **`/DMO/C`** and using auto-complete (**`Ctrl+Space`**).

3. Then add a semi-colon:

    ```ABAP
    city: /dmo/city;

    ```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create domain)]
1. From your package, select **Dictionary**, then choose **New > Domain** from the context menu.

    !![step4a-new-domain](step4a-new-domain.png)

2. Enter following, then choose **Next**.
    - Name: **`Z_CHAR_32`**
    - Description: **Character Domain of Length 32**,


    !![step5b-domain-name](step5b-domain-name.png)

3. Accept the default transport request and choose **Finish**.

4. In the editor, enter the following and choose **Save ( Ctrl+S )**.
    - Data Type: **`CHAR`**
    - Length: **32**

    !![step5c-domain-attributes](step5c-domain-attributes.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Add field bank using new data element)]
Now add the field **`bank`**, based on a new data element, `z_bank_name_xxx`. You will get an error, which you will also fix in this step.

1. Select the new data element and choose **Get Quick Fix (Ctrl+1)**. From the list, choose **Create data element …** :

    !![step5a-quick-fix-new-dtel](step5a-quick-fix-new-dtel.png)

2. The Create data element wizard appears. Enter a name and description and choose **Next**:

    !![step6a-new-dtel](step6a-new-dtel.png)

3. Accept the default transport request and choose **Finish**:

4. You want your data element to have a character type. Enter the type name of your domain, by entering `Z_CHAR` and choosing **Auto-complete (`Ctrl+Space`)**:

    !![step6c-dtel-w-domain](step6c-dtel-w-domain.png)

5. Now enter the field labels and lengths:

    !![step6d-field-labels](step6d-field-labels.png)

6. Save and activate the data element (`Ctrl+S, Ctrl+F3`).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Remove error)]
Go back to your table, **`ZDB_ACCOUNT_XXX`**. Run a syntax check  with **`Ctrl+F2`**. The error should disappear.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add all other fields)]

  1. Now add other fields, so your code looks as follows. The field `Balance` will cause an error. Ignore this for now.

    ```ABAP
    define table ZACCOUNTS_TABLE_XXX {

      key client          : abap.clnt not null;
      key account_number  : abap.numc(8) not null;
      bank_customer_id    : /dmo/customer_id not null;
      customer_first_name : /dmo/first_name;
      customer_last_name  : /dmo/last_name;
      city                : /dmo/city;
      bank_name           : z_bank_name_xxx;
      balance             : abap.curr(16,2);
      currency            : /dmo/currency_code;
      account_category            : abap.numc(2);
      lastchangedat       : timestampl;
    }
    ```

  2. Then choose Save (`Ctrl+S`) but **do not** activate your table yet.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Fix error)]
You will now fix the error caused by the field `Balance`:

1. Place your cursor on the error symbol (it will change from an arrow to a pointing finger). Then click on it:

    ![Image depicting step8a-fix-error](step8a-fix-error.png)

2. The quick fix proposal appears. Choose (double-click on) the proposal **Assign currency code reference to field currency**

    !![Image depicting step8b-assign-curr-code](step8b-assign-curr-code.png)
  The error message disappears.

3. Save your changes (`Ctrl+S`), but again, do not activate the table yet.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Change technical settings)]
Before you activate the table, change the technical settings at the top as follows (or copy the code at the end of this step):

1. The label is derived from the description you entered; leave this.

2. **`EnhancementCategory`** : Place your cursor immediately after the hash symbol (#), delete the existing text, then choose **Auto-complete (`Ctrl+Space`)**:

    ![Image depicting step9a-tech-settings](step9a-tech-settings.png)

3. Then choose `#EXTENSIBLE_CHARACTER_NUMERIC` from the dropdown list. Your table contains both character-type and numeric-type fields but does not contain any deep structures (such as a structure included within a table row).

4. Complete the other settings as follows (or copy the code below).
    - **`tableCategory`** : `#TRANSPARENT` = your table represents a table in the database.
      Tables are defined independently of the database in the ABAP Dictionary. When you activate the table in the Data Dictionary, the table is created in the underlying database. There is no need for any code to define the data in the database, nor for any vendor-specific code. Your database tables will be created in any database supported by the ABAP server.

    - **`deliveryClass`** : `#A` = application table, which stores master data and transaction data (default)

    - **`dataMaintenance`** : `#ALLOWED` = allows users to enter data manually in Table Maintenance (transaction SE16). (Generally, you would not do this, but it is useful for test purposes.)

```ABAP
@EndUserText.label : 'Bank Accounts'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Save and activate)]
Save (`Ctrl+S`) and activate (`F3`) your table.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Add check table)]
Now you will add a check table for the field `bank_customer_id`. This checks the value of `bank_customer_id` against the field `customer_id` of the table `/dmo/customer`, using a foreign key relationship.

  1. Add the foreign key pointing to table `/dmo/customer`, where your field `bank_customer_id` points to the check table field `customer_id` :

    **`with foreign key [0..*,1] /dmo/customer
    where customer_id = zdb_account_001.bank_customer_id;`**

  2. Add the screen check, which checks user input against the values in `t000.mandt`:
  	`@AbapCatalog.foreignKey.keyType : #KEY
     @AbapCatalog.foreignKey.screenCheck : true`

  3. Your code should look like this:

```ABAP
@AbapCatalog.foreignKey.keyType : #KEY
@AbapCatalog.foreignKey.screenCheck : true
key bank_customer_id : /dmo/customer_id not null
  with foreign key [0..*,1] /dmo/customer
    where customer_id = zdb_account_001.bank_customer_id;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Save, activate, and check code)]
Now, save (`Ctrl+S`) and activate (`Ctrl+F3`) your table. Your code should look like this:

```ABAP
@EndUserText.label : 'Bank Accounts'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #LIMITED
define table zdb_account_XXX {
  key client           : mandt not null;

  @AbapCatalog.foreignKey.keyType : #KEY
  @AbapCatalog.foreignKey.screenCheck : true
  key bank_customer_id : /dmo/customer_id not null
    with foreign key [0..*,1] /dmo/customer
      where customer_id = zdb_account_XXX.bank_customer_id;

  customer_first_name  : /dmo/first_name;
  customer_last_name   : /dmo/last_name;
  account_number       : abap.numc(8) not null;
  city                 : /dmo/city;
  bank_name            : z_bank_name_xxx;

  @Semantics.amount.currencyCode : 'zdb_account_XXX.currency'
  balance              : abap.curr(16,2);
  currency             : /dmo/currency_code;
  account_category     : abap.numc(2);
  lastchangedat        : timestampl;

}

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

### More Information
- SAP Help Portal: [Database Tables](https://help.sap.com/viewer/ec1c9c8191b74de98feb94001a95dd76/7.5.9/en-US/cf21ea43446011d189700000e8322d00.html)

- SAP Help Portal: [Domains, Data ELements, and Field Names](https://help.sap.com/saphelp_47x200/helpdata/en/90/8d72feb1af11d194f600a0c929b3c3/frameset.htm)

- ABAP Keyword Documentation: [Domains](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenddic_domains.htm)

- ABAP Keyword Documentation: [Data Elements](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenddic_data_elements.htm)

- ABAP Keyword Documentation: [Glossary](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abenclient_glosry.htm)

- ABAP for Cloud Keyword Documentation: [Glossary](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenclient_glosry.htm)

- Using the ABAP  Development Tools (ADT) editor for database tables, see the application help in ADT: click on the editor and choose `F1`
---
