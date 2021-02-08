---
title: Enhance the Handler Class With Filtering
description: Add filtering by three values - product ID, supplier name, or category
auto_validation: true
time: 30
tags: [ tutorial>advanced, topic>abap-development, products>sap-cloud-platform, topic>abap-connectivity, tutorial>license]
primary_tag: products>sap-cloud-platform--abap-environment
---

## Prerequisites
- **IMPORTANT**: This tutorial cannot be completed on a trial account
- **IMPORTANT**: This tutorial is part of the mission [Connect Your On-Premise System with SAP Cloud Platform, ABAP Environment](https://developers.sap.com/mission.abap-env-connect-onpremise.html). Please work through the previous tutorials in the mission first; otherwise this tutorial may not work.

## Details
### You will learn
  - How to filter by product `id`, supplier name, or product category

---

[ACCORDION-BEGIN [Step 1: ](Duplicate your class)]
1. Select the class from the previous tutorial, [Get Data from a Remote System Using a Custom Entity](abap-environment-rfc-custom-entity), and choose **Duplicate...** from the context menu.

2. Enter a name for your duplicate class, **`ZCL_PRODUCT_W_FILTER_XXX`**, and choose **Finish**.

The duplicate class appears in a new editor.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Define variables)]
1. First, you will define the range tables. These tables will be filled with the filter value(s) the user enters - for example, all suppliers beginning with "**T**". Each range table contains four columns: **Sign** (i.e. include or exclude), **Operation** (e.g. "Equals" or "Contains"), **Low** (either the first value in a range, or a single value), **High** (in ranges, the highest value; otherwise empty).

    For more information on range tables, see:
        - [SAP Help Portal: SAP Gateway Foundation: Ranges Table and Structure](https://help.sap.com/viewer/68bf513362174d54b58cddec28794093/7.52.5/en-US/acdb22512c312314e10000000a44176d.html)
        - [Third-Party Content: My Experiments with ABAP: SAP Range Table](https://www.samplecodeabap.com/sap-range-table-example-abap/)

    ```ABAP
    "select options
    DATA lt_filter_ranges_productid TYPE RANGE OF ZCE_product_001-productid.
    DATA ls_filter_ranges_productid LIKE LINE OF lt_filter_ranges_productid.
    DATA lt_filter_ranges_supplier  TYPE RANGE OF ZCE_product_001-suppliername.
    DATA ls_filter_ranges_supplier  LIKE LINE OF lt_filter_ranges_supplier.
    DATA lt_filter_ranges_category  TYPE RANGE OF ZCE_product_001-category.
    DATA ls_filter_ranges_category  LIKE LINE OF lt_filter_ranges_category.

    ```

2. Next define the variable that will contain the selected columns, passed to it from the table, `lt_fields`. (See step 4-5).

    ```ABAP
		DATA lv_select_string TYPE string.
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Define filter variable)]
1. Define the variable that contains the filter value(s).

2. Wrap it in a `TRY ... CATCH` block.

	```ABAP
	TRY.
		"get and add filter
		DATA(lt_filter_cond) = io_request->get_filter( )->get_as_ranges( ).

		"get_filter_conditions( ).
			CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_sel_option).

			  "@todo :
			  " raise an exception that the filter that has been provided
			  " cannot be converted into select options
			  " here we just continue

		  ENDTRY.
	```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Create a table for the selected columns)]
Get the names of the selected columns and store them in a table, **`lt_fields`**.

```ABAP
DATA(lt_fields)  = io_request->get_requested_elements( ).
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Specify which columns are to be retrieved)]
If the user has specified that only some columns should be displayed, then store these column names in a string.
Otherwise, retrieve all columns in the table from the database.

	```ABAP
	" $select handling
	IF lt_fields IS NOT INITIAL.
	CONCATENATE LINES OF lt_fields INTO lv_select_string  SEPARATED BY ','.
	ELSE.
	"check coding. If no columns are specified via $select retrieve all columns from the model instead?
	lv_select_string = '*'.
	ENDIF.
	```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Pass the user's filter values for `productid` to the ranges table)]
If the user has specified a filter for `productid`, then fill the ranges table with the user-specified values

```ABAP
"a filter has been provided

"get filter for ProductID
READ TABLE lt_filter_cond WITH KEY name = 'PRODUCTID' INTO DATA(ls_productid_cond).
IF sy-subrc EQ 0.
  LOOP AT ls_productid_cond-range INTO DATA(ls_sel_opt_productid).
	MOVE-CORRESPONDING ls_sel_opt_productid TO ls_filter_ranges_productid.
	INSERT ls_filter_ranges_productid INTO TABLE lt_filter_ranges_productid.
  ENDLOOP.
ENDIF.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Repeat for Supplier Name or Category)]
Do the same for any filter for `Category` or `Supplier Name`.

```ABAP
"-get filter for SUPPLIERNAME
READ TABLE  lt_filter_cond WITH  KEY name = 'SUPPLIERNAME' INTO DATA(ls_suppliername_cond).
IF sy-subrc EQ 0.
  LOOP AT ls_suppliername_cond-range INTO DATA(ls_sel_opt_suppliername).
    MOVE-CORRESPONDING ls_sel_opt_suppliername TO ls_filter_ranges_supplier.
    INSERT ls_filter_ranges_supplier INTO TABLE lt_filter_ranges_supplier.
  ENDLOOP.
ENDIF.

"-get filter for CATEGORY
READ TABLE  lt_filter_cond WITH  KEY name = 'CATEGORY' INTO DATA(ls_category_cond).
IF sy-subrc EQ 0.
  LOOP AT ls_category_cond-range INTO DATA(ls_sel_opt_category).
    MOVE-CORRESPONDING ls_sel_opt_category TO ls_filter_ranges_category.
    INSERT ls_filter_ranges_category INTO TABLE lt_filter_ranges_category.
  ENDLOOP.
ENDIF.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add the parameters to the BAPI call)]
Add the table parameters for these ranges tables to the `BAPI` call.

```ABAP

CALL FUNCTION 'BAPI_EPM_PRODUCT_GET_LIST'
     DESTINATION lv_rfc_dest_name
     EXPORTING
       max_rows   = lv_maxrows
     TABLES
headerdata            = lt_product
selparamproductid     = lt_filter_ranges_productid
selparamsuppliernames = lt_filter_ranges_supplier
selparamcategories    = lt_filter_ranges_category.

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Specify this new class in your custom entity)]
1. Open your custom entity, **`ZCE_PRODUCT_XXX`**.

2. Change the following annotation to point to the new class:

    ```CDS
    @ObjectModel.query.implementedBy: 'ABAP:ZCL_SHOW_PRODUCTS_W_FILTER'
    ```

3. **Save and activate ( `Ctrl+S, Ctrl+F3` )** the custom entity.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Save, activate, and test your class)]
1. **Save and activate ( `Ctrl+S, Ctrl+F3` )** the class.

2. Open your service binding, **`Z_BIND_PRODUCT_TEST_XXX`** , then choose **Preview...**.

3. In the Fiori Elements preview, enter a filter value, such as "Supplier Name = AVANTEL", then choose **Go**.

The filtered list appears.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Test yourself)]



[VALIDATE_1]
[ACCORDION-END]

---
