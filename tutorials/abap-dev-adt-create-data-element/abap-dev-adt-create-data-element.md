---
title: Create a data element
description: You will learn how to create a data element, which you will use in a later tutorial.
primary_tag: topic>abap-development
tags: [ tutorial>beginner, topic>abap-development ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:**
[Create an ABAP class](https://www.sap.com/developer/tutorials/abap-dev-create-new-class.html)

## Next Steps: Optional
 - [Create `ABAPDoc` comments in your class](https://www.sap.com/developer/tutorials/abap-dev-create-abapdoc.html)

## Details
### You will learn  
You will learn how to create a data element. You will then use this data element to provide boolean logic to one of the columns in the table, which in turn contains the data you have retrieved from the database. (in the previous tutorial, [Create an ABAP class](https://www.sap.com/developer/tutorials/abap-dev-create-new-class.html).)


### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Change the field type (to a new data element))]
Go back to your ABAP Dictionary structure `zso_invoice_item` and change the type of field `payment_status` to **`zso_invoice_payment_status`**:

![Image depicting step24-change-field-type](step24-change-field-type.png)

Since the data element `zso_invoice_payment_status` does not exist, you get a syntax error, which you will fix using a quick fix:


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Choose a Quick Fix)]
Open Quick Fix:

a. Select the data element and display all the available Quick Fixes by choosing **Ctrl+1**.
b. Then choose **Create data element `zso_invoice_payment_status`** and choose **Enter**.

![Image depicting backup-create-DTEL](backup-create-DTEL.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a new data element)]

Enter a description for the new data element in the field **Description**, then choose **Finish** :

![Image depicting step24b-finish-data-element](step24b-finish-data-element.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enter type and field labels)]
In the Data Element editor that appears, enter the following:

a. In the **Type Name** box, enter **Flag**.

b. Under **Field Labels** on the right, enter the following:
•	Short = **Paid**
•	Medium = **Invoice paid**
•	Long = **Invoice paid**
•	Title = **Invoice paid**

![Image depicting step24c-enter-field-labels](step24c-enter-field-labels.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Save and activate the data element)]

Then choose **Save (Ctrl+S)**, then **Activate (Ctrl+F3)** your data element.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check the structure for syntax errors)]

Go back to the structure `ZSO_INVOICE_ITEM` and choose **Check ABAP Development Object (Ctrl+F2)** :

![Image depicting step6-check-object](step6-check-object.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Activate the structure)]

Choose Activate **(Ctrl+F3)**.

> The ABAP Dictionary structure `zso_invoice_item` is now activated.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Use your data element in the ABAP class)]
Go back to the class, `**ZCL_INVOICE_RETRIEVAL**`, which you created in the previous tutorial, ([Create an ABAP class](https://www.sap.com/developer/tutorials/abap-dev-create-new-class.html)).)
You will now transform the values of `payment_status` from **P** to a flag that is set to **X** (true) when the invoice has been paid.

Enter the following code after the `ORDER BY` clause of the `SELECT` statement:

```ABAP

LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<entry>).
    CASE <entry>-payment_status.
        WHEN 'P'.
            <entry>-payment_status = abap_true.
        WHEN OTHERS.
            <entry>-payment_status = abap_false.
    ENDCASE.
ENDLOOP.

```

![Image depicting step8-loop-at-statement](step8-loop-at-statement.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Save and Activate the class)]
Finally, choose **Save (Ctrl+S)**, then **Activate (Ctrl+F3)** your class.

Your code should look like this:

```ABAP
CLASS zcl_invoice_retrieval DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_table_of_zso_invoice_item TYPE STANDARD TABLE OF zso_invoice_item WITH DEFAULT KEY.

    METHODS get_items_from_db
              RETURNING
                VALUE(lt_result) type ty_table_of_zso_invoice_item.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_invoice_retrieval IMPLEMENTATION.

  METHOD get_items_from_db.

  SELECT
    snwd_bpa~company_name,
    snwd_so_inv_item~gross_amount,
    snwd_so_inv_item~currency_code,
    snwd_so_inv_head~payment_status

  FROM
   snwd_so_inv_item
   JOIN snwd_so_inv_head ON snwd_so_inv_item~parent_key = snwd_so_inv_head~node_key
   JOIN snwd_bpa ON snwd_so_inv_head~buyer_guid = snwd_bpa~node_key

   INTO TABLE @lt_result

  WHERE
   snwd_so_inv_item~currency_code = 'USD'

  ORDER BY
   snwd_bpa~company_name.

   LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<entry>).
        CASE <entry>-payment_status.
            WHEN 'P'.
                <entry>-payment_status = abap_true.
            WHEN OTHERS.
                <entry>-payment_status = abap_false.
        ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Test your changes)]
You can now test the result of your changes in the SAP List Viewer.

> There is a preference which allows you to reuse SAP GUI windows when running applications. To enable this feature, choose **Window > Preferences**. In the Preferences dialog, open **ABAP Development > SAP GUI Integration** and tick the appropriate check box.

Go back to your report and execute it by choosing **F8**. Your SAP List Viewer should look roughly like this:

![Image depicting step5-alv-with-dtel](step5-alv-with-dtel.png)


[ACCORDION-END]

## Next Steps: Optional
 - [Create `ABAPDoc` comments in your class](https://www.sap.com/developer/tutorials/abap-dev-create-abapdoc.html)
