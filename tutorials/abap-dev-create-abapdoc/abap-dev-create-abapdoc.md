---
title: Create ABAPDoc comments in your class
description: Learn how to maintain ABAP Doc documentation for your class
primary_tag: topic>abap-development
tags: [ tutorial>beginner, topic>abap-development ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Create an ABAP Class](https://www.sap.com/developer/tutorials/abap-dev-create-new-class.html)


## Details
### You will learn  
In the following exercise you will make your program more readable by learning how to maintain ABAP Doc. You will also learn how to synchronize the documentation and to display it, both in the ABAP Development Tools (ADT) and in SAP GUI.

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Open your ABAP class)]
First, open your ABAP class, which you created in [Create an ABAP Class](https://www.sap.com/developer/tutorials/abap-dev-create-new-class.html):

![Image depicting step-1-open-class](step-1-open-class.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add an ABAP Doc comment)]
To improve readability, add an ABAP Doc comment to the class immediately before the method definition, for example:
`**"! Method reads invoice items from database**` :

![Image depicting step2-add-abap-doc-comment](step2-add-abap-doc-comment.png)

**NOTE**: You must insert the ABAP Doc comment **immediately** before the declaration; otherwise you will get a warning from ADT.

> ABAP Doc comments can be used to document APIs and are displayed in the Element Info. ABAP Doc comments begin with `"!` .


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add parameters to ABAP Doc)]
You can also use ABAP Doc to document method parameters with a Quick Assist. Place the cursor inside of the ABAP Doc comment. Then choose `**Ctrl+1**` to open the Quick Assist menu and double-click on **Add missing parameters to documentation**:

![Image depicting step3-add-parameters](step3-add-parameters.png)

The ABAP Doc comment is extended by a `@parameter ... | `. You can now use this to document the method parameters: To do so, just enter the documentation after the pipe symbol (` | `).

![Image depicting step3a-parameters-added](step3a-parameters-added.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Synchronize short texts)]
We have documented our method using ABAP Doc. However, we also want to see the same short texts in the description fields of the form-based Class Builder in SAP GUI.
To do this, we need to tag the required text in the ABAP Doc as "synchronized" to ensure that it is synchronized with the Class Builder.
a.	Mark the short text for your method in ABAP Doc as "synchronized" by surrounding it with the tag **`<p class="shorttext synchronized">...</p>`**.
b.	Do the same for the short text of your parameter `lt_result`:

![Image depicting step4-sync-texts](step4-sync-texts.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Save and activate)]
Save ( **Ctrl+S** ) and activate ( **Ctrl+F3** ) the class.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Link with Editor)]
Finally you will check that the synchronized short texts are also shown in the Class Builder. First we have to open the class in SAP GUI.
To easily find the class in SAP GUI, first choose Link with Editor:

![Image depicting step6-link-w-editor](step6-link-w-editor.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](View the ABAP Doc comments in SAP GUI)]
a.	In the Project Explorer, select the class **`ZCL_INVOICE_RETRIEVAL`** and choose Open with SAP GUI from the context menu:

![Image depicting step7a-open-w-sapgui](step7a-open-w-sapgui.png)

b.	The method description shows the text we entered in the ABAP Doc comment in the synchronized tag:

![Image depicting step7b-method-text](step7b-method-text.png)

c. Now choose **Parameters**. You will see that the description of the parameter has also been synchronized:

![Image depicting step7b-param-text](step7b-param-text.png)

d. Finally, close the class in SAP GUI and return to your ABAP program by choosing Close:

![Image depicting step8-close](step8-close.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Display element info)]
a. Back in the ABAP Program, position the cursor on the method call `GET_ITEMS_FROM_DB` and display the Element Info of the method by choosing **Element Info (`F2`)**. In addition to the method signature it also shows the ABAP Doc you wrote before:

![Image depicting step8-final-abap-doc](step8-final-abap-doc.png)

b. Close the Element Info by choosing **ESC**.

The code for your class should now look like this:

```ABAP

CLASS zcl_invoice_retrieval DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_table_of_zso_invoice_item TYPE STANDARD TABLE OF zso_invoice_item WITH DEFAULT KEY.

"! <p class="shorttext synchronized">Read items from DB</p>
"! Method reads invoice items from the database
"! @parameter lt_result | <p class="shorttext synchronized">Table of invoice items</p>
"!
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


---
