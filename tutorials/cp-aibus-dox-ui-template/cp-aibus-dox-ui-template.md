---
parser: v2
auto_validation: true
time: 20
tags: [tutorial>intermediate, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>document-information-extraction, tutorial>free-tier]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Create Custom Template for Purchase Order Documents
<!-- description --> Create a custom template for purchase order documents to extract information from similar documents using the Document Information Extraction service.

## You will learn
  - How to create a template for purchase order documents using your schema
  - How to associate documents with your template and how to edit them
  - How to use your template when extracting information from documents

---

### Create template


In [Create Custom Schema for Purchase Order Documents](cp-aibus-dox-ui-schema), you defined a schema that includes a list of header and line item fields you want to extract from purchase order documents. In this tutorial, you'll create a template that uses your schema and associate documents with your template. You'll **Edit** the **Extraction Results** of the associated documents to teach the Document Information Extraction service where certain field values are located on the document to improve the accuracy of the extraction prediction for similar documents.

In the Document Information Extraction UI, access **Template** on the left navigation pane and click **+** to create a new template.

<!-- border -->![Template](access-template.png)

In the dialog, proceed as follows:

1. Enter a name for your template, `Custom_purchase_order_template`, for example.

2. Choose `Purchase Order` as your `Document Type`.

3. Select the schema you created in [Create Custom Schema for Purchase Order Documents](cp-aibus-dox-ui-schema).

4. Click **Create** to create your template.

    <!-- border -->![Template](create-template.png)

5. After your template is created, a confirmation dialog pops up. Click **OK** to acknowledge it. It will now navigate you to the detail page of your template.

    <!-- border -->![Template](template-created-dialog.png)

    You now see a list of the associated documents (in our case, **No data** yet) and a list of the extraction fields that you have defined in your schema.

    <!-- border -->![Template](associated.png)

    <!-- border -->![Template](extraction.png)

    In the next steps, you'll add a document, get the extraction results and edit them so that you can associate the document with your template.


>**CAUTION:**

>When using the free tier option for Document Information Extraction or a trial account, be aware of the technical limits listed in [Free Tier Option and Trial Account Technical Constraints](https://help.sap.com/docs/document-information-extraction/document-information-extraction/free-tier-option-and-trial-account-technical-constraints).



### Add document and get extraction results


>This tutorial uses the following purchase order documents. Download them to follow along (right click on the link, then click ***Save link as*** to download the files locally):

> - [Sample Purchase Order 1](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/cp-aibus-dox-ui-template/sample-purchase-order-1.pdf)
> - [Sample Purchase Order 2](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/cp-aibus-dox-ui-template/sample-purchase-order-2.pdf)
> - [Sample Purchase Order 3](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/cp-aibus-dox-ui-template/sample-purchase-order-3.pdf)


1.  Access **Document** on the left navigation pane and click **+** to upload a new document.

    <!-- border -->![Template](access-document.png)

2. On the Select Document screen, choose `Purchase Order` for the **Document Type**.

3. Select the **Schema** you created. Don't select any **Template** this time.

4. Drop the file directly or click **+** to upload one of the purchase order documents. The following images use the [Sample Purchase Order 1](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/cp-aibus-dox-ui-template/sample-purchase-order-1.pdf). 

5. Click **Step 2**.

    <!-- border -->![Template](upload.png)

6. The selection of header fields and line item fields comes from the schema and cannot be edited. Click **Step 3** and then click **Review**.

7.  Review your selection. Click **Edit** if you want to change anything. Click **Confirm**.

    <!-- border -->![Template](review.png)

    The document status changes from `PENDING` to `READY`.

    <!-- border -->![Template](ready.png)

8. Click the document row and **Extraction Results** to see the information extracted from the document using the schema you created.

    <!-- border -->![Template](results.png)



### Edit extraction results


Before associating the document with your template, make sure the extraction results are correct.

1. Click **Edit**. You can edit extracted values manually on the right of the screen. You can also select them from the page preview in the middle of the screen. To do the latter, hover your mouse over the page preview. The mouse pointer changes to a crosshair cursor. Position the cursor at the corner of the value you wish to select. Then, hold down the left mouse button. Move the cursor diagonally to the opposite corner to draw a bounding box around the value you want to select. Select the appropriate header or line item field from the **Field** dropdown in the **Assign Field** dialog. Add or change the value, as necessary. If you choose a line item, set the number in the **Row Index** field. Make sure the number you enter here matches the appropriate line item in the **Label** column on the right of the screen. 

    <!-- border -->![Template](edit.png)

    This is an example on how to edit the field `cancelDate`:

    <!-- border -->![Template](cancelDate.png)

    Once you've edited all incorrect header fields, the extraction results should look like the following:

    <!-- border -->![Template](header.png)

    Once you've edited at least two lines of incorrect line item fields, the extraction results should look like the following:

    <!-- border -->![Template](line.png)
    
2. Save your changes.

    <!-- border -->![Template](save.png)



### Add associated documents


1. Click **Add to template** to associate the document with your template.
   
    <!-- border -->![Template](add.png)

2. Select the relevant template from the dropdown and click **Add**.

    <!-- border -->![Template](add2.png)

3. Click **OK**. The document file is now associated with the template you selected. You can repeat the preceding steps to associate additional documents with your template.

    <!-- border -->![Template](ok.png)



### Activate template


The last step that remains is to activate the template. 

1. Access **Template** on the left navigation pane and click the row of the template you created to access it.

    <!-- border -->![Template](draft.png)

2. Click **Activate**.

    <!-- border -->![Template](activate.png)

    Now, the status of your template changes from `DRAFT` to `ACTIVE`, which means you can use the template to extract information from documents. To make changes to your template, you have to **Deactivate** it first.

    <!-- border -->![Template](active.png)
    

   
### Use template


1.  Access **Document** on the left navigation pane and click **+** to upload a new document.

    <!-- border -->![Template](access-document2.png)

2. In the Select Document screen, choose `Purchase Order` for the **Document Type**.

3. Select the **Schema** and **Template** you created.

4. Drop the file directly or click **+** to upload one of the other two purchase order documents. First, use the [Sample Purchase Order 2](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/cp-aibus-dox-ui-template/sample-purchase-order-2.pdf).

5. Click **Step 2**.

    <!-- border -->![Template](upload2.png)

6. The selection of header fields and line item fields comes from the schema and cannot be edited. Click **Step 3** and then click **Review**.

7. Review your selection. Click **Edit** if you want to change anything. Click **Confirm**.

    <!-- border -->![Template](review2.png)

    The document status changes from `PENDING` to `READY`.

    <!-- border -->![Template](ready2.png)

8. Click the document row and **Extraction Results** to see the information extracted from the document using the schema and template you created.

    <!-- border -->![Template](results2.png)

    You can repeat this step using the [Sample Purchase Order 3](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/cp-aibus-dox-ui-template/sample-purchase-order-3.pdf).

    Congratulations, you have completed this tutorial.