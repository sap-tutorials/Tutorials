---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>document-information-extraction, tutorial>free-tier]
primary_tag: topic>machine-learning
---

# Create Custom Template for Custom Documents
<!-- description --> Create a custom template for custom documents (that are not supported out of the box) to extract information from similar documents using the Document Information Extraction service.

## You will learn
  - How to create a template for custom documents using your schema
  - How to add sample documents to your template and how to annotate them
  - How to use your template when extracting information from documents

---

### Create template


In [Create Custom Schema for Custom Documents](cp-aibus-dox-ui-schema-custom), you defined a schema that includes a list of header fields you want to extract from power of attorney documents. In this tutorial, you'll create a template that uses your schema and additional sample documents. You'll add annotations to the sample documents to teach the Document Information Extraction service where certain field values are located on the document to improve the accuracy of the extraction prediction for similar documents.

>This tutorial uses the following power of attorney documents. Download them to follow along (right click on the link, then click ***Save link as*** to download the files locally):

> - [Sample Power of Attorney 1](https://github.com/SAPDocuments/Tutorials-Contribution/raw/master/tutorials/cp-aibus-dox-ui-template-custom/sample-power_of_attorney-1.pdf)
> - [Sample Power of Attorney 2](https://github.com/SAPDocuments/Tutorials-Contribution/raw/master/tutorials/cp-aibus-dox-ui-template-custom/sample-power_of_attorney-2.pdf)
> - [Sample Power of Attorney 3](https://github.com/SAPDocuments/Tutorials-Contribution/raw/master/tutorials/cp-aibus-dox-ui-template-custom/sample-power_of_attorney-3.pdf)

In Document Information Extraction UI, access **Template** on the left navigation pane and click **+** to create a new template.

<!-- border -->![Create Template](access-template.png)

In the dialog, proceed as follows:

1. Enter a name for your template, `Custom_power_of_attorney_template`, for instance.

2. Choose `Custom` as your `Document Type`.

3. Select the schema you created in [Create Custom Schema for Custom Documents](cp-aibus-dox-ui-schema-custom).

4. Choose one of the sample documents by clicking **Browse**. The following images use the [Sample Power of Attorney 1](https://github.com/SAPDocuments/Tutorials-Contribution/raw/master/tutorials/cp-aibus-dox-ui-template-custom/sample-power_of_attorney-1.pdf). Upload the same document in the file dialog to follow along.

5. Click **Create** to create your template.

    <!-- border -->![Create Template Dialog](create-template.png)

6. After your template is created, a confirmation dialog pops up. Click **OK** to acknowledge it. It will now navigate you to the detail page of your template.

<!-- border -->![Template Created](template-created-dialog.png)

Here, you find a list of your sample documents as well as a list of the included extraction fields that you have defined in your schema.

In the next step, you'll start annotating your sample document.

>**CAUTION:**

>Be aware of the following Document Information Extraction Trial UI trial account limitations:​

>- Maximum 40 uploaded document pages per week​ (the documents can have more than 1 page)​
>- Maximum 10 schemas per trial account
>- Maximum 3 templates per trial account (with a maximum of 5 sample documents each)



### Annotate header fields


Click on the row of your sample document to open it.

<!-- border -->![Sample Document](access-sample-document.png)

Next, click **Annotations** to open the annotations pane on the right side.

<!-- border -->![Annotations](sample-document-annotations.png)

To start annotating, click **Edit**.

<!-- border -->![Edit Annotations](edit-sample-document-annotations.png)

If you now hover your mouse over the document, the mouse pointer changes to a crosshair cursor. This allows you to draw a box around the information on the document.

You may further notice that all the text on the document has a gray background now. This indicates that Document Information Extraction has recognized the text. Now you only have to connect the text with the field, without actually entering the text yourself. Start with `shipperNumber`:

Find the shipper number at the top of the document. Draw a box around it by holding down the left mouse button and moving the cursor diagonally to the opposite corner. Do not include the "Required"! If you are on a smaller screen, zoom in and out by using the magnifying glass icons on the top.

Once you drew the bounding box, a dialog appears. Here, you find that the service recognized the shipper number of your power of attorney document and pre-filled the field. Select `shipperNumber` as the field to which this value belongs. Then, click **Apply**.

<!-- border -->![Annotate Number](annotate-number.png)

Now, the field for `shipperNumber` on the right is filled with the shipper number of your power of attorney document. Additionally, the shipper number on the document is surrounded by a blue box.

<!-- border -->![View Number](annotated-number.png)

Continue until you fill out all header fields with a value and a corresponding box.

Once you are done, and have successfully annotated the sample document, click **Save** to ensure your hard work is not lost.

<!-- border -->![All Header Fields Annotated](all-header-fields-annotations.png)



### Activate template


The last step that remains is to activate the template. Therefore, click **Activate**.

<!-- border -->![Activate Template](activate-template.png)

Now, the status of your template changes from `DRAFT` to `ACTIVE` which means you can use the template to extract information from documents. To make changes to your template (add more sample document files, for example), you have to **Deactivate** it first.

Congratulations, you have created and activated your first Document Information Extraction schema.




### Use template


1.  Access **Document** on the left navigation pane and click **+** to upload a new document.

    <!-- border -->![Use Template](access-document.png)

2. In the Select Document screen, choose `Custom` for the **Document Type**.

3. Select the **Schema** and **Template** you created.

4. Drop files directly or click **+** to upload one or more document files. First, use the [Sample Power of Attorney 2](https://github.com/SAPDocuments/Tutorials-Contribution/raw/master/tutorials/cp-aibus-dox-ui-template-custom/sample-power_of_attorney-2.pdf).

5. Click **Step 2**.

    <!-- border -->![Use Template](step-1.png)

6. The selection of header fields comes from the schema and cannot be edited. Click **Step 3** and then click **Review**.

7. Review your selection. Click **Edit** if you want to change anything. Click **Confirm**.

    <!-- border -->![DOX-UI-App](review.png)

    The document status changes from `PENDING` to `READY`.

    <!-- border -->![DOX-UI-App](pending.png)


    <!-- border -->![DOX-UI-App](ready.png)

8. Click the document row and **Extraction Results** to see the information extracted from the document using the schema and template you created.

<!-- border -->![DOX-UI-App](results.png)

You can repeat this step using the [Sample Power of Attorney 3](https://github.com/SAPDocuments/Tutorials-Contribution/raw/master/tutorials/cp-aibus-dox-ui-template-custom/sample-power_of_attorney-3.pdf).

Congratulations, you have completed this tutorial.

