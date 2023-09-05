---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>document-information-extraction]
primary_tag: topic>machine-learning
---

# Use Machine Learning to Extract Information from Documents with Document Information Extraction Trial UI
<!-- description --> Get machine learning model predictions for the documents you upload using the Document Information Extraction Trial UI.

## You will learn
  - How to use the Document Information Extraction Trial UI to upload new documents
  - How to see and edit the extraction results
  - How to delete documents

## Intro
The core functionality of Document Information Extraction is to automatically extract structured information from documents using machine learning. When you finish this tutorial, you will get field value predictions for the documents you upload to Document Information Extraction Trial UI.

---

### Upload documents


>Document Information Extraction uses a globally pre-trained machine learning model that currently obtains better accuracy results with invoices and payment advices in the languages listed in [Supported Languages and Countries/Regions](https://help.sap.com/docs/DOCUMENT_INFORMATION_EXTRACTION/5fa7265b9ff64d73bac7cec61ee55ae6/5bf847f7d1a848dcb3513eff9ec70412.html). The team is working to support additional document types and languages in the near future.

Upload to the service any document file in PDF or single-page PNG and JPEG format that has content in headers and tables, such as an invoice.

>As an alternative to uploading your own documents to the service, you can use the following sample invoice files (right click on the link, then click ***Save link as*** to download the files locally):

>- [Sample Invoice 1](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/sample-invoice-1.pdf)

>- [Sample Invoice 2](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/sample-invoice-2.pdf)

>- [Sample Invoice 3](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/sample-invoice-3.pdf)


1. Open the Document Information Extraction Trial UI, as described in the tutorial: [Use Trial to Set Up Account for Document Information Extraction and Go to Application](cp-aibus-dox-booster-app).

    <!-- border -->![DOX-UI-App](app.png)

2. In the top right, click **+** (Upload a new document).

    <!-- border -->![DOX-UI-App](upload.png)

3. In the Select Document screen, drop files directly or click **+** to upload one or more document files.

    <!-- border -->![DOX-UI-App](drop-files.png)

4. Select the **Document Type**. Click **Step 2**.

    <!-- border -->![DOX-UI-App](file-type.png)

5. In **Step 2**, select the header fields you want to extract from the documents you've uploaded. Click **Step 3**.

    <!-- border -->![DOX-UI-App](step-2.png)

6. In **Step 3**, select the line items you want to extract from the documents you've uploaded. Click **Review**.

    <!-- border -->![DOX-UI-App](step-3.png)

7. Review your selection. Click **Edit** if you want to change anything. Click **Confirm**.

    <!-- border -->![DOX-UI-App](review.png)

    You see the Document Name, Upload Date and Status of the documents you have just uploaded.

    <!-- border -->![DOX-UI-App](pending.png)

    Status changes from PENDING to READY. This means the selected header fields and line items have been extracted, and the extraction results are ready to be validated and changed if necessary. If status changes from PENDING to FAILED, this means it was not possible to get the extraction results, and you need to upload the document once again.

    <!-- border -->![DOX-UI-App](ready.png)


>**CAUTION:**

>When using the free tier option for Document Information Extraction or a trial account, be aware of the technical limits listed in [Free Tier Option and Trial Account Technical Constraints](https://help.sap.com/docs/document-information-extraction/document-information-extraction/free-tier-option-and-trial-account-technical-constraints).



### See and edit extraction results


1. In the Documents screen, click the document row where you see Document Name, Upload Date and Status.

    <!-- border -->![DOX-UI-App](choose.png)

    You see the page preview of the document file you uploaded.

    <!-- border -->![DOX-UI-App](extraction-results.png)

2. Click **Extraction Results** to see the Header Fields and Line Items extraction results.

    <!-- border -->![DOX-UI-App](extraction-results-done.png)

    See also the machine learning model **Extraction Confidence Range** classified by colors: red (confidence between 0% and 50%), yellow (confidence between 51% and 79%), and green (confidence between 80% and 100%).

    <!-- border -->![DOX-UI-App](confidence-range.png)

    See the prediction confidence score for each header field and line item extraction result by hovering the mouse over a field name, for example **Invoice Number**.

    <!-- border -->![DOX-UI-App](confidence.png)

3. In case corrections are needed and the document status is READY, you can **Edit** the Header Fields and Line Items extraction results.

    <!-- border -->![DOX-UI-App](edit-1.png)

    See an example where the **Currency Code** header field extraction result is edited:

    <!-- border -->![DOX-UI-App](edit-currency-code-1.png)

    <!-- border -->![DOX-UI-App](edit-currency-code-2.png)

4. Click **+** to insert a new line item at the bottom.

    <!-- border -->![DOX-UI-App](edit-2.png)

5. Select values in the document page preview, one each time, to **Assign Field** by choosing in the dropdown list the Field name. Add or change the extraction Value if necessary. Click **Apply** to add the selected field into the Header Fields or Line Items extraction results.

    See an example where the Buyer Contact value is selected in the document page preview and added to the Header Fields extraction results:

    <!-- border -->![DOX-UI-App](edit-buyer-1.png)

    <!-- border -->![DOX-UI-App](edit-buyer-2.png)

6. Save your changes.

    <!-- border -->![DOX-UI-App](save.png)

7. You can also **Edit** and **Confirm** the document.

    <!-- border -->![DOX-UI-App](confirm.png)

    Status changes from READY to CONFIRMED. This means the extraction results have been confirmed and can no longer be changed.



### Delete documents


1. In the Documents screen, click the document row where you see Document Name, Upload Date and Status.

    <!-- border -->![DOX-UI-App](choose-delete.png)

    You see the page preview of the document file you uploaded.

    <!-- border -->![DOX-UI-App](extraction-results-delete.png)

2. Click **Delete** and then click **OK** to delete the document you selected.

    The document is then removed from the Documents list.

    <!-- border -->![DOX-UI-App](gone.png)

Congratulations, you have completed this tutorial.

