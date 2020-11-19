---
title: Use Machine Learning to Classify Documents
description: Get the machine learning model category predictions for the documents you upload to Document Classification.
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, products>sap-cloud-platform, products>sap-ai-business-services, products>document-classification]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to call and test Document Classification
  - How to access and use Swagger UI (User Interface)
  - How to classify documents into categories with Document Classification

The core functionality of Document Classification is to automatically classify documents into categories. The categories are not predefined and can be chosen by the user.

In the trial version of Document Classification, however, a predefined and pre-trained machine learning model is made available for all users. Thus, the classification categories cannot be customized. This tutorial gives you an introduction to the service. If you want to try out the Document Classification training endpoints to create your own classification model, you can use an Enterprise Account (see [Trial Accounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/046f127f2a614438b616ccfc575fdb16.html) versus [Enterprise Accounts](https://help.sap.com/viewer/3504ec5ef16548778610c7e89cc0eac3/Cloud/en-US/171511cc425c4e079d0684936486eee6.html)) or a paid SAP Cloud Platform global account.

---

[ACCORDION-BEGIN [Step 1: ](Authorize Swagger UI)]

You will use Swagger UI, via all major web browser, to call the Document Classification APIs. Swagger UI allows developers to effortlessly interact and try out every single operation an API exposes for easy consumption. For more information, see [Swagger UI](https://swagger.io/tools/swagger-ui/).

In the service key you created for Document Classification in the previous tutorial: [Create Service Instance for Document Classification with Trial Account](cp-aibus-dc-service-instance), you should find (outside the `uaa` section of the service key) an entry called `url` (as highlighted in the image below).

1. To access the Document Information Extraction Swagger UI, add **`/document-classification/v1`** to the `url` value, paste it in any web browser and press **Enter**.

    ![Service keys](service-key-details.png)

2. To be able to use the Swagger UI endpoints you need to authorize yourself. In the top right corner, click **Authorize**.

    ![Swagger UI Authorize](swagger-authorize.png)

3. Copy the `access_token` created in the previous tutorial [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token), add **Bearer** in front of it, and enter in the **Value** field.

    ```
    Bearer <access_token>
    ```

    ![Access Token](access-token.png)

4. Click **Authorize**, and then click **Close**.

    ![Close Authorization](close-authorization.png)

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Classify documents)]

In this step, you can upload documents and get the service classification results.

In the Document Classification trial, there is a pre-trained model available for all users. The model can classify product descriptions into categories of product types. Feel free to use and upload any PDF documents you want.

>As an alternative to uploading your own document to the service, you can use the following file which is a product description of SAP Data Intelligence (right click on the link, then click ***Save link as*** to download the files locally):

>[SAP Data Intelligence](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dc-swagger-ui/data/data-intelligence.pdf)

In Swagger UI, open the group `classification`. Then click the second endpoint, called `/classification/models/{modelName}/versions/{modelVersion}/documents`. The request is marked by `POST` which indicates that data is sent to the service.

Next, click **Try it out** to try the request.

![Open Classification Endpoint](open-classify-endpoint.png)

Once you click the button, several input fields show up:

  1.  In the field `document`, upload the PDF document you want to classify.

  2.  The field `parameters` should be empty. In case the field has any pre-filled content, go ahead and delete it.

  3.  The field `modelVersion` needs to be filled. As machine learning models can have multiple versions, the version of the model needs to be specified with every request. Write `latest` into the field so that the latest version of the model is used.

  4.  The field `modelName` needs to be specified. Enter `DocumentInfoRecord/AUT` to use the model that categorizes products.

  5.  Finally, click **Execute** to send the document to the service.

![Classify Documents](classify-documents.png)

Once you have executed the request, you will receive a response from the server. To see the response, scroll down a bit until you find the `Response body`.

The response includes the current `status` of the classification process. The status should be `RUNNING`, which indicates that the classification is in process. Additionally, a `documentId` is included in the response. This ID is automatically assigned to the document by the server. Copy or make note of the ID, as you will need it in the next step.

![Classification Request Response](classify-response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](See classification results)]

To obtain the results of the classification, a different endpoint has to be used.

Open the third endpoint within the group `classification`, called `/classification/models/{modelName}/versions/{modelVersion}/documents/{documentId}`. The `GET` in front indicates that this time data is requested from the server.

Click **Try it out** to try the request.

![Open Result Endpoint](get-result-endpoint.png)

Once you click the button, similar input fields, as before, show up:

  1.  Enter `latest` in the field `modelVersion`.
  2.  Enter `DocumentInfoRecord/AUT` in the field `modelName`.
  3.  In the field `documentId`, enter the ID you received in the previous step.

Finally, click **Execute** to request the classification results.

![Get Classification Result](get-result.png)

Once you have executed the request, you will receive a response from the server which you find under `Response body`, as in the previous step. This time, the response includes the classification results for every characteristic by which the document is classified.

The only characteristic in this model is `PRODUCT_TYPE`. For this characteristic, the service provides a list of results. Each result includes a `label` which represents the predicted value plus a `score` which represents how confident the service is about the prediction. The whole list of results is ranked by the `score`, in other words, the most probable value comes first.

![Classification Result](classification-result-response.png)

Go ahead and upload other PDF documents to see the categories outcome. Just note that there is a limited amount of documents that you can classify using a trial account. Find the trial account input limits for Document Classification [here](https://help.sap.com/viewer/ca60cd2ed44f4261a3ae500234c46f37/SHIP/en-US/baeb1924c84343158f483d82b21bd6a4.html).

Congratulations, you have now successfully used our machine learning model to get category predictions for the documents you uploaded to Document Classification.

[DONE]
[ACCORDION-END]
