---
title: Use Machine Learning to Extract Information from Documents
description: Get the prediction for the uploaded document extracted fields.
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>cloud, products>sap-cloud-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to call and test Document Information Extraction
  - How to access and use Swagger UI (User Interface)
  - How to extract information from files with Document Information Extraction

The core functionality of Document Information Extraction is to extract structured information from documents automatically using machine learning. When you finish this tutorial, you will get the prediction for the uploaded document extracted fields.

---

[ACCORDION-BEGIN [Step 1: ](Authorize Swagger UI)]

You will use Swagger UI, via any web browser, to call the Document Information Extraction APIs. Swagger UI allows end developers to effortlessly interact and try out every single operation an API exposes for easy consumption. For more information, see [Swagger UI](https://swagger.io/tools/swagger-ui/).  

In the service key you created for Document Information Extraction in [Create Service Instance for Document Information Extraction](cp-aibus-dox-service-instance), you should find (outside the `uaa` section of the service key) an entry called `url` (as highlighted in the image below).

1. To access the Document Information Extraction Swagger UI, add **`/document-information-extraction/v1`** to the `url` value, paste it in any web browser and choose **Enter**.

    ![DOX](png-files\service-key-url.png)

2. To be able to use the Swagger UI endpoints you need to authorize yourself. On the top right corner, click **Authorize**.

    ![DOX](png-files/swagger0.png)

3. Get the `access_token` value created in [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token), then add **Bearer** in front of it, and enter in the **Value** field.

    ```
    Bearer <access_token>
    ```

    ![DOX](png-files/Authorize.png)

4. Click **Authorize**, and then click **Close**.

    ![DOX](png-files/Authorize2.png)

[VALIDATE_1]

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](See list of document fields you can extract)]

Use the **GET /capabilities** endpoint to see a list of document fields you can extract with Document Information Extraction.

Click the endpoint name to expand it, click **Try it out**, and then **Execute**.

![DOX](png-files/capabilities.png)

![DOX](png-files/capabilities2.png)

You should receive a response like the following:

![DOX](png-files\capabilitiesResponse.png)

>If you get an error response code 401 (Unauthorized), your token is probably incorrect. Check if you have added the word **`Bearer`** before the token and if the token value is complete and has been properly copied from the **`access_token`** value you received in [Get OAuth Access Token for Document Information Extraction Using a REST Client](cp-aibus-dox-rest-oauth-token).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create a client)]

To use Document Information Extraction, you need to create a client. This client is used in most of the endpoints to distinguish and separate data.

You can either create only one client or create a list of clients in the **payload** field of the **POST /clients** endpoint. The **`clientId`** values created here can be used in all the other service endpoints.

1. Expand the **POST /clients** endpoint.

2. Click **Try it out**.

3. Enter your **`clientId`** and **`clientName`** values in the **payload** field in the format you see in **Examples for payload parameter**.

4. Click **Execute**.

![DOX](png-files/createClient.png)

You should receive a response like the following:

![DOX](png-files/createClientResponse.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](See created clients)]

To see a list of the clients you have created:

1. Expand the **GET /clients** endpoint.

2. Click **Try it out**.

3. Enter a maximum number of clients to be listed in the **limit** field.

4. Click **Execute**.

![DOX](png-files/listClient.png)

You should receive a response like the following:

![DOX](png-files/listClientResponse.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Upload a document)]

Upload a PDF document, such as an invoice, to the service using the **POST /document/jobs** endpoint. You need to specify:

  - A file

  - A **`clientId`**

  - The fields to be extracted. You can find the list of available fields that can be extracted in the response of the **GET /capabilities** endpoint.

Do the following:

1. Expand the **POST /document/jobs** endpoint.

2. Click **Try it out**.

3. Upload a document PDF file.

4. Enter the list of fields to be extracted from the uploaded file (`documentNumber,taxId,purchaseOrder,shippingAmount,subTotalAmount,vendorAddress,vendorName,totalAmount,currencyCode`, for example), a client id or a list of client ids (`c_00`, for example), and the document type (`invoice`, for example).

5. Click **Execute**.

    ![DOX](png-files/testInvoice.png)

After you click **Execute**, you should receive a response like the following:

![DOX](png-files/testInvoiceResult.png)

Copy the **`id`** from the **Response body** to see the result of the extraction in the **GET /document/jobs/{`uuid`}** endpoint.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](See extracted fields)]

You can now use the **GET /document/jobs/{`uuid`}** endpoint to receive the prediction.

1. Expand the endpoint.

2. Click **Try it out**.

3. Enter the **`clientId`** you used in the previous step (`c_00`, for example).

4. Enter the **`id`** received in the **POST /jobs** endpoint as the **`uuid`**.

5. Click **Execute**.

    ![DOX](png-files/getResults.png)

You should receive a response like the following:

![DOX](png-files/getResultsResponse.png)

In the response, you will find some general information about the document you uploaded. In the `headerFields`, such as `documentType` and `fieldName`, you will find the prediction for the extracted fields. If the status of the document (indicated by the **status** field) is **PENDING** instead of **DONE** then it means that the service is still extracting some fields and the returned JSON file does not yet contain all the requested fields.

[DONE]
[ACCORDION-END]
