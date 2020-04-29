---
title: Use Machine Learning to Enrich Business Data
description: Create, update, list and delete enrichment data using the Data API business entity from Document Information Extraction, one of the SAP AI Business Services in SAP Cloud Platform.
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-cloud-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Prerequisites
- [Create Service Instance for Document Information Extraction](cp-aibus-dox-service-instance)
- [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token)
- [Use Machine Learning to Extract Information from Documents](cp-aibus-dox-swagger-ui) (step 1 only)

## Details
### You will learn
  - How to create, update, list and delete enrichment data using the business entity

After completing the tutorial mission [Get Started with Document Information Extraction](https://developers.sap.com/mission.cp-aibus-extract-document-service.html), you can also use Document Information Extraction to enrich the information extracted from documents with your own master data. You can, for example, match enrichment data entities, such as supplier IDs, with the document [Extracted Header Fields](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/b1c07d0c51b64580881d11b4acb6a6e6.html), such as sender names.

When enriching data with Document Information Extraction, you use 2 types of entities that you find in business documents. The `business entity` represents different kinds of organizations with which you deal as a company. It can represent, for example, suppliers and customers. The `employee entity` represents an employee in the company.

When you finish this tutorial, you will have explored all Data API functionalities to create, update, list and delete enrichment data using the `business entity` type. See [Enrichment Data API documentation](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/ca4b609107dd47a78d880cb5eaceb8c8.html).

---

[ACCORDION-BEGIN [Step 1: ](Create client)]

After completing the prerequisite tutorials [Create Service Instance for Document Information Extraction](cp-aibus-dox-service-instance) and [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token), and performing at least step 1 in the tutorial [Use Machine Learning to Extract Information from Documents](cp-aibus-dox-swagger-ui) to access and authorize the Document Information Extraction Swagger UI, you need to create a client. This client is used in most of the endpoints to distinguish and separate data.

You can either create a single client or multiple clients in the **payload** field of the **POST /clients** endpoint. The **`clientId`** values created here will be used in other service endpoints.

1. Expand the **POST /clients** endpoint.

2. Click **Try it out**.

3. Enter your **`clientId`** and **`clientName`** values in the **payload** field in the format you see in **Examples for payload parameter** (`c_29` and `client 29`, for example).

4. Click **Execute**.

![DOX](1create_clients_request.png)

You should receive a response like the following:

![DOX](1create_clients_response.png)


>**CAUTION:**

>Be aware of the following Document Information Extraction trial account limitations:​

>- Maximum 40 uploaded documents per week​

>- Maximum 1 created `clientId`

>- Maximum 10 created enrichment `dataIds`


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create enrichment data)]

Use the **POST /data/jobs** endpoint to add your own master data records to the database to enrich the information extracted from documents.

1. Expand the **POST /data/jobs** endpoint.

2. Click **Try it out**.

3. Define the data in the **`payload`** field, so that the system knows which extracted field (using, for example, supplier IDs from master data) should be enriched.

    ```JSON
    {
       "value":[
          {
             "id":"BE0001",
             "name":"DEMO - Sliced",
             "accountNumber":"",
             "address1":"123 Somewhere Street Your AZ 12345 123 Somewhere St Melbourne, VIC 3000",
             "address2":"",
             "city":"",
             "countryCode":"",
             "postalCode":"",
             "state":"",
             "email":"",
             "phone":"",
             "bankAccount":"",
             "taxId":""
          },
          {
             "id":"BE0002",
             "name":"DEMO",
             "accountNumber":"",
             "address1":"123 Somewhere Street Your AZ 12345 123 Somewhere St Melbourne, VIC 3000",
             "address2":"",
             "city":"",
             "countryCode":"",
             "postalCode":"",
             "state":"",
             "email":"",
             "phone":"",
             "bankAccount":"",
             "taxId":""
          }
       ]
    }
    ```

4. Choose the enrichment data **`type`** `businessEntity`.

5. Enter your **`clientId`** (created in the previous step).

6. When you choose the enrichment data **`type`** business entity, you have the option to choose a **`subtype`** (`supplier`, `customer` or `companyCode`). In this example, choose `supplier`.

7. Click **Execute**.

![DOX](1post_data_jobs_request.png)

> ### What just happened?
>
> In this example, in the **`payload`** field, several master data records (name, ID and address, for example) from 2 different suppliers (DEMO - Sliced and DEMO) are provided, so this additional information can be added to the document extracted fields prediction when the information matches.

You should receive a response like the following with status PENDING:

![DOX](1post_data_jobs_response.png)

Copy the **`id`** from the **Response body** to see the result of the enrichment data status in the **GET /data/jobs/{`uuid`}** endpoint.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](See created enrichment data status)]

Use the **GET /data/jobs/{`uuid`}** endpoint to see the status of the uploaded enrichment data.

1. Expand the **GET /data/jobs/{`uuid`}** endpoint.

2. Click **Try it out**.

3. Enter the **`id`** received in the **POST /data/jobs** endpoint as the **`uuid`**.

4. Click **Execute**.

![DOX](1get_data_jobs_uuid_request.png)

You should receive a response like the following with status SUCCESS:

![DOX](1get_data_jobs_uuid_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Upload document to get prediction with enrichment data)]

>Document Information Extraction uses a globally pre-trained machine learning model that currently obtains better accuracy results with invoices and payment advices in English. The team is working to support additional document types and languages in the near future.

When enrichment data has been uploaded and fits to a certain prediction it is added to the results from the **GET /document/jobs/{`uuid`}** endpoint. To have the enrichment data in the prediction, you need to have the following part in the query of the **POST /document/jobs** endpoint (it is usually already there by default):

```JSON
"enrichment": {
    "sender": {
      "top": 5,
      "type": "businessEntity",
      "subtype": "supplier"
    },
    "employee": {
      "type": "employee"
    }
  }
```

1. Expand the **POST /document/jobs** endpoint.

2. Click **Try it out**.

<<<<<<< HEAD
3. Right click [Sample Invoice 1](`https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/sample-invoice-1.pdf`), then use the ***Save link as*** option to download locally the document PDF file recommended for this enrich business data example.
=======
3. Right click [Sample Invoice 1](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-data-api-bus/sample-invoice-1.pdf), then use the ***Save link as*** option to download locally the document PDF file recommended for this enrich business data example.
>>>>>>> upstream/master
> You can also upload and enrich your own document in PDF format with content in headers and tables (an invoice, for example). In this case, make sure the data you define in the **`payload`** field, in step 2, matches your document fields.

4. Upload the document PDF file you want to enrich.

5. In **options**, enter the list of fields to be extracted from the uploaded file (`documentNumber,taxId,purchaseOrder,shippingAmount,subTotalAmount,vendorAddress,vendorName,totalAmount,currencyCode`, for example), the client id you created in step 1 (`c_29`, for example), the document type (`invoice`, for example), the enrichment data type `businessEntity` and subtype `supplier`.

6. Click **Execute**.

This is how the request should look like:

![DOX](1post_document_jobs_request.png)

And that's how the response looks like:

![DOX](1post_document_jobs_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Get enrichment data prediction)]

When enrichment data has been uploaded and fits to a certain prediction it is added to the results from the **GET /document/jobs/{`uuid`}** endpoint.

1. Expand the **GET /document/jobs/{`uuid`}** endpoint.

2. Click **Try it out**.

3. Set **`extractedValues`** to `true` to get the extracted values.

4. Enter the **`clientId`** you created in step 1 (`c_29`, for example).

5. Enter the **`id`** received in the **POST /document/jobs** endpoint as the **`uuid`**.

6. Click **Execute**.

The endpoint request and response look as follows:

![DOX](1get_document_jobs_uuid_request.png)

![DOX](1get_document_jobs_uuid_response.png)

> ### What just happened?
>
> In this example, in the response, one of the extracted fields is the sender name DEMO - Sliced. This information is enriched with the supplier ID enrichment data created in step 2. The prediction suggests the supplier ID BE0001 (from sender name DEMO - Sliced) with higher probability than the supplier ID BE0002 (from sender name DEMO).

This is an example of a full prediction including the enrichment data part:

```JSON
{
  "status": "DONE",
  "id": "149e33f5-7113-420e-ab8c-0b0382d660f9",
  "documentType": "invoice",
  "fileName": "sample-invoice-1.pdf",
  "country": "XX",
  "created": "2020-04-02T13:21:58.831658+00:00",
  "extraction": {
    "headerFields": [
      {
        "name": "documentNumber",
        "category": "document",
        "value": "INV-3337",
        "type": "string",
        "confidence": 0.989006499449412,
        "page": 1,
        "coordinates": {
          "x": 0.760887096774194,
          "y": 0.141676168757127,
          "w": 0.060483870967742,
          "h": 0.00769669327251996
        }
      },
      {
        "name": "senderAddress",
        "category": "sender",
        "value": "123 Somewhere Street Your AZ 12345 123 Somewhere St Melbourne, VIC 3000",
        "type": "string",
        "confidence": 0.640021374932042,
        "page": 1,
        "coordinates": {
          "x": 0.0717741935483871,
          "y": 0.192987457240593,
          "w": 0.150403225806452,
          "h": 0.13939566704675
        }
      },
      {
        "name": "senderName",
        "category": "sender",
        "value": "DEMO - Sliced",
        "type": "string",
        "confidence": 0.808902713987562,
        "page": 1,
        "coordinates": {
          "x": 0.0725806451612903,
          "y": 0.163055872291904,
          "w": 0.0955645161290323,
          "h": 0.00798175598631701
        }
      },
      {
        "name": "receiverContact",
        "category": "receiver",
        "value": "",
        "type": "string",
        "confidence": 0,
        "page": 1,
        "coordinates": {
          "x": 0.000403225806451613,
          "y": 0.000570125427594071,
          "w": 0.000806451612903226,
          "h": 0.000570125427594071
        }
      },
      {
        "name": "taxAmount",
        "category": "amounts",
        "value": 8.5,
        "type": "number",
        "confidence": 1,
        "page": 1,
        "coordinates": {
          "x": 0.878225806451613,
          "y": 0.486031927023945,
          "w": 0.913709677419355,
          "h": 0.49515393386545
        },
        "group": 1
      },
      {
        "name": "purchaseOrderNumber",
        "category": "details",
        "value": "12345",
        "type": "string",
        "confidence": 1,
        "page": 1,
        "coordinates": {
          "x": 0.760887096774194,
          "y": 0.160775370581528,
          "w": 0.799596774193548,
          "h": 0.168187001140251
        }
      },
      {
        "name": "netAmount",
        "category": "amounts",
        "value": 85,
        "type": "number",
        "confidence": 0.990608950455984,
        "page": 1,
        "coordinates": {
          "x": 0.870161290322581,
          "y": 0.467217787913341,
          "w": 0.0435483870967742,
          "h": 0.0091220068415051
        }
      },
      {
        "name": "grossAmount",
        "category": "amounts",
        "value": 93.5,
        "type": "number",
        "confidence": 0.994631409645081,
        "page": 1,
        "coordinates": {
          "x": 0.870161290322581,
          "y": 0.505131128848347,
          "w": 0.0435483870967742,
          "h": 0.0091220068415051
        }
      },
      {
        "name": "currencyCode",
        "category": "amounts",
        "value": "USD",
        "type": "string",
        "confidence": 0.70153421163559,
        "page": 1,
        "coordinates": {
          "x": 0,
          "y": 0,
          "w": 0,
          "h": 0
        }
      },
      {
        "name": "documentDate",
        "category": "document",
        "value": "2016-01-25",
        "type": "date",
        "confidence": 0.989444378763437,
        "page": 1,
        "coordinates": {
          "x": 0.759677419354839,
          "y": 0.179304446978335,
          "w": 0.113306451612903,
          "h": 0.00969213226909921
        }
      }
    ],
    "lineItems": [
      [
        {
          "name": "description",
          "category": "details",
          "value": "Web Design",
          "type": "string",
          "confidence": 0.782224123883579,
          "page": 1,
          "coordinates": {
            "x": 0.172177419354839,
            "y": 0.419042189281642,
            "w": 0.0798387096774194,
            "h": 0.00997719498289623
          }
        },
        {
          "name": "quantity",
          "category": "details",
          "value": 1,
          "type": "number",
          "confidence": 0.712613101516451,
          "page": 1,
          "coordinates": {
            "x": 0.102822580645161,
            "y": 0.42502850627138,
            "w": 0.0266129032258064,
            "h": 0.00769669327251993
          }
        },
        {
          "name": "netAmount",
          "category": "amounts",
          "value": 85,
          "type": "number",
          "confidence": 0.915386190920165,
          "page": 1,
          "coordinates": {
            "x": 0.870161290322581,
            "y": 0.424173318129989,
            "w": 0.0439516129032258,
            "h": 0.00940706955530218
          }
        },
        {
          "name": "unitPrice",
          "category": "details",
          "value": 85,
          "type": "number",
          "confidence": 0,
          "page": 1,
          "coordinates": {
            "x": 0.610483870967742,
            "y": 0.424173318129989,
            "w": 0.0435483870967742,
            "h": 0.00912200684150516
          }
        }
      ]
    ]
  },
  "enrichment": {
    "sender": [
      {
        "id": "BE0001",
        "confidence": 0.920635
      },
      {
        "id": "BE0002",
        "confidence": 0.825397
      }
    ],
    "employee": []
  }
}
```

You have now successfully used the business entity to get enrichment data predictions for the document you uploaded to Document Information Extraction.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](See all enrichment data entries)]

To see a list of the enrichment data entries you have created:

1. Expand the **GET /data** endpoint.

2. Click **Try it out**.

3. Choose the enrichment data **`type`** `businessEntity`.

4. Enter your **`clientId`**.

5. Choose the enrichment data **`subtype`** `supplier`.

6. Click **Execute**.

![DOX](1get_data_request.png)

You should receive a response like the following:

![DOX](1get_data_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Delete enrichment data)]

To delete enrichment data which has been uploaded before:

1. Expand the **DELETE /data** endpoint.

2. Click **Try it out**.

3. Define the data in the **`payload`** field, so that the system knows which data entry (using, for example, the data entry ID) should be deleted.

4. Choose the enrichment data **`type`** `businessEntity`.

5. Enter your **`clientId`**.

6. Choose the enrichment data **`subtype`** `supplier`.

7. Click **Execute**.

![DOX](1delete_data_request.png)

You should receive a response like the following:

![DOX](1delete_data_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Delete client)]

If you want to delete a client you created in Step 1, use the **DELETE /clients** endpoint.

1. Expand the **DELETE /clients** endpoint.

2. Click **Try it out**.

3. Enter in the **payload** field the client id or multiple client ids (`c_29`, for example) you want to delete.

4. Click **Execute**.

![DOX](1delete_clients_request.png)

You should receive a response like the following:

![DOX](1delete_clients_response.png)

Congratulations, you have completed this tutorial.

[DONE]
[ACCORDION-END]
