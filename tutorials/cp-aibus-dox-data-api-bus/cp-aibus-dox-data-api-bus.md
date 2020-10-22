---
title: Use Machine Learning to Enrich Business Data
description: Create, update, list and delete enrichment data using the Data API business entity from Document Information Extraction, one of the SAP AI Business Services in SAP Cloud Platform.
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-cloud-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Prerequisites
- [Set Up Account for Document Information Extraction](cp-aibus-dox-service-instance-booster)
- [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token)
- [Use Machine Learning to Extract Information from Documents with Swagger UI](cp-aibus-dox-swagger-ui) (step 1 only)

## Details
### You will learn
  - How to create, update, list and delete enrichment data using the business entity

After completing the tutorial mission [Use Machine Learning to Process Business Documents](https://developers.sap.com/mission.cp-aibus-extract-document-service.html), you can also use Document Information Extraction to enrich the information extracted from documents with your own master data. You can, for example, match enrichment data entities, such as supplier IDs, with the document [Extracted Header Fields](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/b1c07d0c51b64580881d11b4acb6a6e6.html), such as sender names.

When enriching data with Document Information Extraction, you use 2 types of entities that you find in business documents. The `business entity` represents different kinds of organizations with which you deal as a company. It can represent, for example, suppliers and customers. The `employee entity` represents an employee in the company.

When you finish this tutorial, you will have explored all Data API functionalities to create, update, list and delete enrichment data using the `business entity` type. See [Enrichment Data API documentation](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/ca4b609107dd47a78d880cb5eaceb8c8.html).

---

[ACCORDION-BEGIN [Step 1: ](Create client)]

After completing the prerequisite tutorials [Set Up Account for Document Information Extraction](cp-aibus-dox-service-instance-booster) and [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token), and performing at least step 1 in the tutorial [Use Machine Learning to Extract Information from Documents with Swagger UI](cp-aibus-dox-swagger-ui) to access and authorize the Document Information Extraction Swagger UI, you need to create a client. This client is used in most of the endpoints to distinguish and separate data.

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

>- Maximum 40 uploaded document pages per week​ (the documents can have more than 1 page)

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
             "name":"Sliced Invoices",
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
             "name":"Sliced",
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

!![DOX](1post_data_jobs_request.png)

> ### What just happened?
>
> In this example, in the **`payload`** field, several master data records (name, ID and address, for example) from 2 different suppliers (Sliced Invoices and Sliced) are provided, so this additional information can be added to the document extracted fields prediction when the information matches.

You should receive a response like the following with status PENDING:

!![DOX](1post_data_jobs_response.png)

Copy the **`id`** from the **Response body** to see the result of the enrichment data status in the next step.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](See created enrichment data status)]

Use the **GET /data/jobs/{`id`}** endpoint to see the status of the uploaded enrichment data.

1. Expand the **GET /data/jobs/{`id`}** endpoint.

2. Click **Try it out**.

3. Enter the **`id`** received in the **POST /data/jobs** endpoint as the **`id`**.

4. Click **Execute**.

!![DOX](1get_data_jobs_id_request.png)

You should receive a response like the following with status SUCCESS:

!![DOX](1get_data_jobs_id_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Upload document to get prediction with enrichment data)]

>Document Information Extraction uses a globally pre-trained machine learning model that currently obtains better accuracy results with invoices and payment advices in the languages listed in [Supported Languages and Countries](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/5bf847f7d1a848dcb3513eff9ec70412.html). The team is working to support additional document types and languages in the near future.

When enrichment data has been uploaded and fits to a certain prediction it is added to the results from the **GET /document/jobs/{`id`}** endpoint. To have the enrichment data in the prediction, you need to have the following part in the query of the **POST /document/jobs** endpoint (it is usually already there by default):

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

Do the following:

1. Expand the **POST /document/jobs** endpoint.

2. Click **Try it out**.

3. Right click [Sample Invoice 1](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/data/sample-invoice-1.pdf), then click ***Save link as*** to download locally the document file for this enrich business data example.
> You can also upload to the service and enrich any document file in PDF or single-page PNG and JPEG format that has content in headers and tables, such as an invoice. In this case, make sure the data you define in the **`payload`** field, in step 2, matches your document fields.

4. Upload the document file you want to enrich.

5. In **options**, enter the list of fields to be extracted from the uploaded file (`documentNumber`, `taxId`, `purchaseOrderNumber`, `shippingAmount`, `netAmount`, `senderAddress`, `senderName`, `grossAmount`, for example), the client id you created in step 1 (`c_29`, for example), the document type (`invoice`, for example), `receivedDate` (2020-02-17, for example), the enrichment data type `businessEntity` and subtype `supplier`.

    ```JSON
    {
       "extraction":{
          "headerFields":[
             "documentNumber",
             "taxId",
             "purchaseOrderNumber",
             "shippingAmount",
             "netAmount",
             "senderAddress",
             "senderName",
             "grossAmount",
             "currencyCode",
             "receiverContact",
             "documentDate",
             "taxAmount",
             "taxRate",
             "receiverName",
             "receiverAddress"
          ],
          "lineItemFields":[
             "description",
             "netAmount",
             "quantity",
             "unitPrice",
             "materialNumber"
          ]
       },
       "clientId":"c_29",
       "documentType":"invoice",
       "receivedDate":"2020-02-17",
       "enrichment":{
          "sender":{
             "top":5,
             "type":"businessEntity",
             "subtype":"supplier"
          },
          "employee":{
             "type":"employee"
          }
       }
    }
    ```

6. Click **Execute**.

This is how the request should look like:

!![DOX](1post_document_jobs_request.png)

And that's how the response looks like:

!![DOX](1post_document_jobs_response.png)

Copy the **`id`** from the **Response body** to get enrichment data prediction in the next step.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Get enrichment data prediction)]

When enrichment data has been uploaded and fits to a certain prediction it is added to the results from the **GET /document/jobs/{`id`}** endpoint.

> Enrichment data is refreshed automatically every 4 hours. It might take up to 4 hours until the enrichment data prediction is available in the response.

1. Expand the **GET /document/jobs/{`id`}** endpoint.

2. Click **Try it out**.

3. Set **`extractedValues`** to `true` to get the extracted values.

4. Enter the **`id`** received in the **POST /document/jobs** endpoint as the **`id`**.

5. Click **Execute**.

The endpoint request and response look as follows:

!![DOX](1get_document_jobs_id_request.png)

!![DOX](1get_document_jobs_id_response.png)

> ### What just happened?
>
> In this example, in the response, one of the extracted fields is the sender name Sliced Invoices. This information is enriched with the supplier ID enrichment data created in step 2. The prediction suggests the supplier ID BE0001 (from sender name Sliced Invoices) with higher probability than the supplier ID BE0002 (from sender name Sliced).

This is an example of a full prediction including the enrichment data part:

    ```JSON
    {
      "status": "DONE",
      "id": "97a96f7c-ea6a-456c-a58c-1cee1121a210",
      "fileName": "sample-invoice-1.pdf",
      "documentType": "invoice",
      "created": "2020-06-15T09:59:04.091971+00:00",
      "finished": "2020-06-15T09:59:29.115803+00:00",
      "country": "XX",
      "extraction": {
        "headerFields": [
          {
            "name": "currencyCode",
            "category": "amounts",
            "value": "USD",
            "type": "string",
            "confidence": 0.919029772281647,
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
            "confidence": 0.983220152556896,
            "page": 1,
            "coordinates": {
              "x": 0.759677419354839,
              "y": 0.174458380843786,
              "w": 0.113709677419355,
              "h": 0.00997719498289626
            }
          },
          {
            "name": "documentNumber",
            "category": "document",
            "value": "INV-3337",
            "type": "string",
            "confidence": 0.655578226513333,
            "page": 1,
            "coordinates": {
              "x": 0.760887096774194,
              "y": 0.136830102622577,
              "w": 0.0608870967741936,
              "h": 0.00826681870011403
            }
          },
          {
            "name": "grossAmount",
            "category": "amounts",
            "value": 93.5,
            "type": "number",
            "confidence": 0.656007521080248,
            "page": 1,
            "coordinates": {
              "x": 0.870161290322581,
              "y": 0.500285062713797,
              "w": 0.0447580645161291,
              "h": 0.00940706955530213
            }
          },
          {
            "name": "netAmount",
            "category": "amounts",
            "value": 85,
            "type": "number",
            "confidence": 0.64037944782864,
            "page": 1,
            "coordinates": {
              "x": 0.870161290322581,
              "y": 0.462656784492588,
              "w": 0.0447580645161291,
              "h": 0.00940706955530218
            }
          },
          {
            "name": "paymentTerms",
            "category": "payment",
            "value": "within 30 days from date of invoice.",
            "type": "string",
            "confidence": 0.600183615235057,
            "page": 1,
            "coordinates": {
              "x": 0.192741935483871,
              "y": 0.863169897377423,
              "w": 0.23508064516129,
              "h": 0.0102622576966933
            }
          },
          {
            "name": "purchaseOrderNumber",
            "category": "details",
            "value": "12345",
            "type": "string",
            "confidence": 0,
            "page": 1,
            "coordinates": {
              "x": 0.760887096774194,
              "y": 0.155929304446978,
              "w": 0.0391129032258065,
              "h": 0.00741163055872293
            }
          },
          {
            "name": "receiverAddress",
            "category": "receiver",
            "value": "123 Somewhere St Melbourne, VIC 3000",
            "type": "string",
            "confidence": 0.583759787255599,
            "page": 1,
            "coordinates": {
              "x": 0.0729838709677419,
              "y": 0.303591790193843,
              "w": 0.138306451612903,
              "h": 0.024230330672748
            }
          },
          {
            "name": "receiverName",
            "category": "receiver",
            "value": "Test Business",
            "type": "string",
            "confidence": 0.611305298987362,
            "page": 1,
            "coordinates": {
              "x": 0.0717741935483871,
              "y": 0.288768529076397,
              "w": 0.092741935483871,
              "h": 0.00798175598631695
            }
          },
          {
            "name": "senderAddress",
            "category": "sender",
            "value": "Suite 5A-1204 123 Somewhere Street Your City AZ 12345",
            "type": "string",
            "confidence": 0.60411537696182,
            "page": 1,
            "coordinates": {
              "x": 0.0721774193548387,
              "y": 0.173318129988597,
              "w": 0.150806451612903,
              "h": 0.040193842645382
            }
          },
          {
            "name": "senderName",
            "category": "sender",
            "value": "Sliced Invoices",
            "type": "string",
            "confidence": 0.622503629965442,
            "page": 1,
            "coordinates": {
              "x": 0.0713709677419355,
              "y": 0.157069555302166,
              "w": 0.100403225806452,
              "h": 0.00798175598631698
            }
          },
          {
            "name": "taxRate",
            "category": "amounts",
            "value": 0,
            "type": "number",
            "confidence": 0.818773365020752,
            "page": 1,
            "coordinates": {
              "x": 0.743548393249512,
              "y": 0.420182436704636,
              "w": 0.0411290526390076,
              "h": 0.00769668817520142
            },
            "group": 1
          }
        ],
        "lineItems": [
          [
            {
              "name": "description",
              "category": "details",
              "value": "Web Design",
              "type": "string",
              "confidence": 0.607532449066639,
              "page": 1,
              "coordinates": {
                "x": 0.172983870967742,
                "y": 0.414196123147092,
                "w": 0.0794354838709678,
                "h": 0.0102622576966933
              }
            },
            {
              "name": "quantity",
              "category": "details",
              "value": 1,
              "type": "number",
              "confidence": 0.63710602976027,
              "page": 1,
              "coordinates": {
                "x": 0.102822580645161,
                "y": 0.42018244013683,
                "w": 0.0270161290322581,
                "h": 0.00769669327251998
              }
            },
            {
              "name": "netAmount",
              "category": "amounts",
              "value": 85,
              "type": "number",
              "confidence": 0.633846010222579,
              "page": 1,
              "coordinates": {
                "x": 0.870161290322581,
                "y": 0.419612314709236,
                "w": 0.0447580645161291,
                "h": 0.00940706955530213
              }
            },
            {
              "name": "unitPrice",
              "category": "details",
              "value": 85,
              "type": "number",
              "confidence": 0.635079625881079,
              "page": 1,
              "coordinates": {
                "x": 0.610483870967742,
                "y": 0.419612314709236,
                "w": 0.0447580645161291,
                "h": 0.00940706955530213
              }
            }
          ]
        ]
      },
      "enrichment": {
        "sender": [
          {
            "id": "BE0001",
            "confidence": 0.5625
          },
          {
            "id": "BE0002",
            "confidence": 0.4625
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

    ```JSON
    {
       "value":[
          {
             "id":"BE0001"
          }
       ]
    }
    ```

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
