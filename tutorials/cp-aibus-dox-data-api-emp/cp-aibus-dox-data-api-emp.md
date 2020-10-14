---
title: Use Machine Learning to Enrich Employee Data
description: Create, update, list and delete enrichment data using the Data API employee entity from Document Information Extraction, one of the SAP AI Business Services in SAP Cloud Platform.
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
  - How to create, update, list and delete enrichment data using the employee entity

After completing the tutorial mission [Use Machine Learning to Process Business Documents](https://developers.sap.com/mission.cp-aibus-extract-document-service.html), you can also use Document Information Extraction to enrich the information extracted from documents with your own master data. You can, for example, match enrichment data entities, such as employee IDs, with the document [Extracted Header Fields](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/b1c07d0c51b64580881d11b4acb6a6e6.html), such as receiver contacts.

When enriching data with Document Information Extraction, you use 2 types of entities that you find in business documents. The `business entity` represents different kinds of organizations with which you deal as a company. It can represent, for example, suppliers and customers. The `employee entity` represents an employee in the company.

When you finish this tutorial, you will have explored all Data API functionalities to create, update, list and delete enrichment data using the `employee entity` type. See [Enrichment Data API documentation](https://help.sap.com/viewer/5fa7265b9ff64d73bac7cec61ee55ae6/SHIP/en-US/ca4b609107dd47a78d880cb5eaceb8c8.html).

---

[ACCORDION-BEGIN [Step 1: ](Create client)]

After completing the prerequisite tutorials [Set Up Account for Document Information Extraction](cp-aibus-dox-service-instance-booster) and [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token), and performing at least step 1 in the tutorial [Use Machine Learning to Extract Information from Documents with Swagger UI](cp-aibus-dox-swagger-ui) to access and authorize the Document Information Extraction Swagger UI, you need to create a client. This client is used in most of the endpoints to distinguish and separate data.

You can either create a single client or multiple clients in the **payload** field of the **POST /clients** endpoint. The **`clientId`** values created here will be used in other service endpoints.

1. Expand the **POST /clients** endpoint.

2. Click **Try it out**.

3. Enter your **`clientId`** and **`clientName`** values in the **payload** field in the format you see in **Examples for payload parameter** (`c_27` and `client 27`, for example).

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

3. Define the data in the **`payload`** field, so that the system knows which extracted field (using, for example, the employee ID from master data) should be enriched.

    ```JSON
    {
       "value":[
          {
             "id":"E0001",
             "email":"",
             "firstName":"Linda",
             "middleName":"",
             "lastName":"Owens"
          },
          {
             "id":"E0002",
             "email":"",
             "firstName":"Lin",
             "middleName":"",
             "lastName":"Owens"
          }
       ]
    }
    ```

4. Choose the enrichment data **`type`** `employee`.

5. Enter your **`clientId`** (created in the previous step).

6. Click **Execute**.

!![DOX](1post_data_jobs_request.png)

> ### What just happened?
>
> In this example, in the **`payload`** field, several master data records (full name and ID, for example) from 2 employees (Linda Owens and Lin Owens) are provided, so this additional information can be added to the document extracted fields prediction when the information matches.

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

>Document Information Extraction uses a globally pre-trained machine learning model that currently obtains better accuracy results with invoices and payment advices in English. The team is working to support additional document types and languages in the near future.

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

3. Right click [Sample Invoice 2](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-aibus-dox-swagger-ui/data/sample-invoice-2.pdf), then click ***Save link as*** to download locally the document file for this enrich employee data example.
> You can also upload to the service and enrich any document file in PDF or single-page PNG and JPEG format that has content in headers and tables, such as an invoice. In this case, make sure the data you define in the **`payload`** field, in step 2, matches your document fields.

4. Upload the document file you want to enrich.

5. In **options**, enter the list of fields to be extracted from the uploaded file (`documentNumber`, `taxId`, `purchaseOrderNumber`, `shippingAmount`, `netAmount`, `senderAddress`, `senderName`, `grossAmount`, for example), the client id you created in step 1 (`c_27`, for example), the document type (`invoice`, for example), `receivedDate` (2020-02-17, for example) and the enrichment data type `employee`.

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
       "clientId":"c_27",
       "documentType":"invoice",
       "receivedDate":"2020-02-17",
       "enrichment":{
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
> In this example, in the response, one of the extracted fields is the receiver contact Linda Owens. This information is enriched with the employee ID enrichment data created in step 2. The prediction suggests the employee ID from Linda Owens (E0001) with 100% probability. The employee ID from Lin Owens (E0002) is not even considered by the machine leaning model.

This is an example of a full prediction including the enrichment data part:

    ```JSON
    {
      "status": "DONE",
      "id": "97a96f7c-ea6a-456c-a58c-1cee1121a210",
      "fileName": "sample-invoice-2.pdf",
      "documentType": "invoice",
      "created": "2020-06-02T12:39:42.622045+00:00",
      "finished": "2020-06-02T12:39:58.396881+00:00",
      "country": "XX",
      "extraction": {
        "headerFields": [
          {
            "name": "documentNumber",
            "category": "document",
            "value": "9001321",
            "type": "string",
            "confidence": 0.654112005233765,
            "page": 1,
            "coordinates": {
              "x": 0.864058087938685,
              "y": 0.26824401368301,
              "w": 0.0572811617587736,
              "h": 0.00826681870011403
            }
          },
          {
            "name": "senderAddress",
            "category": "sender",
            "value": "Cupertino,CA 95014",
            "type": "string",
            "confidence": 0.596276225127054,
            "page": 1,
            "coordinates": {
              "x": 0.0677692617991126,
              "y": 0.0929304446978335,
              "w": 0.139169019766035,
              "h": 0.0105473204104903
            }
          },
          {
            "name": "receiverName",
            "category": "receiver",
            "value": "Linda Owens",
            "type": "string",
            "confidence": 0.573929733802111,
            "page": 1,
            "coordinates": {
              "x": 0.141589350544574,
              "y": 0.26824401368301,
              "w": 0.0947962888261396,
              "h": 0.00826681870011403
            }
          },
          {
            "name": "receiverAddress",
            "category": "receiver",
            "value": "5584 Nickel Road KINTA, Oklahoma 74552",
            "type": "string",
            "confidence": 0.585673252540807,
            "page": 1,
            "coordinates": {
              "x": 0.141185962081484,
              "y": 0.286488027366021,
              "w": 0.171036708350141,
              "h": 0.0279361459521095
            }
          },
          {
            "name": "senderName",
            "category": "sender",
            "value": "Apple Store One Infinite Loop",
            "type": "string",
            "confidence": 0.604747431454333,
            "page": 1,
            "coordinates": {
              "x": 0.0669624848729326,
              "y": 0.0547320410490308,
              "w": 0.127470754336426,
              "h": 0.0305017103762828
            }
          },
          {
            "name": "documentDate",
            "category": "document",
            "value": "2020-02-20",
            "type": "date",
            "confidence": 0.651767419833763,
            "page": 1,
            "coordinates": {
              "x": 0.863654699475595,
              "y": 0.286488027366021,
              "w": 0.0685760387252924,
              "h": 0.00826681870011403
            }
          },
          {
            "name": "grossAmount",
            "category": "amounts",
            "value": 1998,
            "type": "number",
            "confidence": 0.597997682059512,
            "page": 1,
            "coordinates": {
              "x": 0.794271883824123,
              "y": 0.447263397947548,
              "w": 0.0681726502622025,
              "h": 0.0102622576966933
            }
          },
          {
            "name": "currencyCode",
            "category": "amounts",
            "value": "USD",
            "type": "string",
            "confidence": 0.99053966999054,
            "page": 1,
            "coordinates": {
              "x": 0,
              "y": 0,
              "w": 0,
              "h": 0
            }
          },
          {
            "name": "netAmount",
            "category": "amounts",
            "value": 1998,
            "type": "number",
            "confidence": 0.227668133046892,
            "page": 1,
            "coordinates": {
              "x": 0.794271883824123,
              "y": 0.447263397947548,
              "w": 0.0677692617991126,
              "h": 0.00997719498289629
            }
          },
          {
            "name": "receiverContact",
            "category": "receiver",
            "value": "Linda Owens",
            "type": "string",
            "confidence": 0,
            "page": 1,
            "coordinates": {
              "x": 0.141589350544574,
              "y": 0.26824401368301,
              "w": 0.0943929003630496,
              "h": 0.00798175598631699
            }
          }
        ],
        "lineItems": [
          [
            {
              "name": "description",
              "category": "details",
              "value": "Phone 11 Pro 256GB Gold",
              "type": "string",
              "confidence": 0.617809644720771,
              "page": 1,
              "coordinates": {
                "x": 0.0758370310609117,
                "y": 0.376852907639681,
                "w": 0.18233158531666,
                "h": 0.00826681870011403
              }
            },
            {
              "name": "quantity",
              "category": "details",
              "value": 1,
              "type": "number",
              "confidence": 0.620666921138763,
              "page": 1,
              "coordinates": {
                "x": 0.630496167809601,
                "y": 0.376852907639681,
                "w": 0.0116982654296087,
                "h": 0.00826681870011403
              }
            },
            {
              "name": "netAmount",
              "category": "amounts",
              "value": 1149,
              "type": "number",
              "confidence": 0.633171701803803,
              "page": 1,
              "coordinates": {
                "x": 0.795482049213392,
                "y": 0.376282782212087,
                "w": 0.0681726502622025,
                "h": 0.0102622576966933
              }
            },
            {
              "name": "unitPrice",
              "category": "details",
              "value": 1149,
              "type": "number",
              "confidence": 0.625023552600075,
              "page": 1,
              "coordinates": {
                "x": 0.710367083501412,
                "y": 0.376282782212087,
                "w": 0.0681726502622025,
                "h": 0.0102622576966933
              }
            }
          ],
          [
            {
              "name": "description",
              "category": "details",
              "value": "Apple Watch Edition GPS + Cellular, 44mm Space Black Titanium Case with Anchor Gray Sport Loop",
              "type": "string",
              "confidence": 0.609802418569329,
              "page": 1,
              "coordinates": {
                "x": 0.0746268656716418,
                "y": 0.403078677309008,
                "w": 0.524001613553852,
                "h": 0.0287913340935005
              }
            },
            {
              "name": "quantity",
              "category": "details",
              "value": 1,
              "type": "number",
              "confidence": 0.60388410753674,
              "page": 1,
              "coordinates": {
                "x": 0.630496167809601,
                "y": 0.403078677309008,
                "w": 0.0116982654296087,
                "h": 0.00826681870011403
              }
            },
            {
              "name": "netAmount",
              "category": "amounts",
              "value": 849,
              "type": "number",
              "confidence": 0.627841416459817,
              "page": 1,
              "coordinates": {
                "x": 0.795482049213392,
                "y": 0.402508551881414,
                "w": 0.0552642194433239,
                "h": 0.00997719498289623
              }
            },
            {
              "name": "unitPrice",
              "category": "details",
              "value": 849,
              "type": "number",
              "confidence": 0.615530447236129,
              "page": 1,
              "coordinates": {
                "x": 0.710367083501412,
                "y": 0.402508551881414,
                "w": 0.0552642194433239,
                "h": 0.00997719498289623
              }
            }
          ]
        ]
      },
      "enrichment": {
        "employee": [
          {
            "id": "E0001",
            "confidence": 1
          }
        ]
      }
    }
    ```

You have now successfully used the employee entity to get enrichment data predictions for the document you uploaded to Document Information Extraction.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](See all enrichment data entries)]

To see a list of the enrichment data entries you have created:

1. Expand the **GET /data** endpoint.

2. Click **Try it out**.

3. Choose the enrichment data **`type`** `employee` and enter your **`clientId`**.

4. Click **Execute**.

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
             "id":"E0001"
          }
       ]
    }
    ```

4. Choose the enrichment data **`type`** `employee` and enter your **`clientId`**.

5. Click **Execute**.

![DOX](1delete_data_request.png)

You should receive a response like the following:

![DOX](1delete_data_response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Delete client)]

If you want to delete a client you created in Step 1, use the **DELETE /clients** endpoint.

1. Expand the **DELETE /clients** endpoint.

2. Click **Try it out**.

3. Enter in the **payload** field the client id or multiple client ids (`c_27`, for example) you want to delete.

4. Click **Execute**.

![DOX](1delete_clients_request.png)

You should receive a response like the following:

![DOX](1delete_clients_response.png)

Congratulations, you have completed this tutorial.

[DONE]
[ACCORDION-END]
