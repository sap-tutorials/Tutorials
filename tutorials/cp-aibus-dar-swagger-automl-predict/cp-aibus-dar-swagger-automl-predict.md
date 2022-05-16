---
title: Use the AutoML Model Template to Classify Records
description: Classify product data records into product categories using your machine learning model.
auto_validation: true
time: 20
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>data-attribute-recommendation, tutorial>free-tier]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---


## Details
### You will learn
  - How to classify records using your machine learning model

---

[ACCORDION-BEGIN [Step 1: ](Authorize Swagger UI)]

In the service key you created for Data Attribute Recommendation in the previous tutorial: [Use Free Tier to Set Up Account for Data Attribute Recommendation and Get Service Key](cp-aibus-dar-booster-free-key) or [Use Trial to Set Up Account for Data Attribute Recommendation and Get Service Key](cp-aibus-dar-booster-key), you find a section called `swagger` (as highlighted in the image below) with three entries, called `dm` (data manager), `inference` and `mm` (model manager).

!![Service Key](png-files/service-key-details.png)

For this tutorial, copy the URL of the Swagger UI for `inference` and open it in a browser tab. The Swagger UI for inference allows you to classify new data using your machine learning model that you have created in the previous tutorial: [Use the AutoML Model Template to Train a Machine Learning Model](cp-aibus-dar-swagger-automl-model).

1. To be able to use the Swagger UI endpoints, you need to authorize yourself. In the top right corner, click **Authorize**.

    !![Authorize](png-files/swagger-authorize.png)

2. Get the `access_token` value created in the previous tutorial: [Get OAuth Access Token for Data Attribute Recommendation Using Any Web Browser](cp-aibus-dar-web-oauth-token), then add **Bearer** (with capitalized "B") in front of it, and enter in the **Value** field.

    ```
    Bearer <access_token>
    ```

3. Click **Authorize** and then click **Close**.

    !![Authorize](png-files/swagger-token.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Classify records)]

To classify records, proceed as follows:

1. Expand the endpoint `POST /models/{modelName}/versions/1` by clicking on it. Then click **Try it out**.

    !![Inference Endpoint](png-files/inference-endpoint.png)

2. Fill the parameter `modelName` with the name of your machine learning model.

3. In the parameter `body`, you have to provide the data that needs to be classified. According to the dataset schema that you have created in [Use the AutoML Model Template to Upload Data to Data Attribute Recommendation with Swagger UI](cp-aibus-dar-swagger-automl-upload), the machine learning model takes the manufacturer, description and price of the product as input and predicts the product category. Replace the text in the parameter `body` with the following:

    ```JSON
    {
       "topN":1,
       "objects":[
          {
             "objectId":"optional-identifier-1",
             "features":[
                {"name":"manufacturer", "value":"Energizer"},
                {"name":"description", "value":"Alkaline batteries; 1.5V"},
                {"name":"price", "value":"5.99"}
             ]
          },
          {
             "objectId":"optional-identifier-2",
             "features":[
                {"name":"manufacturer","value":"Eidos"},
                {
                   "name":"description",
                   "value":"Unravel a grim conspiracy at the brink of Revolution"
                },
                {"name":"price","value":"19.99"}
             ]
          }
       ]
    }
    ```

4. Click **Execute** to send the above input for classification to the service.

    !![Inference Execute](png-files/inference-execute.png)

In the response of the service, you find the values that the model predicted. The probability represents how certain the model is about its prediction. The higher the probability the more confident the model is that the prediction is actually correct. If the probability is close to 1, the model is very certain.

!![Inference Execute](png-files/inference-response.png)

```JSON
{
  "id": "8caba3f2-f2af-4bb6-7f4c-079b9f87caee",
  "predictions": [
    {
      "labels": [
        {
          "name": "F3_readable",
          "results": [
            {
              "probability": 1,
              "value": "Household Batteries"
            }
          ]
        }
      ],
      "objectId": "optional-identifier-1"
    },
    {
      "labels": [
        {
          "name": "F3_readable",
          "results": [
            {
              "probability": 1,
              "value": "Sheet Music"
            }
          ]
        }
      ],
      "objectId": "optional-identifier-2"
    }
  ],
  "processedTime": "2022-05-16T09:51:36.321470",
  "status": "DONE"
}
```

You have successfully classified products into product categories. Feel free to adapt the examples above and retry the classification.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Undeploy your model)]

Now that you have learned the whole process about how to use the Regression Model Template from the Data Attribute Recommendation service, it's time to clean up. This way, the technical limits won't get in your way when trying out other Data Attribute Recommendation tutorials. See [Technical Constraints](https://help.sap.com/docs/Data_Attribute_Recommendation/105bcfd88921418e8c29b24a7a402ec3/686d2ae094014c8085cebecdb1d37e37.html) and [Free Service Plan and Trial Account Technical Constraints](https://help.sap.com/docs/Data_Attribute_Recommendation/105bcfd88921418e8c29b24a7a402ec3/c03b561eea1744c9b9892b416037b99a.html).

First, undeploy your model. For that, go back to the Swagger UI for `mm` and:

 1. Expand the endpoint `DELETE /deployments/{deploymentId}` by clicking on it. Then click **Try it out**.

     !![Inference Endpoint](png-files/deploy-endpoint.png)

 2. Fill the parameter `deploymentId` with the ID of your deployment. Use the `GET /deployments` endpoint in case you no longer have the deployment `id` in hand.

     !![Inference Endpoint](png-files/deploy-execute.png)

If the response code is `204`, the model has been successfully undeployed.

!![Inference Endpoint](png-files/deploy-response.png)

You have successfully undeployed your model, but the model is not yet deleted. Instead it isn't in production which means that you cannot make inference requests. You can deploy it again at any time using the `POST /deployments` endpoint.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Delete your model)]

Once undeployed, you can delete your model.

1. Expand the endpoint `DELETE /models/{modelName}` by clicking on it. Then click **Try it out**.

    !![Inference Endpoint](png-files/model-endpoint.png)

2. Fill the parameter `modelName` with the name of your machine learning model (`ior_tutorial_model`). Use the `GET /models` endpoint in case you no longer have the model `name` in hand.

    !![Inference Endpoint](png-files/model-execute.png)

If the response code is `204`, the model has been successfully deleted.

!![Inference Endpoint](png-files/model-response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Delete your training job)]

Now that the model is deleted, you can delete the training job that created the model.

1. Expand the endpoint `DELETE /jobs/{jobId}` by clicking on it. Then click **Try it out**.

    !![Inference Endpoint](png-files/job-endpoint.png)

2. Fill the parameter `jobId` with the ID of your training job. Use the `GET /jobs` endpoint in case you no longer have the job `id` in hand.

    !![Inference Endpoint](png-files/job-execute.png)

If the response code is `204`, the training job has been successfully deleted.

!![Inference Endpoint](png-files/job-response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Delete your dataset)]

To clear the uploaded data, you can now delete the dataset as the associated training job is already deleted. For that, go back to the Swagger UI for `dm` and:

 1. Expand the endpoint `DELETE /datasets/{id}` by clicking on it. Then click **Try it out**.

     !![Inference Endpoint](png-files/data-endpoint.png)

 2. Fill the parameter `id` with the ID of your dataset. Use the `GET /datasets` endpoint in case you no longer have the dataset `id` in hand.

     !![Inference Endpoint](png-files/data-execute.png)

If the response code is `204`, the dataset has been successfully deleted.

!![Inference Endpoint](png-files/data-response.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Delete your dataset schema)]

If you do not need your dataset schema anymore, you can delete it as well.

1. Expand the endpoint `DELETE /datasetSchemas/{id}` by clicking on it. Then click **Try it out**.

    !![Inference Endpoint](png-files/schema-endpoint.png)

2. Fill the parameter `id` with the ID of your dataset schema. Use the `GET /datasetSchemas` endpoint in case you no longer have the dataset schema `id` in hand.

    !![Inference Endpoint](png-files/schema-execute.png)

If the response code is `204`, the dataset schema has been successfully deleted.

!![Inference Endpoint](png-files/schema-response.png)

Congratulations, you have completed this tutorial.

[DONE]
[ACCORDION-END]
