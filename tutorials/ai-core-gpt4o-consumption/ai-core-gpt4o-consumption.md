---
parser: v2
auto_validation: true
time: 45
primary_tag: software-product>sap-business-technology-platform
tags: [ tutorial>beginner, topic>artificial-intelligence, topic>machine-learning, software-product>sap-business-technology-platform ]
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

# Using Multimodal inputs with GPT4o for Image Recognition on SAP AI Core
<!-- description --> In this tutorial we are going to learn on how to consume GPT4o LLM on AI core deployed on SAP AI core.

## You will learn
- How to inference GPT4o with multimodal inputs on AI core

## Prerequisites
Ai core setup and basic knowledge: [Link to documentation](https://developers.sap.com/tutorials/ai-core-setup.html)
Ai core Instance with Standard Plan or Extended Plan


### Scene Detection

[OPTION BEGIN [curl]]

The following example shows how you can consume this generative AI model using curl. For more information about prompts, see the tutorial [Prompt LLMs in the Generative AI Hub in SAP AI Core & Launchpad Information published on SAP site](https://help.sap.com/docs/link-disclaimer?site=https%3A%2F%2Fdevelopers.sap.com%2Ftutorials%2Fai-core-generative-ai.html).

Before you use this model, please ensure that the deployment has already been created. You can create the deployment either through [generative-ai-hub-sdk or AI Launchpad](https://developers.sap.com/tutorials/ai-core-generative-ai.html#ad7ffc1e-e94e-4de4-b70f-116b038aff04).

For inferencing the model through curl,

- open Windows PowerShell (for Windows based devices)

NOTE: **do not use DOS Prompt instead of PowerShell**

- open Terminal (for macOS based devices)

Enter the following command after replacing `<deployment_url>`, `<resource-group>`, `<token>` with the values for the corresponding model.

NOTE: 
 - for windows devices, **replace "curl" with "curl.exe"**
 - Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```powershell
curl -L '<deployment_url>/chat/completions?api-version=2023-05-15' \
--header 'AI-Resource-Group: <resource-group>' \
--header 'Content-Type: application/json' \
--header 'Authorization: Bearer <token>' \
--data '{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "describe the scene"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/scene detection.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}'
```
[OPTION END]

[OPTION BEGIN [Postman]]

To begin using the APIs in AI Core, we start with setting up the authentication methods.

![image](img/consumption1.png)
![image](img/consumption2.png)
For ease of access, we set up the region, baseUrl and deploymentUrl variables as a pre-requisite. This avoids the need of passing these values repeatedly for different scenarios. 
NOTE: the deployment URL is specific to the model we intend to use.

![image](img/consumption34.png)
Add the name of your respective resource group. 

![image](img/consumption5.png)
Next, to begin making API calls, we’ll create a new access token. Now we’re ready to use the API for various models.

![image](img/consumption6.png)

Now that we’re done with the pre-requisites, we’ll proceed to using the API for GPT4o for Scene Detection.

**NOTE:** Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```Body
{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "describe the scene."
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/scene detection.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}
```

Add the above data in the body of the POST call, then hit ‘Send’ as follows - 

![image](img/scene%20detection.png)

We get the following output accurately describing the scene in the image:

![image](img/scene%20detection%20output.png)
[OPTION END]

For more information on the models refer to [Hello GPT-4o](https://openai.com/index/hello-gpt-4o/)

### Object Detection

[OPTION BEGIN [curl]]

The following example shows how you can consume this generative AI model using curl. For more information about prompts, see the tutorial [Prompt LLMs in the Generative AI Hub in SAP AI Core & Launchpad Information published on SAP site](https://help.sap.com/docs/link-disclaimer?site=https%3A%2F%2Fdevelopers.sap.com%2Ftutorials%2Fai-core-generative-ai.html).

Before you use this model, please ensure that the deployment has already been created. You can create the deployment either through [generative-ai-hub-sdk or AI Launchpad](https://developers.sap.com/tutorials/ai-core-generative-ai.html#ad7ffc1e-e94e-4de4-b70f-116b038aff04).

For inferencing the model through curl,

- open Windows PowerShell (for Windows based devices)

NOTE: **do not use DOS Prompt instead of PowerShell**

- open Terminal (for macOS based devices)

Enter the following command after replacing `<deployment_url>`, `<resource-group>`, `<token>` with the values for the corresponding model.

NOTE: 
 - for windows devices, **replace "curl" with "curl.exe"**
 - Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```powershell
curl -L '<deployment-url>/chat/completions?api-version=2023-05-15' \
--header 'AI-Resource-Group: <resource-group>' \
--header 'Content-Type: application/json' \
--header 'Authorization: Bearer <token>' \
--data '{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "give me the bottle color and its count"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/object detection.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}'
```
[OPTION END]

[OPTION BEGIN [Postman]]

To begin using the APIs in AI Core, we start with setting up the authentication methods.

![image](img/consumption1.png)
![image](img/consumption2.png)
For ease of access, we set up the region, baseUrl and deploymentUrl variables as a pre-requisite. This avoids the need of passing these values repeatedly for different scenarios. 
NOTE: the deployment URL is specific to the model we intend to use.

![image](img/consumption34.png)
Add the name of your respective resource group. 

![image](img/consumption5.png)
Next, to begin making API calls, we’ll create a new access token. Now we’re ready to use the API for various models.

![image](img/consumption6.png)

Now that we’re done with the pre-requisites, we’ll proceed to using the API for GPT4o for Object Detection.

**NOTE:** Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```Body
{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "give me the bottle color and its count"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/object detection.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}
```

Add the above data in the body of the POST call, then hit ‘Send’ as follows - 

![image](img/scene%20detection.png)

We get the following output accurately describing the scene in the image:

![image](img/object%20detection%20output.png)
[OPTION END]

For more information on the models refer to [Hello GPT-4o](https://openai.com/index/hello-gpt-4o/)

### Graph Analysis

[OPTION BEGIN [curl]]

The following example shows how you can consume this generative AI model using curl. For more information about prompts, see the tutorial [Prompt LLMs in the Generative AI Hub in SAP AI Core & Launchpad Information published on SAP site](https://help.sap.com/docs/link-disclaimer?site=https%3A%2F%2Fdevelopers.sap.com%2Ftutorials%2Fai-core-generative-ai.html).

Before you use this model, please ensure that the deployment has already been created. You can create the deployment either through [generative-ai-hub-sdk or AI Launchpad](https://developers.sap.com/tutorials/ai-core-generative-ai.html#ad7ffc1e-e94e-4de4-b70f-116b038aff04).

For inferencing the model through curl,

- open Windows PowerShell (for Windows based devices)

NOTE: **do not use DOS Prompt instead of PowerShell**

- open Terminal (for macOS based devices)

Enter the following command after replacing `<deployment_url>`, `<resource-group>`, `<token>` with the values for the corresponding model.

NOTE: 
 - for windows devices, **replace "curl" with "curl.exe"**
 - Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```powershell
curl -L '<deployment-url>/chat/completions?api-version=2023-05-15' \
--header 'AI-Resource-Group: <resource-group>' \
--header 'Content-Type: application/json' \
--header 'Authorization: Bearer <token>' \
--data '{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "what is this graph about"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/graph.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}'
```
[OPTION END]

[OPTION BEGIN [Postman]]

To begin using the APIs in AI Core, we start with setting up the authentication methods.

![image](img/consumption1.png)
![image](img/consumption2.png)
For ease of access, we set up the region, baseUrl and deploymentUrl variables as a pre-requisite. This avoids the need of passing these values repeatedly for different scenarios. 
NOTE: the deployment URL is specific to the model we intend to use.

![image](img/consumption34.png)
Add the name of your respective resource group. 

![image](img/consumption5.png)
Next, to begin making API calls, we’ll create a new access token. Now we’re ready to use the API for various models.

![image](img/consumption6.png)

Now that we’re done with the pre-requisites, we’ll proceed to using the API for GPT4o for Graph Analysis.

**NOTE:** Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```Body
{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "what is this graph about"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/graph.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}
```

Add the above data in the body of the POST call, then hit ‘Send’ as follows - 

![image](img/scene%20detection.png)

We get the following output accurately describing the scene in the image:

![image](img/graph%20analysis%20output.png)
[OPTION END]

For more information on the models refer to [Hello GPT-4o](https://openai.com/index/hello-gpt-4o/)

### Math

[OPTION BEGIN [curl]]

The following example shows how you can consume this generative AI model using curl. For more information about prompts, see the tutorial [Prompt LLMs in the Generative AI Hub in SAP AI Core & Launchpad Information published on SAP site](https://help.sap.com/docs/link-disclaimer?site=https%3A%2F%2Fdevelopers.sap.com%2Ftutorials%2Fai-core-generative-ai.html).

Before you use this model, please ensure that the deployment has already been created. You can create the deployment either through [generative-ai-hub-sdk or AI Launchpad](https://developers.sap.com/tutorials/ai-core-generative-ai.html#ad7ffc1e-e94e-4de4-b70f-116b038aff04).

For inferencing the model through curl,

- open Windows PowerShell (for Windows based devices)

NOTE: **do not use DOS Prompt instead of PowerShell**

- open Terminal (for macOS based devices)

Enter the following command after replacing `<deployment_url>`, `<resource-group>`, `<token>` with the values for the corresponding model.

NOTE: 
 - for windows devices, **replace "curl" with "curl.exe"**
 - Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```powershell
curl -L '<deployment-url>/chat/completions?api-version=2023-05-15' \
--header 'AI-Resource-Group: <resource-group>' \
--header 'Content-Type: application/json' \
--header 'Authorization: Bearer <token>' \
--data '{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "find x"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/math.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}'
```
[OPTION END]

[OPTION BEGIN [Postman]]

To begin using the APIs in AI Core, we start with setting up the authentication methods.

![image](img/consumption1.png)
![image](img/consumption2.png)
For ease of access, we set up the region, baseUrl and deploymentUrl variables as a pre-requisite. This avoids the need of passing these values repeatedly for different scenarios. 
NOTE: the deployment URL is specific to the model we intend to use.

![image](img/consumption34.png)
Add the name of your respective resource group. 

![image](img/consumption5.png)
Next, to begin making API calls, we’ll create a new access token. Now we’re ready to use the API for various models.

![image](img/consumption6.png)

Now that we’re done with the pre-requisites, we’ll proceed to using the API for GPT4o for Math.

**NOTE:** Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```Body
{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "find x"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/math.jpg"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}
```

Add the above data in the body of the POST call, then hit ‘Send’ as follows - 

![image](img/scene%20detection.png)

We get the following output accurately describing the scene in the image:

![image](img/math%20output.png)
[OPTION END]

For more information on the models refer to [Hello GPT-4o](https://openai.com/index/hello-gpt-4o/)

### Image to Text

[OPTION BEGIN [curl]]

The following example shows how you can consume this generative AI model using curl. For more information about prompts, see the tutorial [Prompt LLMs in the Generative AI Hub in SAP AI Core & Launchpad Information published on SAP site](https://help.sap.com/docs/link-disclaimer?site=https%3A%2F%2Fdevelopers.sap.com%2Ftutorials%2Fai-core-generative-ai.html).

Before you use this model, please ensure that the deployment has already been created. You can create the deployment either through [generative-ai-hub-sdk or AI Launchpad](https://developers.sap.com/tutorials/ai-core-generative-ai.html#ad7ffc1e-e94e-4de4-b70f-116b038aff04).

For inferencing the model through curl,

- open Windows PowerShell (for Windows based devices)

NOTE: **do not use DOS Prompt instead of PowerShell**

- open Terminal (for macOS based devices)

Enter the following command after replacing `<deployment_url>`, `<resource-group>`, `<token>` with the values for the corresponding model.

NOTE: 
 - for windows devices, **replace "curl" with "curl.exe"**
 - Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```powershell
curl -L '<deployment-url>/chat/completions?api-version=2023-05-15' \
--header 'AI-Resource-Group: <resource-group>' \
--header 'Content-Type: application/json' \
--header 'Authorization: Bearer <token>' \
--data '{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "extract text"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/Sample-handwritten-text-input-for-OCR-1.png"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}'
```
[OPTION END]

[OPTION BEGIN [Postman]]

To begin using the APIs in AI Core, we start with setting up the authentication methods.

![image](img/consumption1.png)
![image](img/consumption2.png)
For ease of access, we set up the region, baseUrl and deploymentUrl variables as a pre-requisite. This avoids the need of passing these values repeatedly for different scenarios. 
NOTE: the deployment URL is specific to the model we intend to use.

![image](img/consumption34.png)
Add the name of your respective resource group. 

![image](img/consumption5.png)
Next, to begin making API calls, we’ll create a new access token. Now we’re ready to use the API for various models.

![image](img/consumption6.png)

Now that we’re done with the pre-requisites, we’ll proceed to using the API for GPT4o for text extraction.

**NOTE:** Update the “url” to the link of the image resource you want to query the model upon and give the corresponding query in the “text” parameter.

```Body
{
    "messages": [
      {
        "role": "user",
        "content": [
           {
              "type": "text",
              "text": "extract text"
           },
           {
              "type": "image_url",
              "image_url": {
                 "url": "https://raw.githubusercontent.com/Cibi-SAP/ai-core/main/Sample-handwritten-text-input-for-OCR-1.png"
              }
          }
        ]
      }
    ],
    "max_tokens": 4096
}
```

Add the above data in the body of the POST call, then hit ‘Send’ as follows - 

![image](img/scene%20detection.png)

We get the following output accurately describing the scene in the image:

![image](img/imagetotext%20output.png)
[OPTION END]

For more information on the models refer to [Hello GPT-4o](https://openai.com/index/hello-gpt-4o/)
