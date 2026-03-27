---
parser: v2
auto_validation: true
time: 45
primary_tag: software-product>sap-ai-core
tags: [ tutorial>beginner, topic>artificial-intelligence, topic>machine-learning, software-product>sap-ai-core ]
author_name: Smita Naik
author_profile: https://github.com/I321506
---

# Leveraging Orchestration(V2) Capabilities to Enhance Responses
<!-- description -->  In this tutorial, we will explore optional orchestration service V2 capabilities available in the Gen AI Hub, such as Data Masking, translation and Content Filtering.

## You will learn
- Inference of GenAI models using orchestration service V2 along with Data Masking, translation and Content Filtering features

## Prerequisites
1. **BTP Account**  
   Set up your SAP Business Technology Platform (BTP) account.  
   [Create a BTP Account](https://developers.sap.com/group.btp-setup.html)
2. **For SAP Developers or Employees**  
   Internal SAP stakeholders should refer to the following documentation: [How to create BTP Account For Internal SAP Employee](https://me.sap.com/notes/3493139), [SAP AI Core Internal Documentation](https://help.sap.com/docs/sap-ai-core)
3. **For External Developers, Customers, or Partners**  
   Follow this tutorial to set up your environment and entitlements: [External Developer Setup Tutorial](https://developers.sap.com/tutorials/btp-cockpit-entitlements.html), [SAP AI Core External Documentation](https://help.sap.com/docs/sap-ai-core?version=CLOUD)
4. **Create BTP Instance and Service Key for SAP AI Core**  
   Follow the steps to create an instance and generate a service key for SAP AI Core and ensure to use service plan **extended**:  
   [Create Service Key and Instance](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/create-service-key?version=CLOUD)
5. **AI Core Setup Guide**  
   Step-by-step guide to set up and get started with SAP AI Core:  
   [AI Core Setup Tutorial](https://developers.sap.com/tutorials/ai-core-genaihub-provisioning.html)
6. An **Extended** SAP AI Core service plan is required, as the Generative AI Hub is not available in the Free or Standard tiers. For more details, refer to 
[SAP AI Core Service Plans](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/service-plans?version=CLOUD)
8. **AI Launchpad Setup Guide**
 Step-by-step guide to set up AI Launchpad:
 [AI Launchpad Tutorial](https://developers.sap.com/tutorials/ai-launchpad-provisioning.html)
9. **Orchestration Deployment**:   
    Refer to the tutorial [the basic consumption of GenAI models using orchestration](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html) and ensure at least one orchestration deployment is ready to be consumed during this process. 
10. Basic Knowledge:
    Familiarity with the orchestration workflow is recommended


### Pre-Read

This tutorial builds on the foundational orchestration concepts introduced in the [beginner's tutorial](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html) and focuses on enhancing GenAI responses using orchestration service V2 modules such as **data masking**, **translation** and **content filtering**.

Previously in the [beginner's tutorials](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html), we used a **resume processing use case** to illustrate how to create orchestration workflow to consume models using harmonized API. In this tutorial, we use a sentiment analysis use case to demonstrate how optional orchestration service V2 modules such as **Data Masking**, **Translation**, and **Content Filtering** can be applied to protect sensitive information, translate multilingual support requests, and filter out undesirable or non-compliant content—thereby enhancing the quality, safety, and compliance of generative AI outputs.

**Data masking** in SAP AI Core allows you to anonymize or pseudonymize personal or confidential data before sending it to the generative AI model.  
🔗 [Learn more about Data Masking in SAP AI Core](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/data-masking?version=CLOUD)

**Translation** in SAP GenAI Orchestration enables automatic language conversion of inputs and outputs during LLM processing.
🔗 [Learn more about Data Masking in SAP AI Core](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/input-translation?version=CLOUD)

**Content filtering** helps identify and block inappropriate, offensive, or non-compliant input and output content within an orchestration workflow.  
🔗 [Learn more about Content Filtering in SAP AI Core](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/input-filtering?version=CLOUD)

In this tutorial, we specifically focus on **data masking**, **translation** and **content filtering**. Other orchestration modules such as **grounding** are also available in SAP AI Core and it is covered in Separate tutorials.

You will learn how to:

- Integrate data masking within the orchestration flow to safeguard personal or confidential information.
- Apply content filtering to identify and restrict inappropriate or non-compliant responses.
- Use relevant SAP AI Core features and configurations to support these capabilities.

By the end of this tutorial

  * you'll understand how to design a secure and controlled orchestration pipeline suitable for **enterprise-grade GenAI applications**.
  
  * Learn how to implement the solution using **SAP AI Launchpad**, **Python SDK**, **Java**, **JavaScript**, and **Bruno**.

### Accessing Orchestration Capabilities

**In this tutorial**, we will build upon the orchestration framework introduced in [Tutorial](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html). The focus will shift from basic orchestration to leveraging optional modules to enhance data privacy and refine response quality. These enhancements include: 

    **Data Masking** : Hiding sensitive information like phone numbers, organizational details, or personal identifiers. 

    **Content Filtering** : Screening for categories such as hate speech, self-harm, explicit content, and violence to ensure safe and relevant responses.

    **Translation** : Automatically converts input and/or output text between source and target languages to support multilingual processing.

- Here, we use a **sentiment analysis** use case, where orchestration is enhanced by incorporating data masking, translation or content filtering. These additions help improve data privacy, security, and response quality.

[OPTION BEGIN [AI Launchpad]] 

**Access the Generative AI Hub:** 
- Navigate to the resource group where your orchestration has been deployed. 

- Go to Generative AI Hub. 

- Select Orchestration and click on create. 

![img](img/image_ail_orch.png)

![img](img/image_ail_orch1.png)


[OPTION END]

[OPTION BEGIN [Python SDK]]

**NOTE** : If you are continuing with the same notebook from the previous tutorial, skip steps 1 and 2. Otherwise, create a new notebook using the already deployed orchestration URL to access the Harmonized API. 

- The [support-request.txt](img/support-request.txt) file, containing the support request content, must be added to the working directory. Use the following code to load the file content:


```python
from gen_ai_hub.orchestration_v2.utils import load_text_file  
# Load the support request file content 
support_request_path = "support-request.txt"  # Specify the correct path to the file 
support_request = load_text_file(support_request_path) 
# Print the content to verify it has been loaded 
print(support_request)
```

[OPTION END]

[OPTION BEGIN [JavaScript SDK]]

**NOTE** : If you are continuing with the same project from the previous tutorial, skip steps 1 and 2. Otherwise, create a new project using the already deployed orchestration URL to access the Harmonized API.

For more information, refer to the official [documentation](https://sap.github.io/ai-sdk/docs/js/orchestration/chat-completion) of the [`@sap-ai-sdk/orchestration`](https://github.com/SAP/ai-sdk-js/tree/main/packages/orchestration) package.

- The [support-request.txt](img/support-request.txt) file, containing the support request content, must be added to the working directory. Use the following code to load the file content:


```javascript
const txtContent = await Deno.readTextFile('./support-request.txt');
console.log(txtContent);
```

[OPTION END]

[OPTION BEGIN [Bruno]]

**Bruno Setup** : If you have already completed the environment setup, configuration, and deployment as described in the [Tutorial](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html), you can directly proceed to the Data Masking Configuration. If you're new to this, please follow the steps in the [Tutorial](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html) to set up your environment, configure, and deploy the orchestration before proceeding with the modules.  

[OPTION END]


### Template Configuration  

The templating module is a mandatory step in orchestration. It allows you to define dynamic inputs using placeholders, construct structured prompts, and generate a final query that will be passed to the model configuration module.

In this step, we create a template that defines how the sentiment analysis prompt will be structured using message components:

• `system`: Defines assistant behavior and task. 

• `user`: Provides the support request input.

[OPTION BEGIN [AI Launchpad]]

- In the **Templating** section, locate the **message** icon with three tabs: **User, Assistance, and System**. 

- Click on the **User** tab. Enter the following details:  

```PROMPT
Please analyze the sentiment of the following support request: {{ ?support_text }}
```
**Variable Definitions**: 

- The variable **“support_text ”** will be created. 

- Enter default values based on your use case. For this sentiment analysis example, use the following support text: 

```TEXT
"Subject: Bestellung #1234567890 Verspätet - John Johnson Nachricht: Halle, ich schreibe ihnen um mich nach dem Status meiner Bestellung mit der Bestellnr. +1234567890 zu erkundigen. Die Lieferung war eigentlich für gestern geplant, ist bisher jedoch nicht erfolgt. Mein Name ist John Johnson und meine Lieferadresse lautet 125 Cole Meadows Drive Palo Alto, California 94301. Bitte lassen Sie mich per Telefon unter der Nummer +1 505802 2172 wissen, wann ich mit meiner Lieferung rechnen kann. Danke!"
```

![img](img/image_ail_019.png)

- After entering the details, click on **Add**. 

- A new message box will appear. Proceed to configure the **System** tab. 

- In the **System** tab, enter the following details: 

```PROMPT
You are a customer support assistant. Analyze the sentiment of the user request provided and return whether the sentiment is positive, neutral, or negative. Also provide a one-line justification for your classification.
```
![img](img/image005.png)  

[OPTION END]

[OPTION BEGIN [Python SDK]]

Use the following code to create the template: 

```python
from gen_ai_hub.orchestration_v2.models.message import SystemMessage, UserMessage
from gen_ai_hub.orchestration_v2.models.template import Template

# Define the sentiment analysis template
template = Template(
    template=[
        SystemMessage(content="""You are a customer support assistant. Analyze the sentiment of the user request provided and return whether the sentiment is positive, neutral, or negative. Also provide a one-line justification for your classification."""
        ),
        UserMessage(content="Please analyze the sentiment of the following support request: {{ ?support_text }}"
        ),
    ],
    defaults=
        {"support_text":"User is unhappy with the latest update and facing usability issues."}
)
```
- Select the models to be used for this orchestration: 

```python
from gen_ai_hub.orchestration_v2.models.llm_model_details import LLMModelDetails

llm = LLMModelDetails(name="gpt-5-nano", parameters={"max_completion_tokens": 1028})
```
[OPTION END]

[OPTION BEGIN [JavaScript SDK ]]

```javascript
import { OrchestrationClient } from '@sap-ai-sdk/orchestration';

const orchestrationClient = new OrchestrationClient({
  promptTemplating: {
    model: {
      name: 'gpt-4o',
      params: {
        max_completion_tokens: 200,
        temperature: 0
      }
    },
    prompt: {
      template: [
        {
          role: 'system',
          content:
            'You are a customer support assistant. Analyze the sentiment of the user request provided and return whether the sentiment is positive, neutral, or negative. Also provide a one-line justification.'
        },
        {
          role: 'user',
          content:
            'Please analyze the sentiment of the following support request: {{ ?support_text }}'
        }
      ]
    }
  }
});

const response = await orchestrationClient.chatCompletion({
  placeholderValues: {
    support_text: 'User is unhappy with the latest update and facing usability issues.'
  }
});
```

Orchestration provides direct access to models without requiring separate deployments, you can use any available models. 

[OPTION END]

### Setting Up Data Masking Parameters 

The **Data Masking** Module ensures data privacy by anonymizing or pseudonymizing sensitive information before it is processed. 

    **Anonymization** : Irreversibly replaces personal identifiers with placeholders (e.g., MASKED_ENTITY). 

    **Pseudonymization** : Substitutes identifiers with reversible tokens (e.g., MASKED_ENTITY_ID).

[OPTION BEGIN [AI Launchpad]]

- Navigate to the **Data Masking** section (see the screenshot below). 

 In this tutorial, we have chosen 'anonymize' for enhanced privacy. Depending on your requirements, you can opt for either approach. 

- Check the boxes for the following fields that you want to mask: 
    - Email Address 
    - Organization Name 
    - Person's Name 
    - Person's Phone Number 
    - Username & Password 

- Ensure all 5 boxes are checked (refer to the screenshot for reference)

![img](img/image009.png)

[OPTION END]

[OPTION BEGIN [Python SDK]]

For this tutorial, we use anonymization: 

```python
from gen_ai_hub.orchestration_v2.models.data_masking import MaskingModuleConfig, MaskingProviderConfig, MaskingMethod, DPIStandardEntity, ProfileEntity

data_masking_config = MaskingModuleConfig(
    masking_providers=[MaskingProviderConfig(
        method=MaskingMethod.ANONYMIZATION,
        entities=[
            DPIStandardEntity(type=ProfileEntity.ADDRESS),
            DPIStandardEntity(type=ProfileEntity.EMAIL),
            DPIStandardEntity(type=ProfileEntity.PHONE),
            DPIStandardEntity(type=ProfileEntity.PERSON),
        ]
    )],

)
```

**NOTE:** We are anonymizing name, phone number, address (location), and email to protect user privacy in the support text.  

[OPTION END]

[OPTION BEGIN [JavaScript SDK]]

For this tutorial, we use anonymization: 

```javascript
import { buildDpiMaskingProvider } from '@sap-ai-sdk/orchestration';

const maskingProvider = buildDpiMaskingProvider({
  method: 'anonymization',
  entities: [
    'profile-person',
    'profile-email',
    'profile-phone',
    {
      type: 'custom',
      // Example: customer / ticket reference IDs
      regex: '\\b(TICKET|CASE)-[0-9]{4,}\\b',
      replacement_strategy: {
        method: 'constant',
        value: 'MASKED_REFERENCE_ID'
      }
    }
  ],
  allowlist: ['SAP'] // Optional
});
```

**NOTE** : Here, we apply data masking to customer support messages in German, masking sensitive user data like name, phone, and email. 

[OPTION END]

[OPTION BEGIN [Bruno]]

- Before proceeding with the data masking configuration, ensure the following:
    - You have completed the Bruno collection and setup as per the [Tutorial](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html).
    - The deployment for the orchestration is already done and configured correctly.

**Note**: If you have already completed these setup steps, you can proceed directly to the data masking configuration. If not, please follow the steps in the [Tutorial](https://developers.sap.com/tutorials/ai-core-orchestration-consumption.html) to complete the environment setup and deployment.

For this tutorial, we use anonymization:

- Navigate to the **'orchestration'** section.

- In the list of requests, select the **completion** option to open the request designed for consuming the deployed model.

- Expand the Body section of the request. Replace the current JSON in the Body with the following updated JSON, which includes the data masking configuration

```JSON
"masking": {
        "masking_providers": [
          {
            "type": "sap_data_privacy_integration",
            "method": "anonymization",
            "entities": [
              { "type": "profile-email" },
              { "type": "profile-person" },
              { "type": "profile-phone" },
              { "type": "profile-org" },
              { "type": "profile-location" }
            ]
          }
        ]
      }
```

- After replacing the JSON, click Send to execute the request.

- Upon sending the request, the response will return the masked result, where sensitive information like email, phone numbers, and other personal identifiers are anonymized. For reference, you can check the screenshot provided showing how the masked result will appear.

![img](img/data_masking.png)

**NOTE:** This will mask sensitive fields from support queries — even if written in non-English languages like German

[OPTION END]

### Translation

The Translation Module enables multilingual processing by translating content sent to and received from the generative AI model. This is especially useful when the user input or model output is not in the default language expected by the LLM.

 - The module uses SAP’s Document Translation service.

 - The target language is mandatory.

 - If source language is not specified, it will be automatically detected.

[OPTION BEGIN [AI Launchpad]]

Navigate to the Translation section in the orchestration editor.

Specify the source and target languages for both:

 - Input Translation: before sending data to the model.

 - Output Translation: after receiving the model's response.

For example:

 - Input Translation: German ➝ English

 - Output Translation: English ➝ German

Refer to the screenshots below for guidance:

![img](img/image004.png)

![img](img/image025.png)

[OPTION END]

[OPTION BEGIN [Python SDK]]

```Python
from gen_ai_hub.orchestration_v2.models.translation import TranslationModuleConfig, SAPDocumentTranslation, TranslationConfig

translation_config = TranslationModuleConfig(
    input=SAPDocumentTranslation(
        config=TranslationConfig(
            source_language="de-DE",
            target_language="en-US"
        )
    ),
    output=SAPDocumentTranslation(
        config=TranslationConfig(
            source_language="en-US",
            target_language="de-DE"
        )
    )
)
```

[OPTION END]

[OPTION BEGIN [JavaScript SDK]]

Use the buildTranslationConfig helper to configure translation.

``` javascript
import { buildTranslationConfig } from '@sap-ai-sdk/orchestration';

const inputTranslation = buildTranslationConfig('input', {
  sourceLanguage: 'de-DE',
  targetLanguage: 'en-US'
});

const outputTranslation = buildTranslationConfig('output', {
  sourceLanguage: 'en-US',
  targetLanguage: 'de-DE'
});

const translationConfig = {
  input: inputTranslation,
  output: outputTranslation
};

console.log('✅ Translation configuration defined successfully');
```

[OPTION END]

[OPTION BEGIN [Bruno]]

To test translation in Bruno:

1. Open the request in the **05_orchestration** collection.

2. Add both input and output translation configurations under module_configurations.


``` JSON
"translation": {
          "input": {
              "type": "sap_document_translation",
              "config": {
                  "source_language": "de-DE",
                  "target_language": "en-US"
              }
          },
          "output": {
              "type": "sap_document_translation",
              "config": {
                  "source_language": "en-US",
                  "target_language": "de-DE"
            }
          }
  }
```
  3. Click Send.

  4. The response will show the model output in the target language, with the input also translated before being passed to the LLM.

![img](img/translation.png)

[OPTION END]

### Defining Content Filtering Rules

The **Content Filtering** Module allows screening of both input and output content to remove inappropriate or unwanted elements such as hate speech or violent content. This ensures that sentiment analysis is performed on safe and relevant inputs, and the responses generated are also safe for consumption.

[OPTION BEGIN [AI Launchpad]]

Navigate to the **Input Filtering** section. 

- Adjust the filtering levels for sentiment analysis inputs, based on your requirements:

    - Hate 

    - Self-Harm 

    - Sexual Content 

    - Violence 

- This step is optional but helps sanitize user-generated content (e.g., tweets, reviews, comments) before performing sentiment analysis. 

![img](img/image013.png)

![img](img/image026.png)

Navigate to the Model Configuration section and:

- Select your Deployment ID

- Choose an LLM appropriate for text classification tasks (e.g., GPT-4 or Claude) 

**NOTE** : Ensure that your orchestration deployment is in Running Status and ready to be consumed during this process.  


![img](img/image015.png)

![img](img/image024.png)

- Click on the **Output Filtering** section. 

- Adjust filtering levels for content safety criteria, similar to the **Input Filtering** configuration: 
 
      - Hate 

      - Self-Harm 

      - Sexual Content 

      - Violence 

- This step is also optional. 

![img](img/image019.png) 

[OPTION END]

[OPTION BEGIN [Python SDK]]

```python
from gen_ai_hub.orchestration_v2.models.azure_content_filter import AzureContentFilter, AzureThreshold
from gen_ai_hub.orchestration_v2.models.llama_guard_3_filter import LlamaGuard38bFilter
from gen_ai_hub.orchestration_v2.models.content_filtering import FilteringModuleConfig, InputFiltering, OutputFiltering
from gen_ai_hub.orchestration_v2.models.content_filter import ContentFilter, ContentFilterProvider

content_filter_config = FilteringModuleConfig(
    input=InputFiltering(filters=[
        ContentFilter(type=ContentFilterProvider.AZURE, config=AzureContentFilter(hate=AzureThreshold.ALLOW_SAFE,
                                                                                  violence=AzureThreshold.ALLOW_SAFE,
                                                                                  self_harm=AzureThreshold.ALLOW_SAFE,
                                                                                  sexual=AzureThreshold.ALLOW_SAFE)),
        ContentFilter(type=ContentFilterProvider.LLAMA_GUARD_3_8B, config=LlamaGuard38bFilter(hate=True))
        ]),
    output=OutputFiltering(filters=[
        ContentFilter(type=ContentFilterProvider.AZURE, config=AzureContentFilter(hate=AzureThreshold.ALLOW_SAFE,
                                                                                  violence=AzureThreshold.ALLOW_SAFE,
                                                                                  self_harm=AzureThreshold.ALLOW_SAFE,
                                                                                  sexual=AzureThreshold.ALLOW_SAFE)),
        ContentFilter(type=ContentFilterProvider.LLAMA_GUARD_3_8B, config=LlamaGuard38bFilter(hate=True))
    ])

)
```

**NOTE** : Adjust thresholds for hate, sexual, self-harm, and violence categories based on your use case.  

 
- Then Combine the template, models, and modules into orchestration configurations: 

```python
from gen_ai_hub.orchestration_v2.models.template import Template, PromptTemplatingModuleConfig
from gen_ai_hub.orchestration_v2.models.config import ModuleConfig, OrchestrationConfig

prompt_template = PromptTemplatingModuleConfig(prompt=template,
                                               model=llm)


module_config = ModuleConfig(prompt_templating=prompt_template, filtering=content_filter_config, masking=data_masking_config,
                            translation= translation_config)

config = OrchestrationConfig(modules=module_config)
```
**NOTE** : Ensure that your orchestration deployment is in Running Status and ready to be consumed during this process. 

[OPTION END]

[OPTION BEGIN [JavaScript SDK]]

```javascript
import { buildAzureContentSafetyFilter } from '@sap-ai-sdk/orchestration';

// Input filter: protects what users send (support tickets)
const inputFilter = buildAzureContentSafetyFilter('input', {
  hate: 'ALLOW_SAFE_LOW',
  self_harm: 'ALLOW_SAFE_LOW',
  sexual: 'ALLOW_SAFE_LOW',
  violence: 'ALLOW_SAFE_LOW',
  prompt_shield: true 
});

// Output filter: protects what the model returns
const outputFilter = buildAzureContentSafetyFilter('output', {
  hate: 'ALLOW_SAFE',
  self_harm: 'ALLOW_SAFE',
  sexual: 'ALLOW_SAFE',
  violence: 'ALLOW_SAFE'
});
```

**NOTE** : Adjust thresholds for hate, sexual, self-harm, and violence categories based on your use case.

- Then Combine the template, models, and modules into orchestration configurations:

```javascript
import { OrchestrationClient } from '@sap-ai-sdk/orchestration';

const orchestrationClient = new OrchestrationClient({
  resourceGroup: 'grounding',

  // Sentiment analysis prompt
  promptTemplating: {
    model: {
      name: 'gpt-4o',
      params: {
        max_completion_tokens: 200,
        temperature: 0
      }
    },
    prompt: {
      template: [
        {
          role: 'system',
          content:
            'You are a customer support assistant. Analyze the sentiment of the user request provided and return whether the sentiment is positive, neutral, or negative. Also provide a one-line justification.'
        },
        {
          role: 'user',
          content:
            'Please analyze the sentiment of the following support request: {{ ?support_text }}'
        }
      ]
    }
  },

  translation: translationConfig,

  masking: {
    masking_providers: [maskingProvider]
  },

  filtering: {
    input: {
      filters: [inputFilter]
    },
    output: {
      filters: [outputFilter]
    }
  }
});
```

Multiple content filters can be applied for both input and output. In this tutorial, we use Azure Content Safety Filter, but you can choose from the available providers based on your use case. For more information, refer to the official [documentation](https://sap.github.io/ai-sdk/docs/js/orchestration/chat-completion) of the [`@sap-ai-sdk/orchestration`](https://github.com/SAP/ai-sdk-js/tree/main/packages/orchestration) package.

The `filtering` configuration created in this step will be used in the next step to initialize an `OrchestrationClient` and consume the orchestration service.

[OPTION END]

[OPTION BEGIN [Bruno]] 

Update your JSON body for the **05_orchestration** section:

```JSON
"filtering": {
        "input": {
          "filters": [
            {
              "type": "azure_content_safety",
              "config": {
                "hate": 4,
                "self_harm": 4,
                "sexual": 4,
                "violence": 4
              }
            }
          ]
        },
        "output": {
          "filters": [
            {
              "type": "azure_content_safety",
              "config": {
                "hate": 0,
                "self_harm": 0,
                "sexual": 0,
                "violence": 0
              }
            }
          ]
        }
      }
```
**NOTE** : Adjust thresholds for hate, sexual, self-harm, and violence categories based on your use case.
![img](img/image028.png)

[OPTION END]

### Executing the Orchestration Workflow

This step runs the orchestration pipeline for each selected LLM model using the provided input text for sentiment analysis. It captures and stores the model-generated responses, enabling comparison of output quality across different models.

[OPTION BEGIN [AI Launchpad]]

- After configuring the filtering and model settings, click on the Test icon and run the orchestration. 

- Check the Result section for the response. 

![img](img/image023.png)

- You can save the orchestration created for future use like shown in below image

![img](img/image_ail_sav.png)
  
[OPTION END]

[OPTION BEGIN [Python SDK]]

Finally, execute the orchestration and collect the results: 

```python
from gen_ai_hub.orchestration_v2.service import OrchestrationService

orchestration_service = OrchestrationService()

# Run orchestration with the provided input (for example, candidate resume content)
result = orchestration_service.run(config=config, placeholder_values={"support_text" : support_request})  

# Extract the response content
response = result.final_result.choices[0].message.content
print(response)
```

- response will be generated, containing outputs from the defined llm model.

![img](img/image_py_resp.png)

[OPTION END]

[OPTION BEGIN [JavaScript SDK]]

```javascript
try {
  const response = await orchestrationClient.chatCompletion({
    placeholderValues: {
      support_text: txtContent
    }
  });

  console.log(response.getContent());

} catch (error: any) {
  console.error('❌ Error during support sentiment analysis');
  console.error(error.message);
  console.error(error.cause?.response?.data);
}
```

- response will be generated, containing outputs from the defined llm model.  

**Note**: Ensure that your orchestration deployment is in Running Status and ready to be consumed during this process.

![img](img/image_js_resp.png)

![img](img/image_js_v2.png)

[OPTION END]

[OPTION BEGIN [Bruno]]

- Click Send to execute the request with the updated configuration. Validate the returned response. It should contain:

    -  Masked Results: Sensitive phrases will be anonymized.

    -  Translation: Input and output translation for sentiment analysis.

    -  Filtered Content: Unsafe or biased sentiment analysis output will be flagged

By following these steps, you can successfully mask sensitive data and apply content filtering while consuming the deployed model.

```JSON
{ 
"config": {
    "modules": {
      "prompt_templating": {
        "prompt": {
          "template": [
            {
              "role": "assistant",
              "content": "Support Issue: '''{{?support-issue}}'''\n Context Information: '''{{?issue-context}}'''"
            },
            {
              "role": "system",
              "content": "You are a helpful support assistant. Your task is to help answer a given support issue. \n Your proceed as follows: \n First, check if the provided context information answers the issue. Based on the result do one of the following: \n a) If yes, provide an answer based on the provided context information in form of an email and then finish. \n b) If no, only if you cannot answer the issue you summarize the issue for the human support team. Ignore the context information in this case and provide your answer only based on the support issue. Answer in the following format:\n - Sentiment: [your sentiment analysis] \n - Key Theme: [theme of the support issue] \n - Contact: [any contact information available in the issue]"
            }
          ]
        },
        "model": {
          "name": "gpt-4o",
          "params": {
            "max_completion_tokens": 300,
            "temperature": 0.1,
            "frequency_penalty": 0,
            "presence_penalty": 0
          }
        }
      },
      "filtering": {
        "input": {
          "filters": [
            {
              "type": "azure_content_safety",
              "config": {
                "hate": 4,
                "self_harm": 4,
                "sexual": 4,
                "violence": 4
              }
            }
          ]
        },
        "output": {
          "filters": [
            {
              "type": "azure_content_safety",
              "config": {
                "hate": 0,
                "self_harm": 0,
                "sexual": 0,
                "violence": 0
              }
            }
          ]
        }
      },
      "masking": {
        "providers": [
          {
            "type": "sap_data_privacy_integration",
            "method": "anonymization",
            "entities": [
              {
                "type": "profile-email"
              },
              {
                "type": "profile-person"
              },
              {
                "type": "profile-phone"
              },
              {
                "type": "profile-address"
              }
            ]
          }
        ]
      },
      "grounding": {
        "type": "document_grounding_service",
        "config": {
          "filters": [
            {
              "id": "helpRepo",
              "data_repositories": [
                "*"
              ],
              "search_config": {
                "max_chunk_count": 3
              },
              "data_repository_type": "help.sap.com"
            }
          ],
          "placeholders": {
            "input": [
              "support-issue"
            ],
            "output": "issue-context"
          }
        }
      },
      "translation": {
          "input": {
              "type": "sap_document_translation",
              "config": {
                  "source_language": "de-DE",
                  "target_language": "en-US"
              }
          },
          "output": {
              "type": "sap_document_translation",
              "config": {
                  "source_language": "en-US",
                  "target_language": "de-DE"
              }
          }
      }
    }
  },
  "placeholder_values": {
    "support-issue": "Betreff: Unterstützung benötigt \nNachricht: \nHallo, ich benötige Unterstützung mit SAP Signavio. Insbesondere möchte ich Benachrichtigungen im SAP Signavio Process Manager konfigurieren. Bitte kontaktieren Sie mich mit unter Jane.Janeson@gmx.net."
  }
}
```

![img](img/image027.png) 
 
[OPTION END]

### Conclusion :  
Once the orchestration completes, you can observe that the output is now more refined, with sensitive information masked and inappropriate content filtered. This demonstrates the power of modules like data masking and content filtering to enhance privacy and ensure response quality.  

While this tutorial used a sentiment analysis use case, the same principles can be applied to other use cases. You can customize the Data Masking and Content Filtering settings based on your specific requirements to handle sensitive or categorized data effectively.  

By incorporating these optional modules, you can tailor your Response to meet organizational data security policies and ensure safe, reliable responses for diverse scenarios.
