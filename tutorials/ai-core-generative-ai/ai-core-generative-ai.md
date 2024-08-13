---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-ai-core, topic>machine-learning]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul 
author_profile: https://github.com/dhrubpaul
---


# Prompt LLMs in the generative AI hub in SAP AI Core & Launchpad
<!-- description --> Understand the principles of prompt engineering and learn to create effective prompts for AI models like ChatGPT. Prompt engineering is crucial for obtaining desired outputs from AI models. It helps in guiding the model's response in a particular direction.

## Prerequisites

- You have an SAP AI Core service instance and service key. 
- You're using the Extended service plan.
- You have completed the client authorization for your preferred user interface. 

For more information, see [SAP AI Core documentation](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/initial-setup).

## You will learn
- How to deploy an LLM model.
- How to use the models to execute different queries.


### Provisioning SAP AI Core

#### Provision SAP AI Core in your global account

Open the SAP BTP cockpit and access your global account.

<!-- border -->![BTP Cockpit](images/btpcockpit.png)

Check the entitlements for your account by clicking `Entitlements` and searching for SAP AI Core.

<!-- border -->![Check Entitlements](images/checkentitlements.png)

Click `Configure Entitlements` > `Add Service Plans`.

<!-- border -->![Set SAP AI Core as an entitlement](images/configureentitlements.png)
<!-- border -->![Set SAP AI Core as an entitlement](images/addserviceplan.png)

Select SAP AI Core and the `extended` service plan.

<!-- border -->![Set SAP AI Core as an entitlement](images/aicoreentitlement.png)

Save your new entitlement.

<!-- border -->![Save](images/saveentitlement.png)

#### Run the booster for SAP AI Core

Choose `Boosters` from the navigation pane. Find and choose the booster for `SAP AI Core` from the selection. 

<!-- border -->![Locate the SAP AI Core booster](images/boostercore.png)

The booster tile contains information about SAP AI Core.  Click `Start` when you are ready. 

When you start a booster, a wizard opens up which guides you through the required steps.

<!-- border -->![Start the booster](images/coreboosterstart.png)
#### View your instances and create your keys

In the subaccount section of SAP BTP Cockpit, choose `Services` from the left navigation menu and `Instances and subscriptions` from the page. 

<!-- border -->![View instances and subscriptions](images/instancesandsubscriptions.png)

To see the details of your new instance, click the chevron on the entry.

To create the keys that you need to access your instance, click the three dots > `Create Service Key`.

<!-- border -->![Create keys](images/createkeys1.png)

Enter a `Key Name` of your choice and click `Create`.

<!-- border -->![Create keys](images/createkeys2.png)
<!-- border -->![View new keys](images/keys.png)

Once your keys have been created, you can view or download them at any time by locating the key and clicking the three dots and choosing from the available options.

<!-- border -->![Locate the keys in SAP AI Cockpit](images/viewkeys.png)

#### Provision SAP AI Launchpad in your global account

> **Note:** SAP AI Launchpad is optional, but is the recommended interface for use with SAP AI Core.

Configure your entitlement as before, but select `SAP AI Launchpad`.

#### Run the booster for SAP AI Launchpad

Choose `Boosters` from the navigation pane, and then choose the booster for `SAP AI Launchpad` from the selection. 

<!-- border -->![Locate the booster for SAP AI Launchpad](images/boosterailp.png)

Click `Start` when you are ready.

<!-- border -->![Start the booster](images/lpboosterstart.png)

### Checking for foundation-models scenario

[OPTION BEGIN [SAP AI Launchpad]]

Go to **ML Operations** -> **Scenarios**. Check whether the **foundation-models** scenario is present in your AI-Core workspace.


![image](images/ail05.png)

[OPTION END]

[OPTION BEGIN [POSTMAN]]

To begin using the APIs in AI Core, we start with setting up the authentication methods.

![image](images/consumption1.png)
![image](images/consumption2.png)
For ease of access, we set up the region, baseUrl and deploymentUrl variables as a pre-requisite. This avoids the need of passing these values repeatedly for different scenarios. 
NOTE: the deployment URL is specific to the model we intend to use.

![image](images/consumption34.png)
Add the name of your respective resource group. 

![image](images/consumption5.png)
Next, to begin making API calls, we’ll create a new access token. Now we’re ready to use the API for various models.

![image](images/consumption6.png)

Next, we'll check whether the **foundation-models** scenario is present in your workspace. Send a GET request to 

```
{{baseUrl}}/lm/scenarios
```

Always make sure that AI-Resource-Group header is set to your resource group id.

![image](images/pm01.png)

[OPTION END]

### Creating a configuration

[OPTION BEGIN [SAP AI Launchpad]]

Go to **ML Operations** -> **Configurations**. Click on the **Create** button

![image](images/ail04.png)


1. Give a name for the configuration.
1. Select the **foundation-models** scenario. 
1. Choose the version.
1. Choose the Executable ID.


![image](images/ail03.png)

In input parameters, give name and version of the model you want to use.

![image](images/ail02.png)

These are the models available as of now:

![image](images/ail01.png)

Note that the model version can be given as *latest*. If the model version is not specified, it will be set to *latest* by default.

Click on **Next** -> **Review** -> **Create** to create the configuration.

Model versions have deprecation dates. Where a model version is specified, the deployment will stop working on the deprecation date of that model version.

Implement one of the following model upgrade options:

- **Auto Upgrade:** Create or patch an LLM configuration with model version latest. When a new model version is supported by SAP AI Core, existing deployments will automatically use the latest version of the given model.

- **Manual Upgrade:** Patch an LLM configuration with your chosen replacement model version. This model version will be used in your deployments irrespective of updates to the models supported by SAP AI Core.

[OPTION END]

[OPTION BEGIN [POSTMAN]]

Create a new configuration by sending a POST request to 

```
{{baseUrl}}/lm/configurations
```

Include the following in the body of the request.

```JSON
{

    "name": "yourNameChoice",
    "executableId": "azure-openai",
    "scenarioId": "foundation-models",
    "versionId":"0.0.1",
    "parameterBindings": [
        {
            "key": "modelName",
            "value": "gpt-35-turbo"
        },
        {
            "key": "modelVersion",
            "value": "0613"
        }
    ],
    "inputArtifactBindings": []
}
```

- ```name``` is your free choice of identifier.

- ```scenarioId``` must be foundation-models.

- ```versionId``` is the version of the foundation-models scenario.

![image](images/pm02.png)

Take note of the configuration id generated in the response.

Note that the model version can be set to *latest*, to use the latest version of the model. If the model version is not specified, it will be set to *latest* by default.

Model versions have deprecation dates. Where a model version is specified, the deployment will stop working on the deprecation date of that model version.

Implement one of the following model upgrade options:

- **Auto Upgrade:** Create or patch an LLM configuration with model version latest. When a new model version is supported by SAP AI Core, existing deployments will automatically use the latest version of the given model.

- **Manual Upgrade:** Patch an LLM configuration with your chosen replacement model version. This model version will be used in your deployments irrespective of updates to the models supported by SAP AI Core.


[OPTION END]

### Creating a deployment

[OPTION BEGIN [SAP AI Launchpad]]

You can make LLM available for use by creating a virtual LLM deployment. You can do so once for each model and model version.

Once you created the configuration, click on **Create Deployment**.

![image](images/ail06.png)

Set duration as standard and click on the **Review** button.

![image](images/ail07.png)

Once you create the deployment, wait for the current status to be set to *RUNNING*.

![image](images/ail08.png)

Once the deployment is running, you can access your models in the Generative AI Hub.

[OPTION END]

[OPTION BEGIN [POSTMAN]]

You can make LLM available for use by creating a virtual LLM deployment. You can do so once for each model and model version.

Create a deployment by sending a POST request to 

```
{{baseUrl}}/lm/deployments
```

Include the following JSON in the body of the request

```JSON
{
  "configurationId":  "<configuration-id>"
}
```

Replace the ```<configuration-id>``` value with the *id* you received in the previous step.

![image](images/pm03.png)

Once the deployment is created, check the status of deployment by sending a GET request to 

```
{{baseUrl}}/lm/deployments
```

![image](images/pm04.png)

Note down the ```deploymentUrl``` in the response, which you will be using for executing your queries.

[OPTION END]


### Prompt Management

[OPTION BEGIN [SAP AI Launchpad]]

In the Generative AI Hub, you can manage your prompts in the Prompt Management page. Go to **Generative AI Hub** -> **Prompt Management**. Here you can click on a prompt to go to its details page.

![image](images/p_manage06.png)

1. Here on the left hand pane, you can see the different versions of the prompt. Once you choose a new version, the details of that version will be displayed.

1. Click on **Open in Prompt Editor** to open the chosen prompt in the prompt editor.

1. You can delete the prompt by clicking the **Delete** button.

![image](images/p_manage05.png)

In the prompt management page, you can filter the prompts based on created date, collection, model used, etc. 

![image](images/p_manage04.png)

If the filter pane is not visible, click on the toggle button to display it.

![image](images/p_manage03.png)

You can add some prompts to favorite for quick access. Click on the star icon to add  prompts to favorites. Click on the **Favorites** tab to see them.

![image](images/p_manage02.png)

You can also select multiple prompts and delete them.

![image](images/p_manage01.png)

[OPTION END]

### Prompt Administration

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Administration**.

![image](images/adm02.png)

Here you can delete all the data of a particular user. Type the email id of the user and press *Enter*.

If the user data is found, the **Delete** button will become active. Click on the **Delete** button to delete all the prompts saved by that particular user.

![image](images/adm01.png)

[OPTION END]

### Querying the LLMs

[OPTION BEGIN [SAP AI Launchpad]]

Navigate to the Generative AI Hub in AI Launchpad. 

![image](images/p_edit08.png)

Here you can see the following options

**Prompt Editor:** In the prompt editor, you can query your prompts and save the responses.

**Prompt Management:** In the prompt management page, you can manage all your saved prompts.

**Prompt Administration:** In the prompt administration page, you can manage the prompts in the current workspace.

In the *Prompt Editor* page, give a name for your prompt. You can optionally give a collection name. Name and collection name will come handy in prompt management.

![image](images/p_edit07.png)

Scroll down and click on the arrow to select Model and parameters.

![image](images/p_edit06.png)

1. Click here to select the model of your choice.

2. You can set additional parameters here.

![image](images/p_edit05.png)

- ```Frequency Penalty``` Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.

- ```Presence Penalty``` Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.

- ```Max-Tokens``` The maximum number of tokens allowed for the generated answer.

- ```Temperature``` What sampling temperature to use, between 0 and 2. Higher values will make the output more random, while lower values will make it more focused and deterministic.

After selecting the model and setting the parameters, give your prompt in the prompt field, and click on the **Run** button to get your output. You can save the response by clicking on the **Save** button.

![image](images/p_edit04.png)

Click on the **Create New** button if you want to start a new prompt.

![image](images/p_edit03.png)

You can open a previously saved prompt by clicking on the **Select** button.

![image](images/p_edit02.png)

Choose the prompt and click **Select** to open the prompt.

![image](images/p_edit01.png)

[OPTION END]

[OPTION BEGIN [POSTMAN]]

To make a query, send a POST request to 

```
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

NOTE: For each API call, we need to generate a fresh Access Token (as demonstrated in Step 2), and update the deploymentUrl to the corresponding model in use. We also need to update our AI-Resource-Group (also demonstrated in Step 2). These steps need to completed prior to sending the POST request to avoid receiving erroneous response.

Include the following JSON in the body of the request

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "What's the difference between flammable and inflammable?"
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

The body of your request must include:

```messages:``` Your query.

You can also include optional parameters such as:

```max-tokens:``` A number that defines the maximum number of tokens allowed for the generated answer. The default value is 4,096.

```temperature:``` A number between 0 and 2. Higher values make the output more random, lower values make it more focused and deterministic.

```frequency_penalty:``` A number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.

```presence_penalty:``` A number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.

```stop:``` Can be a string or array, which if found, will stop generating further output.

![image](images/pm16.png)


[OPTION END]


[OPTION BEGIN [GENERATIVE AI HUB SDK]]

To install the generative-ai-hub-sdk package in your system, open your terminal or command prompt and run the following command.

```TEXT
pip3 install generative-ai-hub-sdk
```

Now you have generative-ai-hub-sdk installed in your system.


You have to configure proxy modules to use the large language models. 

We recommend setting these values as environment variables for AI core creds via config file. The default path for this file is ~/.aicore/config.json.

open Notepad and replace the values in below json with your AI core Service keys that you downloaded from BTP and press `Command + S` to save file. A pop up will appear on the screen where navigate to ~/.aicore/ and location and save the file as config.json

```JSON
{
  "AICORE_AUTH_URL": "https://* * * .authentication.sap.hana.ondemand.com",
  "AICORE_CLIENT_ID": "* * * ",
  "AICORE_CLIENT_SECRET": "* * * ",
  "AICORE_RESOURCE_GROUP": "* * * ",
  "AICORE_BASE_URL": "https://api.ai.* * *.cfapps.sap.hana.ondemand.com/v2"
}
```

You can use the gen_ai_hub for prompting large language models such as gpt-3.5, gpt-4 and falcon-40b.

Open a python programming environment of your choice and run the following code we deployed these models at step 3.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

llm = init_llm('gpt-4', temperature=0., max_tokens=256)
llm.invoke('What is generative AI?').content
```

[OPTION END]


### Text Summarization

This example tasks the LLM with condensing and summarizing a given text. The text, clearly demarcated with triple backticks, is expected to be distilled into a concise summary of no more than 30 words. The focus is on extracting the most salient points and presenting them in a succinct manner, ensuring that the essence of the original content is retained without excessive verbosity. This format is designed to challenge the LLM's capability to discern key details and convey them efficiently. For this demo we have taken 2 pages from SAP annual report 2 on Independent Assurance Practitioner's Report by KPMG.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
Your task is to generate a short summary of a text.
Summarize the text below, in at most 30 words. 

Review: Independent Assurance Practitioner's Report
To the Supervisory Board of SAP SE, Walldorf
We have performed a limited assurance engagement on the non-financial statement of SAP SE (further "Company" or "SAP") and on the non-financial statement of the parent company that is combined with it, which are published in the Management Report, (further "combined non-financial statement") for the period from January 1 to December 31, 2022.
Responsibilities of Management
Management of the company is responsible for the preparation of the combined non-financial statement in accordance with Sections 315c in conjunction with 289c to 289e HGB ["Handelsgesetzbuch": German Commercial Code] and Article 8 of REGULATION (EU) 2020/852 OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL of June 18, 2020 on establishing a framework to facilitate sustainable investment and amending Regulation (EU) 2019/2088 (hereinafter the "EU Taxonomy Regulation") and the Delegated Acts adopted thereunder, as well as for making their own interpretation of the wording and terms contained in the EU Taxonomy Regulation and the delegated acts adopted thereunder as set out in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement.
This responsibility includes the selection and application of appropriate non-financial reporting methods and making assumptions and estimates about individual non-financial disclosures of the group that are reasonable in the circumstances. Furthermore, management is responsible for such internal control as they consider necessary to enable the preparation of a combined non-financial statement that is free from material misstatement, whether due to fraud or error.
The EU Taxonomy Regulation and the Delegated Acts issued thereunder contain wording and terms that are still subject to considerable interpretation uncertainties and for which clarifications have not yet been published in every case. Therefore, management has disclosed their interpretation of the EU Taxonomy Regulation and the Delegated Acts adopted thereunder in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement. They are responsible for the defensibility of this interpretation. Due to the immanent risk that indeterminate legal terms may be interpreted differently, the legal conformity of the interpretation is subject to uncertainties.
Independence and Quality Assurance of the Assurance Practitioner's firm
We have complied with the independence and quality assurance requirements set out in the national legal provisions and professional pronouncements, in particular the Professional Code for German Public Auditors and Chartered Accountants (in Germany) and the quality assurance standard of the German Institute of Public Auditors (Institut der Wirtschaftsprufer, IDW) regarding quality assurance requirements in audit practice (IDW QS 1).
Responsibility of the Assurance Practitioner
Our responsibility is to express a conclusion with limited assurance on the combined non-financial statement based on our assurance engagement.
We conducted our assurance engagement in accordance with International Standard on Assurance Engagements (ISAE) 3000 (Revised): "Assurance Engagements other than Audits or Reviews of
41/335
  SAP Integrated Report 2022
 To Our Stakeholders
    Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
  Additional Information
  Historical Financial Information" issued by the IAASB. This standard requires that we plan and perform the assurance engagement to obtain limited assurance about whether any matters have come to our attention that cause us to believe that the company's non-financial statement, is not prepared, in all material respects, in accordance with Sections 315c in conjunction with 289c to 289e HGB and the EU Taxonomy Regulation and the Delegated Acts issued thereunder as well as the interpretation by management disclosed in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement. We do not, however, issue a separate conclusion for each disclosure.
In a limited assurance engagement, the procedures performed are less extensive than in a reasonable assurance engagement, and accordingly, a substantially lower level of assurance is obtained. The selection of the assurance procedures is subject to the professional judgment of the assurance practitioner.
In the course of our assurance engagement we have, among other things, performed the following assurance procedures and other activities:
– Interviewing employees responsible for the materiality analysis at group level in order to obtain an understanding on the approach for identifying key issues and related reporting limits of SAP,
– Carrying out a risk assessment, inclusive of media analysis, on relevant information on sustainability performance of SAP in the reporting period,
– Assessing the design and implementation of systems and processes for identifying, handling, and monitoring information on environmental, employee and social matters, human rights and combating corruption and bribery, including the consolidation of data,
– Interviewing staff on group level, who are responsible for the disclosures on concepts, due diligence processes, results and risks, the performance of internal control activities and the consolidation of the disclosures,
– Inspecting selected internal and external documents,
– Analytically assessing the data and trends of the quantitative information, which is reported on group level of all locations,
– Evaluating the local data collection, validation, and reporting processes as well as the reliability of the reported data by means of a sampling survey at two locations,
– Interviewing of responsible staff on group level to obtain an understanding of the approach to identify relevant economic activities in accordance with the EU taxonomy,
– Evaluating the design and implementation of systems and procedures for identifying, processing, and monitoring information on turnover, capital expenditures and operating expenditures for the taxonomy-relevant economic activities for the first two environmental objectives climate change mitigation and climate change adaptation,
– Evaluating the data collection, validation, and reporting processes, as well as the reliability of the reported data for the taxonomy-aligned economic activities in conjunction with the assessment of the technical evaluation criteria for the substantial contribution, the fulfilment of the DNSH-criteria and the documentation of the minimum safeguard,
– Assessment of the overall presentation of the disclosures.
In determining the disclosures in accordance with Article 8 of the EU Taxonomy Regulation, management is required to interpret undefined legal terms. Due to the immanent risk that undefined legal terms may be interpreted differently, the legal conformity of their interpretation and, accordingly, our assurance engagement thereon are subject to uncertainties.
Assurance Opinion
Based on the assurance procedures performed and the evidence obtained, nothing has come to our attention that causes us to believe that the combined non-financial statement of SAP SE, Walldorf for
42/335

 SAP Integrated Report 2022
To Our Stakeholders
Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
Additional Information
the period from January 1 to December 31, 2022 has not been prepared, in all material respects, in accordance with Sections 315c in conjunction with 289c to 289e HGB and the EU Taxonomy Regulation and the Delegated Acts issued thereunder as well as the interpretation by management as disclosed in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement.
Restriction of Use
This assurance report is solely addressed to SAP SE, Walldorf.
Our assignment for SAP SE, Walldorf and professional liability is governed by the General Engagement Terms for Wirtschaftsprufer (German Public Auditors) and Wirtschaftsprufungs- gesellschaften (German Public Audit Firms) (Allgemeine Auftragsbedingungen fur Wirtschaftsprufer und Wirtschaftsprufungsgesellschaften) in the version dated January 1, 2017 (https://www.kpmg.de/bescheinigungen/lib/aab_english.pdf). By reading and using the information contained in this assurance report, each recipient confirms having taken note of provisions of the General Engagement Terms (including the limitation of our liability for negligence to EUR 4 million as stipulated in No. 9) and accepts the validity of the attached General Engagement Terms with respect to us.
Mannheim, den 22. Februar 2023
KPMG AG Wirtschaftsprufungsgesellschaft
Beyer
Wirtschaftsprufer [German Public Auditor]
Wiegand Wirtschaftsprufer [German Public Auditor]
```

![image](images/ail12.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "Your task is to generate a short summary of a text.Summarize the text below, in at most 30 words. Review: Independent Assurance Practitioner's ReportTo the Supervisory Board of SAP SE, WalldorfWe have performed a limited assurance engagement on the on-financial statement of SAP SE (further \"Company\" or \"SAP\") and on the on-financial statement of the parent company that is combined with it, which re published in the Management Report, (further 'combined non-financial tatement') for the period from January 1 to December 31, 2022.Responsibilities of Management Management of the company is responsible for he preparation of the combined non-financial statement in accordance with ections 315c in conjunction with 289c to 289e HGB ['Handelsgesetzbuch': German Commercial Code] and Article 8 of REGULATION (EU) 2020/852 OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL of June 18, 2020 on establishing a framework to facilitate sustainable investment and amending Regulation (EU) 2019/2088 (hereinafter the \"EU Taxonomy Regulation\") and the Delegated Acts adopted thereunder, as well as for making their own interpretation of the wording and terms contained in the EU Taxonomy Regulation and the delegated acts adopted thereunder as set out in section 'Sustainable Finance: EU Taxonomy Disclosures' of the combined non-financial statement. This responsibility includes the selection and application of appropriate non-financial reporting methods and making assumptions and estimates about individual non-financial disclosures of the group that are reasonable in the circumstances. Furthermore, management is responsible for such internal control as they consider necessary to enable the preparation of a combined non-financial statement that is free from material misstatement, whether due to fraud or error. The EU Taxonomy Regulation and the Delegated Acts issued thereunder contain wording and terms that are still subject to considerable interpretation uncertainties and for which clarifications have not yet been published in every case. Therefore, management has disclosed their interpretation of the EU Taxonomy Regulation and the Delegated Acts adopted thereunder in section 'Sustainable Finance: EU Taxonomy Disclosures' of the combined non-financial statement. They are responsible for the defensibility of this interpretation. Due to the immanent risk that indeterminate legal terms may be interpreted differently, the legal conformity of the interpretation is subject to uncertainties. Independence and Quality Assurance of the Assurance Practitioner's firm We have complied with the independence and quality assurance requirements set out in the national legal provisions and professional pronouncements, in particular the Professional Code for German Public Auditors and Chartered Accountants (in Germany) and the quality assurance standard of the German Institute of Public Auditors (Institut der Wirtschaftsprufer, IDW) regarding quality assurance requirements in audit practice (IDW QS 1). Responsibility of the Assurance Practitioner Our responsibility is to express a conclusion with limited assurance on the combined non-financial statement based on our assurance engagement. We conducted our assurance engagement in accordance with International Standard on Assurance Engagements (ISAE) 3000 (Revised): \"Assurance Engagements other than Audits or Reviews of 41/335   SAP Integrated Report 2022  To Our Stakeholders     Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability   Additional Information   Historical Financial Information\" issued by the IAASB. This standard requires that we plan and perform the assurance engagement to obtain limited assurance about whether any matters have come to our attention that cause us to believe that the company's non-financial statement, is not prepared, in all material respects, in accordance with Sections 315c in conjunction with 289c to 289e HGB and the EU Taxonomy Regulation and the Delegated Acts issued thereunder as well as the interpretation by management disclosed in section 'Sustainable Finance: EU Taxonomy Disclosures' of the combined non-financial statement. We do not, however, issue a separate conclusion for each disclosure. In a limited assurance engagement, the procedures performed are less extensive than in a reasonable assurance engagement, and accordingly, a substantially lower level of assurance is obtained. The selection of the assurance procedures is subject to the professional judgment of the assurance practitioner. In the course of our assurance engagement we have, among other things, performed the following assurance procedures and other activities: – Interviewing employees responsible for the materiality analysis at group level in order to obtain an understanding on the approach for identifying key issues and related reporting limits of SAP, – Carrying out a risk assessment, inclusive of media analysis, on relevant information on sustainability performance of SAP in the reporting period, – Assessing the design and implementation of systems and processes for identifying, handling, and monitoring information on environmental, employee and social matters, human rights and combating corruption and bribery, including the consolidation of data, – Interviewing staff on group level, who are responsible for the disclosures on concepts, due diligence processes, results and risks, the performance of internal control activities and the consolidation of the disclosures, – Inspecting selected internal and external documents, – Analytically assessing the data and trends of the quantitative information, which is reported on group level of all locations, – Evaluating the local data collection, validation, and reporting processes as well as the reliability of the reported data by means of a sampling survey at two locations, – Interviewing of responsible staff on group level to obtain an understanding of the approach to identify relevant economic activities in accordance with the EU taxonomy, – Evaluating the design and implementation of systems and procedures for identifying, processing, and monitoring information on turnover, capital expenditures and operating expenditures for the taxonomy-relevant economic activities for the first two environmental objectives climate change mitigation and climate change adaptation, – Evaluating the data collection, validation, and reporting processes, as well as the reliability of the reported data for the taxonomy-aligned economic activities in conjunction with the assessment of the technical evaluation criteria for the substantial contribution, the fulfilment of the DNSH-criteria and the documentation of the minimum safeguard, – Assessment of the overall presentation of the disclosures. In determining the disclosures in accordance with Article 8 of the EU Taxonomy Regulation, management is required to interpret undefined legal terms. Due to the immanent risk that undefined legal terms may be interpreted differently, the legal conformity of their interpretation and, accordingly, our assurance engagement thereon are subject to uncertainties. Assurance Opinion Based on the assurance procedures performed and the evidence obtained, nothing has come to our attention that causes us to believe that the combined non-financial statement of SAP SE, Walldorf for 42/335  SAP Integrated Report 2022 To Our Stakeholders  Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability Additional Information the period from January 1 to December 31, 2022 has not been prepared, in all material respects, in accordance with Sections 315c in conjunction with 289c to 289e HGB and the EU Taxonomy Regulation and the Delegated Acts issued thereunder as well as the interpretation by management as disclosed in section 'Sustainable Finance: EU Taxonomy Disclosures' of the combined non-financial statement. Restriction of Use This assurance report is solely addressed to SAP SE, Walldorf. Our assignment for SAP SE, Walldorf and professional liability is governed by the General Engagement Terms for Wirtschaftsprufer (German Public Auditors) and Wirtschaftsprufungs- gesellschaften (German Public Audit Firms) (Allgemeine Auftragsbedingungen fur Wirtschaftsprufer und Wirtschaftsprufungsgesellschaften) in the version dated January 1, 2017 (https://www.kpmg.de/bescheinigungen/lib/aab_english.pdf). By reading and using the information contained in this assurance report, each recipient confirms having taken note of provisions of the General Engagement Terms (including the limitation of our liability for negligence to EUR 4 million as stipulated in No. 9) and accepts the validity of the attached General Engagement Terms with respect to us. Mannheim, den 22. Februar 2023 KPMG AG Wirtschaftsprufungsgesellschaft Beyer Wirtschaftsprufer [German Public Auditor] Wiegand Wirtschaftsprufer [German Public Auditor]"
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm05.png)

[OPTION END]


[OPTION BEGIN [GENERATIVE AI HUB SDK]]

open Jupter notebook or python IDE and run the following code to infrence the model using GPT-3.5-turbo .

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """Your task is to generate a short summary of a text.
Summarize the text below, in at most 30 words. 

Review: Independent Assurance Practitioner's Report
To the Supervisory Board of SAP SE, Walldorf
We have performed a limited assurance engagement on the non-financial statement of SAP SE (further "Company" or "SAP") and on the non-financial statement of the parent company that is combined with it, which are published in the Management Report, (further "combined non-financial statement") for the period from January 1 to December 31, 2022.
Responsibilities of Management
Management of the company is responsible for the preparation of the combined non-financial statement in accordance with Sections 315c in conjunction with 289c to 289e HGB ["Handelsgesetzbuch": German Commercial Code] and Article 8 of REGULATION (EU) 2020/852 OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL of June 18, 2020 on establishing a framework to facilitate sustainable investment and amending Regulation (EU) 2019/2088 (hereinafter the "EU Taxonomy Regulation") and the Delegated Acts adopted thereunder, as well as for making their own interpretation of the wording and terms contained in the EU Taxonomy Regulation and the delegated acts adopted thereunder as set out in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement.
This responsibility includes the selection and application of appropriate non-financial reporting methods and making assumptions and estimates about individual non-financial disclosures of the group that are reasonable in the circumstances. Furthermore, management is responsible for such internal control as they consider necessary to enable the preparation of a combined non-financial statement that is free from material misstatement, whether due to fraud or error.
The EU Taxonomy Regulation and the Delegated Acts issued thereunder contain wording and terms that are still subject to considerable interpretation uncertainties and for which clarifications have not yet been published in every case. Therefore, management has disclosed their interpretation of the EU Taxonomy Regulation and the Delegated Acts adopted thereunder in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement. They are responsible for the defensibility of this interpretation. Due to the immanent risk that indeterminate legal terms may be interpreted differently, the legal conformity of the interpretation is subject to uncertainties.
Independence and Quality Assurance of the Assurance Practitioner's firm
We have complied with the independence and quality assurance requirements set out in the national legal provisions and professional pronouncements, in particular the Professional Code for German Public Auditors and Chartered Accountants (in Germany) and the quality assurance standard of the German Institute of Public Auditors (Institut der Wirtschaftsprufer, IDW) regarding quality assurance requirements in audit practice (IDW QS 1).
Responsibility of the Assurance Practitioner
Our responsibility is to express a conclusion with limited assurance on the combined non-financial statement based on our assurance engagement.
We conducted our assurance engagement in accordance with International Standard on Assurance Engagements (ISAE) 3000 (Revised): "Assurance Engagements other than Audits or Reviews of
41/335
  SAP Integrated Report 2022
 To Our Stakeholders
    Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
  Additional Information
  Historical Financial Information" issued by the IAASB. This standard requires that we plan and perform the assurance engagement to obtain limited assurance about whether any matters have come to our attention that cause us to believe that the company's non-financial statement, is not prepared, in all material respects, in accordance with Sections 315c in conjunction with 289c to 289e HGB and the EU Taxonomy Regulation and the Delegated Acts issued thereunder as well as the interpretation by management disclosed in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement. We do not, however, issue a separate conclusion for each disclosure.
In a limited assurance engagement, the procedures performed are less extensive than in a reasonable assurance engagement, and accordingly, a substantially lower level of assurance is obtained. The selection of the assurance procedures is subject to the professional judgment of the assurance practitioner.
In the course of our assurance engagement we have, among other things, performed the following assurance procedures and other activities:
– Interviewing employees responsible for the materiality analysis at group level in order to obtain an understanding on the approach for identifying key issues and related reporting limits of SAP,
– Carrying out a risk assessment, inclusive of media analysis, on relevant information on sustainability performance of SAP in the reporting period,
– Assessing the design and implementation of systems and processes for identifying, handling, and monitoring information on environmental, employee and social matters, human rights and combating corruption and bribery, including the consolidation of data,
– Interviewing staff on group level, who are responsible for the disclosures on concepts, due diligence processes, results and risks, the performance of internal control activities and the consolidation of the disclosures,
– Inspecting selected internal and external documents,
– Analytically assessing the data and trends of the quantitative information, which is reported on group level of all locations,
– Evaluating the local data collection, validation, and reporting processes as well as the reliability of the reported data by means of a sampling survey at two locations,
– Interviewing of responsible staff on group level to obtain an understanding of the approach to identify relevant economic activities in accordance with the EU taxonomy,
– Evaluating the design and implementation of systems and procedures for identifying, processing, and monitoring information on turnover, capital expenditures and operating expenditures for the taxonomy-relevant economic activities for the first two environmental objectives climate change mitigation and climate change adaptation,
– Evaluating the data collection, validation, and reporting processes, as well as the reliability of the reported data for the taxonomy-aligned economic activities in conjunction with the assessment of the technical evaluation criteria for the substantial contribution, the fulfilment of the DNSH-criteria and the documentation of the minimum safeguard,
– Assessment of the overall presentation of the disclosures.
In determining the disclosures in accordance with Article 8 of the EU Taxonomy Regulation, management is required to interpret undefined legal terms. Due to the immanent risk that undefined legal terms may be interpreted differently, the legal conformity of their interpretation and, accordingly, our assurance engagement thereon are subject to uncertainties.
Assurance Opinion
Based on the assurance procedures performed and the evidence obtained, nothing has come to our attention that causes us to believe that the combined non-financial statement of SAP SE, Walldorf for
42/335

 SAP Integrated Report 2022
To Our Stakeholders
Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
Additional Information
the period from January 1 to December 31, 2022 has not been prepared, in all material respects, in accordance with Sections 315c in conjunction with 289c to 289e HGB and the EU Taxonomy Regulation and the Delegated Acts issued thereunder as well as the interpretation by management as disclosed in section "Sustainable Finance: EU Taxonomy Disclosures" of the combined non-financial statement.
Restriction of Use
This assurance report is solely addressed to SAP SE, Walldorf.
Our assignment for SAP SE, Walldorf and professional liability is governed by the General Engagement Terms for Wirtschaftsprufer (German Public Auditors) and Wirtschaftsprufungs- gesellschaften (German Public Audit Firms) (Allgemeine Auftragsbedingungen fur Wirtschaftsprufer und Wirtschaftsprufungsgesellschaften) in the version dated January 1, 2017 (https://www.kpmg.de/bescheinigungen/lib/aab_english.pdf). By reading and using the information contained in this assurance report, each recipient confirms having taken note of provisions of the General Engagement Terms (including the limitation of our liability for negligence to EUR 4 million as stipulated in No. 9) and accepts the validity of the attached General Engagement Terms with respect to us.
Mannheim, den 22. Februar 2023
KPMG AG Wirtschaftsprufungsgesellschaft
Beyer
Wirtschaftsprufer [German Public Auditor]
Wiegand Wirtschaftsprufer [German Public Auditor]"""

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]


### Question Answering

This example instructs the LLM to formulate a concise response to a specific question, with the context provided for reference. The LLM's answer should be encapsulated within triple backticks, ensuring a clear distinction between the question and the response. If the LLM is uncertain about the correct answer based on the provided context, it is instructed to reply with "Unsure about answer", offering a clear acknowledgment of uncertainty rather than providing potentially inaccurate information. Where we took a part of memo by Christian Klein from SAP Annual report.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
Answer the question  based on the context below. Keep the answer short and concise. 
Respond "Unsure about answer" if not sure about the answer.
Context: Dear Fellow Shareholders,
It's hard to summarize the year 2022 in a few words – as the pandemic continued, the world also faced new and unexpected challenges, such as the terrible war in Ukraine, that hugely impacted all of our lives. We have faced conflicts and geopolitical tensions, climate change, the energy crisis, inflation, and volatile markets. Yet, once again, we have seen solidarity in times of crisis – people coming together to provide support to those in need when it mattered most. Despite the ongoing uncertainties in the world, SAP has remained in a strong position.
2022 marked the 50th anniversary of SAP, which we celebrated together with our customers, partners, and colleagues across the world. Five decades ago, our founders set out to redefine business software and in doing so, forever changed the way the world runs. Their innovative thinking, pioneering spirit, and drive laid the foundation for the rise of SAP – and they are still the basis for our success today, as we are carrying their legacy forward to drive positive change for our planet and its people – something that has never been more relevant or important than today.
Our hearts remain with the people impacted by the war in Ukraine. As announced, SAP has stopped all sales in Russia and Belarus, and we are in the process of a total withdrawal from these markets. For 2023, while business wind-down continues, our focus is on further reducing the remaining SAP footprint in Russia. We hope for the swift restoration of peace and will continue to help those affected by this war.
7/335
  SAP Integrated Report 2022
 To Our Stakeholders
    Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
  Additional Information
  The last year was a stark reminder that no one business, government, or society can tackle the greatest challenges of our time alone. For that, a change is needed, and technology plays a key role in finding solutions to our global challenges.
Over two years ago, we embarked on our transformation journey to move SAP towards a cloud company. This, together with our vision to enable every organization and every industry to become a network of intelligent, sustainable enterprises, is perfectly aligned with the challenges our customers face. From increasing speed and agility, building transparent and resilient supply chains, and recording, reporting, and acting on sustainability, our solutions provide the value our customers need:
– With RISE with SAP, we enable organizations to become agile, intelligent enterprises.
– With the SAP Business Network, we connect millions of companies, allowing organizations to
benefit from connected networks.
– With our SAP sustainability solutions, we enable organizations to truly operate sustainably.
Looking at our financial numbers, we met all of our outlook metrics in 2022. Our strong full-year 2022 results at a glance:
– Cloud revenue continued to be our main growth driver, increasing by 24%1.
– Current cloud backlog increased by 24%1.
– Total revenue grew 5%1.
– IFRS Operating profit was flat, while non-IFRS operating profit decreased by 7%1.
– Free cash flow was €4.35 billion.
2022 was a volatile year on the market, with technology stocks particularly hard hit. Our shares were not immune from this overall trend. Our share price decreased 22.8% in 2022, below the DAX, which lost 12.4%, but better than the NASDAQ 100, which decreased 33% over the course of the year. We want our shareholders to participate in our success. Therefore, we have proposed an annual dividend of €2.05 per share2,  an increase of approximately 5% over the prior year's regular dividend.
Customer Net Promoter Score (NPS) decreased 7 points year over year to 3 in 2022, hitting the lower end of the revised outlook range. SAP's Employee Engagement Index decreased 3 percentage points to 80%, a continued high level of engagement at the low end of the revised outlook range. The software as a service-industry scores overall have declined over the past few years of the pandemic. SAP continues to get feedback about needed improvements around pricing increases, licensing structure, product-related topics, support, service and stability of account team relationships. This type of transparent feedback and accountability helps provide us with the information to better focus investments and further improve our customer relationships. SAP's retention rate was 92.3% (2021: 92.8%). Further, the proportion of women in management increased to 29.4% (2021: 28.3%) and we also reached 35% of women in the workforce. Net carbon emissions continued to decrease, at 85 kilotons in 2022, down 25 kt year over year.
In addition to driving our ESG goals internally, we also take our wider social and environmental responsibility very seriously:
– In total, SAP donated more than €4.2 million to support Ukraine in cooperation with organizations such as UNICEF, UNHCR, and the German Red Cross. This includes our employee donation campaign which became SAP's largest employee donation campaign to date.
– SAP extended its partnership with UNICEF through Generation Unlimited (GenU), focusing on employability. The partnership also supports SAP Educate to Employ, a new program educating
1 At constant currencies
2 Pending approval of Annual General Meeting of Shareholders
 8/335

  SAP Integrated Report 2022
 To Our Stakeholders
    Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
  Additional Information
  youth in need on soft skills, foundational knowledge, and SAP skills to enable a pathway to a successful career in the SAP ecosystem.
– Together with partners, we launched the TRANSFORM Support Hub offering virtual pro bono consulting opportunities worldwide, connecting SAP employees to social enterprises.
Net-net: We believe that together with our customers, colleagues, and partners around the world, we can turn the world's greatest challenges into opportunities for a prosperous and greener future. 2022 was one of the most important years in our history. As we head into 2023, we are committed to further optimizing and improving our business. We are deepening our focus on delivering lifetime value to current and new customers in the cloud and on high-growth opportunities where SAP can lead. Across SAP, we are laying the foundation for SAP's ongoing success, expanding our position as the #1 Enterprise Application company on the planet, powered by our leading platform. 
Finally, I want to express my deepest thanks for your continuous trust in SAP. I certainly look back on 2022 with pride and gratitude for the many ways SAP's teams around the world are making a difference. I'm very much looking forward to 2023, and the great achievements our over 100,000 colleagues will continue to deliver as we pursue our vision to enable every organization and every industry to become a network of intelligent, sustainable enterprises.
Sincerely,
Christian Klein CEO, SAP SE
9/335

  
 SAP Integrated Report 2022
To Our Stakeholders
Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
Additional Information
SAP Executive Board
Question: How is SAP performing?
```

![image](images/ail13.png)
[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```TEXT
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "Answer the question based on the context below. Keep the answer short and concise. Respond \"Unsure about answer\" if not sure about the answer. Context: ear Fellow Shareholders, It's hard to summarize the year 2022 in a few words – as the pandemic continued, the world also faced new and unexpected challenges, such as the terrible war in Ukraine, that hugely impacted all of our lives. We have faced conflicts and geopolitical tensions, climate change, the energy crisis, inflation, and volatile markets. Yet, once again, we have seen solidarity in times of crisis – people coming together to provide support to those in need when it mattered most. Despite the ongoing uncertainties in the world, SAP has remained in a strong position. 2022 marked the 50th anniversary of SAP, which we celebrated together with our customers, partners, and colleagues across the world. Five decades ago, our founders set out to redefine business software and in doing so, forever changed the way the world runs. Their innovative thinking, pioneering spirit, and drive laid the foundation for the rise of SAP – and they are still the basis for our success today, as we are carrying their legacy forward to drive positive change for our planet and its people – something that has never been more relevant or important than today. Our hearts remain with the people impacted by the war in Ukraine. As announced, SAP has stopped all sales in Russia and Belarus, and we are in the process of a total withdrawal from these markets. For 2023, while business wind-down continues, our focus is on further reducing the remaining SAP footprint in Russia. We hope for the swift restoration of peace and will continue to help those affected by this war. 7/335 SAP Integrated Report 2022 To Our Stakeholders Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability Additional Information The last year was a stark reminder that no one business, government, or society can tackle the greatest challenges of our time alone. For that, a change is needed, and technology plays a key role in finding solutions to our global challenges. Over two years ago, we embarked on our transformation journey to move SAP towards a cloud company. This, together with our vision to enable every organization and every industry to become a network of intelligent, sustainable enterprises, is perfectly aligned with the challenges our customers face. From increasing speed and agility, building transparent and resilient supply chains, and recording, reporting, and acting on sustainability, our solutions provide the value our customers need: – With RISE with SAP, we enable organizations to become agile, intelligent enterprises. – With the SAP Business Network, we connect millions of companies, allowing organizations to benefit from connected networks. – With our SAP sustainability solutions, we enable organizations to truly operate sustainably. Looking at our financial numbers, we met all of our outlook metrics in 2022. Our strong full-year 2022 results at a glance: – Cloud revenue continued to be our main growth driver, increasing by 24%1. – Current cloud backlog increased by 24%1. – Total revenue grew 5%1. – IFRS Operating profit was flat, while non-IFRS operating profit decreased by 7%1. – Free cash flow was €4.35 billion. 2022 was a volatile year on the market, with technology stocks particularly hard hit. Our shares were not immune from this overall trend. Our share price decreased 22.8% in 2022, below the DAX, which lost 12.4%, but better than the NASDAQ 100, which decreased 33% over the course of the year. We want our shareholders to participate in our success. Therefore, we have proposed an annual dividend of €2.05 per share2, an increase of approximately 5% over the prior year's regular dividend. Customer Net Promoter Score (NPS) decreased 7 points year over year to 3 in 2022, hitting the lower end of the revised outlook range. SAP's Employee Engagement Index decreased 3 percentage points to 80%, a continued high level of engagement at the low end of the revised outlook range. The software as a service-industry scores overall have declined over the past few years of the pandemic. SAP continues to get feedback about needed improvements around pricing increases, licensing structure, product-related topics, support, service and stability of account team relationships. This type of transparent feedback and accountability helps provide us with the information to better focus investments and further improve our customer relationships. SAP's retention rate was 92.3% (2021: 92.8%). Further, the proportion of women in management increased to 29.4% (2021: 28.3%) and we also reached 35% of women in the workforce. Net carbon emissions continued to decrease, at 85 kilotons in 2022, down 25 kt year over year. In addition to driving our ESG goals internally, we also take our wider social and environmental responsibility very seriously: – In total, SAP donated more than €4.2 million to support Ukraine in cooperation with organizations such as UNICEF, UNHCR, and the German Red Cross. This includes our employee donation campaign which became SAP's largest employee donation campaign to date. – SAP extended its partnership with UNICEF through Generation Unlimited (GenU), focusing on employability. The partnership also supports SAP Educate to Employ, a new program educating 1 At constant currencies 2 Pending approval of Annual General Meeting of Shareholders 8/335 SAP Integrated Report 2022 To Our Stakeholders Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability Additional Information youth in need on soft skills, foundational knowledge, and SAP skills to enable a pathway to a successful career in the SAP ecosystem. – Together with partners, we launched the TRANSFORM Support Hub offering virtual pro bono consulting opportunities worldwide, connecting SAP employees to social enterprises. Net-net: We believe that together with our customers, colleagues, and partners around the world, we can turn the world's greatest challenges into opportunities for a prosperous and greener future. 2022 was one of the most important years in our history. As we head into 2023, we are committed to further optimizing and improving our business. We are deepening our focus on delivering lifetime value to current and new customers in the cloud and on high-growth opportunities where SAP can lead. Across SAP, we are laying the foundation for SAP's ongoing success, expanding our position as the #1 Enterprise Application company on the planet, powered by our leading platform. Finally, I want to express my deepest thanks for your continuous trust in SAP. I certainly look back on 2022 with pride and gratitude for the many ways SAP's teams around the world are making a difference. I'm very much looking forward to 2023, and the great achievements our over 100,000 colleagues will continue to deliver as we pursue our vision to enable every organization and every industry to become a network of intelligent, sustainable enterprises. Sincerely, Christian Klein CEO, SAP SE 9/335 SAP Integrated Report 2022 To Our Stakeholders Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability Additional Information SAP Executive Board Question: How is SAP performing?"
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```
![image](images/pm06.png)

[OPTION END]

[OPTION BEGIN [GENERATIVE AI HUB SDK]]

Similarly change the value for prompt and re-run the cell in jupter notebook.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """Answer the question  based on the context below. Keep the answer short and concise. 
Respond "Unsure about answer" if not sure about the answer.
Context: Dear Fellow Shareholders,
It's hard to summarize the year 2022 in a few words – as the pandemic continued, the world also faced new and unexpected challenges, such as the terrible war in Ukraine, that hugely impacted all of our lives. We have faced conflicts and geopolitical tensions, climate change, the energy crisis, inflation, and volatile markets. Yet, once again, we have seen solidarity in times of crisis – people coming together to provide support to those in need when it mattered most. Despite the ongoing uncertainties in the world, SAP has remained in a strong position.
2022 marked the 50th anniversary of SAP, which we celebrated together with our customers, partners, and colleagues across the world. Five decades ago, our founders set out to redefine business software and in doing so, forever changed the way the world runs. Their innovative thinking, pioneering spirit, and drive laid the foundation for the rise of SAP – and they are still the basis for our success today, as we are carrying their legacy forward to drive positive change for our planet and its people – something that has never been more relevant or important than today.
Our hearts remain with the people impacted by the war in Ukraine. As announced, SAP has stopped all sales in Russia and Belarus, and we are in the process of a total withdrawal from these markets. For 2023, while business wind-down continues, our focus is on further reducing the remaining SAP footprint in Russia. We hope for the swift restoration of peace and will continue to help those affected by this war.
7/335
  SAP Integrated Report 2022
 To Our Stakeholders
    Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
  Additional Information
  The last year was a stark reminder that no one business, government, or society can tackle the greatest challenges of our time alone. For that, a change is needed, and technology plays a key role in finding solutions to our global challenges.
Over two years ago, we embarked on our transformation journey to move SAP towards a cloud company. This, together with our vision to enable every organization and every industry to become a network of intelligent, sustainable enterprises, is perfectly aligned with the challenges our customers face. From increasing speed and agility, building transparent and resilient supply chains, and recording, reporting, and acting on sustainability, our solutions provide the value our customers need:
– With RISE with SAP, we enable organizations to become agile, intelligent enterprises.
– With the SAP Business Network, we connect millions of companies, allowing organizations to
benefit from connected networks.
– With our SAP sustainability solutions, we enable organizations to truly operate sustainably.
Looking at our financial numbers, we met all of our outlook metrics in 2022. Our strong full-year 2022 results at a glance:
– Cloud revenue continued to be our main growth driver, increasing by 24%1.
– Current cloud backlog increased by 24%1.
– Total revenue grew 5%1.
– IFRS Operating profit was flat, while non-IFRS operating profit decreased by 7%1.
– Free cash flow was €4.35 billion.
2022 was a volatile year on the market, with technology stocks particularly hard hit. Our shares were not immune from this overall trend. Our share price decreased 22.8% in 2022, below the DAX, which lost 12.4%, but better than the NASDAQ 100, which decreased 33% over the course of the year. We want our shareholders to participate in our success. Therefore, we have proposed an annual dividend of €2.05 per share2,  an increase of approximately 5% over the prior year's regular dividend.
Customer Net Promoter Score (NPS) decreased 7 points year over year to 3 in 2022, hitting the lower end of the revised outlook range. SAP's Employee Engagement Index decreased 3 percentage points to 80%, a continued high level of engagement at the low end of the revised outlook range. The software as a service-industry scores overall have declined over the past few years of the pandemic. SAP continues to get feedback about needed improvements around pricing increases, licensing structure, product-related topics, support, service and stability of account team relationships. This type of transparent feedback and accountability helps provide us with the information to better focus investments and further improve our customer relationships. SAP's retention rate was 92.3% (2021: 92.8%). Further, the proportion of women in management increased to 29.4% (2021: 28.3%) and we also reached 35% of women in the workforce. Net carbon emissions continued to decrease, at 85 kilotons in 2022, down 25 kt year over year.
In addition to driving our ESG goals internally, we also take our wider social and environmental responsibility very seriously:
– In total, SAP donated more than €4.2 million to support Ukraine in cooperation with organizations such as UNICEF, UNHCR, and the German Red Cross. This includes our employee donation campaign which became SAP's largest employee donation campaign to date.
– SAP extended its partnership with UNICEF through Generation Unlimited (GenU), focusing on employability. The partnership also supports SAP Educate to Employ, a new program educating
1 At constant currencies
2 Pending approval of Annual General Meeting of Shareholders
 8/335

  SAP Integrated Report 2022
 To Our Stakeholders
    Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
  Additional Information
  youth in need on soft skills, foundational knowledge, and SAP skills to enable a pathway to a successful career in the SAP ecosystem.
– Together with partners, we launched the TRANSFORM Support Hub offering virtual pro bono consulting opportunities worldwide, connecting SAP employees to social enterprises.
Net-net: We believe that together with our customers, colleagues, and partners around the world, we can turn the world's greatest challenges into opportunities for a prosperous and greener future. 2022 was one of the most important years in our history. As we head into 2023, we are committed to further optimizing and improving our business. We are deepening our focus on delivering lifetime value to current and new customers in the cloud and on high-growth opportunities where SAP can lead. Across SAP, we are laying the foundation for SAP's ongoing success, expanding our position as the #1 Enterprise Application company on the planet, powered by our leading platform. 
Finally, I want to express my deepest thanks for your continuous trust in SAP. I certainly look back on 2022 with pride and gratitude for the many ways SAP's teams around the world are making a difference. I'm very much looking forward to 2023, and the great achievements our over 100,000 colleagues will continue to deliver as we pursue our vision to enable every organization and every industry to become a network of intelligent, sustainable enterprises.
Sincerely,
Christian Klein CEO, SAP SE
9/335

  
 SAP Integrated Report 2022
To Our Stakeholders
Combined Group Consolidated Financial Further Information on Management Report Statements IFRS Sustainability
Additional Information
SAP Executive Board
Question: How is SAP performing?
"""

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]

### Text Classification - Sentiment Analysis

This example directs an LLM to perform sentiment analysis on a provided product review. The LLM is instructed to assess the sentiment of the review text and respond with a single word, either "positive" or "negative". The review text is clearly delineated using triple backticks, ensuring clarity about which portion of the text needs to be analyzed. This format aims to extract concise and direct sentiment evaluations without any ambiguity.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
What is the sentiment of the following product review, 
which is delimited with triple single quotes?

Give your answer as a single word, either "positive" or "negative".

Review text: '''SAP has Best work Environment and Best ERP product'''
```
![image](images/ail14.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```TEXT
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "What is the sentiment of the following product review, which is delimited with triple backticks? Give your answer as a single word, either \"positive\" or \"negative\". Review text: '''SAP has Best work Environment and Best ERP product'''"
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm07.png)

[OPTION END]

[OPTION BEGIN [GENERATIVE AI HUB SDK]]

Similarly change the value for prompt and re-run the cell in jupter notebook.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """What is the sentiment of the following product review, 
which is delimited with triple single quotes?

Give your answer as a single word, either "positive" or "negative".

Review text: '''SAP has Best work Environment and Best ERP product'''"""

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]

### Expansion

Here in this demo we are going to try out show text expansion qualities of AI core, where we wrote a few sentences about AI core and asked the LLM model to write a 500 word blog post or paragraph based on the context it understands.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
Expand it into a 500 word blog post
SAP AI core is a platform for building AI applications. which can be used to train and deploy AI applications. as well as act as a model and dataset artifactory.
```

![image](images/ail17.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```TEXT
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "Expand it into a 500 word blog post SAP AI core is a platform for building AI applications. which can be used to train and deploy AI applications. as well as act as a model and dataset artifactory."
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm10.png)

[OPTION END]

[OPTION BEGIN [GENERATIVE AI HUB SDK]]

Similarly change the value for prompt and re-run the cell in jupter notebook.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """Expand it into a 500 word blog post
SAP AI core is a platform for building AI applications. which can be used to train and deploy AI applications. as well as act as a model and dataset artifactory."""

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]

### Tone adjustment

Here in tone Adjustment we are trying to showcase use the LLM's capabilities to change/modify the tone of a text written by a new employee at SAP to proper professional tone.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
Translate the following input to a Corporate language
Sap Blue a new product from SAP. that is a gig based product. launched in 2016 relaunched in 2023.
```

![image](images/ail18.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```TEXT
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "Translate the following input to a Corporate language. Sap Blue a new product from SAP. that is a gig based product. lauched in 2016 relaunched in 2023."
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm11.png)

[OPTION END]


[OPTION BEGIN [GENERATIVE AI HUB SDK]]

Similarly change the value for prompt and re-run the cell in jupter notebook.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """Translate the following input to a Corporate language
Sap Blue a new product from SAP. that is a gig based product. launched in 2016 relaunched in 2023."""

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]


### Spell-check / Grammar-check

Here we picked a paragraph on SAP and made a few spelling and grammatical errors. Now we will be asking the LLM to fix those errors by proofreading the content.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
proofread and correct this review: SAP SE is a German multinationl software company based in Walldorf, Baden-Wurttemberg. It develops enterprise software to manage business operations and customer relations. The company is the world leading enterprise resource planing software vendor.
```

![image](images/ail19.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "proofread and correct this review: SAP SE is a German multinationl software company based in Walldorf, Baden-Wurttemberg. It develops enterprise software to manage business operations and customer relations. The company is the world leading enterprise resource planing software vendor."
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm12.png)

[OPTION END]

[OPTION BEGIN [GENERATIVE AI HUB SDK]]

Similarly change the value for prompt and re-run the cell in jupter notebook.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """proofread and correct this review: SAP SE is a German multinationl software company based in Walldorf, Baden-Wurttemberg. It develops enterprise software to manage business operations and customer relations. The company is the world leading enterprise resource planing software vendor."""

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]

### Doing multiple tasks at once

We picked up a random review from trust pilot on SAP ERP and want the LLM to perform multiple tasks at once which include sentiment analysis, checking if the user is angry with the product, which product/item they are talking about and which brand does it belong to.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
Identify the following items from the review text: 
- Sentiment (positive or negative)
- Is the reviewer expressing anger? (true or false)
- Item purchased by reviewer
- Company that made the item

The review is delimited with triple backticks.
Format your response as a JSON object with "Sentiment", "Anger", "Item" and "Brand" as the keys.
If the information isn't present, use "unknown" as the value.
Make your response as short as possible. Format the Anger value as a boolean.

Review text: '''A true ERP software available in the market which captures 60% of market share and known as a ERP leader. The best part about the product that it can be used cloud based and it can be integrated with several modules which are equally relevant as a department. I have been a part of SAP from past 12+ years and I am extremely happy of using and referring this product to others as well. This isn't only beneficial for companies but can also make careers for humans as well. Now a days cloud based functionality and integration with API tools are the best part in it. Easy to customize according to the requirement of a client. It has several features and capabilities 1. Cost efficient, 2. Advance data management, 3. Saves time, 4. Increase productivity, 5. Real time data saving to server's, 6. Avoid duplication and ensures transparency. Best product available in the market.'''
```

![image](images/ail20.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "Identify the following items from the review text: - Sentiment (positive or negative) - Is the reviewer expressing anger? (true or false) - Item purchased by reviewer - Company that made the item The review is delimited with triple backticks. Format your response as a JSON object with \"Sentiment\", \"Anger\", \"Item\" and \"Brand\" as the keys. If the information isn't present, use \"unknown\" as the value. Make your response as short as possible. Format the Anger value as a boolean. Review text: '''A true ERP software available in the market which captures 60% of market share and known as a ERP leader. The best part about the product that it can be used cloud based and it can be integrated with several modules which are equally relevant as a department. I have been a part of SAP from past 12+ years and I am extremely happy of using and referring this product to others as well. This isn't only beneficial for companies but can also make careers for humans as well. Now a days cloud based functionality and integration with API tools are the best part in it. Easy to customize according to the requirement of a client. It has several features and capabilities 1. Cost efficient, 2. Advance data management, 3. Saves time, 4. Increase productivity, 5. Real time data saving to server's, 6. Avoid duplication and ensures transparency. Best product available in the market.'''"
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm13.png)

[OPTION END]

[OPTION BEGIN [GENERATIVE AI HUB SDK]]

Similarly change the value for prompt and re-run the cell in jupter notebook.

```PYTHON
from gen_ai_hub.proxy.langchain.init_models import init_llm

prompt = """Identify the following items from the review text: 
- Sentiment (positive or negative)
- Is the reviewer expressing anger? (true or false)
- Item purchased by reviewer
- Company that made the item

The review is delimited with triple backticks.
Format your response as a JSON object with "Sentiment", "Anger", "Item" and "Brand" as the keys.
If the information isn't present, use "unknown" as the value.
Make your response as short as possible. Format the Anger value as a boolean.

Review text: '''A true ERP software available in the market which captures 60% of market share and known as a ERP leader. The best part about the product that it can be used cloud based and it can be integrated with several modules which are equally relevant as a department. I have been a part of SAP from past 12+ years and I am extremely happy of using and referring this product to others as well. This isn't only beneficial for companies but can also make careers for humans as well. Now a days cloud based functionality and integration with API tools are the best part in it. Easy to customize according to the requirement of a client. It has several features and capabilities 1. Cost efficient, 2. Advance data management, 3. Saves time, 4. Increase productivity, 5. Real time data saving to server's, 6. Avoid duplication and ensures transparency. Best product available in the market.''' """

llm = init_llm('gpt-35-turbo', temperature=0., max_tokens=256)
llm.invoke(prompt).content
```

[OPTION END]

### Few-Shot Prompting

The following example demonstrates a few-shot learning approach in prompt engineering, where the model is provided with a couple of examples to understand the desired task and format. Instead of explicitly stating the task, the LLM is given a context in which it should operate.

- **Contextual Setup:** The "Child" and "Grandparent" dialog sets up a context. The model is implicitly being taught that it should generate responses in the style of a wise grandparent answering a child's questions.
- **Example Provided:** The first complete interaction (about *patience*) serves as a *shot* or *example*, guiding the model on how it should structure its response.
- **Task Indication:** The second interaction (about *unity*) is incomplete, indicating the task the model needs to perform. The goal is to get the model to continue the pattern and provide a similarly styled, profound answer to the child's new question.

The model's generated completion is then printed, providing insight into its understanding and continuation of the provided examples.

In summary, by using a few-shot learning approach, the model is guided to understand and emulate the style of the conversation without explicitly being told the exact format or context. This method leverages the model's ability to generalize from few examples and produce consistent and contextually relevant outputs.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
Your task is to answer in a consistent style.

Child: Teach me about patience.

Grandparent: The river that carves the deepest 
valley flows from a modest spring;
the grandest symphony originates from a single note; 
the most intricate tapestry begins with a solitary thread.

Child: Teach me about unity.

Grandparent:
```

![image](images/ail21.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "Your task is to answer in a consistent style. Child: Teach me about patience. Grandparent: The river that carves the deepest valley flows from a modest spring; the grandest symphony originates from a single note; the most intricate tapestry begins with a solitary thread. Child: Teach me about unity. Grandparent:"
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm14.png)

[OPTION END]

### Zero-Shot prompting

The following example showcases an advanced application of prompt engineering that encapsulates a multi-step task for the LLM.

- **Role Emulation:** The initial statement sets the stage by instructing the LLM to assume the role of a customer service AI assistant. This prepares the model to respond in a specific, customer-service oriented manner.

- **Multi-step Instruction:** The prompt is divided into two distinct steps, each guiding the model to perform a specific action:

    - Step 1: **Sentiment Analysis** - The model is directed to discern the sentiment of a provided customer review. This sentiment could be positive, negative, or neutral.
    - Step 2: **Contextual Reply** - Based on the extracted sentiment, the model must generate an appropriate email reply. The instructions are clear:
        - Positive or Neutral Sentiment: Express gratitude.
        - Negative Sentiment: Offer an apology and provide an avenue for further assistance. The model is also guided to incorporate details from the review to ensure the response feels tailored and specific to the customer's concerns.

- **Tone and Format:** The instructions emphasize writing in a concise and professional tone. Moreover, the model is guided to sign off the email as "AI customer agent", reinforcing the context and ensuring the generated response follows a proper email format.

- **Multi-step Continuation:** This prompt is an excellent example of multi-step continuation. The LLM first determines the sentiment of the review and then uses that sentiment to guide its next action, which is generating a contextually appropriate email reply.

In summary, from a prompt engineering standpoint, this example effectively leverages a structured, multi-step instruction set to guide the LLM through a complex task. The clarity and specificity of the instructions, combined with the defined role and context, aim to elicit a precise and contextually relevant response from the model.

[OPTION BEGIN [SAP AI Launchpad]]

Go to **Generative AI Hub** -> **Prompt Editor**.

Copy the following text into the message box and click on the **Run** button.

```TEXT
You are a customer service AI assistant. 
Given the customer email, perform the following steps: 

Step 1: Extract the sentiment of the customer review as positive, negative and neutral. 

Step 2: Now, Your task is to send an email reply to a valued customer.
Generate a reply to thank the customer for their review.
If the sentiment is positive or neutral, thank them for their review.
If the sentiment is negative, apologize and suggest that they can reach out to customer service. 
Make sure to use specific details from the review. Write in a concise and professional tone.
Sign the email as `AI customer agent`.
Customer review: So, they still had the 17 piece system on seasonal sale for around $49 in the month of November, about half off, but for some reason (call it price gouging) around the second week of December the prices all went up to about anywhere from between $70-$89 for the same system. And the 11 piece system went up around $10 or so in price also from the earlier sale price of $29. So it looks okay, but if you look at the base, the part where the blade locks into place doesn't look as good as in previous editions from a few years ago, but I plan to be very gentle with it (example, I crush very hard items like beans, ice, rice, etc. in the blender first then pulverize them in the serving size I want in the blender then switch to the whipping blade for a finer flour, and use the cross cutting blade first when making smoothies, then use the flat blade if I need them finer/less pulpy). Special tip when making smoothies, finely cut and freeze the fruits and vegetables (if using spinach-lightly stew soften the spinach then freeze until ready for use-and if making sorbet, use a small to medium sized food processor) that you plan to use that way you can avoid adding so much ice if at all-when making your smoothie. After about a year, the motor was making a funny noise. I called customer service, but the warranty expired already, so I had to buy another one. FYI: The overall quality has gone done in these types of products, so they are kind of counting on brand recognition and consumer loyalty to maintain sales. Got it in about two days.
```

![image](images/ail22.png)

[OPTION END]

[OPTION BEGIN[POSTMAN]]

Send a POST request to 

```
{{deploymentUrl}}/chat/completions?api-version=2023-05-15
```

Include the following in the body of the request.

```JSON
{
"messages": [
    {
        "role": "user",
        "content": "You are a customer service AI assistant. Given the customer email, perform the following steps: Step 1: Extract the sentiment of the customer review as positive, negative and neutral. Step 2: Now, Your task is to send an email reply to a valued customer. Generate a reply to thank the customer for their review. If the sentiment is positive or neutral, thank them for their review. If the sentiment is negative, apologize and suggest that they can reach out to customer service. Make sure to use specific details from the review. Write in a concise and professional tone. Sign the email as `AI customer agent`. Customer review: So, they still had the 17 piece system on seasonal sale for around $49 in the month of November, about half off, but for some reason (call it price gouging) around the second week of December the prices all went up to about anywhere from between $70-$89 for the same system. And the 11 piece system went up around $10 or so in price also from the earlier sale price of $29. So it looks okay, but if you look at the base, the part where the blade locks into place doesn't look as good as in previous editions from a few years ago, but I plan to be very gentle with it (example, I crush very hard items like beans, ice, rice, etc. in the blender first then pulverize them in the serving size I want in the blender then switch to the whipping blade for a finer flour, and use the cross cutting blade first when making smoothies, then use the flat blade if I need them finer/less pulpy). Special tip when making smoothies, finely cut and freeze the fruits and vegetables (if using spinach-lightly stew soften the spinach then freeze until ready for use-and if making sorbet, use a small to medium sized food processor) that you plan to use that way you can avoid adding so much ice if at all-when making your smoothie. After about a year, the motor was making a funny noise. I called customer service, but the warranty expired already, so I had to buy another one. FYI: The overall quality has gone done in these types of products, so they are kind of counting on brand recognition and consumer loyalty to maintain sales. Got it in about two days."
    }
],
"max_tokens": 100,
"temperature": 0.0,
"frequency_penalty": 0,
"presence_penalty": 0,
"stop": "null"
}
```

![image](images/pm15.png)

[OPTION END]
