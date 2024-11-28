---
parser: v2
auto_validation: true
time: 45
primary_tag: software-product>sap-business-technology-platform
tags: [ tutorial>beginner, topic>artificial-intelligence, topic>machine-learning, software-product>sap-business-technology-platform ]
author_name: Smita Naik
author_profile: https://github.com/I321506
---

# Basic Consumption of GenAI models Using Orchestration 
<!-- description --> In this tutorial, we are going to learn the basic consumption of Gen AI models using the Orchestration.

## You will learn
- How to inference GenAI models Using Orchestration

## Prerequisites
- A BTP global account
- If you are an SAP Developer or SAP employee, please refer to the following links ( **for internal SAP stakeholders only** )  
- [How to create a BTP Account (internal)](https://me.sap.com/notes/3493139)
- [SAP AI Core](https://help.sap.com/docs/sap-ai-core?version=INTERNAL&locale=en-US&state=PRODUCTION)
- If you are an external developer or a customer or a partner kindly refer to this [tutorial](https://developers.sap.com/tutorials/btp-cockpit-entitlements.html)
- Ai core setup and basic knowledge: [Link to documentation](https://developers.sap.com/tutorials/ai-core-setup.html)
- Ai core Instance with Standard Plan or Extended Plan

### Set Up Your Environment and Configure Access

[OPTION BEGIN [AI Launchpad]]

•	Open AI Launchpad.

•	Connect to your instance using your credentials.

•	Navigate to the desired Resource Group where you plan to deploy the orchestration.

[OPTION END]

[OPTION BEGIN [Gen AI Hub SDK]]

•  Configure proxy modules by setting up environment variables for AI Core credentials.

•  Replace placeholder values in ~/.aicore/config.json with AI Core service keys from BTP.

•  Optionally, set the AICORE_HOME environment variable to override the default config path.


![img](img/image005.png)

[OPTION END]

[OPTION BEGIN [SAP Cloud SDK]]

• Download the service key for the AI Core service instance.  

• Set the downloaded service key as the `AICORE_SERVICE_KEY` environment variable in your local environment.  

• Optionally, set the `AICORE_HOME` environment variable to override the default configuration path.

• For detailed installation and usage of the **SAP Cloud SDK for AI**, visit the official [GitHub repository](https://github.com/SAP/ai-sdk-js/tree/main?tab=readme-ov-file#sap-ai-sdkorchestration). This page provides comprehensive steps to set up and integrate the SDK effectively in your projects.
 
**Tip:**  

• Installing JavaScript Kernel for Jupyter Notebooks: If you want to use JavaScript in Jupyter Notebooks, you can refer to [Deno v1.37 blog post](https://deno.com/blog/v1.37) for detailed steps to install the Javascript kernel. Follow the instructions provided to set up the environment and enable JavaScript support in Jupyter.  

• Ways to load environment variables may vary depending on the framework. 

• SAP Cloud SDK for AI: Uses the `dotenv` library to load environment variables.  If you encounter issues with the dotenv library, ensure it is installed correctly by running:  

    ```CODE 
    npm install dotenv 
    ```

```CODE
import dotenv from 'dotenv';
dotenv.config();

console.log(process.env.AICORE_SERVICE_KEY); 
```

![img](img/image006.png)

• **Next.js:** Requires specific configuration for loading environment variables.

[OPTION END]

[OPTION BEGIN [Postman]]

1.	Download the service key for the AI Core service instance:

    •	Once your account is ready, go to your AI Core service in BTP and download the service keys. These keys will be used in the OAuth 2.0 authorization setup.

2.	Configure OAuth 2.0 Authorization in Postman:

    •	Open Postman and create a new tab.

    •	Go to the Authorization section.

    •	Set the Type to OAuth 2.0.

3.	Scroll down to Configure New Token and input the following details:

    •	Access Token URL: [Your Access Token URL]

    •	Client ID: [Your Client ID from the service keys]

    •	Client Secret: [Your Client Secret from the service keys]
    
    •	Scroll down and click on Generate Token.

**Note:** Follow the screenshots provided for step-by-step guidance

![img](img/image001.png)

![img](img/image002.png)

![img](img/image003.png)


In the Headers tab, add the following headers:

    •	Resource Group: [Your Resource Group Name]

    •	Content-Type: application/json`

![img](img/image004.png)

[OPTION END]


### Create Configuration for Orchestration deployment 

[OPTION BEGIN [AI Launchpad]]

Go to the Configuration section within your chosen Resource Group. 

![img](img/image008.png)

• Fill in Deployment Details, Under configuration, input the following details: 

        Name:       "orchestration" 

        Executable: "orchestration" 

        Scenario:   "orchestration" 

        Version:    "0.0.1" 

• Click Next after entering each detail. 

![img](img/image009.png)

[OPTION END]

[OPTION BEGIN [Gen AI SDK]]

• Create a folder named orchestration, then navigate to this folder using VS Code. 

•  Inside the folder, create a new file with any name but ensure it has the .ipynb extension. 

![img](img/image010.png)

You'll create a configuration that defines the orchestration setup. Use the following code to initialize your configuration. 

```CODE

# Define scenario ID, executable ID, and configuration suffix 
scenario_id = "orchestration" 
executable_id = "orchestration" 
config_suffix = "config-new"   # Enter your configuration name 
config_name = f"{config_suffix}-orchestration" 

# Create a new configuration 
config = ai_core_client.configuration.create( 
    scenario_id=scenario_id, 
    executable_id=executable_id, 
    name=config_name 
) 
print(f"Configuration created successfully with ID: {config.id} and Name: {config_name}") 
```

**Note**:  

• scenario_id and executable_id: Both are set to "orchestration" for this tutorial. 

• config_name: Choose a unique name for the configuration (e.g., "config-new-orchestration") 

![img](img/image011.png)

[OPTION END]

[OPTION BEGIN [SAP Cloud SDK ]]

In this step, we define a function to create an orchestration configuration using the ConfigurationApi from the SAP AI SDK. This configuration integrates various parameters needed for orchestration, such as the executable ID and scenario ID.

```CODE

import { ConfigurationApi } from '@sap-ai-sdk/ai-api';

// Function to create orchestration configuration
async function createOrchestrationConfiguration() {
  const requestBody = {
      name: 'orchestration-config', // Choose a meaningful name
      executableId: 'orchestration', // Orchestration executable ID
      scenarioId: 'orchestration', // Orchestration scenario ID
      parameterBindings: [
          {
              "key": "modelFilterList", // Define the parameters you need for orchestration
              "value": "null"  // Example orchestration version
          },
          {
              "key": "modelFilterListType",
              "value": "allow"
          }
      ],
      inputArtifactBindings: []  // Orchestrations may not require input bindings directly, but this can be modified
  };

  try {
      const responseData = await ConfigurationApi
          .configurationCreate(requestBody, {'AI-Resource-Group': 'default'}) // Use the correct resource group
          .execute();
      
      console.log('Orchestration configuration created successfully:', responseData);
      return responseData; // Return the configuration response
  } catch (errorData) {
      const apiError = errorData.response.data.error; // Handle API errors
      console.error('Status code:', errorData.response.status);
      throw new Error(`Configuration creation failed: ${apiError.message}`);
  }
}

// usage
const orchestrationConfig = await createOrchestrationConfiguration();
orchestrationConfig;
```

**Note**: 

• scenario_id and executable_id: Both are set to "orchestration" for this tutorial. 

• config_name: Choose a unique name for the configuration (e.g., "config-new-orchestration")


[OPTION END]

[OPTION BEGIN [Postman]]

In Postman, send a POST request to the endpoint: {{apiurl}}/v2/lm/configurations. 

Include the following parameters in the request body: 

```CODE
{ 

    "name": "orchestration", 
    "executableId": "orchestration", 
    "scenarioId": "orchestration", 
    "versionId": "0.0.1", 
    "parameterBindings": [ 
        { 
            "key": "modelFilterList", 
            "value": "null" 
        }, 

        { 
            "key": "modelFilterListType", 
            "value": "allow" 
        } 
    ] 
} 
```

**Parameter Details:** 

        name: Identifier of your choice for this configuration. 
        executableId: "orchestration" 
        scenarioId: "orchestration" 
        versionId: "0.0.1" 

![img](img/image007.png)

Response: You’ll receive a unique configurationId in the response. Refer to the screenshot for an example response. 

[OPTION END]

### Create and Monitor Orchestration deployment

[OPTION BEGIN [AI Launchpad]]

When prompted, click on Create Deployment. Continue through the setup by clicking Next until you reach the deployment confirmation. 

![img](img/image014.png)

Once the deployment begins, continue to the status page. Verify that the Deployment Status changes to Running (see attached screenshot for reference). 

![img](img/image015.png)

[OPTION END]

[OPTION BEGIN [Gen AI SDK]]

With the configuration ID, you can proceed to deploy the orchestration and monitor its progress. 

**Create the Deployment:** 

Run the following code to create a deployment using the configuration ID obtained in Step 2. 

```CODE

# Create a deployment using the configuration ID from the previous cell 

deployment = ai_core_client.deployment.create(configuration_id=config.id) 
print(f"Deployment created successfully with ID: {deployment.id}") 

```

![img](img/image016.png)

**Monitor Deployment Status:** 

Execute the following code to monitor the deployment until it’s fully active. The status should eventually display as "Running". 

```CODE

from ai_api_client_sdk.models.status import Status 

def spinner(check_callback, timeout=300, check_every_n_seconds=10): 
    start = time.time() 
    while time.time() - start < timeout: 
        return_value = check_callback() 
        if return_value: 
            return return_value 

        for char in '|/-\\': 
            clear_output(wait=True) 
            print(f'Waiting for the deployment to become ready... {char}') 
            time.sleep(0.2) 

# Define the callback to check if the deployment is ready 
def check_ready(): 
    updated_deployment = ai_core_client.deployment.get(deployment.id) 
    return updated_deployment if updated_deployment.status == Status.RUNNING else None 

# Wait for the deployment to be ready 
ready_deployment = spinner(check_ready) 
print(f"Deployment is ready with status: {ready_deployment.status}") 

```

Result: The code will display a loading spinner until the deployment status updates to "Running." Refer to the attached screenshot for confirmation. 

**Note:** API note need to be added here 

![img](img/image017.png)

[OPTION END]

[OPTION BEGIN [SAP Cloud SDK ]]

This step involves creating a deployment using the specified configuration and resource group. The deployment is handled via the DeploymentApi, which streamlines the process of activating the orchestration setup. 

```CODE

import { DeploymentApi } from '@sap-ai-sdk/ai-api'; 
import type { AiDeploymentCreationResponse } from '@sap-ai-sdk/ai-api'; 

/** 
 * Create a deployment using the configuration specified by configurationId. 
 * @param configurationId - ID of the configuration to be used. 
 * @param resourceGroup - AI-Resource-Group where the resources are available. 
 * @returns Deployment creation response with 'targetStatus': 'RUNNING'. 
 */ 

export async function createDeployment( 
  configurationId: string, 
  resourceGroup: string 
): Promise<AiDeploymentCreationResponse> { 
  return DeploymentApi.deploymentCreate( 
    { configurationId }, 
    { 'AI-Resource-Group': resourceGroup } 
  ).execute(); 
} 

/** 
 * Deploy the orchestration using the given configuration ID. 
 * @param resourceGroup - AI-Resource-Group where the resources are available. 
 * @returns A message indicating the result of the deployment operation. 
 */ 

export async function deployOrchestration( 
  resourceGroup: string 
): Promise<string> { 
  // Fetch the configuration ID (can be retrieved or passed dynamically) 
   const configurationId = orchestrationConfig.id;

  try { 
    // Step: Create deployment using the configuration ID 
    const response = await createDeployment(configurationId, resourceGroup); 
    // console.log(`Orchestration deployment created with ID: ${response.id}`); 
    return `Orchestration deployment created with ID: ${response.id}`; 
  } catch (error) { 
    console.error('Error creating orchestration deployment:', error); 
    return 'Failed to create orchestration deployment.'; 
  } 
} 

// usage to deploy orchestration 

(async () => { 
    const resourceGroup = 'default'; // Replace with your actual resource group name 
    try { 
      const result = await deployOrchestration(resourceGroup); 
      console.log(result); // Outputs deployment creation response 
    } catch (error) { 
      console.error('Error executing orchestration deployment:', error); 
    } 
  })(); 

```

[OPTION END]

[OPTION BEGIN [Postman]]

• Send a POST request to the endpoint: {{apiurl}}/v2/lm/deployments. 

• Include the configurationId obtained from the previous step in the request body: 

```CODE

{ 
    "configurationId": "yourConfigurationId" // Replace with the actual configuration ID 
} 

```
![img](img/image012.png)

• Retrieve the details of your deployment to monitor its status. 

• Send a GET request to the endpoint: {{apiurl}}/v2/lm/deployments. 

**Deployment Status:** 

The status should display as Running once the deployment is complete (see the provided screenshot for reference). 

![img](img/image013.png)

[OPTION END]

### Consume Deployed Orchestration

[OPTION BEGIN [AI Launchpad]]

• Navigate to the resource group where your orchestration has been deployed. 

• Go to Generative AI Hub. 

• Select Orchestration and click on Templating. 

• In the Templating section, locate the message icon with three tabs: User, Assistance, and System. 


Click on the User tab, Enter the following details: 

**Prompt:** 

```CODE

Here is a candidate's resume: {{?candidate_resume}} 

```
**Variable Definitions:** 

• The variable “candidate_resume” will be created. 

• Enter the default values according to your use case. For this example, use the following resume   information (you can copy-paste this text): 

```TEXT
John Doe 
1234 Data St, San Francisco, CA 94101 
(123) 456-7890 
johndoe@email.com 
LinkedIn Profile 
GitHub Profile 

Objective 
Detail-oriented Data Scientist with 3+ years of experience in data analysis, statistical modeling, and machine learning. Seeking to leverage expertise in predictive modeling and data visualization to help drive data-informed decision-making at [Company Name]. 

Education 
Master of Science in Data Science 
University of California, Berkeley 
Graduated: May 2021 
Bachelor of Science in Computer Science 
University of California, Los Angeles 
Graduated: May 2019 

Technical Skills 
Programming Languages: Python, R, SQL, Java 
Data Analysis & Visualization: Pandas, NumPy, Matplotlib, Seaborn, Tableau 
Machine Learning: Scikit-learn, TensorFlow, Keras, XGBoost 
Big Data Technologies: Hadoop, Spark 
Databases: MySQL, PostgreSQL 
Version Control: Git 

Professional Experience 
Data Scientist 
DataCorp Inc., San Francisco, CA 
June 2021 – Present 

Developed predictive models to optimize marketing campaigns, which increased ROI by 20%. 
Conducted in-depth data analysis using Python and SQL to identify trends and patterns in large datasets. 
Collaborated with cross-functional teams to implement data-driven strategies that improved customer satisfaction scores by 15%. 
Created interactive dashboards using Tableau to visualize KPIs for stakeholders. 

Data Analyst Intern 
Analytics Solutions, Los Angeles, CA 
June 2020 – August 2020 

Analyzed large datasets to identify opportunities for business growth and improvement. 
Assisted in the development of automated reporting tools using Python and Excel. 
Worked with data visualization tools to create insightful reports for management. 

Projects 
Customer Segmentation Analysis 
Conducted K-means clustering on customer data to segment the customer base into distinct groups, enabling targeted marketing strategies. 

Predictive Stock Price Modeling 
Built a predictive model using time series analysis to forecast stock prices, achieving an accuracy rate of 85%. 

Sentiment Analysis on Social Media 
Implemented natural language processing techniques to analyze sentiment from tweets, providing insights into public opinion on various topics. 

Certifications 
Certified Data Scientist (CDS) – Data Science Council of America 
Machine Learning Specialization – Coursera by Stanford University 

Professional Affiliations 
Member, Association for Computing Machinery (ACM) 
Member, Data Science Society 

References 
Available upon request.

Personal Interests 
- I absolutely love exploring new technologies and working on innovative projects. 
- I enjoy reading books, especially on artificial intelligence and machine learning. 
- I hate people who are dishonest and unreliable. 
- I love traveling and experiencing new cultures. 
- I enjoy playing video games, especially competitive ones. 
- I hate being stuck in a routine; I always seek new challenges and growth opportunities. 
-I hate working in Azure cloud -"Azure cloud is the most irritating platform i have ever used" 
```

![img](img/image019.png)

• After entering the details, click on Add. 

• A new message box will appear. Proceed to configure the System tab. enter the following details: 

**Prompt:** 

```CODE

You are a helpful AI assistant for HR. Summarize the following CV in 10 sentences, focusing on key qualifications, work experience, and achievements. Include personal contact information, organizational history, and personal interests. 

```

![img](img/image020.png)

Data masking and input content filtering are optional advanced modules. In this tutorial, our primary focus is on consumption. Please set the values as shown in the screenshots below. 

![img](img/image021.png)

![img](img/image022.png)

• Navigate to the Model Configuration section. 

• Select your Deployment ID and choose the model you want to use for this orchestration. 

![img](img/image023.png)

![img](img/image024.png)

Output content filtering is an optional advanced module. In this tutorial, our focus is on consumption. Please configure it as shown in the screenshots below. 

![img](img/image025.png)

After configuring model settings, click on the Test icon and run the orchestration. 

Check the Result section for the response. 

![img](img/image026.png)

**Important Note**

Ensure at least one orchestration deployment is ready to be consumed during this process.  

**Optional Advanced Modules**

Data masking and content filtering are available to enhance data privacy and safety. Data masking hides sensitive information like phone numbers or organization names, while content filtering can screen for categories such as hate self-harm, sexual content, and violence. In this tutorial, the response generated by the LLM models may carry sensitive information, such as names and phone numbers. For further enhancement, refer to the next tutorial on implementing these modules. 

[OPTION END]

[OPTION BEGIN [Gen AI SDK]]

To begin the consumption process for the orchestration you’ve deployed, follow the process below: 

**Prepare the CV File**

- Download the [cv.txt](img/cv.txt) file, which contains the CV data that will be used in this use case. 

- Place the cv.txt file in the same folder where you have created your .ipynb file. 

- Load the CV file using the following code to read its content

```CODE

from gen_ai_hub.orchestration.utils import load_text_file 

# Load the CV file content 
cv_file_path = "cv.txt"  # Specify the correct path to the CV file 
cv_content = load_text_file(cv_file_path) 

# Print the content to verify it has been loaded 
print(cv_content) 

```

The next step involves creating a template that specifies how the AI should handle the resume content. The template will include both SystemMessage and UserMessage components. 

• SystemMessage: Defines the AI assistant's role and instructions. 

• UserMessage: Represents the user's input (i.e., the CV content) to be processed by the AI. 

```CODE

from gen_ai_hub.orchestration.models.message import SystemMessage, UserMessage 
from gen_ai_hub.orchestration.models.template import Template, TemplateValue 

# Define the template for resume screening 
template = Template( 
    messages=[ 
        SystemMessage("""You are a helpful AI assistant for HR. Summarize the following CV in 10 sentences,                      focusing on key qualifications, work experience, and achievements. Include personal contact information,  
                      organizational history, and personal interests"""), 

        UserMessage( 
            "Here is a candidate's resume: {{?candidate_resume}}" 
        ), 
    ], 

    defaults=[ 
        TemplateValue(name="candidate_resume", value="John Doe's resume content goes here..."), 
    ], 
) 

```

We can define multiple models for the use case. Only use those models that are already deployed in your instances. For this example, we have selected the following three models:

Here’s an example of how to configure them:

```CODE

from gen_ai_hub.orchestration.models.llm import LLM 
from gen_ai_hub.orchestration.models.config import OrchestrationConfig 

# List of models to use 
models = [ 
    LLM(name="gpt-4o", version="latest", parameters={"max_tokens": 1000, "temperature": 0.6}), 
    LLM(name="mistralai--mistral-large-instruct", version="latest", parameters={"max_tokens": 1000, "temperature": 0.6}), 
    LLM(name="anthropic--claude-3-sonnet", version="latest", parameters={"max_tokens": 1000, "temperature": 0.6}), 
] 

# Create configurations for each model 
configs = [] 
for model in models: 
    # Create orchestration config for each model 
    config = OrchestrationConfig( 
        template=template,   
        llm=model,   
    ) 
    configs.append(config) 

print("Model configurations created successfully:") 

```

**Note:** Make sure that the models you choose are already deployed in your instance. You can replace the models in this example with any other models from your deployment as required. 

**Execute the Orchestration and Collect Results**

Now, you can run the orchestration with the prepared configurations. The results will be saved in a text file for later review.

```CODE

from gen_ai_hub.orchestration.service import OrchestrationService 

# Initialize an empty list to store the responses 
responses = [] 

# Iterate through each config and get the response using the filtered input 
for i, config in enumerate(configs): 
    orchestration_service = OrchestrationService(api_url=ready_deployment.deployment_url, config=config) 

    # Run orchestration with the provided input (for example, candidate resume content) 
    result = orchestration_service.run(template_values=[ 
        TemplateValue(name="candidate_resume", value=cv_content)  # Adjust 'cv_content' as needed 
    ]) 

    # Extract the response content 
    response = result.orchestration_result.choices[0].message.content 

    # Append the response to the responses list 
    responses.append({ 
        "model": models[i].name,  # Store model name 
        "response": response      # Store the corresponding model response 
    }) 

# Store the responses in a text file 
with open("model_responses.txt", "w") as file: 
    for response_data in responses: 
        file.write(f"Response from model {response_data['model']}:\n") 
        file.write(f"{response_data['response']}\n") 
        file.write("-" * 80 + "\n")  # Add a separator between model responses 

```

After running the orchestration, a model_responses.txt file will be generated in the same folder. This file will contain the responses from all the models you used, each separated by a line for clarity.  

**Important Note** 

Ensure at least one orchestration deployment is ready to be consumed during this process.  

**Optional Advanced Modules**

Data masking and content filtering are available to enhance data privacy and safety. Data masking hides sensitive information like phone numbers or organization names, while content filtering can screen for categories such as hate self-harm, sexual content, and violence. In this tutorial, the response generated by the LLM models may carry sensitive information, such as names and phone numbers. For further enhancement, refer to the next tutorial on implementing these modules. 

[OPTION END]

[OPTION BEGIN [SAP Cloud SDK ]]

To begin the consumption process for the orchestration you’ve deployed, follow the process below: 

**Prepare the CV File**

- Download the [cv.txt](img/cv.txt) file, which contains the CV data that will be used in this use case. 

- Place the cv.txt file in the same folder where you have created your .ipynb file. 

- Load the CV file using the following code to read its content

```CODE

const filePath = './cv.txt'; 
let txtContent;  
try { 
     txtContent = await Deno.readTextFile(filePath); 
    console.log(txtContent); 
} catch (error) { 
    console.error('Error reading the file:', error); 
} 
console.log(txtContent); 

```

The next step involves creating a template that specifies how the AI should handle the resume content. The template will include both SystemMessage and UserMessage components. 

• SystemMessage: Defines the AI assistant's role and instructions. 

• UserMessage: Represents the user's input (i.e., the CV content) to be processed by the AI. 

```CODE

// Define the template for resume screening 

const templateConfig = { 
  templating: { 
    template: [ 
      { 
        role: 'system', 
        content: 'You are an AI assistant designed to screen resumes for HR purposes. Please assess the candidate qualifications based on the provided resume.', 
      }, 
      { 
        role: 'user', 
        content: 'Candidate Resume:\n{{?candidate_resume}}', 
      }, 
    ], 
  }, 
}; 
console.log('Resume screening template configuration defined successfully.'); 

```

We can define multiple models for the use case. Only use those models that are already deployed in your instances. For this example, we have selected the following three models

Here’s an example of how to configure them:

```CODE

// List of models to iterate through 
const models = [ 
    'gpt-4o', 
    'mistralai--mistral-large-instruct', 
    'anthropic--claude-3.5-sonnet', 
  ]; 

```

```CODE

 // Function to create configuration for each model 
const createModelConfig = (modelName) => ({ 
  llm: { 
    model_name: modelName, 
    model_params: { 
      max_tokens: 1000, 
      temperature: 0.6, 
    }, 
  }, 
  ...templateConfig, 
}); 

const deploymentConfig = { 
  resourceGroup: 'default', 
}; 

```

**Generate Responses for Multiple Models** 

This step outlines the process of generating responses for a set of queries using different models. The generateResponsesForModels function iterates through each model and executes queries to gather AI-generated responses. 
 
**Key Points:**
 
Model Iteration: Iterates over the list of model names to update the configuration dynamically. 

Query Execution: Uses OrchestrationClient to generate responses for each query. 

```CODE

import { writeFileStrSync } from "https://deno.land/std@0.52.0/fs/mod.ts"; 
import {OrchestrationClient} from '@sap-ai-sdk/orchestration'; 

// Function to generate responses from multiple models 
async function generateResponsesForModels(txtContent) { 
    const responses = []; 
    for (const modelName of models) { 
      console.log(`\n=== Responses for model: ${modelName} ===\n`);

      // Create configuration for the current model 
      const modelConfig = createModelConfig(modelName); 
      
      // Initialize OrchestrationClient with dynamic model configuration 
      const orchestrationClient = new OrchestrationClient({ 
        ...deploymentConfig, 
        ...modelConfig, 
      }); 
      try { 
        // Run orchestration with the provided input (candidate resume content) 
        const response = await orchestrationClient.chatCompletion({ 
          inputParams: { candidate_resume: txtContent }, 
        }); 
        // Extract the response content 
        const content = response.getContent(); 
        console.log(`Response from ${modelName}:\n`, content); 

        // Store the response in the list 
        responses.push({ 
          model: modelName, 
          response: content, 
        }); 
      } catch (error) { 
        console.error(`Error with model ${modelName}:`, error.response?.data || error.message); 
      } 
    } 
    // Optionally save the responses to a file (similar to Python code) 
    await writeFileStrSync( 
      'model_responses_js.txt', 
      responses 
        .map((res) => `Response from model ${res.model}:\n${res.response}\n${'-'.repeat(80)}\n`) 
        .join(''), 
      'utf-8' 
    ); 
} 

  // Example usage with resume content 
  generateResponsesForModels(txtContent); 

```

**Important Note**

Ensure at least one orchestration deployment is ready to be consumed during this process.  

**Optional Advanced Modules**

Data masking and content filtering are available to enhance data privacy and safety. Data masking hides sensitive information like phone numbers or organization names, while content filtering can screen for categories such as hate self-harm, sexual content, and violence. In this tutorial, the response generated by the LLM models may carry sensitive information, such as names and phone numbers. For further enhancement, refer to the next tutorial on implementing these modules. 

[OPTION END]

[OPTION BEGIN [Postman]]

**Request Setup:**

• Request Type: POST 

• URL: https://$ORCH_DEPLOYMENT_URL/completion 
                
(Replace $ORCH_DEPLOYMENT_URL with the actual deployment URL) 

**Headers:**

Add the following headers in Postman: 

• content-type: application/json 

• ai-resource-group: <RESOURCE_GROUP> 

(Replace <RESOURCE_GROUP> with the actual resource group value) 

**Body:**

• Select raw as the input type. 

• Set the format to JSON. 

Copy and paste the following JSON configuration: 

```CODE
{ 
    "orchestration_config": { 
        "module_configurations": { 
            "llm_module_config": { 
                "model_name": "gpt-4o", 
                "model_params": {}, 
                "model_version": "2024-05-13" 
            }, 

            "templating_module_config": { 
                "template": [ 
                    { 
                        "role": "system", 
                        "content": "You are an AI assistant designed to screen resumes for HR purposes. Please assess the candidate qualifications based on the provided resume." 
                    }, 
                    { 
                        "role": "user", 
                        "content": "Candidate Resume:\n{{?candidate_resume}}" 
                    } 
                ], 

                "defaults": { 
                    "candidate_resume": "John Doe\n1234 Data St, San Francisco, CA 94101\n(123) 456-7890\njohndoe@email.com\nLinkedIn Profile\nGitHub Profile\n\nObjective\nDetail-oriented Data Scientist with 3+ years of experience in data analysis, statistical modeling, and machine learning. Seeking to leverage expertise in predictive modeling and data visualization to help drive data-informed decision-making at [Company Name].\n\nEducation\nMaster of Science in Data Science\nUniversity of California, Berkeley\nGraduated: May 2021\n\nBachelor of Science in Computer Science\nUniversity of California, Los Angeles\nGraduated: May 2019\n\nTechnical Skills\n\nProgramming Languages: Python, R, SQL, Java\nData Analysis & Visualization: Pandas, NumPy, Matplotlib, Seaborn, Tableau\nMachine Learning: Scikit-learn, TensorFlow, Keras, XGBoost\nBig Data Technologies: Hadoop, Spark\nDatabases: MySQL, PostgreSQL\nVersion Control: Git\n\nProfessional Experience\n\nData Scientist\nDataCorp Inc., San Francisco, CA\nJune 2021 – Present\n\nDeveloped predictive models to optimize marketing campaigns, which increased ROI by 20%.\nConducted in-depth data analysis using Python and SQL to identify trends and patterns in large datasets.\nCollaborated with cross-functional teams to implement data-driven strategies that improved customer satisfaction scores by 15%.\nCreated interactive dashboards using Tableau to visualize KPIs for stakeholders.\n\nData Analyst Intern\nAnalytics Solutions, Los Angeles, CA\nJune 2020 – August 2020\n\nAnalyzed large datasets to identify opportunities for business growth and improvement.\nAssisted in the development of automated reporting tools using Python and Excel.\nWorked with data visualization tools to create insightful reports for management.\n\nProjects\n\nCustomer Segmentation Analysis\nConducted K-means clustering on customer data to segment the customer base into distinct groups, enabling targeted marketing strategies.\n\nPredictive Stock Price Modeling\nBuilt a predictive model using time series analysis to forecast stock prices, achieving an accuracy rate of 85%.\n\nSentiment Analysis on Social Media\nImplemented natural language processing techniques to analyze sentiment from tweets, providing insights into public opinion on various topics.\n\nCertifications\n\nCertified Data Scientist (CDS) – Data Science Council of America\nMachine Learning Specialization – Coursera by Stanford University\n\nProfessional Affiliations\n\nMember, Association for Computing Machinery (ACM)\nMember, Data Science Society\n\nReferences\nAvailable upon request.\n\nPersonal Interests\n- I absolutely love exploring new technologies and working on innovative projects.\n- I enjoy reading books, especially on artificial intelligence and machine learning.\n- I hate people who are dishonest and unreliable.\n- I love traveling and experiencing new cultures.\n- I enjoy playing video games, especially competitive ones.\n- I hate being stuck in a routine; I always seek new challenges and growth opportunities.\n-I hate working in Azure cloud -\"Azure cloud is the most irritating platform I have ever used\"\n" 
                } 
            } 
        } 
    } 
} 

```

**Execution**

• Click Send in Postman to execute the request 

• Review the response in the Body section 

Refer to the attached screenshot for reference. 

![img](img/image018.png)

**Important Note**

Ensure at least one orchestration deployment is ready to be consumed during this process. 

**Optional Advanced Modules**

Data masking and content filtering are available to enhance data privacy and safety. Data masking hides sensitive information like phone numbers or organization names, while content filtering can screen for categories such as hate self-harm, sexual content, and violence. In this tutorial, the response generated by the LLM models may carry sensitive information, such as names and phone numbers etc.. For further enhancement, refer to the next tutorial on implementing these modules. 

[OPTION END]