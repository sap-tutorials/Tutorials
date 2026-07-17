# Generative AI Custom Evaluation Workflow

This notebook demonstrates a workflow for using AI Core's custom evaluation capabilities to benchmark Large Language Models (LLMs), and evaluate different prompts for a specific use case. It utilizes the public [MedicationQA dataset](https://langtest.org/docs/pages/benchmarks/medical/medicationqa/) to showcase how to compute industry-standard metrics and assess the reliability of LLM-generated responses.

## Prerequisites

Before running this notebook, ensure you have the following:

1.  **Python Environment**: A running Jupyter Notebook environment.
2.  **Dependencies**: The required Python packages can be installed by running the pip command in the notebook:
    ```bash
    pip install -r requirements.txt
    ```
3.  **Environment Variables**: Create a `.env` file in the same directory as the notebook. This file should contain your credentials for SAP AI Core and AWS. A `sample.env` file is provided as a template. The notebook will prompt for any missing values.

    Your `.env` file should look like this:
    ```
    # SAP AI Core Credentials
    AICORE_BASE_URL=<YOUR_AICORE_BASE_URL>
    AICORE_RESOURCE_GROUP=<YOUR_AICORE_RESOURCE_GROUP>
    AICORE_AUTH_URL=<YOUR_AICORE_AUTH_URL>
    AICORE_CLIENT_ID=<YOUR_AICORE_CLIENT_ID>
    AICORE_CLIENT_SECRET=<YOUR_AICORE_CLIENT_SECRET>

    # AWS Credentials
    AWS_ACCESS_KEY=<YOUR_AWS_ACCESS_KEY>
    AWS_BUCKET_ID=<YOUR_AWS_BUCKET_ID>
    AWS_REGION=<YOUR_AWS_REGION>
    AWS_SECRET_ACCESS_KEY=<YOUR_AWS_SECRET_ACCESS_KEY>

    # Optional Orchestration Deployment URL
    DEPLOYMENT_URL=<YOUR_EXISTING_DEPLOYMENT_URL>
    ```

## Workflow Overview

The notebook is structured into the following key steps:

### Step 1: Setup

*   **Install Dependencies**: Installs the necessary Python packages from `requirements.txt`.
*   **Load Credentials**: Loads the necessary credentials and configuration from the `.env` file. It initializes the `GenAIHubProxyClient` for interacting with SAP AI Core.

### Step 2: Prepare for Evaluation

This section involves preparing all the necessary assets for the evaluation run.

1.  **Register Object Store Secret**: Registers your AWS S3 bucket credentials with SAP AI Core. This allows the evaluation job to access your dataset.

An [object store secret](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/register-your-object-store-secret) is required to store credentials to access your AWS S3 buckets, and limit access to a particular directory.
User needs to select a resource group while creating secret.
To read more about resources groups visit: [Resource Group](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/resource-groups)

The API endpoint can be found in : [Object store secret endpoint](https://api.sap.com/api/AI_CORE_API/resource/Object_Store_Secret)
```
    {   
        "name": "genai-data-notebook",
        "data": {
            "AWS_ACCESS_KEY_ID": AWS_ACCESS_KEY,
            "AWS_SECRET_ACCESS_KEY": AWS_SECRET_ACCESS_KEY
        },
        "type": "S3",
        "bucket": AWS_BUCKET_ID,
        "endpoint": "https://s3.aws.com",
        "region": AWS_REGION,
        "pathPrefix": ""    
    }
```

2.  **Upload Data to S3**: Uploads the local dataset from `DATASET` to your S3  object store and registers the root folder as artifact with AI Core. The File Upload and Artifact endpoints of AI Core API may be used for this purpose. In this example `genaiEvaluation\{prefix_guid}` is the root folder containing the orchestration configurations and test data which is registered as AI Core artifact.

3.  **Register Artifact with AI Core**: Registers the uploaded dataset in S3 as an artifact in SAP AI Core. This makes the data accessible to the evaluation workflow.
The input [artifact](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/manage-artifacts) is a placeholder in an executable or template that enables the attachment of datasets or models required for the execution of an AI workflow or pipeline. 
To register an artifact with AI Core
    - Upload the input files to the path specified in the object store.
    - Register an artifact with AICore by providing the path to the input artifact

The API endpoint can be found in : [Register Artifact](https://api.sap.com/api/AI_CORE_API/resource/Artifact)
```
    {          
        "labels": [
            {
            "key": "ext.ai.sap.com/prompt-evaluation",
            "value": "true"
            }
        ],
        "name": "genai-eval-simplified-test-data",
        "kind": "other",
        "url": input_artifact_path, # input artifact path
        "description": "demo artifacts for evaluation flow.",
        "scenarioId": "genai-evaluations"
    }
```
    - The url needs to be constructed as ai://genai-data-notebook/genaiEvaluation/{prefix_guid}, where genai-data-notebook is the object store secret name created previously. Hence the path translates as ai://genai-data-notebook/<PATH_TO_DATASET> which is the directory that your dataset file is located in.
    - The url points to a directory, not a file, which gives you advantage that you can store multiple files in an AWS S3 directory and register the directory containing all files as a single artifact.
    - All the files present in the path referenced by artifact will be copied from your S3 storage to your SAP AI Core instance during training or inferencing. This includes subfolders, apart from where Kind = MODEL.
    - The scenario Id here referes to the Global Workflow already present in AI Core.


4.  **Create Orchestration Deployment**: If you don't have an existing orchestration deployment, this step creates one. The deployment provides the endpoint for running the LLM.


5.  **Select Metrics**: You can select from a list of system-defined metrics (e.g., ROUGE, BERT Score, Answer Relevance) and/or register your own custom metrics through the notebook.

The following **system-defined computed metrics** are supported:

| Name                     | Description                                                   | Reference required |
---------------------------------------------------------------------------------------------|------------------|
| BERT Score               | https://huggingface.co/spaces/evaluate-metric/bertscore                                                                                    | Yes              |
| BLEU                     | https://huggingface.co/spaces/evaluate-metric/bleu                                                                                         | Yes              |
| ROUGE                    | https://huggingface.co/spaces/evaluate-metric/rouge                                                                                        | Yes              |
| JSON Schema Match        | validates LLM generated response against a predefined Json schema, returns boolean result.                                                                                      | Yes              |
| Content Filter on Input  | Whether orchestration input was rejected by the input filter                                                                                       | No               |
| Content Filter on Output | Whether orchestration output was rejected by the output filter                                                                                       | No               |
| Exact Match              | Whether the output exactly matches the reference                                                                                    | Yes              |
| Language Match           | The metric returns true/false to indicate if the text matches the given language                                                                                     | No               |


The following **system-defined model-as-a-judge metrics** are supported:

| Name                            | Description                                              | Reference required |--------------------------------------------------------------------------------------------------------------------
| Pointwise Instruction Following | assess the model's ability to follow instructions provided in the user prompt                                                                                       | No               |
| Pointwise Correctness          | assess the model's ability to provide a correct response based on the user prompt                                                                                       | Yes              |
| Pointwise Answer Relevance     | assess the model's response is related to user prompt                                                                                       | No               |
| Pointwise Conciseness          | assess the model's response is a short and concise answer to user prompt                                                                                       | No               |
| 
*Entries marked with an asterisk (*) are experimental metrics.



## Model-as-a-Judge metrics internally follow this template:
<details>
<summary><b>Pointwise Instructions Following prompt template:</b></summary>

```text
Please act as an impartial judge and evaluate the quality of the responses based on the prompt and following criteria:

## Metric Definition
You will be assessing model's the ability to follow instructions provided in the user prompt.

## Criteria
Instruction following: The response demonstrates a clear understanding of the instructions in the user prompt, satisfying all of the instruction's requirements.
Evaluate the responses STRICTLY on the ability to follow instruction ONLY.

## Rating Rubric
5: (Complete fulfillment). Response addresses all aspects and adheres to all requirements of the instruction. The user would feel like their instruction was completely understood.
4: (Good fulfillment). Response addresses most aspects and requirements of the instruction. It might miss very minor details or have slight deviations from requirements. The user would feel like their instruction was well understood.
3: (Some fulfillment). Response does not address some minor aspects and/or ignores some requirements of the instruction. The user would feel like their instruction was partially understood.
2: (Poor fulfillment). Response addresses some aspects of the instruction but misses key requirements or major components. The user would feel like their instruction was misunderstood in significant ways.
1: (No fulfillment). Response does not address the most important aspects of the instruction. The user would feel like their request was not at all understood.


User Prompt:
{{?aicore_prompt_template}}

Model Response:
{{?aicore_llm_completion}}

Begin your evaluation by providing a short explanation. Be as unbiased as possible. After providing your explanation, please rate the response according to the rubric and outputs STRICTLY following this JSON format:
{
    "explanation": string, 
    "rating": integer
}
Output:
```
</details>

<details>
<summary><b>Pointwise Correctness prompt template:</b></summary>

```text
You are an expert evaluator. Your task is to evaluate the quality of the responses generated by AI models.
We will provide you with the user input, an AI-generated responses and a reference answer.
You should first read the user input carefully for analyzing the task, and then evaluate the quality of the responses based on the criteria provided in the Evaluation section below.
You will assign the response a rating following the Rating Rubric and Evaluation Steps.
Give step-by-step explanations for your rating, and only choose ratings from the Rating Rubric.

## Metric Definition 
You will be assessing correctness, which measures the ability to provide a correct response based on the user prompt and the reference.

## Criteria 
Correctness: Is the response correct, accurate, and factual?

## Rating Rubric 
5: (Completely correct). The response is completely correct, accurate, and factual.
4: (Mostly correct). The response is mostly correct, accurate, and factual.
3: (Somewhat correct). The response is somewhat correct, accurate, and factual.
2: (Somewhat incorrect). The response is somewhat incorrect, inaccurate, and fictitious.
1: (Incorrect). The response is incorrect, inaccurate, and fictitious.

## Evaluation Steps 
STEP 1: Assess the response in aspects of Correctness. Identify any information in the response and provide assessment according to the Criteria.
STEP 2: Score based on the rating rubric. Give a brief rationale to explain your evaluation considering Correctness.

Prompt:
{{?aicore_prompt_template}}

Response:
{{?aicore_llm_completion}}

Reference:
{{?reference}}

Begin your evaluation by providing a short explanation. Be as unbiased as possible. After providing your explanation, please rate the response according to the rubric and outputs STRICTLY following this JSON format: 
{ 
    "explanation": string,  
    "rating": integer 
} 

Output:
```

</details>

<details>
<summary><b>Pointwise Answer Relevance prompt template:</b></summary>

```text
You are an expert evaluator. Your task is to evaluate the relevance of responses generated by AI models.
We will provide you with the user input and an AI-generated response.
You should first read the user input carefully to understand the context and intention, and then evaluate the relevance of the response based on the criteria provided in the Evaluation section below.
You will assign the response a rating following the Rating Rubric and Evaluation Steps.
Give step-by-step explanations for your rating, and only choose ratings from the Rating Rubric.

## Metric Definition
You will be assessing relevance, which measures the ability to provide a response that is pertinent and useful based on the user prompt and the context provided.

## Criteria
Relevance: Does the response address the user's query appropriately and provide pertinent information?

## Rating Rubric
5: (Highly relevant). The response is highly relevant, directly addresses the user's query, and provides useful information.
4: (Mostly relevant). The response is mostly relevant and generally addresses the user's query with useful information.
3: (Somewhat relevant). The response is somewhat relevant but may miss key aspects of the user's query.
2: (Slightly relevant). The response is slightly relevant and largely misses the user's query.
1: (Irrelevant). The response is irrelevant and does not address the user's query.

## Evaluation Steps
STEP 1: Assess the response in terms of Relevance. Identify how well the response aligns with the user's query and context according to the Criteria.
STEP 2: Score based on the rating rubric. Give a brief rationale to explain your evaluation considering Relevance.

Prompt:
{{?aicore_prompt_template}}

Response:
{{?aicore_llm_completion}}

Begin your evaluation by providing a short explanation. Be as unbiased as possible. After providing your explanation, please rate the response according to the rubric and outputs STRICTLY following this JSON format:
{
    "explanation": string,
    "rating": integer
}

Output:
```

</details>

<details>
<summary><b>Pointwise Conciseness prompt template:</b></summary>

```text
You are an expert evaluator. Your task is to evaluate the conciseness of responses generated by AI models.
We will provide you with the user input and an AI-generated response.
You should first read the user input carefully to understand the context and intention, and then evaluate the conciseness of the response based on the criteria provided in the Evaluation section below.
You will assign the response a rating following the Rating Rubric and Evaluation Steps.
Give step-by-step explanations for your rating, and only choose ratings from the Rating Rubric.

## Metric Definition
You will be assessing conciseness, which measures the ability to convey the necessary information in a clear and succinct manner.

## Criteria
Conciseness: Does the response deliver the essential information without unnecessary words or redundancy?

## Rating Rubric
5: (Highly concise). The response is very concise, delivering all necessary information in a succinct manner without any superfluous content.
4: (Mostly concise). The response is mostly concise and generally avoids unnecessary words while covering the essential information.
3: (Somewhat concise). The response is somewhat concise but may include some unnecessary words or slightly redundant information.
2: (Slightly concise). The response is slightly concise and contains a significant amount of unnecessary or redundant information.
1: (Not concise). The response is not concise and is filled with unnecessary or redundant content that obscures the main points.

## Evaluation Steps
STEP 1: Assess the response in terms of Conciseness. Identify how effectively the response communicates essential information without unnecessary words according to the Criteria.
STEP 2: Score based on the rating rubric. Give a brief rationale to explain your evaluation considering Conciseness.

Prompt:
{{?aicore_prompt_template}}

Response:
{{?aicore_llm_completion}}

Begin your evaluation by providing a short explanation. Be as unbiased as possible. After providing your explanation, please rate the response according to the rubric and outputs STRICTLY following this JSON format:
{
    "explanation": string,
    "rating": integer
}

Output:
```

</details>

#### User-defined metrics (Custom metrics)
 
User-defined metrics can be used to evaluate the LLM outputs according to the unique needs of a use case.
A **user-defined llm-as-a-judge metric** uses a judge LLM along with a rubric to compute a metric rating. The output of a llm-as-a-judge metric can be numeric or text. 



The system defines a structure for the judge prompts and users provide the metric definition in the pre-defined format. Relevant instructions, such as output instructions, are automatically added to ensure the desired output from the LLM.

Example definition
```json
{
  "scenario": "genai-evaluations", #required only if metricId is not provided
  "metricName": "my_custom_metric", #required only if metricId is not provided
  "version": "0.0.1", #required only if metricId is not provided
  "type": "structured", # structured .
  "model_configuration": { # model parameters are system-defined for structured prompts. User-defined model parameters will be ignored.
    "model_name": "string",
    "model_version": "string",
  },
  "prompt_configuration": {
    "evaluation_task": "string", #Describe the goal of this evaluation.
    "criteria": "string", #Describe in a one or two sentences how the evaluation is done.
    "rating_rubric": [
      {
        "rating": "number", #Rating is always an integer.
        "rule": "string" #Describe the criteria for choosing this rating.
      },
      ...
    ],
    "include_properties": ["prompt", "reference"], #If present, a variable to hold the value (prompt, reference, etc) will be included.
    "examples": [ #optional, few shot examples to provide context to the judge llm for better results. Ensure that examples cover all ratings for good results. 
      {
        "prompt": "string", #required only if prompt is present in include_properties
        "response": "string", #mandatory
        "reference": "string", #required only if reference is present in include_properties
        "rating": "number", #mandatory
        "explanation": "string", #mandatory, providing this value will improve the response from the judge llm.
      },
      ...
    ],
  }
}
```

The constructed prompt will be:

```
Please act as an impartial judge and evaluate the quality of the responses based on the following criteria:

## Evaluation Task
{{?evaluation_task}} # Example: You will be assessing correctness, which measures the ability to provide a correct response based on the user prompt and the reference.

## Criteria
{{?Criteria}} # Example: Correctness: Is the response correct, accurate, and factual?

## Rating Rubric
5: (Complete fulfillment). Response addresses all aspects and adheres to all requirements of the evaluation criteria. The user would feel like their expectations were completely met.
4: (Good fulfillment). Response addresses most aspects and requirements of the evaluation criteria. It might miss very minor details or have slight deviations from expectations. The user would feel like their expectations were well met.
3: (Some fulfillment). Response does not address some minor aspects and/or ignores some requirements of the evaluation criteria. The user would feel like their expectations were partially met.
2: (Poor fulfillment). Response addresses some aspects of the evaluation criteria but misses key requirements or major components. The user would feel like their expectations were misunderstood in significant ways.
1: (No fulfillment). Response does not address the most important aspects of the evaluation criteria. The user would feel like their expectations were not at all met.

Prompt:
{{?aicore_llm_prompt}}

Response:
{{?aicore_llm_completion}}

Reference:
{{?reference}}

Begin your evaluation by providing a short explanation. Be as unbiased as possible. After providing your explanation, please rate the response according to the rubric and outputs STRICTLY following this JSON format:
{
    "explanation": string,
    "rating": integer
}

Output:
```
**NOTE**: "scenario" and "metricName" and "version" is a required parameter for the custom metric in evaluation configuration.

**NOTE**: The user must provide at least one prompt, system or user prompt, or both prompts can be provided.


6.  **Select Models**: Choose the foundation models you want to evaluate from a list of available models in your AI Core instance.


⚠️ **Model Availability Notice**  
If you are in a region where the `gpt-4.1` model (version `2025-04-14`) is not available, the existing LLM-as-a-Judge metrics evaluation cannot be performed. Currently, the evaluation service relies on this specific model version for metrics computation.


7.  **Create orchestration Registry Config**: Prompt and models to be provided as part of orchestration configuration (Inline Prompt).

Sample Body:
```json
{
      "name": "genai-eval-test",
      "version": "1.0.0",
      "scenario": "genai-evaluations",
      "spec": {
        "modules": {
          "prompt_templating": {
            "model": {
              "name": "model_name",
              "version": "model_version"
            },
            "prompt": {
                    "template": [
                    {
                        "role": "user",
                        "content": "List the benefits and side effects of the drug in the following consumer health question: {{?question}}."
                    }
                ]
            }
          }
        }
      }
    }
```

### Step 3: Start Evaluation Run

*   **Create AI Core Configuration**: A configuration is created that binds together the dataset artifact, the selected models, the chosen metrics, and the prompt template.
After registering input artifacts, we create AI Core configuration using the global executable of genai-evaluations global scenario. 
The evaluation configuration takes the following input parameters which are provided as parameterBindings.

After registering input artifacts, we create AI Core configuration using the global executable of genai-evaluations global scenario. The evaluation configuration takes the following input parameters which are provided as parameterBindings.

| Input parameter | Description |
| --------------- | ----------- |
| orchestrationDeploymentURL | The orchestration deployment to use for calling the LLM. |
| metrics | A string containing comma-separated names of system-defined metrics or scenario/metricName/version for custom metrics to be evaluated. |
| testDataset | JSON containing the path to a test dataset relative to the rootFolder and its file type. |
| orchestrationRegistryIds | The ID of the orchestration config stored in the orchestration registry. |
| tags (Optional) | A JSON containing name-value pairs containing user-defined metadata |
| orchestrationDeploymentURL  | The orchestration deployment to use for calling the LLM |
| repetitions (Optional) | The number of times the same input is submitted to the LLM to evaluate the consistency of the LLM outputs. Should be greater than 1 if specified. Default is 1. |
| testRowCount (Optional) | Specifies the number of rows to be selected from the testDataset for evaluation.


Below is an example of a configuration request body:
The API endpoint for this can be found here: [Configuration Endpoint](https://api.sap.com/api/AI_CORE_API/resource/Configuration) 

request_body = {
    "name": "genai-eval-conf",
    "scenarioId": "genai-evaluations",
    "executableId": "genai-evaluations-simplified",
    "inputArtifactBindings": [
        {
            "key": "datasetFolder",
            "artifactId": artifact_id
        }
    ],
    "parameterBindings": [
        {
            "key": "repetitions",
            "value": repetitions
        },
        {
            "key": "orchestrationDeploymentURL",
            "value": orchestration_deployment_url
        },
        {
            "key": "metrics",
            "value": metrics_list
        },
        {
            "key": "testDataset",
            "value": test_datasets
        },
        {
            "key": "orchestrationRegistryIds",
            "value": orchestration_registry_id
        },
        {
            "key": "debugMode",
            "value": "ON"
        }
    ]
}


*   **Execute Evaluation**: Once Configration is created, we create the AI Core execution which triggers the evaluation workload.
The status of the execution needs to be Completed for the workflow to have succeeded.
The evaluation job produces two outputs
1. A SQLite DB file which stores the orchestration input, orchestration output, values for all the metrics calculated for this orchestration output and statistics such as latency for this orchestration output. These metric values are called raw metric values. This SQLite DB file is stored in the object store as an AI Core output artifact.
2. A set of metrics whose values are aggregated from the raw metric values. The aggregate metrics are stored in the tracking service. The user-defined tags along with the run names are stored with the metrics.
Post execution completion the runs generated by the workload along with the aggregate metrics can be seen by calling the tracking api.

The API endpoints to create an execution and monitor it's status is : [Execution Endpoint](https://api.sap.com/api/AI_CORE_API/resource/Execution).

*   **Monitor Execution**: You can monitor the status of the execution until it is `COMPLETED`.

### Step 4: Analyze Evaluation Results

Once the execution is complete, you can analyze the results.

1.  **Retrieve Aggregate Metrics**: Fetches the high-level, aggregated metrics from the AI Core Tracking service for each evaluation run.The evaluation job will report the following aggregate statistics.

| statistic | description |
| --------- | ----------- |
| average_latency | The average time taken in seconds to get a completion from the orchestration service |
| completion_count | Number of completions evaluated |
| total_prompt_tokens | Sum of prompt_tokens of completions |
| total_completion_tokens | Sum of completion_tokens of completions |

The aggregation metrics can be found by calling the tracking endpoint. There are two ways we can do this:
- By using the execution id from the previous step:

{base_url}/v2/lm/metrics?tagFilters=evaluation.ai.sap.com/child-of={execution_id}

- By using the run name used in the dataset:

{base_url}/v2/lm/metrics?tagFilters=evaluation.ai.sap.com/run-name={run_name}


For further drill down, the output artifacts can be downloaded. The output artifact contains the following information. The results will be stored in aws as an output artifact in the location 
object-store-secret-path/<execution_id>

- folders containing results from each step of the workflow execution. The final result is stored in the sqlite_combined folder in the results.db .
The results.db contains the following tables : 
    | table name | description |
    | --------- | ----------- |
    | runs |Stores the prompt templates provided in the dataset|
    | aggregation_results | Stores the aggregated statistics obtained from the tracking service|
    | completion | Stores response body received based on the provided configuration|
    | submissions |Stores the requests that will be sent to the orchestration service|
    | submissions_results | Stores the output received from the orchestration service |
    | evaluation_results |Stores the computed results after applying evaluation metrics|

- Custom_logs :  Useful for debugging in case of errors.


2.  **Download Raw Results**: Downloads the detailed, instance-level results, which are stored in a SQLite database (`results.db`) in your S3 bucket.

3.  **View Detailed Results**: The notebook provides code to connect to the downloaded SQLite database and display the contents of various tables, including:
    *   `run`: Information about each run.
    *   `configuration`: The configuration used for the run.
    *   `submission` & `submission_result`: Details of the requests and responses to the LLM.
    *   `evaluation_result`: The raw, per-instance metric scores.
    *   `aggregation_result`: The aggregated results for the entire run.
4.  **Process and Rank Results**: The notebook includes scripts to process the raw results further:
    *   It calculates mean and standard deviation for numerical metrics.
    *   It processes categorical and boolean metrics by applying a scoring system.
    *   Finally, it combines all the processed metrics and provides a weighted ranking of the different runs, helping you identify the best-performing model and prompt configuration based on your criteria.
