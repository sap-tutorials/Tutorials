---
title: Train Object Detection model with SAP Computer Vision Package
description: Train Object Detection model with SAP Computer Vision package on SAP AI Core to read Electricty Meters automatically using AI
auto_validation: true
time: 30
tags: [ tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, software-product>sap-ai-core, tutorial>license]
primary_tag: topic>artificial-intelligence
author_name: Kannan Presanna Kumar
author_profile: https://github.com/kannankumar
---

## Prerequisites
 - You have completed the tutorial [Setup SAP Computer Vision package and its dependencies](cv-package-aicore-setup)
 - You have [set up your Git Repository with SAP AI Core](https://help.sap.com/viewer/808d9d442fb0484e9b818924feeb9add/LATEST/en-US/3269092e37d141a293f0dbd7eaafc829.html)
 - You have [created docker registry secret in SAP AI Core](https://help.sap.com/viewer/2d6c5984063c40a59eda62f4a9135bee/LATEST/en-US/b29c7437a54f46f39c911052b05aabb1.html)


## Details

### You will learn
  - How to use the Command Line Interface of AI Core SDK for Content Packages
  - How to create Docker images using SAP Computer Vision package
  - How to create Templates using SAP Computer Vision package.
  - How to execute the training template on SAP AI Core


---
[ACCORDION-BEGIN [Step 1: ](Explore Computer Vision content package from Command Line)]
The `ai-core-sdk` you installed comes with a command line interface to discover content packages (like the Computer Vision package). You can use that to explore the contents of the CV package.

To be able to use it, let's open a new Terminal and activate the `sap_cv_env` virtual environment you created previously.

```Shell
source sap_cv_env/bin/activate
```
Then you can use the command line interface which can be called using the command `aicore-content`. The `list` subcommand lists all the content packages available in this environment:
```Shell
aicore-content list
```

You should see list of content packages available like this:
!![image](img/aicore-cli-list-packages.png)

Currently you only have the `sap-cv` content package installed.

If you pass content package name (`sap-cv`) to `list` subcommand, it shows all the pipelines available.

```Shell
aicore-content list sap-cv
```

You would see a list of pipelines relevant for Computer vision tasks here:
!![image](img/aicore-cli-list-workflows.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select a Pipeline to use)]
You can also explore the available pipelines using the `ai-core-sdk` python package.

To see the list of available content packages, run the following snippet in a new cell in Jupyter notebook:

```Python
from ai_core_sdk.content import get_content_packages

pkgs = get_content_packages()

for pkg in pkgs.values():
    print(pkg)
```
Screenshot below shows how the snippet outputs `ContentPackage` objects available in the current environment:
!![image](img/aicore-sdk-list-packages.png)

To see a list of available pipelines in the `sap-cv` content package, run the following snippet in a new Jupyter cell:

```Python
sap_cv_pkg = pkgs['sap-cv']

for workflow in sap_cv_pkg.workflows.values():
    print(workflow)
```
Here it shows all the available `Workflow` objects with their `name` and `type` (either for Execution and Deployment).
!![image](img/aicore-sdk-list-workflows.png)

> Details: The `Execution` workflows use `Metaflow` based python scripts and the `Deployment` workflows use a `YAML` configuration.

Now that you have explored the pipelines available in this content package, let's select a pipeline for our tutorial.
You'll be using the `object-detection-train` pipeline to train a Object Detection model for Meter Reading.

Run the following line of code in a new cell to use the `object-detection-train` pipeline:
```Python
workflow = sap_cv_pkg.workflows['object-detection-train']
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create user specific config)]

To allow `sap-cv` package to automatically generate docker image and templates for us, you need to maintain a configuration file. This configuration file contains details that would be used for creation of docker image and template in next steps.

Copy this format below into a cell on Jupyter notebook and fill your details as explained below:

```Python
workflow_config = {
   'name': 'sap-cv-package-tutorial-obj-detection-train',
   'image': '<YOUR_DOCKER_USERNAME>/sap-cv-package-object-detection-train:0.0.1',
   'labels': {
       'scenarios.ai.sap.com/id': "sap-cv-package-tutorial",
       'ai.sap.com/version': "0.0.1"
   },
   "annotations": {
       "scenarios.ai.sap.com/name": "SAP CV Package Tutorial",
   },
   "imagePullSecret": "docker-registry-secret",
   "objectStoreSecret": "default-object-store-secret"
}
```

 - `name` : Name of workflow. Can be a string with alphanumeric characters and underscores.

 - `image`: The docker image you'll use for the tutorial. You can use Docker Hub URL like this `<YOUR_DOCKER_USERNAME>/sap-cv-package-object-detection-train:0.0.1`.

 - `labels.scenarios.ai.sap.com/id` : Name of the Machine Leaning scenario. Can be any valid string. Should be unique on this AI Core instance.

 - `labels.ai.sap.com/version` : Scenario version. Can be any versioning (example: "0.0.1")

 - `annotations.scenarios.ai.sap.com/name` : Descriptive name of the scenario. Can have spaces

 - `imagePullSecret` : Secret used to access the docker repository. Should match with the registered secret name on AI Core

 - `objectStoreSecret` : Secret used to access the Object Store (either from BTP/S3). Should match with the registered object store secret name on AI Core.

> Please add `-object-store-secret` to the name of your Object store secret when adding to the configuration file.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create Docker Image)]
Next you create the docker image that will be used as an execution environment for our Training.

`sap-cv` content package automatically generates the Docker image required for the selected Workflow with the required dependencies included.

Run the below snippet in Jupyter notebook cell to create the docker image:
```Python
workflow.create_image(workflow_config, silent=True)
print(f'!docker push {workflow_config["image"]}')
```
!![image](img/docker-generate-image.png)

This will build the required docker image for the workflow with the `image` tag specified in the workflow configuration in the previous step.

Now with the docker image build you need to push it to our docker repository so AI Core can fetch it during execution. You can run the command mentioned in the output above inside a new cell. Make sure to add a `!` before the command when running it in a Jupyter cell like this:
```
!docker push <YOUR_DOCKER_USERNAME>/sap-cv-package-object-detection-train:0.0.1
```
!![image](img/docker-push.png)


> Alternative to using the python interface you can also build the docker using Command Line interface like this: `aicore-content create-image -p <CONTENT_PACKAGE> -w <WORKFLOW> <WORKFLOW_CONFIG.YAML>`

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create AI Core Template)]


The `sap-cv` content package fetches the details from the workflow configuration (details of workflow, scenario, docker repo secrets, object store secrets, etc) and automatically generates a template for the selected workflow. You are using the `object-detection` workflow in this tutorial, so you generate an workflow for training an object detection model.

```Python
output_file = '<GitOps_Repo_Path>/sap-cv-package-tutorial-obj-detection-train.json'
```
Replace `<GitOps_Repo_Path>` with the absolute path to the directory that contains your repository on-boarded on AI Core. This repository syncs using Git Ops.

Run the below snippet to create the workflow template:
```python
workflow.create_template(workflow_config, output_file, silent=True)
```
!![image](img/workflow-template-generate.png)

You now need to commit and push this generated template to the Git repository on-boarded on AI Core.

You can generate commands required for committing and pushing your template to git repo using the below snippet. Run the below snippet:
```Python
import pathlib

print(f'''Run in Terminal:
cd <repository_path>
git add <path_within_repo>/{pathlib.Path(output_file).name}
git commit -m \'updated template {workflow_config["name"]}\'
git push
''')
```

Replace the parts of command within `< >` with specified paths inside the Git repo and run the commands one by one in a terminal.

> Alternative to using the python interface you can also build the docker using Command Line interface like this: `aicore-content create-template -p <CONTENT_PACKAGE> -w <WORKFLOW> <WORKFLOW_CONFIG.YAML> -o <OUTPUT_TEMPLATE_FILE.JSON`



> ### What's going on?
Once the template is pushed into the Git repo, you need to wait for AI Core to sync with this repository. AI Core syncs with the on-boarded Git repositories at periodic intervals. Once the template is synced with AI Core you can execute the template to start training.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Download data from public datasource)]

You will be using a dataset which contains images of electricity meters. The Machine Learning model will attempt to read the meters using Object Detection.

Run the below snippet in a Jupyter notebook cell to download the dataset.
```Shell
! [ -d "MeterDataset" ] && echo "skipping" || (wget -nc --no-check-certificate "http://artelab.dista.uninsubria.it/downloads/datasets/automatic_meter_reading/gas_meter_reading/gas_meter_reading.zip" && unzip -qq gas_meter_reading -d .)
```

!![image](img/dataset-download.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Upload Data to Object Store)]

You need to upload the data to the on-boarded object store so AI Core can fetch it to run training.

Add the path to object store bucket (eg-S3 bucket) where you want to upload the dataset to. This object store should be the same one whose secret you've used in previous tutorials.

```Python
s3_target = "s3://<YOUR_S3_BUCKET>/meter-reading/Rough-Digit-Classification"
# Example: s3://hcp-787c1894-e893-4c8edf-b406-440347f6b411/kannan/meter-reading/Rough-Digit-Classification

! mv MeterDataset/Rough-Digit-Classification/JPEGImages MeterDataset/Rough-Digit-Classification/Images

! aws s3 cp --recursive --quiet MeterDataset/Rough-Digit-Classification {s3_target}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Register dataset as artifact on AI core)]

Copy the snippet and update the values in `< >` with your own values as described below:

- `ai_core_path` - Name of the dataset artifact to be created on AI Core. It follows the format: `ai://<object-store-secret-name>/<path-to-artifact>`.

- `artifact_name` - Name of the artifact used for registering.


```Python
from ai_core_sdk.models import Artifact

ai_core_path =  'ai://default/meter-reading/Rough-Digit-Classification'
# example of artifact-path: 'ai://default/meter-reading/Rough-Digit-Classification'

artifact_name = 'tutorial-dataset-meter-reading-digits'

try:
    artifact = [r for r in ai_api_client.artifact.query().resources if r.name == artifact_name][0]
    print('Found artifact')
except IndexError:
    print('Artifact Created')
    artifact = ai_api_client.artifact.create(
        name=artifact_name,
        scenario_id=workflow_config["labels"]["scenarios.ai.sap.com/id"],
        kind=Artifact.Kind.DATASET,
        url=ai_core_path,
        description='Meter Reading Digits Dataset')

```

The above snippet checks if an artifact of same name is already registered on AI Core.
If it is already registered it will return it instead of recreating it. If the artifact does not exist it creates a new artifact.


!![image](img/create-artifact.png)

The following details are used for creation of the artifact:

 - `name`: artifact name used for recognizing it.

 - `scenario_id`: It is fetched from the workflow configuration you created previously

 - `kind`: specifies the type of the artifact. Here it is of type `DATASET`. Artifacts could also be of `MODEL` type as you will see later when you register our trained model as an artifact.

 - `uri`: refers to the path where the artifact should be registered. This path would be used in our training script


[VALIDATE_6]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Set Parameters for the Template)]

Run the below snippet in Jupyter notebook cell to create a `params` object with Parameter Bindings you'll be using later for creating our execution configuration.
```Python
from ai_core_sdk.models import ParameterBinding

params = [
    ParameterBinding("baselr", "0.001"),
    ParameterBinding("earlystopping", "False"),
    ParameterBinding("ntrainsteps", "5000"),
    ParameterBinding("train", "0.8"),
    ParameterBinding("validation", "0.1"),
    ParameterBinding("test", "0.1")
]
```
These are parameters that affect the training of our machine learning model for object detection.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Create Configuration for Execution)]

Run the below code snippet in a Jupyter notebook cell. You can leave all fields as is and only replace the `<config-name>` with any meaningful name for configuration.
```Python
from ai_core_sdk.models import InputArtifactBinding

config_name = 'tutorial-sapcv-meter-reading'
# example config name 'sapcv-tutorial-meter-reading'

try:
    configuration = [r for r in ai_api_client.configuration.query().resources if r.name == config_name][0]
    print('Found configuration')
except IndexError:
    print('Configuration Created')
    configuration = ai_api_client.configuration.create(
        name=config_name,
        scenario_id=workflow_config["labels"]["scenarios.ai.sap.com/id"],
        executable_id=workflow_config["name"],
        input_artifact_bindings=[InputArtifactBinding('datain', artifact.id)],
        parameter_bindings=params
    )
```

The above snippet checks if a configuration of the same name already exists in AI Core. If it exists, it will use the existing configuration.

!![image](img/create-configuration.png)

If it does not exist, it will create a new configuration using the following details:

- `name` : Name of configuration used to recognize it

- `scenario_id`: Connect the configuration to the scenario id mentioned previously in our workflow configuration.

- `executable_id`: Name of the workflow mentioned in workflow configuration.

- `input_artifact_binding` - Binds the dataset artifact you created in previous tutorial and binds it as an Input Artifact for this workflow. [TODO: Explain succinctly]

- `parameter_bindings` - Parameters for our Machine learning training you set in previous step.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ](Create Execution)]

Creating an execution is just a single line as shown below.
It uses the configuration you created above.

Run this line in a Jupyter notebook cell:
```Python
execution = ai_api_client.execution.create(configuration.id)
```

Wait for the training to finish.
You can use the below snippet to check if training has finished
```python
from ai_core_sdk.models import Status

if 'execution' not in locals():
    execution_id = input('Restarting this Notebook again? Provide ExecutionID: ')
else:
    execution_id = execution.id
execution = ai_api_client.execution.get(execution_id)
if execution.status == Status.COMPLETED:
    trained_model = execution.output_artifacts[0]
    print('Training finished!')
else:
    trained_model = None
    print('Training not finished!')
```
This snippet checks if the execution variable is present in notebook environment. If it is present it checks for the status of this execution.

!![image](img/create-execution.png)

> **CAUTION:** Do not close this Jupyter Notebook, you'll continue here once the training is finished.
You can open the SAP AI Launchpad to monitor the training execution in the next step. Come back to this notebook once the execution is in Completed state.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Monitor Training Execution)]
Open up the **SAP AI Launchpad**, select the appropriate **Workspace** and **Resource Group** and navigate to the **Executions** tab.

You can see that a new execution is shown in the list with configuration you created. You can verify that the Current Status is either `PENDING` or `RUNNING`.
!![image](img/execution-1-started.png)

Navigate to the details of this execution. You can see further detailed information about the execution. It shows the entire pipeline with the name of the Executable, Artifact used and Configuration.
!![image](img/execution-2-started-process-flow.png)

The `Logs` tab shows the logs from the executing pipeline.
!![image](img/execution-3-started-pipelinelogs.png)


Once the model training has started you can see the loss metrics of the Computer Vision model in the logs and also notice the estimated time to completion of training under `eta`.
!![image](img/execution-4-running-pipelinelogs.png)


Once training is completed, a trained model would show up under **Output Artifacts**. As part of the pipeline, this model would get stored in the object store. The template generated by SAP Computer Vision package handles all this behind the scenes for you.

!![image](img/execution-5-completed.png)

You can now rerun the previous cell. It would fetch the trained model from the output artifacts of the execution and now mention `Training Completed`.

!![image](img/execution-6-training-finished.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 13: ](Recap)]

As a recap, in this tutorial, you have looked at creating Docker images and templates for our computer vision scenario.
Docker images and templates need to be created for content onboarding on AI Core.

**Cheat sheet:**  

Build docker image:  

- **`Python`**: `workflow.create_image(workflow_config)`

- **`CLI`**: `ai-core-content create-image <workflow_config_file>`  

Create Templates:  

- **`Python`**: `workflow.create_template(workflow_config, out_file)`  

- **`CLI`**: `ai-core-content create-template workflow_config out_file`  

Everything else is standard AI Core usage.

[DONE]
[ACCORDION-END]
---
