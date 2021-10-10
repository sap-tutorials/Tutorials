---
title: Creation of Training and Serving Docker Images
description: Learn to build Docker images and orchestrate their execution with for SAP AI Core.
auto_validation: true
time: 20
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, products>sap-business-technology-platform ]
primary_tag: topic>artificial-intelligence
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---


## Details
### You will learn
- How to build docker images for python code
- How to upload docker image to docker hub.
- How set write Workflows to train-serve your model over SAP AI Core
- How to Set Computing Hardware for model using workflows

---

[ACCORDION-BEGIN [Step 1: ](Create ML code and Docker files)]

Docker will be used to store python code in form of containers( *portable environments*).

Create files and **copy provided contents *(see below)*** as-it-is. Resulting directory structure may look like following:  

!![folder structure](img/build-ml/folder.png)

File Links

Training Scripts

| `train/`| Download Link |
| ---  | ----- |
| `Dockerfile` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-clientsdk-workflows/files/train/Dockerfile) |
| `evaluate_scikit.py` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-clientsdk-workflows/files/train/evaluate_scikit.py)
| `requirements.txt` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-cliensdk-workflows/files/train/requirements.txt)
| `train_scikit.py` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-clientsdk-workflows/files/train/train_scikit.py)


Inference/ Serving Scripts

| `infer/` | Download Link |
| -------- | ------------- |
| `Dockerfile` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-clientsdk-workflows/files/infer/Dockerfile)
| `infer_scikit.py` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-clientsdk-workflows/files/infer/infer_scikit.py)
| `requirements.txt` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-cliensdk-workflows/files/infer/requirements.txt)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Convert python to Docker Image)]

Connect your Docker Desktop to the Docker account.

```BASH
docker login docker.io -u <your-docker-username>
```

Type your password *(nothing will appear on screen: just type your password and press enter)*  

!![docker login](img/docker/dd-login.png)

Navigate to place where you have stored the code files.  

!![directory](img/docker/build-1.png)

Navigate inside `train` and build the docker image using the following command to build docker image for training the model.  

```BASH
cd train
docker build -t <your-dockerhub-username>/text-clf-train:0.0.1 .
```

!![docker build train](img/docker/build-2.png)

Similarly navigate inside `infer` folder and execute the following command to build docker image for serving the model.:

```BASH
docker build -t <your-dockerhub-username>/text-clf-serve:0.0.1 .
```

!![docker build infer](img/docker/build-3.png)

### [OPTIONAL] Viewing Local Built Docker images
You can see your local docker images using the command.

```BASH
docker images
```

New images are listed on the top of the list. `Tag` refers to the version number.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Upload the Docker images to the Docker repository)]

Push your local docker image to docker cloud.

Execute the following on terminal *(make changes below)*

```BASH
docker push docker.io/<your-username>/text-clf-train:0.0.1
```

!![docker push](img/docker/docker-push.png)

Similarly push your serving docker image.

Execute the following on terminal *(make changes below)*

```BASH
docker push docker.io/<your-docker-username>/text-clf-serve:0.0.1
```

Visit <https://hub.docker.com>, go inside listed repository and view your uploads.  

!![docker uploads](img/docker/docker-final.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create Workflow files)]

Workflows will be used to tell how to execute the docker images, and what inputs to provide(data) and what outputs it gives.
 *(Further read [`ArgoWorkflows`](https://github.com/argoproj/argo-workflows) )*

Create a folder `workflows` locally and create the following file as depicted in the image **Contents of the file are provided below**  

!![workflows folder](img/build-ml/workflows.png)

Download Files

| File | Link |
| --- | --- |
| `training_workflow_tutorial.yaml` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/workflows/training_workflow_tutorial.yaml)  |
| `serving_workflow_tutorial.yaml` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/workflows/serving_workflow_tutorial.yaml)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Choosing Hardware Resources)]

For our example we used the `ai.sap.com/resourcePlan: starter`, *(mentioned inside the workflow YAML files)*   

If you want to have a more powerful computer hardware, for example with GPUs you can use a different resource plan.

|     `resourcePlan` ID    |     GPUs          |     CPU cores    |     Memory (Gb)    |
|------------------------|-------------------|------------------|--------------------|
|     Train-L            |     1 V100 GPU    |     5            |     47             |
|     Infer-S            |     1 T4 GPU      |     3            |     10             |
|     Infer-M            |     1 T4 GPU      |     7            |     25             |
|     Infer-L            |     1 T4 GPU      |     15           |     55             |
|     Starter            |                   |     1            |     2.5            |
|     Basic              |                   |     3            |     10             |
|     Basic.8x           |                   |     31           |     115            |


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Adding docker information to Workflow files)]

Edit the following lines in the each workflow files

- `training_workflow_tutorial.yaml`

  ```YAML[5]
  ...
  spec:
      ...
      container:
        image: "<your_docker_repo_url>/<your_username_in_docker_repo>/text-clf-train:0.0.1"
      ...
  ```

  !![train docker image yaml](img/build-ml/train-img.png)

- `serving_workflow_tutorial.yaml`

  ```YAML[10]
  ...
  spec:
      ...
      template:
          ...
          spec:
              predictor
                  ...
                  containers:
                      image: "<your_docker_repo_url>/<your_username_in_docker_repo>/text-clf-serve:0.0.1"
  ...
  ```

  !![train docker image yaml](img/build-ml/serve-img.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Upload Workflow files to your GitHub repository)]

Copy the `workflows` folder that we created now to your cloned local GitHub folder.Ensure your files are placed as per image below  

!![workflow GitHub local](img/build-ml/git-1.png)

Open GitHub Desktop. You should exactly see the same as the image below.  

!![GitHub commit](img/build-ml/git-2.png)  

1. Fill the message `workflows added`.
2. Click on `Commit to main` button.


Click on `Push Origin`.  

!![GitHub push](img/build-ml/git-push.png)

Now SAP AI Core will auto sync workflows from your GitHub and associated docker images from Docker.

> The SAP AI Core will automatically sync with the GitHub registered with it. (~3 minutes).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Check Workflow sync status with SAP AI API Client SDK)]

> **Warning** ensure you have properly set up `files/aic_service_key.json` and `ai_api_client` from Tutorial 1 before executing this code.

Execute the following python code on your Jupyter notebook cell

```PYTHON
app_name = "aicore-test-app"

ai_api_client.rest_client.get(
    path=f"/admin/applications/{app_name}/status"
)
```

Example Output

```PYTHON
{
    'health_status': 'Healthy',
    'message': 'successfully synced (all tasks run)',
    'reconciled_at': '2021-09-21T11:10:10Z',
    'source': {'path': 'workflows',
     'repourl': 'https://GitHub.com/John/aicore-test',
     'revision': '87f3f1c2cedb686235e8cfe395266d651ffb44b1'},
    'sync_finished_at': '2021-09-20T11:10:10Z',
    'sync_ressources_status': [
     {'kind': 'ServingTemplate',
      'message': 'servingtemplate.ai.sap.com/text-clf-infer-tutorial created',
      'name': 'text-clf-infer-tutorial',
      'status': 'Synced'},
     {'kind': 'WorkflowTemplate',
      'message': 'workflowtemplate.argoproj.io/text-clf-train-tutorial created',
      'name': 'text-clf-train-tutorial',
      'status': 'Synced'}],
    'sync_started_at': '2021-09-20T11:10:10Z',
    'sync_status': 'Synced'}
```


### Summary: What we have achieved.

- The training and serving docker images are pushed to the docker repository
- The training and serving workflows(*templates*) are uploaded to the GitHub repository.

!![AIF Setup](img/aifexercisesetup.png)


[VALIDATE_6]
[ACCORDION-END]
