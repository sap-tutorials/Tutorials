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
  - Build Docker images and host on Docker Repository
  - Write ML Pipeline via Workflows.

---

[ACCORDION-BEGIN [Step 1: ](Create ML Code and Docker files)]


Docker will be used to store python code in form of containers( *portable environments*).

Create files and **copy provided contents *(see below)*** as-it-is. Resulting directory structure may look like following:

!![folder structure](img/postman/code-folder.png)


File Links

Training Scripts

| `train/`| Download Link |
| ---  | ----- |
| `Dockerfile` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/train/Dockerfile) |
| `evaluate_scikit.py` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/train/evaluate_scikit.py)
| `requirements.txt` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/train/requirements.txt)
| `train_scikit.py` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/train/train_scikit.py)


Inference/ Serving Scripts

| `infer/` | Download Link |
| -------- | ------------- |
| `Dockerfile` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/infer/Dockerfile)
| `infer_scikit.py` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/infer/infer_scikit.py)
| `requirements.txt` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/infer/requirements.txt)

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 2: ](Build Docker Image)]

Connect your Docker Desktop to the Docker account you created previously.

```BASH
docker login docker.io -u <your-dockerhub-username>
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

[OPTIONAL] <br>
You can see your local docker images using the command.

```BASH
docker images
```


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Upload the Docker Image to the Docker Repository)]

Push your local docker image to Docker Hub cloud.

```BASH
docker push docker.io/<your-dockerhub-username>/text-clf-train:0.0.1
```

(*sample image if previously uploaded*)  
!![docker push](img/docker/docker-push.png)

Similarly push your serving docker image.

```BASH
docker push docker.io/<your-dockerhub-username>/text-clf-serve:0.0.1
```

Visit <https://hub.docker.com>, go inside listed repository and view your uploads.

!![docker uploads](img/docker/docker-final.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create Workflow Files)]

Workflows will be used to tell how to execute the docker images, and what inputs to provide(data) and what outputs it gives.
 *(Further read [`ArgoWorkflows`](https://github.com/argoproj/argo-workflows) )*


Create a folder `workflows` inside to your cloned local GitHub folder. And create the following file as depicted in the image **Contents of the file are provided below**; Ensure your files are placed as per image below(similarly)

!![workflow GitHub local](img/build-ml/git-1.png)

Download Files


| File | Link |
| --- | --- |
| `training_workflow_tutorial.yaml` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/workflows/training_workflow_tutorial.yaml)  |
| `serving_workflow_tutorial.yaml` | [Download](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/ai-core-aiapi-postman-workflows/files/workflows/serving_workflow_tutorial.yaml)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Set Hardware Resources for Computing)]

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

[ACCORDION-BEGIN [Step 6: ](Add Docker Details to Workflow)]

Edit the following lines in the each workflow files.
Replace the part `<your_username_in_docker_repo>` with your docker username.

- `training_workflow_tutorial.yaml`
    ```YAML[5]
    ...
    spec:
        ...
        container:
            image: "docker.io/<your_username_in_docker_repo>/text-clf-train:0.0.1"
        ...
    ```

    Example output

    !![train docker image yaml](img/postman/train-img.png)

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
                        image: "docker.io/<your_username_in_docker_repo>/text-clf-serve:0.0.1"
    ...
    ```

    Example output

    !![train docker image yaml](img/postman/serve-img.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Upload Workflow to GitHub)]

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

[ACCORDION-BEGIN [Step 8: ](API to Check Workflow Sync Status)]

**IMPORTANT:** WAIT ATLEAST 3 MINS before sync is done.

Check the sync status of your workflows with postman.

### ADD POSTMAN Environment Variable

| Key | Value |
| --- | --- |
| `appName` | `aicore-test-app`

!![add environment variable](img/postman/add-env.png)


> **COLLECTIONS** > admin > applications > *GET* get application status

### Endpoint
**GET**
`{{apiurl}}/v2/admin/applications/{{appName}}/status`


**SEND**

### RESPONSE

*(if 404, See troubleshooting below.)*

```
{
    "healthStatus": "Healthy",
    "message": "successfully synced (all tasks run)",
    "reconciledAt": "2021-09-27T03:16:41Z",
    "source": {
        "path": "workflows",
        "repoURL": "https://github.com/john/aicore-test",
        "revision": "8505d9235fc19f672096e22354a934e429dad9a3"
    },
    "syncFinishedAt": "2021-09-27T03:16:41Z",
    "syncRessourcesStatus": [
        {
            "kind": "WorkflowTemplate",
            "message": "workflowtemplate.argoproj.io/text-clf-train-tutorial configured",
            "name": "text-clf-train-tutorial",
            "status": "Synced"
        }
    ],
    "syncStartedAt": "2021-09-27T03:16:40Z",
    "syncStatus": "Synced"
}
```

!![get app status](img/postman/call-app-status.png)



### Troubleshooting

1. `"message": "KeyError : 'operationState'"`

    Your repository folder is empty from last sync. Wait for 3 minutes after uploading something to GitHub.

2. `"message": "Error retrieving status for application aicore-test-app"`

    Unable to reach out application(SAP AI Core) named `aicore-test-app`, check if you have registered the GitHub directory as application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Summary)]

**A small recap of what has been done so far.**

- The training and serving docker images are pushed to the docker repository
- The training and serving workflows(*templates*) are uploaded to the GitHub repository.

!![AIF Setup](img/aifexercisesetup.png)


[VALIDATE_1]
[ACCORDION-END]

---
