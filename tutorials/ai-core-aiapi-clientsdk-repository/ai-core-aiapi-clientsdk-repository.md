---
title: Connect GitHub and Docker to SAP AI Core through SAP AI API Client SDK
description: Learn to setup GitHub and Docker registry. Syncing the same with SAP AI Core through SAP AI API Client SDK.
auto_validation: true
time: 15
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, products>sap-business-technology-platform ]
primary_tag: topic>artificial-intelligence
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---


## Details
### You will learn
- How to create GitHub repository and sync files
- How to create Docker repository
- How to connect GitHub and Docker using SAP AI API Client SDK
---


[ACCORDION-BEGIN [Step 1: ](Create a GitHub repository)]

Sign-up for GitHub, Go to [github.com](https://github.com/).

Create a repository. A repository is a folder pertaining to your project.
Click **New**

!![Create repository](img/github/1.png)  

!![Repository Created](img/github/3.png)  

Set the repository name as `aicore-test`

!![Repository Name: aicore-test](img/github/2.png)

To manage GitHub repository from your computer

Install GitHub Desktop. [Download here](https://desktop.github.com/)  

Login using the GitHub-Desktop to your GitHub Account.

Open GitHub Desktop and click **File > Clone a repository**

!![clone repo](img/github/clone-repo.png)

Enter the repository we created. In the URL field, type `<your github username>/aicore-test`.

!![repo url](img/github/local-repo-2.png)

> **CAUTION**: In this tutorial, the sample images shown are two folders with same name `aicore-test`. We will notify your which one is used where.
>
> GitHub synced *(C:/john/Documents/GitHub/aicore-test)* : **Screenshots with File Explorer** which will be used to upload workflows(explained later) and connected to SAP AI Core
>
> Local Un-synced *(C:/aicore-test)* : One which is depicted in **Screenshots with IDE** where we Jupyter notebook is running and private keys are located. **Because never upload/sync your private keys with GitHub.**


Create a folder name `workflows`. Inside the local copy of your GitHub *(cloned repository)*

!![folder creation](img/github/folder-1.png)

Create **an empty file** named `training_workflow_tutorial.yaml` inside the `workflows` folder. *(We will fill its contents later)*

!![file creation](img/github/folder-2.png)

Sync your local git repo with online. Open GitHub-Desktop.
Click on **Commit to main** button

!![commit](img/github/commit.png)

Then press **Push Origin** button. This will sync our local changes with online GitHub.

!![push](img/github/push.png)

GitHub Desktop will only show un-synced changes.

!![normal GitHub Desktop look](img/github/local-repo-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Connect the Git repository to the SAP AI Core)]

Create the `git_setup.json` with the following contents:
> **CAUTION** Use your own GitHub URL and your credentials!

```JSON
{
		"repo": {
				"name": "aicore-test",
				"url": "https://github.com/john/aicore-test",
				"username": "$username",
				"password": "$password"
		},
		"app": {
				"applicationName": "aicore-test-app",
				"repositoryUrl": "https://github.com/john/aicore-test",
				"revision": "HEAD",
				"path": "workflows"
		}
}
```

Store it inside the files folder: `files/git_setup.json`.  

!![git setup](img/github/git-setup.png)

Execute the following python code on your Jupyter notebook cell, to connect your GitHub repository to your SAP AI Core service instance through SAP AI API Client SDK.

> **IMPORTANT:** DO NOT Execute the following code, if your SAP AI Core Account is already connected to a GitHub Account. Contact the support if you want to connect to a different account.

```PYTHON
# Read git_setup.json
git_setup_file_path = "files/git_setup.json"
#
# NO CHANGES REQUIRED BELOW
#
# Loads your git_setup.json
with open(git_setup_file_path) as gs:
		setup_json = json.load(gs)
#
# Connects your repo
repo_json = setup_json["repo"]
response = ai_api_client.rest_client.post(
		path="/admin/repositories",
		body={
				"name": repo_json["name"],
				"url": repo_json["url"],
				"username": repo_json["username"],
				"password": repo_json["password"]
		}
)
print(response)
#
# Registers the directory as app,
app_json = setup_json["app"]
response = ai_api_client.rest_client.post(
		path="/admin/applications",
		body={
				"applicationName": app_json["applicationName"],
				"repositoryUrl": app_json["repositoryUrl"],
				"revision": app_json["revision"],
				"path": app_json["path"]
		}
)
```

Example Output
```PYTHON
{
	 'count': 1
	 'resources':[{
	 		'name': 'aicore-test',
			'status': 'COMPLETED',
			'url': 'https://github.com/john/aicore-test'
		}]
}

[
  {
    'application_name': 'aicore-test-app',
  	'path': 'workflows'
  	'repository_url': 'https://github.com/john/aicore-test',
  	'revision': 'HEAD'
  },
]
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](List Connected Git Repositories)]

Execute the following python code on your Jupyter notebook cell

```PYTHON
ai_api_client.rest_client.get(
		path="/admin/repositories"
)
```

Example Output
```PYTHON
{
	 'count': 1,
	 'resources': [
			{
					 'name': 'aicore-test',
					 'status': 'COMPLETED',
					 'url': 'https://github.com/john/aicore-test'
			 }
	 ]
}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](List the apps used to synchronize the Git repository)]

The apps refers to directory inside the connected GitHub repository. This helps you manage multiple projects with same connected repository.

Execute the following python code on your Jupyter notebook cell

```PYTHON
ai_api_client.rest_client.get(
    path="/admin/applications"
)
```

Example Output

```
{
    'count': 1,
    'resources': [{
        'application_name': 'aicore-test-app',
        'path': 'workflows',
        'repository_url': 'https://github.com/john/aicore-test',
        'revision': 'HEAD'
    }]
}
```


[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a Docker account and repository)]


Sign-up at [docker.com](https://www.docker.com)

Create Repository  

!![docker create repository button](img/docker/repo_create.png)

Name the repository `text-clf-train`

!![docker repo for training](img/docker/repo_name.png)

Create another repository similarly, named `text-clf-serve`.

!![docker repo structure](img/docker/repos.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Install Docker on your machine)]

Download and Install Docker Desktop. [Download here](https://www.docker.com/products/docker-desktop)

After Installation. You will see Docker Desktop icon on your desktop tray.

!![docker desktop icon](img/docker/dd-icon.png)

Verify with your terminal *(command prompt)*. Execute the following on terminal.

```BASH
docker --version
```

!![docker terminal version](img/docker/dd-terminal.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Connect your Docker registry to the SAP AI Core)]

Create a file `docker_secret.json`, with following contents .

> **CAUTION**: Replace `$username` with your docker-hub username and `$password` with password.

```JSON
{
  "name": "docker-registry-secret",
  "data": {
    ".dockerconfigjson": "{\"auths\": {\"docker.io\": {\"username\": \"$username\", \"password\": \"$password\"}}}"
  }
}
```

Store it inside the files folder: `files/docker_secret.json`.

!![docker secret.json](img/docker/docker-secret.png)

Execute the following python code on your Jupyter notebook cell to connect your Docker repository to SAP AI Core with the help of SAP AI API Client SDK.

```PYTHON
docker_secret_file_path = 'files/docker_secret.json'
#
# NO CHANGES REQUIRED BELOW
#
# Loads the json file
with open(docker_secret_file_path) as dsf:
    docker_secret = json.load(dsf)

response = ai_api_client.rest_client.post(
    path="/admin/dockerRegistrySecrets",
    body={
        "name": docker_secret["name"],
        "data": docker_secret["data"]
    }
)
print(response)
```

Example Output

```PYTHON
{'message': 'secret has been been created'}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](List connected Docker registry secret)]

Execute the following python code on your Jupyter notebook cell


```PYTHON
ai_api_client.rest_client.get(
    path=f"/admin/dockerRegistrySecrets"
)
```

Example Output

```PYTHON
{
    'count': 1,
    'resources': [{
        'name': 'docker-registry-secret'
    }]
}
```


[DONE]
[ACCORDION-END]


---
