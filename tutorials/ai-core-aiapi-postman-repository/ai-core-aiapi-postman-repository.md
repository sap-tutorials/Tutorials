---
title: Connect GitHub and Docker to SAP AI Core (Postman)
description: Learn to enable SAP AI Core to auto-sync code and workflows files hosted on GitHub and Docker.
auto_validation: true
time: 20
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

## Details
### You will learn
  - How to create GitHub and Docker repository.
  - Make API calls connect repository to SAP AI Core

---

[ACCORDION-BEGIN [Step 1: ](Create GitHub repository)]

A GitHub repository refers to a folder *(project)*, with Version-Control, which means you can track any changes to files contained within revert them.

1. Sign up for GitHub, Go to [github.com](https://github.com/).

2. Create a repository, Click **New**.

    !![Create repository](img/github/1.png)  

3. Set the repository name as `aicore-test` ( *customizable*)

    !![Repository Name: aicore-test](img/github/2.png)

    !![Repository Name: aicore-test](img/github/3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Clone GitHub using GitHub Desktop)]

Cloning is the process of managing your GitHub repositories on you local computer

GitHub Desktop helps you clone, create, edit and sync files of your GitHub repository from you local computer.

1. Install GitHub Desktop. [Download here](https://desktop.github.com/)  

2. Open GitHub Desktop.

3. From welcome screen login to your GitHub Account *(Alternative login: **File** > **Options** > **Accounts**).

3. Click **File > Clone a repository**.

    !![clone repo](img/github/clone-repo.png)

4. Go to **URL** tab. Type `<your github username>/aicore-test`.

    !![repo url](img/github/local-repo-2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Upload Files to GitHub)]

1. Create a folder name `workflows`. Inside the local copy of your GitHub *(cloned repository)*

    !![folder creation](img/github/folder-1.png)

2. Create **an empty file** named `training_workflow_tutorial.yaml` inside the `workflows` folder.

    !![file creation](img/github/folder-2.png)

    > **CAUTION**: Avoid storing any service keys or passwords inside a Git tracked folder.

3. Sync your local git repo with online. Open GitHub Desktop.

4. Click on **Commit to main** button

    !![commit](img/github/commit.png)

5. Then pres **Push Origin** button.

    !![push](img/github/push.png)

This will sync up your local files with GitHub repository(online).

GitHub Desktop will only show **un-synced** changes.

!![github Desktop normal view](img/github/local-repo-1.png)


[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 4: ](API to connect GitHub SAP AI Core)]


> **CAUTION** At present SAP AI Core can connect to single GitHub repository, skip this step if its already connected to one. Contact support to change if required.

After connecting GitHub repository to your SAP AI Core account, whenever you will push changes *(upload files)* to GitHub, the SAP AI Core can pull(sync) them automatically.

Make the following API call using Postman

> **COLLECTIONS** > admin > repositories > *POST* Create repository

### Endpoint
**POST**
`{{apiurl}}/v2/admin/repositories`

### Body

- Get or generate GitHub Personal Access Tokens (PAT) [See how](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
>**IMPORTANT** Do not use your GitHub Passwords in the request body.

- Edit your GitHub URL and your credentials, *(highlighted lines)* below.

```JSON[3, 4, 5]
{
    "name": "aicore-test",
    "url": "https://github.com/<your-username>/aicore-test",
    "username": "Your-github-username",
    "password": "your-git-PAT-token"
}
```

!![create repository](img/postman/call-repo.png)

**SEND**

### Response

```
{   
    "message": "Repository has been on-boarded."
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](API to list connected GitHub repository)]
List Connected Git Repositories

> **COLLECTIONS** > admin> repositories > *GET* List repositories


### Endpoint
**GET**
`{{apiurl}}/v2/admin/repositories`

**SEND**

### RESPONSE
```
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



[ACCORDION-BEGIN [Step 6: ](API to create application on SAP AI Core)]

Application is an SAP AI Core entity which links a sub-folder inside your connected GitHub repository. SAP AI Core will look into these folders for any workflow *(explained later)*, and sub-folder within the same will not be searched for.

> **COLLECTIONS** > admin > applications > *POST* Create application

### Endpoint
**POST**
`{{apiurl}}/v2/admin/applications`

### Body

Edit your GitHub username *(highlighted lines)* below.

```JSON[3]
{
    "applicationName": "aicore-test-app",
    "repositoryUrl": "https://github.com/<your-username>/aicore-test",
    "revision": "HEAD",
    "path": "workflows"
}
```

!![application create](img/postman/call-app.png)

**SEND**

### RESPONSE
```
{
    "id": "aicore-test-app",
    "message": "Application has been successfully created."
}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](API to list connected Applications)]

> **COLLECTIONS** > admin > applications > *GET* List applications

### Endpoint
**GET**
`{{apiurl}}/v2/admin/applications`

**SEND**

### Response

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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create Docker repository)]

1. Sign up at [docker.com](https://www.docker.com)

2. Click **Create Repository**.

    !![docker create repository button](img/docker/repo_create.png)

3. Name the repository "text-clf-train"  

    !![docker repo for training](img/docker/repo_name.png)

4. Create another repository similarly, named **text-clf-serve**.

    !![docker repo structure](img/docker/repos.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Manage Docker locally)]

1. Download and Install Docker Desktop. [Download here](https://www.docker.com/products/docker-desktop)

2. After Installation. You will see Docker Desktop icon on your desktop tray.

    !![docker desktop icon](img/docker/dd-icon.png)

3. Verify with your terminal *(command prompt)*. Execute the following on terminal.

    ```BASH
    docker --version
    ```

    !![docker terminal version](img/docker/dd-terminal.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](API to Create Docker Registry Secret.)]

Secret hold your docker credentials which enables SAP AI Core to fetch your docker images (code files).

You can create multiple secrets.

> **COLLECTIONS** > admin > *POST* Create docker registry secret

### Endpoint
**POST**
`{{apiurl}}/v2/admin/dockerRegistrySecrets`

Create the following file `docker_secret.json` .

### Body

Edit and replace `$username` with your Docker username and `$password` with password. *(highlighted line)*.

```JSON[4]
{
  "name": "docker-registry-secret",
  "data": {
    ".dockerconfigjson": "{\"auths\": {\"docker.io\": {\"username\": \"$username\", \"password\": \"$password\"}}}"
  }
}
```

!![docker secret](img/postman/call-docker.png)

**SEND**

### Response
```
{'message': 'secret has been been created'}
```

#### List connected Docker registry secret
> **COLLECTIONS** > admin > *GET* List docker registry secrets

`RESPONSE`

```
{
    'count': 1,
    'resources': [{
        'name': 'docker-registry-secret'
    }]
}
```

> **IMPORTANT:** Creating docker secret does not imply docker account gets connected. It only implies that whenever SAP AI Core search(if required) for docker images it will use these credentials to search docker repository. At that time it may happen that the provided credentials are incorrect.

[VALIDATE_1]
[ACCORDION-END]

---
