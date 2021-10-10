---
title: Connect GitHub and Docker to SAP AI Core through Postman Client
description: Learn to enable SAP AI Core to auto-sync code and workflows files hosted on GitHub and Docker.
auto_validation: true
time: 20
tags: [ tutorial>license, tutorial>advanced, topic>artificial-intelligence, topic>machine-learning, products>sap-business-technology-platform ]
primary_tag: topic>artificial-intelligence
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

## Details
### You will learn
  - How to create GitHub and Docker repository.
  - Make API calls connect repository to SAP AI Core

---

[ACCORDION-BEGIN [Step 1: ](Create GitHub Repository)]

1. Sign up for GitHub, Go to [github.com](https://github.com/).

2. Create a repository, Click **New**.

    > A GitHub repository refers to a folder ( *project*), with Version-Control( i.e. track changes and are reversible ).

    !![Create repository](img/github/1.png)  

3. Set the repository name as `aicore-test` ( *customizable*)

    !![Repository Name: aicore-test](img/github/2.png)

    !![Repository Name: aicore-test](img/github/3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Manage GitHub(online) Locally)]


1. Install GitHub Desktop. [Download here](https://desktop.github.com/)  

2. Login using the GitHub Desktop to your GitHub Account.

3. Open GitHub Desktop and click **File > Clone a repository**

    !![clone repo](img/github/clone-repo.png)

4. Enter the repository we created. In the URL field, type `<your github username>/aicore-test`.

    !![repo url](img/github/local-repo-2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Upload Files to GitHub)]

1. Create a folder name `workflows`. Inside the local copy of your GitHub *(cloned repository)*

    !![folder creation](img/github/folder-1.png)

2. Create **an empty file** named `training_workflow_tutorial.yaml` inside the `workflows` folder. *(We will fill its contents later)*

    !![file creation](img/github/folder-2.png)

    > **CAUTION**: Avoid storing any service keys or passwords inside a Git tracked folder.

3. Sync your local git repo with online. Open GitHub Desktop.

4. Click on **Commit to main** button

    !![commit](img/github/commit.png)

5. Then pres **Push Origin** button.

    !![push](img/github/push.png)

This will sync up your GitHub repository(online) with new changes you made locally.

GitHub Desktop will only show **un-synced** changes.

!![github Desktop normal view](img/github/local-repo-1.png)


[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 4: ](API to Connect GitHub SAP AI Core)]


> **CAUTION** At present SAP AI Core can connect to single GitHub repository, skip this step if its already connected to one. Contact support to change if required.

After we connect GitHub repository to our SAP AI Core account, whenever we will push changes ( *upload files*) to GitHub, the SAP AI Core can pull them automatically.

Perform the below on Postman.

> **COLLECTIONS** > admin > repositories > *POST* Create repository

### Endpoint
**POST**
`{{apiurl}}/v2/admin/repositories`

### Body

> **CAUTION** Ensure you use your GitHub URL and your credentials, below.

```JSON
{
    "name": "aicore-test",
    "url": "https://github.com/<your-username>/aicore-test",
    "username": "$username",
    "password": "$password"
}
```

**SEND**

### Response
```
{'count': 1,
 'resources':[{'name': 'aicore-test',
               'status': 'COMPLETED',
               'url': 'https://github.com/john/aicore-test'
               }]
}
```

!![create repository](img/postman/call-repo.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](API to List Connected GitHub Repository)]
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



[ACCORDION-BEGIN [Step 6: ](API to Create Application on SAP AI Core)]

Application is an entity which links a sub-folder ( *representing project*) inside your connected GitHub repository.

> **COLLECTIONS** > admin > applications > *POST* Create application

### Endpoint
**POST**
`{{apiurl}}/v2/admin/applications`

### Body
> **CAUTION** Ensure you use your GitHub username below
```
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


[ACCORDION-BEGIN [Step 7: ](API to List Connected Applications)]

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

[ACCORDION-BEGIN [Step 8: ](Create Docker Repository)]

1. Sign up at [docker.com](https://www.docker.com)

2. Create Repository  

    !![docker create repository button](img/docker/repo_create.png)

3. Name the repository "text-clf-train"  

!![docker repo for training](img/docker/repo_name.png)

4. Create another repository similarly, named **text-clf-serve**.

    !![docker repo structure](img/docker/repos.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Manage Docker locally)]

1. Download and Install Docker Desktop. [Download here](https://www.docker.com/products/docker-desktop)
<br>After Installation. You will see Docker Desktop icon on your desktop tray.

    !![docker desktop icon](img/docker/dd-icon.png)

2. Verify with your terminal *(command prompt)*. Execute the following on terminal.

```
docker --version
```

!![docker terminal version](img/docker/dd-terminal.png)



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ](Connect Docker to SAP AI Core)]

> **COLLECTIONS** > admin > *POST* Create docker registry secret

### Endpoint
**POST**
`{{apiurl}}/v2/admin/dockerRegistrySecrets`

Create the following file `docker_secret.json` .

### Body
> **CAUTION**: Replace `$username` with your Docker-Hub username and `$password` with password.
```JSON
{
  "name": "docker-registry-secret",
  "data": {
    ".dockerconfigjson": "{\"auths\": {\"docker.io\": {\"username\": \"$username\", \"password\": \"$password\"}}}"
  }
}
```

!![docker secret](img/postman/call-docker.png)

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

**IMPORTANT:** Creating docker secret does not imply docker account gets connected. It only implies that whenever SAP AI Core search(if required) for docker images it will use these credentials to search docker repository. At that time it may happen that the provided credentials are incorrect.

[VALIDATE_1]
[ACCORDION-END]

---
