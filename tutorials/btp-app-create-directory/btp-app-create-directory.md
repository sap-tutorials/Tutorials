---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Create a Directory for Development
description: This tutorial shows you how to start building your application with CAP.
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Local Development using VS Code](btp-app-set-up-local-development) or [Set Up SAP Business Application Studio](btp-app-set-up-appstudio) depending on which editor you have installed

## Details
### You will learn
 - How to develop the app
 - How to download the complete tutorial
 - How to create a GitHub repository
 - How to clone your GitHub repository

---

[ACCORDION-BEGIN [Step 1: ](Develop the App)]

If a tutorial has prerequisites in other tutorials, they're listed at the beginning as prerequisites. Thus, you can skip a number of tutorials or stop the tutorial at any point and still have a running service or app, just with fewer features.

If you want to go through the tutorial fast, you can just do the tasks in the lists and skip the rest of the text.

For the tutorial, you need a directory to develop the app and you need access to the template files. We recommend choosing one root directory (that is the tutorial root directory) for both the directory for your app (`cpapp`) and the tutorial directory (`tutorial`) with the template files. See the table below:

| Directory | Contents |
|-|-|
| tutorial root directory | the directory containing `cpapp` and `tutorial` |
| `tutorial` | the directory containing the clone of the tutorial repository |
| `tutorial/templates` | templates provided by the tutorial repository |
| `cpapp` | the directory that will contain your application |

> _If you're working with SAP Business Application Studio, you can use `/user/projects` as tutorial root directory._



[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Download the Tutorial)]

Downloading the tutorial gives you easy access to template files that are required for some tutorials.

1. Open a terminal.

2. Navigate to the previously decided tutorial root directory:

    ```bash
    cd <tutorial root directory>
    ```

3. Clone the tutorial:

    ```bash
    git clone https://github.com/SAP-samples/cloud-cap-risk-management tutorial
    ```


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Create a GitHub Repository for Your Project)]

We recommend creating a public [GitHub](https://github.com) repository to save your tutorial application. This way, if you have issues with your tutorial application, you can refer to it.

For real application development, you need to consider the right place for your repository, of course.

Go to [GitHub](https://github.com/) and create a new GitHub repository.

<!--
    Currently, SAP Continuous Integration and Delivery supports only [GitHub](https://github.com/) repositories.
    To be able to connect SAP Continuous Integration and Delivery with your repository, create a new repository on [GitHub](https://github.com/).
-->


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ]((Optional) Import GitHub Repository in SAP Business Application Studio)]

Read how to [Connect to Your Git Source Control System](https://help.sap.com/viewer/9d1db9835307451daa8c930fbd9ab264/Cloud/en-US/e7a42bcb9d124b43be7e396b11d5e808.html).

When using [SAP Business Application Studio](btp-app-set-up-appstudio), we recommend using a Personal access token.

1. In your GitHub account go to **Settings** **&rarr;** **Developer settings** **&rarr;** **Personal access token**:

    [https://github.com/settings/tokens](https://github.com/settings/tokens)

2. Generate a new **Personal access token**.
3. Start your **Dev Space** of SAP Business Application Studio.
4. Choose **Clone from Git** on the **Welcome** page.
5. Paste the GitHub repository URL in the box and choose **Enter**.
6. Use your GitHub user name and *Personal Access Token* as **user** and **password**.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 5: ](Clone Your GitHub Repository)]

1. Copy the repository's URL you have created before.

2. Open a terminal.

3. Navigate to the tutorial root directory:

    ```bash
    cd <tutorial root directory>
    ```

4. Clone your new repository:

    ```bash
    git clone <git-repository-url> cpapp
    ```

    > _Replace `<git-repository-url>` with your GitHub repository's URL._



[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 6: ]((Optional) Copy the Files to Start from an Example)]

If you don't want to start from scratch, but from a specific example of the tutorial, you can copy the required files in your project. Alternatively, you can fork the project and work on the fork.

> _The name of the prerequisite branch is given on the top of each tutorial that requires code changes._

1. Look-up the branch in the tutorial.

2. Open a terminal.

3. Navigate to the tutorial root directory:

    ```bash
    cd <tutorial root directory>
    ```

4. Check out the example's branch:

    ``` bash
    cd tutorial
    git checkout <branch>
    ```

    > _Replace `<branch>` with the name of the branch of the example that you want to start with._

5. Copy all files from the example to a local directory `<project-dir>`, except the `.git` directory.

    ```bash
    cp -r .gitignore $(ls -1A | grep -v .git) ../cpapp
    ```

6. Check out the `master` branch to get access to the template files again:

    ```bash
    git checkout master
    ```

7. Switch to your app directory:

    ```bash
    cd ../cpapp
    ```

[VALIDATE_1]
[ACCORDION-END]
