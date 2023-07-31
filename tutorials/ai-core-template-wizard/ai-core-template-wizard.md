---
parser: v2
auto_validation: true
time: 45
tags: [ tutorial>beginner, topic>artificial-intelligence, topic>machine-learning, software-product>sap-business-technology-platform, software-product>sap-ai-launchpad, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

# Deploying a movie recommendation system to AI core using Tempelate generator
<!-- description --> Building a portable AI code with Vs code template generator and Deploying the same on SAP AI Core. 

## Prerequisites
- Understanding Of SAP AI Core

## You will learn
- Setup Vs code Template generator
- Deploy generated code to AI core

## Intro
In this tutorial we would be Creating a movie recommendation Template and deploying the same using AI core.
---


### Genrating template for Movie Recommendation

step 1:**  press Command + shift + p for *mac* and Control + shift + p for *Windows* to open control center and choose open template wizard.

![](img/tempelate_gen1.png)

**Step 2:** Once the Template generator opens choose AI core and press start.

![](img/tempelate_gen2.png)

**Step 3:** If you have your custom code choose Basic Project or if you wanted to use the pre-generated training and serving code use AI core sample project for this demo we are going to use.

![](img/tempelate_gen3.png)

**Step 4:** enter the project name and version and press enter

![](img/tempelate_gen4.png)

**Step 5:** fill up the details like scenario ID and Resource plan.

![](img/tempelate_gen5.png)

**Step 6:** Similarly fill the details for Serving Plan.

![](img/tempelate_gen6.png)

**Step 7:** Choose the demo sample project as Movie Recommendation and click on next.

![](img/tempelate_gen7.png)


**Step 8:** As a last step add the Docker Details like Docker registry URL and username and click next to create the project.

![](img/tempelate_gen8.png)

Step 9: It will create all the required folders in the current directory.

![](img/tempelate_gen9.png)


### Deploying Template to AI Core

In the Generated tempelate visit Template folder > Pipelines and replace `ai-core_creds.json` and `S3_creds.json` with your Actual AI core Creds.

![](img/run_movie1.jpg)

Once done Under the same pipelines folder open AI-core-skd-notebook and replace the Configure variables with your own Variables and click on run all.

![](img/run_movie2.jpg)