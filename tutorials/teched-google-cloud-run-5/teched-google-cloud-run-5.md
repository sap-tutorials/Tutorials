---
title: SAP TechEd 2019 - Prepare Your Docker Container Image
description: Configure and package your application into a Docker container image
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-hana, products>sap-cloud-platform]
primary_tag: products>sap-hana\,-express-edition
---

## Prerequisites
- These series of tutorials can be completed at the AppSpace at SAP TechEd.
- You have completed [the previous tutorial](teched-google-cloud-run-4)

## Details
### You will learn
  - How to create your own Docker image with your application
  - How to upload your image into the container registry

These tutorials are meant to be completed at the Developer Garage at SAP TechEd. The experts at the Google Cloud booth will provide you with an access to an account.

---

[ACCORDION-BEGIN [Step 1: ](Create project NPM configuration file)]

Copy the `.npmrc` global configuration file to the project's root folder with the following command:

```shell
cp ~/.npmrc ~/teched
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Dockerfile)]

You will create a Docker image from a Node.js image using the application you created using the Cloud Application Programming model. This image will be uploaded into a repository and deployed into Cloud Run.

Create a Docker file using the following command:

```shell
touch ~/teched/Dockerfile
edit ~/teched/Dockerfile
```

Insert the following contents into the `Dockerfile`

```Dockerfile
# Use the official Node.js 8 image.
# https://hub.docker.com/_/node
FROM node:8

# Create and change to the app directory.
WORKDIR /usr/src/app

# Copy application dependency manifests to the container image.
# A wildcard is used to ensure both package.json AND package-lock.json are copied.
# Copying this separately prevents re-running npm install on every code change.
COPY /package*.json ./
COPY .npmrc ./

# Install dependencies.
RUN npm install

# Copy local code to the container image.
COPY . .

# Run the web service on container startup.
CMD [ "npm", "start" ]

```

![NPM config](3.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Upload the image)]

Open the following URL in a new tab...

```url
https://console.cloud.google.com/apis/api/cloudbuild.googleapis.com/overview
```

...and **Enable** the Cloud Build API.

![NPM config](api.png)

Wait until the API has been enabled. You can close this tab and go back to the Cloud Shell once it has.

Use the following command to create and upload a container image. Answer **y** if prompted to activate Cloud Run for your account.

```shell
cd ~/teched
gcloud builds submit --tag gcr.io/$GOOGLE_CLOUD_PROJECT/teched
```

This will take about a minute.

Make sure you see a **SUCCESS** message once the process is finished.


![NPM config](6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Retrieve the environment variable)]

Use the following command to get the `VCAP_SERVICES` file as a string.

```shell
jq -r "to_entries|map(\"\(.key)=\(.value|tostring)\")|.[]" ~/teched/default-env.json
```

 **Leave this open**. You will need this result later.

 ![NPM config](7x.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Cloud Run deployment)]

Open a new tab and enter [https://console.cloud.google.com/run](https://console.cloud.google.com/run).

Click **Start using Cloud Run**.

![Create Cloud Run](x8.png)

Click **Create Service**.

![Create Cloud Run](8.png)

Choose **Select**.

![Create Cloud Run](10.png)

Expand the list and click on the latest registry image.

![Create Cloud Run](11.png)

Flag **Allow unauthenticated invocations** and click **Show Optional Revision Settings**.

![Create Cloud Run](12.png)

In the environment variable, add a variable called `VCAP_SERVICES` and use the content of the file `default-env.json` as a string from the previous step.

Do not copy the `VCAP_SERVICES=` assignment. You only need the value of the variable.

![Create Cloud Run](13x.png)

Scroll down and click **Create**

![Create Cloud Run](14.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test your service)]

Wait until deployment has finished and click on the URL.

![Create Cloud Run](15.png)

Congratulations! Your cloud run deployment is up and running.

![Create Cloud Run](16.png)

Click on the **Foods** service to complete the validation below.

[VALIDATE_1]
[ACCORDION-END]

---
