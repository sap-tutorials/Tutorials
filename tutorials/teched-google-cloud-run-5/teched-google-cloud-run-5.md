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

[ACCORDION-BEGIN [Step 1: ](Create NPM configuration file)]

Use the following two commands to create a file called `.npmrc` in the `srv` directory. This file is used by `NPM` for configuration.

```ssh
touch ~/teched/srv/.npmrc
edit ~/teched/srv/.npmrc
```

![NPM config](1.png)

Paste the following line into the file:

```text
@sap:registry=https://npm.sap.com
```

![NPM config](2.png)

Copy the `.npmrc` file to the root folder too with the following command:

```ssh
cp ./srv/.npmrc ~/teched
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Dockerfile)]

You will create a Docker image from a Node.js image using the application you created using the Cloud Application Programming model. This image will be uploaded into a repository and deployed into Cloud Run.

Create a Docker file using the following command:

```ssh
touch ~/teched/Dockerfile
edit ~/teched/Dockerfile
```

Insert the following contents into the `Dockerfile`

```text
# Use the official Node.js 8 image.
# https://hub.docker.com/_/node
FROM node:8

# Create and change to the app directory.
WORKDIR /usr/src/app

# Copy application dependency manifests to the container image.
# A wildcard is used to ensure both package.json AND package-lock.json are copied.
# Copying this separately prevents re-running npm install on every code change.
COPY /package*.json ./
COPY /srv/.npmrc .npmrc
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

Use the following command to create and upload a container image. Answer **y** if prompted to activate Cloud Run for your account.

```ssh
cd ~/teched
gcloud builds submit --tag gcr.io/$GOOGLE_CLOUD_PROJECT/teched
```

For example:

![NPM config](5.png)

Make sure you see a **SUCCESS** message once the process is finished.


![NPM config](6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Retrieve the environment variable)]

Use the following command to get the `VCAP_SERVICES` environment variable.

```ssh
echo $VCAP_SERVICES
```
 **Leave this open**. You will need this result later.

 ![NPM config](7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Cloud Run deployment)]

Open a new tab and enter [https://console.cloud.google.com/run](https://console.cloud.google.com/run).

Click **Create Service**.

![Create Cloud Run](8.png)

Choose **Select**.

![Create Cloud Run](10.png)

Expand the list and click on the latest registry image.

![Create Cloud Run](11.png)

Flag **Allow unauthenticated invocations** and click **Show optimal settings**.

![Create Cloud Run](12.png)

In the environment variable, add a variable called `VCAP_SERVICES` and use the content of the result from `echo $VCAP_SERVICES` in the cloud shell:

![Create Cloud Run](13.png)

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
