---
title: Deploy an application built to connect to SAP HANA Express Edition, on App engine of the Google Cloud Platform (GCP)
description: Deploy an application built to connect to SAP HANA Express Edition, on App engine of the Google Cloud Platform (GCP)
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, topic>big-data, topic>cloud, products>sap-hana, products>sap-hana\,-express-edition  ]
---

## Prerequisites and Assumptions
 - **Proficiency:** Beginner
 - You have a working application to connect to SAP HANA, Express Edition.
 - You have a Google Account and/or have registered for the free trial on `cloud.google.com`.
 - You have a project created on the Google Cloud Platform and billing is enabled.
 - **Tutorials:**  [Create a new project using the Cloud Platform Console](https://cloud.google.com/resource-manager/docs/creating-managing-projects) and [SAP HANA, express edition, in Google Cloud Platform Launcher](https://www.sap.com/developer/tutorials/hxe-gcp-getting-started-launcher.html)


## Next Steps
 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page

## Details
### You will learn  
This tutorial will guide you through the process to deploy a sample application to Google Cloud Platform (GCP).

### Time to Complete
**30 Min**

---

[ACCORDION-BEGIN [Step 1: ](Configure Google Cloud SDK)]

1. Install gcloud SDK on your development environment using the link.
    [Google SDK](https://cloud.google.com/sdk/). Follow the prompts and take the default if provided.

2. Once the SDK is installed open a terminal and type the following command to make sure the installation is working.
    ```
    $ gcloud info
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy a Node JS Application)]

1. Go to the directory of where your Node JS Application is located.
   ```
   $ cd hxeapp

   ```

2. Create a file called `app.yaml`:
   ```
   hxeapp$ touch app.yaml
   ```
   Copy the contents below to the `app.yaml` file.

   ```
   env: flex
   runtime: nodejs

   ```
3. Deploy to the Google cloud
    ```
   hxeapp$ gcloud app deploy
   ```
   >**Note:**
   > The deployment will take ~5 minutes

4. After successful deployment, open the application by the following command.
   ```
   gcloud app browse
   ```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Deploy a Python/Flask Application)]

1. Go to the directory of where your Python/Flask Application is located.
   ```
   $ cd hxeapp

   ```

2. Create a file called `app.yaml`:
   ```
   hxeapp$ touch app.yaml
   ```
   Copy the contents below to the `app.yaml` file. Make sure your main python file is renamed to `main.py`

   ```
   runtime: python27
   api_version: 1
   threadsafe: true

   # [START handlers]
   handlers:
   - url: /static
     static_dir: static
   - url: /.*
     script: main.app
   # [END handlers]
   libraries:
   - name: ssl
     version: 2.7.11

   ```
3. Create a file called `appconfig.py`.
   ```
   hxeapp$ touch appconfig.py
   ```

   Copy the contents below to the `appconfig.py` file.
   ```
   from google.appengine.ext import vendor
   vendor.add('lib')
   ```

4. Deploy to the Google cloud
    ```
   hxeapp$ gcloud app deploy
   ```
   >**Note:**
   > The deployment will take ~5 minutes

5. After successful deployment, open the application by the following command.
   ```
   gcloud app browse
   ```
[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Deploy a GO Application)]

1. Go to the directory of where your GO Application is located.
   ```
   $ cd hxeapp

   ```

2. Create a file called `app.yaml`:
   ```
   hxeapp$ touch app.yaml
   ```
   Copy the contents below to the `app.yaml` file.

   ```
   runtime: go
   api_version: go1
   env: flex

   handlers:
   - url: /.*
     script: _go_app

   ```
3. Deploy to the Google cloud
    ```
   hxeapp$ gcloud app deploy
   ```
   >**Note:**
   > The deployment will take ~5 minutes

4. After successful deployment, open the application by the following command.
   ```
   gcloud app browse
   ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy a Ruby on Rails application)]


1. Go to the directory of where your Ruby on Rails Application is located.
   ```
   $ cd hxeapp

   ```

2. Create a file `app.yaml` :
   ```
   hxeapp$ touch app.yaml
   ```
   Copy the below code to the `app.yaml` file:

    ```
    entrypoint: bundle exec rackup --port $PORT
    env: flex
    runtime: ruby

    ```
    >**Note:**
    > If YARN is not installed on the machine, please install YARN using the instructions,
    >[YARN Install](https://yarnpkg.com/lang/en/docs/install/) before running the above command.

3. Run the following command and copy the generated key in the console output.

    ```
    hxeapp$ bundle exec rails secret

    ```

4. Add the below code to `app.yaml` file.
    ```
    env_variables:
      SECRET_KEY_BASE: <YOUR_GENERATED_LONG KEY>

    ```
5. Run the following command.

    ```
    hxeapp$ RAILS_ENV=production bundle exec rails assets:precompile

    ```
6. Execute the command below to deploy the application.

    ```
    hxeapp$ gcloud app deploy

    ```
>**Note:**
> The deployment will take ~5 minutes.

7. After successful deployment, open the application by the following command.
   ```
   gcloud app browse
   ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy a Sinatra/Ruby application)]


1. Go to the directory of where your Sinatra/Ruby Application is located.
   ```
   $ cd hxeapp

   ```

2. Create a file `app.yaml` :
   ```
   hxeapp$ touch app.yaml
   ```
   Copy the below code to the `app.yaml` file:

    ```
    runtime: ruby
    env: flex
    entrypoint: bundle exec ruby app.rb

    ```
    >**Note:**
    > Make sure the `sinatra` gem is listed in your Gemfile in the Application folder.

3. Execute the command below to deploy the application.

    ```
    hxeapp$ gcloud app deploy

    ```
>**Note:**
> The deployment will take ~5 minutes.

4. After successful deployment, open the application by the following command.
   ```
   gcloud app browse
   ```

[DONE]
[ACCORDION-END]
---

## Next Steps
 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page
