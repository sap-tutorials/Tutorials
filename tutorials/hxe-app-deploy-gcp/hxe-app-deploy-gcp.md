---
title: Deploy an application built to connect to SAP HANA Express Edition, on App engine of the Google Cloud Platform (GCP)
description: Deploy an application built to connect to SAP HANA Express Edition, on App engine of the Google Cloud Platform (GCP)
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>how-to, tutorial>beginner, topic>big-data, topic>cloud, products>sap-hana, products>sap-hana\,-express-edition  ]
---

## Prerequisites
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

Install `gcloud` SDK on your development environment using the instructions from [Google SDK](https://cloud.google.com/sdk/). Follow the prompts and take the default if provided.


Once the SDK is installed open a terminal and type the following command to make sure the installation is working.
```
$ gcloud info

```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy a Node JS Application)]

Go to the directory of where your Node JS Application is located.

```
$ cd hxeapp
```

Create a file called `app.yaml`:
```
hxeapp$ touch app.yaml

```

Copy the contents below to the `app.yaml` file.

```
env: flex
runtime: nodejs

```

Deploy to the Google cloud

```
hxeapp$ gcloud app deploy
```
>**Note:**
> The deployment will take ~5 minutes

</br>

After successful deployment, open the application by the following command.

```
gcloud app browse
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Deploy a Python/Flask Application)]

Go to the directory of where your Python/Flask Application is located.
```
$ cd hxeapp
```


Create a file called `app.yaml`:

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

Create a file called `appconfig.py`.
```
hxeapp$ touch appconfig.py
```

Copy the contents below to the `appconfig.py` file.
```
   from google.appengine.ext import vendor
   vendor.add('lib')
```

Deploy to the Google cloud

```
hxeapp$ gcloud app deploy
```

>**Note:**
> The deployment will take ~5 minutes

</br>

After successful deployment, open the application by the following command.


```
gcloud app browse
```


[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Deploy a GO Application)]

Go to the directory of where your GO Application is located.
```
$ cd hxeapp

```

Create a file called `app.yaml`:

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

Deploy to the Google cloud

```
hxeapp$ gcloud app deploy

```

>**Note:**
> The deployment will take ~5 minutes

After successful deployment, open the application by the following command.

```
gcloud app browse
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy a Ruby on Rails application)]

Go to the directory of where your Ruby on Rails Application is located.
```
$ cd hxeapp

```

Create a file `app.yaml` :

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


Run the following command and copy the generated key in the console output.

```
hxeapp$ bundle exec rails secret
```

Add the below code to `app.yaml` file.

```
env_variables:
SECRET_KEY_BASE: <YOUR_GENERATED_LONG KEY>

```

Run the following command.

```
hxeapp$ RAILS_ENV=production bundle exec rails assets:precompile

```

Execute the command below to deploy the application.

```
hxeapp$ gcloud app deploy

```
>**Note:**
> The deployment will take ~5 minutes.

</br>

After successful deployment, open the application by the following command.

```
gcloud app browse
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy a Sinatra/Ruby application)]


Go to the directory of where your Sinatra/Ruby Application is located.

```
$ cd hxeapp

```

Create a file `app.yaml` :

```
hxeapp$ touch app.yaml
```
Copy the below code to the `app.yaml` file:

```
runtime: ruby
env: flex
entrypoint: bundle exec ruby app.rb
```
</br>

>**Note:**
> Make sure the `sinatra` gem is listed in your `Gemfile` in the Application folder.

</br>

Execute the command below to deploy the application.

```
hxeapp$ gcloud app deploy
```

>**Note:**
> The deployment will take ~5 minutes.

</br>

After successful deployment, open the application by the following command.

```
gcloud app browse
```


[ACCORDION-END]
---

## Next Steps
 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page
