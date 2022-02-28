---
author_name: Gergana Tsakova
author_profile: https://github.com/Joysie
title: Create a Python Application via Cloud Foundry Command Line Interface
description: Create a simple Python application in the Cloud Foundry Command Line Interface (cf CLI) and enable services for it.
auto_validation: true
time: 30
tags: [ tutorial>beginner, software-product>sap-hana]
primary_tag: software-product>sap-btp--cloud-foundry-environment
---

## Prerequisites
 - You have registered for a Cloud Foundry [trial account] (hcp-create-trial-account) on SAP Business Technology Platform.
 - [Python] (https://www.python.org/downloads/) version 3.6.x or higher is installed locally. In this tutorial, we use Python **3.9.6**.
 - [cf CLI] (https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/4ef907afb1254e8286882a2bdef0edf4.html) is installed locally.
 - [npm] (https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) is installed locally.
- (Optional) You have an integrated development environment (IDE) installed locally. For example: [Visual Studio Code] (https://code.visualstudio.com/download)
 - You have installed the `virtualenv` tool. It creates a folder, which contains all the necessary executables to use the packages that your Python project would need. To install it locally, execute the following command in the Python installation path:   
&nbsp;
 `<Python_installation_path>\Python39\Scripts>pip install virtualenv`




## Details
### You will learn
  - How to create a simple "Hello World" application in Python
  - How to consume SAP BTP services from it
  - How to run authentication checks for your app
  - How to run authorization checks for your app


This tutorial will guide you through creating and setting up a simple Python application by using cf CLI. You will start by building and deploying a web application that returns sample data – a **Hello World!** message, and then invoking this app through another one - a web microservice (application router).

---

[ACCORDION-BEGIN [Step 1: ](Log on to SAP BTP)]

First, you need to connect to the SAP BTP, Cloud Foundry environment with your trial account. By default, a space `dev` is created for you. Your Cloud Foundry URL depends on the region where the API endpoint belongs to. To find out which one is yours, see:  [Regions and API Endpoints Available for the CF Environment] (https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/f344a57233d34199b2123b9620d0bb41.html?version=Cloud) -> **Regions for Trial Accounts**.

In this tutorial, we use `eu10.hana.ondemand.com` as an example.

1. Open a command-line console.

2. Set the Cloud Foundry API endpoint for your trial account. Execute (using your actual region URL):

    ```Bash/Shell
    cf api https://api.cf.eu10.hana.ondemand.com
    ```
3. Log in to SAP BTP, Cloud Foundry environment:

    ```Bash/Shell
    cf login
    ```

4. When prompted, enter your user credentials – the email and password you have used to register your SAP BTP trial account.

> **IMPORTANT**: If the authentication fails, even though you've entered correct credentials, try [logging in via single sign-on] (https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/e1009b4aa486462a8951c4d499ce6d4c.html?version=Cloud).


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Python application)]

You're going to create a simple Python application.

1. On your local file system, create a new directory. For example: `python-tutorial`

2. Inside this directory, create a `manifest.yml` file with the following content:

    ```YAML
    ---
    applications:
    - name: myapp
      routes:
        - route: python-1234567trial.cfapps.eu10.hana.ondemand.com
      path: ./
      memory: 128M
      buildpack: python_buildpack
      command: python server.py
    ```

    The `manifest.yml` file represents the configuration describing your application and how it will be deployed to Cloud Foundry.

    > **IMPORTANT**: Make sure you don't have another application with the name `myapp` in your `dev` space.   

    > Also bear in mind that your application's technical name (in the route) must be **unique** in the whole Cloud Foundry landscape. We advice that you use your trial account ID to construct the technical name. In this tutorial, we use:  `python-1234567trial`



3. Specify the Python runtime version that your application will run on. To do that, create a `runtime.txt` file with the following content:

    ```TXT
    python-3.9.6
    ```

4. This application will be a web server utilizing the Flask web framework. To specify Flask as an application dependency, create a `requirements.txt` file with the following content:

    ```TXT
    Flask==2.0.1
    ```

5. Create a `server.py` file with the following application logic:

    ```Python
    import os
    from flask import Flask
    app = Flask(__name__)
    port = int(os.environ.get('PORT', 3000))
    @app.route('/')
    def hello():
        return "Hello World!"
    if __name__ == '__main__':
        app.run(host='0.0.0.0', port=port)
    ```

    This is a simple server, which will return a **Hello World!** message when requested.

6.	Deploy the application on Cloud Foundry. To do that, in the `python-tutorial` directory, execute:

    ```Bash/Shell
    cf push
    ```

    > Make sure you always execute `cf push` in the directory where the `manifest.yml` file is located - that is `python-tutorial`.

7.  When the staging and deployment steps are complete, the `myapp` application should be successfully started and its details displayed in the command console.

8.	Now open a browser window and enter the URL of the `myapp` application (see the route).

    That is:  `https://python-1234567trial.cfapps.eu10.hana.ondemand.com`

#### RESULT

Your Python application is successfully deployed and running on the SAP BTP, Cloud Foundry environment. A **Hello World!** message is displayed in the browser.

[VALIDATE_2]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Consume SAP BTP services)]

Now you're going to connect to SAP HANA - a service that runs on the SAP BTP, Cloud Foundry environment - and consume it in your application.

1.	Create an instance of the SAP HANA service. To do that, execute the following command, which creates a `hanatrial` service instance named `pyhana` with plan `securestore`:

    ```Bash/Shell
    cf create-service hanatrial securestore pyhana
    ```

2.	Bind this service instance to the application.
    <ol type="a">
    <li> Add the `pyhana` service in the `manifest.yml` file so that its content looks like this:

    ```YAML
    ---
    applications:
    - name: myapp
      routes:
        - route: python-1234567trial.cfapps.eu10.hana.ondemand.com
      path: ./
      memory: 128M
      buildpack: python_buildpack
      command: python server.py
      services:
      - pyhana
    ```
      </li>
    <li> To consume the service inside the application, you need to read the service settings and credentials from the application. To do that, use the `cfenv` Python module. Add two more lines to the `requirements.txt` file so that its content looks like this:

    ```TXT
    Flask==2.0.1
    cfenv==0.5.3
    hdbcli==2.9.23
    ```
    </li>
    <li> Modify the `server.py` file to include additional lines of code, which read the service information from the environment. Replace the initial content with the following one:

    ```Python
    import os
    from flask import Flask
    from cfenv import AppEnv

    app = Flask(__name__)
    env = AppEnv()

    port = int(os.environ.get('PORT', 3000))
    hana_service = 'hanatrial'
    hana = env.get_service(label=hana_service)

    @app.route('/')
    def hello():
        return "Hello World!"

    if __name__ == '__main__':
        app.run(host='0.0.0.0', port=port)
    ```
</li>
</ol>

3. Restage the application to bind and connect the `pyhana` service  to it. Execute:

    ```bash
    cf restage myapp
    ```

4.	Modify the `server.py` file again to execute a query with the `hdbcli` driver. Also, after being requested, the application will now return a SAP HANA-related result. Replace the current content with the following:

    ```Python
    import os
    from flask import Flask
    from cfenv import AppEnv
    from hdbcli import dbapi

    app = Flask(__name__)
    env = AppEnv()

    hana_service = 'hanatrial'
    hana = env.get_service(label=hana_service)

    port = int(os.environ.get('PORT', 3000))
    @app.route('/')
    def hello():
        if hana is None:
            return "Can't connect to HANA service '{}' – check service name?".format(hana_service)
        else:
            conn = dbapi.connect(address=hana.credentials['host'],
                                 port=int(hana.credentials['port']),
                                 user=hana.credentials['user'],
                                 password=hana.credentials['password'],
                                 encrypt='true',
                                 sslTrustStore=hana.credentials['certificate'])

            cursor = conn.cursor()
            cursor.execute("select CURRENT_UTCTIMESTAMP from DUMMY")
            ro = cursor.fetchone()
            cursor.close()
            conn.close()

            return "Current time is: " + str(ro["CURRENT_UTCTIMESTAMP"])

    if __name__ == '__main__':
        app.run(host='0.0.0.0', port=port)
    ```

5.	In the `python-tutorial` directory, execute:

    ```bash
    cf push
    ```

6.	Refresh the URL of the `myapp` application (previously loaded in a browser window).

    That is:   `https://python-1234567trial.cfapps.eu10.hana.ondemand.com`

#### RESULT

The current SAP HANA time is displayed, in UTC time zone.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run an Authentication Check)]

Authentication in the SAP BTP, Cloud Foundry environment is provided by the Authorization and Trust Management (XSUAA) service. In this example, OAuth 2.0 is used as the authentication mechanism. The simplest way to add authentication is to use the Node.js `@sap/approuter` package. To do that, a separate Node.js micro-service will be created, acting as an entry point for the application.

1.	In the `python-tutorial` directory, create an `xs-security.json` file for your application with the following content:

    ```JSON
    {
      "xsappname" : "myapp",
      "tenant-mode" : "dedicated"
    }
    ```

2.	Create an `xsuaa` service instance named `pyuaa` with plan `application`, by executing the following command:

    ```Bash
    cf create-service xsuaa application pyuaa -c xs-security.json
    ```

3.	Add the `pyuaa` service in `manifest.yml` so the file looks like this:

    ```YAML
    ---
    applications:
    - name: myapp
      routes:
        - route: python-1234567trial.cfapps.eu10.hana.ondemand.com
      path: ./
      memory: 128M
      buildpack: python_buildpack
      command: python server.py
      services:
      - pyhana
      - pyuaa
    ```

    The `pyuaa` service instance will be bound to the `myapp` application during deployment.

4.	To create a second microservice (the application router), go to the `python-tutorial` directory and create a subdirectory named  `web`.

    > **IMPORTANT**: Make sure you don't have another application with the name `web` in your `dev` space!


5.	Inside the `web` directory, create a subdirectory `resources`. This directory will provide the business application's static resources.

6.	Inside the `resources` directory, create an `index.html` file with the following content:

    ```HTML
    <html>
    <head>
    	<title>Python Tutorial</title>
    </head>
    <body>
      <h1>Python Tutorial</h1>
      <a href="/myapp/">My Application</a>
    </body>
    </html>
    ```

7.	In the `web` directory, execute:

    ```Bash
    npm init
    ```

    This will walk you through creating a `package.json` file in the `web` directory. Press **Enter** on every step.

8.	Now you need to create a directory `web/node_modules/@sap` and install an `approuter` package in it. To do that, in the `web` directory execute:

    ```Bash
    npm install @sap/approuter --save
    ```

9.	In the `web` directory, open the `package.json` file and replace the **scripts** section with the following:

    ```JSON
    "scripts": {
          "start": "node node_modules/@sap/approuter/approuter.js"
    },
    ```

10.	Now you need to add the `web` application to your project and bind the XSAUAA service name (`pyuaa`) to it. To do that, insert the following content at the end of your `manifest.yml` file.

    ```YAML
    - name: web
      routes:
        - route: web-1234567trial.cfapps.eu10.hana.ondemand.com
      path: web
      memory: 128M
      env:
        destinations: >
          [
            {
              "name":"myapp",
              "url":"https://python-1234567trial.cfapps.eu10.hana.ondemand.com",
              "forwardAuthToken": true
            }
          ]
      services:
        - pyuaa
    ```

    >   Here you can follow the same pattern for constructing the technical name of the `web` application - by using your trial account ID.

11.	In the `web` directory, create an `xs-app.json` file with the following content:

    ```JSON
    {
      "routes": [
        {
          "source": "^/myapp/(.*)$",
          "target": "$1",
          "destination": "myapp"
        }
      ]
    }
    ```

    With this configuration, the incoming request is forwarded to the `myapp` application, configured as a destination. By default, every route requires OAuth authentication, so the requests to this path will require an authenticated user.

12.	Go to the `python-tutorial` directory and execute:

    ```Bash
    cf push
    ```
    This command will update the `myapp` application and deploy the `web` application.

    > ### What's going on?

    >As of this point of the tutorial, the URL of the `web` application will be requested instead of the `myapp` URL. It will then forward the requests to the `myapp` application.


13.	When the staging and deployment steps are complete, the `web` application should be successfully started and its details displayed in the command console.

14.	Open a new browser tab or window, and enter the URL of the `web` application.

    That is:   `https://web-1234567trial.cfapps.eu10.hana.ondemand.com`

15.	Enter the credentials for your SAP BTP user.

#### RESULT

A simple application page with title **Python Tutorial** is displayed. When you click the `My Application` link, the current SAP HANA time is displayed, in UTC time zone.

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run an Authorization Check)]

Authorization in the SAP BTP, Cloud Foundry environment is provided by the XSUAA service. In the previous example, the `@sap/approuter` package was added to provide a central entry point for the business application and to enable authentication. Now to extend the example, authorization will be added.

1.	Add the `sap-xssec` security library to the `requirements.txt` file, to place restrictions on the content you serve. The file should look like this:

    ```TXT
    Flask==2.0.1
    cfenv==0.5.3
    hdbcli==2.9.23
    sap-xssec==3.0.0
    ```


2.	Modify the `server.py` file to use the SAP `xssec` library. Replace the current content with the following:

    ```Python
    import os
    from flask import Flask
    from cfenv import AppEnv
    from flask import request
    from flask import abort

    from sap import xssec

    from hdbcli import dbapi

    app = Flask(__name__)
    env = AppEnv()

    port = int(os.environ.get('PORT', 3000))
    hana = env.get_service(label='hanatrial')
    uaa_service = env.get_service(name='pyuaa').credentials

    @app.route('/')
    def hello():
         if 'authorization' not in request.headers:
             abort(403)
         access_token = request.headers.get('authorization')[7:]
         security_context = xssec.create_security_context(access_token, uaa_service)
         isAuthorized = security_context.check_scope('openid')
         if not isAuthorized:
             abort(403)

         conn = dbapi.connect(address=hana.credentials['host'],
                               port=int(hana.credentials['port']),
                               user=hana.credentials['user'],
                               password=hana.credentials['password'],
                               encrypt='true',
                               sslTrustStore=hana.credentials['certificate'])

         cursor = conn.cursor()
         cursor.execute("select CURRENT_UTCTIMESTAMP from DUMMY")
         ro = cursor.fetchone()
         cursor.close()
         conn.close()

         return "Current time is: " + str(ro["CURRENT_UTCTIMESTAMP"])

    if __name__ == '__main__':
      app.run(host='0.0.0.0', port=port)
    ```

3.	Go to the `python-tutorial` directory and execute:

    ```Bash
    cf push
    ```

    This command will update both **myapp** and **web**.

4.	Try to access `myapp` again (in a browser) in both ways – directly, and through the `web` application router.


#### RESULT

Accessing the `myapp` application results in the following:

- If you try to access it directly, a `403 Forbidden` response is displayed due to lack or permissions (lack of authorization header).

- If you try to access it through the `web` application router, the current SAP HANA time is displayed (in UTC time zone) – provided that you have the `openid` scope assigned to your user. Since the OAuth 2.0 client is used, the `openid` scope is assigned to your user by default, the correct authorization header is declared, and thus you are allowed to access the `myapp` application.


[VALIDATE_5]
[ACCORDION-END]
