---
parser: v2
author_name: Gergana Tsakova
author_profile: https://github.com/Joysie
title: Create a Node.js Application with Cloud Foundry Node.js Buildpack 
description: Create a simple application and enable services for it, by using  the Cloud Foundry Node.js Buildpack and Cloud Foundry Command Line Interface (cf CLI).
auto_validation: true
time: 40
tags: [ tutorial>beginner, software-product>sap-btp--cloud-foundry-environment, software-product-function>sap-btp-cockpit]
primary_tag: programming-tool>node-js
---


## You will learn
  - How to create a simple "Hello World" application in Node.js
  - How to create an application router for it
  - How to run authentication and authorization checks via the XSUAA service  

## Prerequisites
 - You have a trial or a productive account for SAP Business Technology Platform (SAP BTP). If you don't have such yet, you can create one so you can [try out services for free] (https://developers.sap.com/tutorials/btp-free-tier-account.html).
 - You have created a subaccount and a space on Cloud Foundry Environment.
 - [cf CLI] (https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/4ef907afb1254e8286882a2bdef0edf4.html) is installed locally.
 - [Node.js] (https://nodejs.org/en/about/releases/) and [npm] (https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) are installed locally. Make sure you have the latest Node.js version. In this tutorial, we use v.16.
 - You have installed an integrated development environment, for example [Visual Studio Code] (https://code.visualstudio.com/).

## Intro
This tutorial will guide you through creating and setting up a simple Node.js application in cf CLI. You will start by building and deploying a web application that returns simple data – a **Hello World!** message, and then invoking this app through a web microservice (application router). Finally, you will set authentication checks and authorization roles to properly access and manage your web application.

---

### Log on to SAP BTP


First, you need to connect to the SAP BTP, Cloud Foundry environment with your productive subaccount. Your Cloud Foundry URL depends on the region where the API endpoint belongs to. To find out which one is yours, see:  [Regions and API Endpoints Available for the CF Environment] (https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/f344a57233d34199b2123b9620d0bb41.html?version=Cloud)

In this tutorial, we use `eu20.hana.ondemand.com` as an example.

1. Open a command-line console.

2. Set the Cloud Foundry API endpoint for your subaccount. Execute (using your actual region URL):

    ```Bash/Shell
    cf api https://api.cf.eu20.hana.ondemand.com
    ```
3. Log in to SAP BTP, Cloud Foundry environment:

    ```Bash/Shell
    cf login
    ```

4. When prompted, enter your user credentials – the email and password you have used to register your trial or productive SAP BTP account.
 
    > **IMPORTANT**: If the authentication fails, even though you've entered correct credentials, try [logging in via single sign-on] (https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/e1009b4aa486462a8951c4d499ce6d4c.html?version=Cloud).


5. Choose the org name and space where you want to create your application.
   
    > If you're using a trial account, you don't need to choose anything. You can use only one org name, and your default space is `dev`.


#### RESULT

Details about your personal SAP BTP subaccount are displayed (API endpoint, user, organization, space).


### Create a Node.js application


You're going to create a simple Node.js application.

1. In your local file system, create a new directory (folder). For example: `node-tutorial`

2. From your Visual Studio Code, open the `node-tutorial` folder.

3. In this folder, create a file `manifest.yml` with the following content:

    ```YAML
    ---
    applications:
    - name: myapp
      random-route: true
      path: myapp
      memory: 128M
      buildpack: nodejs_buildpack
    ```

    The `manifest.yml` file represents the configuration describing your application and how it will be deployed to Cloud Foundry.

    > **IMPORTANT**: Make sure you don't have another application with the name `myapp` in your space! If you do, use a different name and adjust the whole tutorial according to it.   


4. Inside `node-tutorial`, create a subfolder `myapp`.

5. In the `myapp` directory, execute:

    ```Bash/Shell
    npm init
    ```

    Press **Enter** on every step. This process will walk you through creating a `package.json` file in the `myapp` folder.

6. Then, still in the `myapp` directory, execute:

    ```Bash/Shell
    npm install express --save
    ```

    This operation adds the `express` package as a dependency in the `package.json` file.

    After the installation is completed, the content of `package.json` should look like this:

    ```JSON
    {
      "name": "myapp",
      "version": "1.0.0",
      "description": "",
      "main": "index.js",
      "scripts": {
        "test": "echo \"Error: no test specified\" && exit 1"
      },
      "author": "",
      "license": "ISC",
      "dependencies": {
        "express": "^4.18.2"
      }
    }
    ```

7. Add engines to the `package.json` file and update the `scripts` section. Your `package.json` file should look like this:

    ```JSON
    {
      "name": "myapp",
      "version": "1.0.0",
      "description": "My simple Node.js app",
      "main": "index.js",
      "engines": {
        "node": "16.x.x"
      },
      "scripts": {
        "start": "node start.js"
      },
      "author": "",
      "license": "ISC",
      "dependencies": {
        "express": "^4.18.2"
      }
    }
    ```

8. Inside the `myapp` folder, create a file `start.js` with the following content:

    ```JavaScript
    const express = require('express');
    const app = express();

    app.get('/', function (req, res) {
      res.send('Hello World!');
    });

    const port = process.env.PORT || 3000;
    app.listen(port, function () {
      console.log('myapp listening on port ' + port);
    });
    ```

    This creates a simple web application returning a **Hello World!** message when requested. The `express` module represents the web server part of this application. Once these steps are completed, you can see that the `express` package has been installed in the `node_modules` folder.

9. Deploy the application on Cloud Foundry. To do that, in the `node-tutorial` directory, execute:

    ```Bash/Shell
    cf push
    ```

    > Make sure you always execute `cf push` in the directory where the `manifest.yml` file is located! In this case, that's `node-tutorial`.

10. When the staging and deployment steps are completed, the `myapp` application should be successfully started and its details displayed in the command console.

11.	Open a browser window and enter the generated URL of the `myapp` application (see `routes`).

    For example:  `https://myapp-purple-tiger.cfapps.eu20.hana.ondemand.com`

#### RESULT

Your Node.js application is successfully deployed and running on the SAP BTP, Cloud Foundry environment. A **Hello World!** message is displayed in the browser.




### Run an Authentication Check


Authentication in the SAP BTP, Cloud Foundry environment is provided by the Authorization and Trust Management (XSUAA) service. In this example, OAuth 2.0 is used as the authentication mechanism. The simplest way to add authentication is to use the Node.js `@sap/approuter` package. To do that, a separate Node.js micro-service will be created, acting as an entry point for the application.

1.	In the `node-tutorial` folder, create an `xs-security.json` file for your application with the following content:

    ```JSON
    {
      "xsappname" : "myapp",
      "tenant-mode" : "dedicated"
    }
    ```

    > **IMPORTANT**: For trial accounts, enter the following additional `oauth2-configuration` code in your `xs-security.json` file:
    
    
    ```JSON
    {
      "xsappname" : "myapp",
      "tenant-mode" : "dedicated",
      "oauth2-configuration": {
        "redirect-uris": [
            "https://*.cfapps.eu20.hana.ondemand.com/**"
          ]
        }
    }
    ``` 

2.	Create an `xsuaa` service instance named `nodeuaa` with plan `application`. To do that, execute the following command in the `node-tutorial` directory:

    ```Bash/Shell
    cf create-service xsuaa application nodeuaa -c xs-security.json
    ```

3.	Add the `nodeuaa` service in `manifest.yml` so the file looks like this:

    ```YAML
    ---
    applications:
    - name: myapp
      random-route: true
      path: myapp
      memory: 128M
      buildpack: nodejs_buildpack
      services:
      - nodeuaa
    ```

    The `nodeuaa` service instance will be bound to the `myapp` application during deployment.

4.	Now you have to create a microservice (the application router). To do that, go to the `node-tutorial` folder and create a subfolder `web`.

    > **IMPORTANT**: Make sure you don't have another application with the name `web` in your space! If you do, use a different name and adjust the rest of the tutorial according to it.

5.	Inside the `web` folder, create a subfolder `resources`. This folder will provide the business application's static resources.

6.	Inside the `resources` folder, create an `index.html` file with the following content:

    ```HTML
    <html>
    <head>
      <title>Node.js Tutorial</title>
    </head>
    <body>
      <h1>Node.js Tutorial</h1>
      <a href="/myapp/">My Node.js Application</a>
    </body>
    </html>
    ```

    This will be the start page of the `myapp` application.

7. In the `web` directory, execute:

    ```Bash/Shell
    npm init
    ```

    Press **Enter** on every step. This process will walk you through creating a `package.json` file in the `web` folder. 

8.	Now you need to create a directory `web/node_modules/@sap` and install an `approuter` package in it. To do that, in the `web` directory execute:

    ```Bash/Shell
    npm install @sap/approuter --save
    ```

9.	In the `web` folder, open the `package.json` file and replace the **scripts** section with the following:

    ```JSON
    "scripts": {
        "start": "node node_modules/@sap/approuter/approuter.js"
    },
    ```

10.	Now you need to add the `web` application to your project and bind the XSUAA service instance (`nodeuaa`) to it. To do that, insert the following content **at the end** of your `manifest.yml` file.


    ```YAML
    - name: web
      random-route: true
      path: web
      memory: 128M
      env:
        destinations: >
          [
            {
              "name":"myapp",
              "url":"https://myapp-purple-tiger.cfapps.eu20.hana.ondemand.com",
              "forwardAuthToken": true
            }
          ]
      services:
      - nodeuaa
    ```
     
    > For the `url` value, enter **your** generated URL for the `myapp` application. 

11.	In the `web` folder, create an `xs-app.json` file with the following content:

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

12. In the `myapp` directory, execute the following commands (one by one) to download packages `@sap/xssec`, `@sap/xsenv`, and `passport`:

    ```Bash/Shell
    npm install @sap/xssec --save

    npm install @sap/xsenv --save

    npm install passport --save
    ```

13. Verify that the request is authenticated. Check the JWT token in the request using the `JWTStrategy` provided by the `@sap/xssec` package. To do that, go to the `myapp` directory, and replace the content of the `start.js` file with the following:

    ```JavaScript
    const express = require('express');
    const passport = require('passport');
    const xsenv = require('@sap/xsenv');
    const JWTStrategy = require('@sap/xssec').JWTStrategy;

    const app = express();

    const services = xsenv.getServices({ uaa:'nodeuaa' });

    passport.use(new JWTStrategy(services.uaa));

    app.use(passport.initialize());
    app.use(passport.authenticate('JWT', { session: false }));

    app.get('/', function (req, res, next) {
      res.send('Application user: ' + req.user.id);
    });

    const port = process.env.PORT || 3000;
    app.listen(port, function () {
      console.log('myapp listening on port ' + port);
    });
    ```

12.	Go to the `node-tutorial` directory and execute:

    ```Bash/Shell
    cf push
    ```

    This command will update the `myapp` application and deploy the `web` application.

    > ### What's going on?

    > At this point of the tutorial, the URL of the `web` application will be requested instead of the `myapp` URL. It will then forward the requests to the `myapp` application.


13.	When the staging and deployment steps are completed, the `web` application should be successfully started and its details displayed in the command console.

14.	Open a new browser tab or window, and enter the generated URL of the `web` application.

    For example:   `https://web-happy-ladybug.cfapps.eu20.hana.ondemand.com`

15.	Enter the credentials for your SAP BTP user.

    > Both the `myapp` and `web` applications are bound to the same Authorization and Trust Management (XSUAA) service instance `nodeuaa`. In this scenario, the authentication is handled by XSUAA through the application router.

#### RESULT

- Click the `My Node.js Application` link. The browser window displays **Application user:** `<e-mail>`, showing the email you have used for your Cloud Foundry logon.

- Check that the `myapp` application is not accessible without authentication. To do that, refresh its previously loaded URL in a web browser – you should get a response `401 Unauthorized`.




### Run an Authorization Check


Authorization in the SAP BTP, Cloud Foundry environment is also provided by the XSUAA service. In the previous example, the `@sap/approuter` package was added to provide a central entry point for the business application and to enable authentication. Now to extend the example, authorization will be added through the implementation of a `users` REST service. Different authorization checks will be introduced for the GET and CREATE operations to demonstrate how authorization works. The authorization concept includes elements such as roles, scopes, and attributes provided in the security descriptor file `xs-security.json`. For more information, see: [Application Security Descriptor Configuration Syntax] (https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/517895a9612241259d6941dbf9ad81cb.html?version=Cloud)

1. To introduce application roles, open the `xs-security.json` in the `node-tutorial` folder, and add scopes and role templates as follows:

    ```JSON
    {
        "xsappname": "myapp",
        "tenant-mode": "dedicated",
        "scopes": [
          {
            "name": "$XSAPPNAME.Display",
            "description": "Display Users"
          },
          {
            "name": "$XSAPPNAME.Update",
            "description": "Update Users"
          }
        ],
        "role-templates": [
          {
            "name": "Viewer",
            "description": "View Users",
            "scope-references": [
              "$XSAPPNAME.Display"
            ]
          },
          {
            "name": "Manager",
            "description": "Maintain Users",
            "scope-references": [
              "$XSAPPNAME.Display",
              "$XSAPPNAME.Update"
            ]
          }
        ]
      }
    ```

    > For trial accounts, adjust the code with the `oauth2-configuration` part. 

    Two roles (`Viewer` and `Manager`) are introduced. These roles represent sets of OAuth 2.0 scopes or actions. The scopes are used later in the microservice's code for authorization checks.


2. Update the XSUAA service. To do that, in the `node-tutorial` directory execute:

    ```Bash/Shell
    cf update-service nodeuaa -c xs-security.json
    ```

3.	In the `myapp` folder, create a file `users.json` with the following content:

    ```JSON
    [{
        "id": 0,
        "name": "John"
      },
      {
        "id": 1,
        "name": "Paula"
    }]
    ```

    This will be the initial list of users for the REST service.

4. You need to add a dependency to `body-parser` that will be used for JSON parsing. To do that, in the `myapp` directory, execute:

    ```Bash/Shell
    npm install body-parser --save
    ```

5.	Change the `start.js` file, adding GET and POST operations for the `users` REST endpoint. You can replace the initial code with the following one:

    ```JavaScript
    const express = require('express');
    const passport = require('passport');
    const bodyParser = require('body-parser');
    const xsenv = require('@sap/xsenv');
    const JWTStrategy = require('@sap/xssec').JWTStrategy;

    const users = require('./users.json');
    const app = express();

    const services = xsenv.getServices({ uaa: 'nodeuaa' });

    passport.use(new JWTStrategy(services.uaa));

    app.use(bodyParser.json());
    app.use(passport.initialize());
    app.use(passport.authenticate('JWT', { session: false }));

    app.get('/users', function (req, res) {
      var isAuthorized = req.authInfo.checkScope('$XSAPPNAME.Display');
      if (isAuthorized) {
        res.status(200).json(users);
      } else {
        res.status(403).send('Forbidden');
      }
    });

    app.post('/users', function (req, res) {
      const isAuthorized = req.authInfo.checkScope('$XSAPPNAME.Update');
      if (!isAuthorized) {
        res.status(403).json('Forbidden');
        return;
      }

      var newUser = req.body;
      newUser.id = users.length;
      users.push(newUser);

      res.status(201).json(newUser);
    });

    const port = process.env.PORT || 3000;
    app.listen(port, function () {
      console.log('myapp listening on port ' + port);
    });
    ```

    > **IMPORTANT:** Authorization checks are enforced by the `xssec` package in the `@sap` directory. To every request object, using `passport` and `xssec.JWTStrategy`, a security context is attached as an `authInfo` object. The resulting request object is initialized with the incoming JWT token. To check the full list of methods and properties of the security context, see: [Authentication for Node.js Applications] (https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/4902b6e66cbd42648b5d9eaddc6a363d.html?version=Cloud)

      As defined in the `start.js` file, for HTTP GET requests users need the `Display` scope to be authorized. For HTTP POST requests, they need to have the `Update` scope assigned.

6.	Update the UI to be able to send POST requests. To do that, go to `web>resources` and in the `index.html` file, replace the content with the following code:

    ```HTML
    <html>
    <head>
      <title>JavaScript Tutorial</title>
      <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
      <script>
        function fetchCsrfToken(callback) {
          jQuery.ajax({
              url: '/myapp/users',
              type: 'HEAD',
              headers: { 'x-csrf-token': 'fetch' }
            })
            .done(function(message, text, jqXHR) {
              callback(jqXHR.getResponseHeader('x-csrf-token'))
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
              alert('Error fetching CSRF token: ' + jqXHR.status + ' ' + errorThrown);
            });
        }

        function addNewUser(token) {
          var name = jQuery('#name').val() || '--';
          jQuery.ajax({
              url: '/myapp/users',
              type: 'POST',
              headers: { 'x-csrf-token': token },
              contentType: 'application/json',
              data: JSON.stringify({ name: name })
            })
            .done(function() {
              alert( 'success' );
              window.location = '/myapp/users'
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
              alert('Error adding new user: ' + jqXHR.status + ' ' + errorThrown);
            });
        }

        function addUser() {
          fetchCsrfToken(addNewUser);
        }
      </script>
    </head>
    <body>
      <h1>JavaScript Tutorial</h1>
      <a href="/myapp/users">Show users</a>
      <br/>
      <br/>
      <input type="text" id="name" placeholder="Type user name"></input>
      <input type="button" value="Add User" onClick="javascript: addUser()"></input>
    </body>
    </html>
    ```

    > The UI contains a link to get all users, an input box to enter a user name, and a button to send "create a new user" requests.

    > In the sample, the code seems more complicated than expected. Clicking the `Add User` button tries to fetch a CSRF token, and on SUCCESS it sends a POST request with the users' data as a JSON body. As you are using the application router, you need to get a CSRF token before sending the POST request. This token is required for all requests that change the state.

7.	Go to the `node-tutorial` directory and execute:

    ```Bash/Shell
    cf push
    ```

    This command will update both applications (`myapp` and `web`).

8. Try to access `myapp` again (in a browser) in both ways – directly and through the `web` application router.

#### RESULT

- When you access it directly, you should still get a response `401 Unauthorized`. This is a correct and expected behavior.
- When you access the `web` application and click the `Show users` link, it should result in a `403 Forbidden` response due to missing permissions. The same error is thrown if you try to add a new user.

To get permissions, you need to create a role collection containing the roles `Viewer` and `Manager` and assign these roles to your user. You can do this only from the SAP BTP cockpit.


### Assigning Roles to a User in SAP BTP Cockpit


1. Open the SAP BTP cockpit and go to your subaccount.

2. From the left-side menu, navigate to `Security` > `Role Collections`.

3. Create a new role collection. For example, `MyNodeAppRC`.

4. Click this role collection and then choose `Edit`.

5. In the `Roles` tab, click the `Role Name` field.

6. Type **Viewer**. From the displayed results, select the `Viewer` role that corresponds to your application, and choose `Add`.

7. Repeat the same steps for `Manager`.

8. Now go to the `Users` tab, and in the `ID` field, enter your e-mail. Then enter the same e-mail in the `E-Mail` field.

9. Save your changes.

    > Your role collection is now assigned to your user and contains the roles you need to view and manage the content of your application.

    Now you need to apply these changes to the `myapp` application by redeploying it again.

10. Go back to the command line, and in the `node-tutorial` directory execute:

    ```Bash/Shell
    cf push
    ```


#### RESULT

Accessing the `myapp` application results in the following:

- If you try to access it directly, a response `401 Unauthorized` is still displayed due to lack or permissions (roles). This is a correct and expected behavior.

- If you try to access it through the `web` application router, the `Show users` link will show the list of users - **John** and **Paula**. If you enter a new name, it will be successfully recorded in the user database.

    > In order for the new result to take effect immediately, you might need to clear the cache of your browser. Or just open the `web` application URL in a private/incognito browser tab.

---
