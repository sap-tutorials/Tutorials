---
title: Deploy a Node.js Application for SAP HANA, Express Edition
description: Deploy a sample Node.jsapplication which connects to SAP HANA, Express Edition.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, topic>big-data, topic>cloud, products>sap-hana, products>sap-hana\,-express-edition  ]
---

## Prerequisites and Assumptions
 - **Proficiency:** Beginner
 - User has Python environment setup already (preferably Python 3.5 or above)
 - User knows how to install packages and develop using Python and Node.js
 - Setup: `HANA, express edition` must be running and accessible from your client platform. For instructions on how to setup a `HANA, express edition` see the [HANA Express database deploy tutorial](https://www.sap.com/developer/how-tos/2017/07/hxe-db-deploy.html).
 - This tutorial assumes that you have a database login that can access the `M_DATABASE` view in the `HANA, express edition` `SystemDB`.
 - **Tutorials:** [Create a new project using Node.js and `HANA, express edition`]


## Next Steps
 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page

## Details
### You will learn  
This tutorial will guide you through the process to deploy a sample Node.js application  which connects to SAP HANA, Express Edition.

### Time to Complete
**30 Min**

---

[ACCORDION-BEGIN [Step 1: ](Prepare Environment for Node.js)]

1. Find the right package to download from [Node.js Foundation](https://nodejs.org/en/download/)

   Look for Windows download

   ###### Note prior to executing the installation steps make sure that proxy configuration are set properly

2. Install support package for `Node.js` and Express

   Follow the prompts and install `Node.js`. The installation downloads to the default location.

   In case you need to change the installation location from the defaults, make sure to update the PATH variable

   ```
   % npm install -g express-generator
   ```

   [ACCORDION-END]

   [ACCORDION-BEGIN [Step 2: ](Develop Simple Node.js Application)]

1. Create a simple standalone `node.js` app:
   ```
   % mkdir hxeapp
   % cd hxeapp
   ```
   Edit a file named `hxeapp.js` with the following content:

   ```
   console.log ("Hello Node World!")
   ```
   Test the simple app created in the previous step

   ```
   % cd hxeapp
   % node hxeapp.js
   ```
2. Access the HANA express content from this Node application.

   First install HANA database driver to Node. Ignore any warnings that it may produce.

   ```
   % npm install hdb
   ```

3. Create a Node application that allows you to access HANA express edition data.

   Open the `hxeapps.js` file and add the following content:

   ###### Note: HXE host IP Address provided should be accessible from any host

   ```j
   // Reference HANA driver in the Node app and update the connection details
   var hdb = require('hdb');

   // Modify the host IP and password based on your system information
     var client = hdb.createClient({
         host     : '<HXE host id IP>',
         port     : 39013,
         user     : 'SYSTEM',
         password : '<HXE SYSTEM user password>'
       });

   // select from a sample table
     var sql = "select * from m_database";

   // Execute the query and output the results
   // Note: this code doesn't handle any errors (e.g. connection failures etc.,)

     client.connect(function (err)
     {
             client.exec (sql, function (err, rows)
         {
             console.log('Results:', rows);
             client.end();
         });
     });
   ```
4. Replace the `<HXE host id IP>` with the IP address or host name of your HANA Express database server. Replace the `<HXE SYSTEM user password>` with your database system user password. Then save changes.

5. Start your application:
   ```
   % node hxeapp.js
   ```

   The above command should produce results something like the below:

   ```
   Results: [ { SYSTEM_ID: 'HXE',
    DATABASE_NAME: 'SYSTEMDB',
    HOST: 'hxehost',
    START_TIME: '2017-04-27T16:40:25.797',
    VERSION: '2.00.010.00.1491294693',
    USAGE: 'DEVELOPMENT' } ]
   ```
   [ACCORDION-END]

[ACCORDION-BEGIN [Step 3: Extend Application](Extend Simple Node.js Application to Run as a Web Application)]

1. Modify this app to be a `WebApp`.

   Install support packages for `Node.js` and Express.
   ###### Ignore warnings, if any are displayed:

   ```
   % npm install express
   ```
   Execute express to create a template for developing `WebApp`

   ```
   % cd hxeapp
   % express
   ```

   Above step will template files with dependencies.

   ###### Note: It was installed manually previously. We want to do it automatically along with other packages

   Modify `package.json` to include `hdb` driver.

   Edit `package.json` and add below line in the dependencies section.

   ```
   "hdb": "~0.12.4"
   ```

   After the edits, your `package.json` file would looks something like:
```
{
  "name": "hxeapp",
  "version": "0.0.0",
  "private": true,
  "scripts": {
    "start": "node ./bin/www"
    },
  "dependencies": {
    "body-parser": "~1.17.1",
    "cookie-parser": "~1.4.3",
    "debug": "~2.6.3",
    "express": "~4.15.2",
    "jade": "~1.11.0",
    "morgan": "~1.8.1",
    "pug": "^2.0.0-beta6",
    "serve-favicon": "~2.4.2",
    "hdb": "~0.12.4"
    }
}
```

2. Call `npm` `install` to pull the dependencies

   ```
   % npm install
   ```

   Create an app that does what our `hxeapp.js` does, but now we should modify the template file to do it.

   Your directory content would look something like:

```
app.js  
hxeapp.js      
public/  
views/
bin/    
node_modules/  
package.json      
routes/
```

```
$ ls routes/
index.js  users.js
```
3. Modify the `index.js` file to include the following code snippet:

```
var express = require('express');
   var hdb    = require('hdb');
   var router = express.Router();
   var client = hdb.createClient({
    host     : '<HXE host id IP>',
    port     : 39013,
    user     : 'SYSTEM',
    password : '<HXE SYSTEM user password>'
});
var sql = "select * from m_database";

router.get('/', function(req, res, next)
{
    client.connect(function (err)
    {
        if (err)
        {
            return console.error('Connect error', err);
        }
        console.log("Connected");
        client.exec(sql, function(err, rows)
        {
            res.render('index', { title: 'Sample Node.js on HANA express', datarow: rows });
        });
        client.end();
        if (err)
        {
            return console.error('Execute error:', err);
        }
      });
});
module.exports = router;
```
4. Replace the `<HXE host id IP>` with the IP address or host name of your HANA Express database server. Replace the `<HXE SYSTEM user password>` with your database system user password. Then save changes.

5. Edit the file `views/index.jade` that controls the `WebApp` output layout as follows:

```
extends layout

block content
  h1= title
  - each v, k in datarow[0]
    p #{k} : #{v}
```

6. Start the application

```
   % npm start
```

   Open a new browser and access `http://localhost:3000/`
   Notice that the browser displays information like what was displayed in earlier output.



[ACCORDION-END]

---
## Next Steps
 - If you intend to deploy your application to Google App Engine, go to the [Google App Engine deployment tutorial](https://www.sap.com/developer/how-tos/2017/07/hxe-app-deploy-gcp.html)
 - If you intend to deploy your application to Azure App Service, go to the [Azure App Service deployment tutorial](https://www.sap.com/developer/how-tos/2017/07/hxe-app-deploy-azure.html)
 - If you intend to deploy your application to AWS Elastic Beanstalk, go to the [AWS Elastic Beanstalk deployment tutorial](https://www.sap.com/developer/how-tos/2017/07/hxe-app-deploy-aws.html)
 - [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)

 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page
