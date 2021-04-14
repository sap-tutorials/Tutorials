---
title: Create an Application for Receiving Messages
description: Develop and deploy a basic Node.js-based messaging application for receiving messages from an SAP Event Mesh Queue.
time: 20
auto_validation: true
tags: [ tutorial>beginner, topic>node-js, products>sap-business-technology-platform, tutorial>license]
primary_tag: products>sap-event-mesh
---

## Prerequisites

## Details
### You will learn
  - How to create a basic messaging client application for receiving messages from a queue using SAP Event Mesh.
  - How to deploy this application to SAP Business Technology Platform
  - How to perform a quick test that your application works

[ACCORDION-BEGIN [Step 1: ](Install Node.js and create directory)]

1. Install Node.js and configure it locally.

2. Add the dependency in applications package.json and run npm for it:

    `npm install`


3. Create a directory that holds the files for your application - name it for example `Consumer`. Into this directory we will create three files:

      * The `manifest.yml` is the deployment descriptor and contains all required information to deploy an application to a SAP Business Technology Platform Cloud Foundry instance.

      * The `package.json` specifies the version of a package that your app depends on.

      * The `application file` holds the executable code.

Now create these files following the descriptions below into the created directory.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create manifest.yml file)]

Use the following code to create a `manifest.yml` file that binds your application to the messaging service.

You need to add `domain`, `messaging service` and your `queue name` in the indicated spaces. These you should have already configured and created in the earlier tutorial.

```yaml
applications:
      - name: consumer
        host: consumer-host
        domain: <REPLACE WITH YOUR DOMAIN>
        buildpack: https://github.com/cloudfoundry/nodejs-buildpack
        memory: 256M
        health-check-type: none
        path: .
        command: node consumer.js
        services:
          - <REPLACE WITH YOUR MESSAGING SERVICE>
        env:
          SAP_JWT_TRUST_ACL: "[{\"clientid\":\"*\",\"identityzone\":\"*\"}]"
          XBEM_INPUT_X: "inputX"
          RECONNECT_RETRY_MS : 5000
          SAP_XBEM_BINDINGS: >
           {
              "inputs": {
                "inputX": {
                  "service": "<REPLACE WITH YOUR MESSAGING SERVICE>",
                  "address": "queue:<REPLACE WITH YOUR QUEUE>",
                  "reliable": true
                }
              }
            }
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create package.json file)]

Create a 'package.json' file to list the packages your project depends on
and to specify versions for each package that your project can use. This
makes your build reproducible, and therefore easier to share with other developers.

```json
{
        "name": "consumer",
        "description": "Simple NodeJS Application consuming messages",
        "version": "0.0.1",
        "engines": {
                "node": ">=6.9.1"
        },
        "dependencies": {
                "@sap/xsenv": "1.2.8",
                "@sap/xb-msg": ">=0.2.4",
                "@sap/xb-msg-env": ">=0.2.1"
        },
        "scripts": {
                "start": "node consumer.js"
        }
}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Create consumer.js file)]

Use the following code to create a `consumer.js` file. This file holds the actual application. Find descriptions of what it does as comments in the coding.

On a higher level we do the following:

- we first perform **settings** in respect to node.js modules and to messaging.

- then we get the **messaging options** from the environment.

- the **messaging client** is instantiated using the options.

- client and stream **handler methods** are defined.

As a result, the application listens for messages and once it receives a message the message payload is written into the log file. The message is taken from the queue during this process.

```javascript
'use strict';

        //------------------------------------------------------------------------------------------------------------------
        // Basic setup in respect to modules, messaging settings and getting messaging options
        //------------------------------------------------------------------------------------------------------------------

        const msg    = require('@sap/xb-msg');
        const msgenv = require('@sap/xb-msg-env');

        const inputX = process.env.XBEM_INPUT_X;
        const reconnect_retry_ms = process.env.RECONNECT_RETRY_MS;

        // Get options from CF environment
        const options = msgenv.msgClientOptions('<REPLACE WITH YOUR MESSAGING SERVICE>', [inputX], []);

        //------------------------------------------------------------------------------------------------------------------
        // Start messaging client
        //------------------------------------------------------------------------------------------------------------------

        // Client for SAP Event Mesh Service instance
        const client = new msg.Client(options);

        //------------------------------------------------------------------------------------------------------------------
        // Messaging client handler methods
        //------------------------------------------------------------------------------------------------------------------

        client
        .on('connected', () => {
          console.log('connected to SAP Event Mesh service');
        })
        .on('error', (err) => {
          console.log('error on SAP Event Mesh service occurred ' + err);
        })
        .on('disconnected', (hadError) => {
          console.log('connection to SAP Event Mesh service lost, trying to reconnect in ' + reconnect_retry_ms + ' ms');
          setTimeout(()=> client.connect(), reconnect_retry_ms);
        });

        //------------------------------------------------------------------------------------------------------------------
        // Input stream handler methods
        //------------------------------------------------------------------------------------------------------------------

        client.istream(inputX)
        .on('subscribed', () => {
          console.log('subscribed to ' + inputX);
        })
        .on('ready', () => {
          console.log('stream ready: ' + inputX);
        })
        .on('data', (message) => {

          let topic = 'dummy';
          if (message.source) {
              if (typeof message.source === 'string') {
                  topic = message.source;
              } else if (message.source.topic) {
                  topic = message.source.topic;
              }
          }

          //------------------------------------------------------------------------------------------------------------------
          // Write the message payload to the log file
          //------------------------------------------------------------------------------------------------------------------

          console.log('message received: ' + message.payload.toString());
          message.done();
        });

        client.connect();
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy receiving application)]

1.	Go to <filepath directory to manifest.yml> and enter `npm install` to download missing node.js modules.
2.	To deploy the sending application to your Cloud Foundry space enter `cf push`.

Your application gets deployed to the cloud and is started up.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test your application)]

Once the application is started up, you can test your message consumer. To do this you send messages via the REST Gateway of SAP Event Mesh using Postman as described in the earlier tutorial.

Alternatively you can skip this step, continue with the next tutorial, create the message producer app and use it for sending messages.

[VALIDATE_6]
[ACCORDION-END]
