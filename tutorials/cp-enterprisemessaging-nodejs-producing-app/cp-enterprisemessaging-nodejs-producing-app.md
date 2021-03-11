---
title: Create an Application for Producing Messages
description: Develop and deploy a basic Node.js-based messaging application for sending messages to an SAP Event Mesh Queue.
time: 20
auto_validation: true
tags: [ tutorial>beginner, topic>node-js, topic>java, products>sap-business-technology-platform, tutorial>license]
primary_tag: products>sap-event-mesh
---


## Details
### You will learn
  - How to create a basic messaging client application for sending messages to a queue
  - How to deploy this application to the SAP Business Technology Platform and test it

[ACCORDION-BEGIN [Step 1: ](Install Node.js)]

1. Install Node.js and configure it locally.

2. Add the dependency in applications package.json and run npm for it:

    `npm install`

3. Create a directory that holds the files for your application - name it for example Producer. Into this directory we will create three files:

    - The `manifest.yml` is the deployment descriptor and contains all required information to deploy an application to a SAP Business Technology Platform Cloud Foundry instance.

    - The `package.json` specifies the version of a package that your app depends on.

    - The application file obviously holds the executable code.

Now create these files following the descriptions below into the created directory.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create manifest.yml file)]

Use the following code to create a  `manifest.yml` file that binds your application to the messaging service.

You need to add domain, messaging service and your queue name in the indicated spaces.

```YAML
applications:
  - name: producer
    host: producer-host
    domain: <REPLACE WITH YOUR DOMAIN>
    buildpack: 'https://github.com/cloudfoundry/nodejs-buildpack'
    memory: 256M
    health-check-type: none
    path: .
    command: node producer.js
    services:
      - <REPLACE WITH YOUR MESSAGING SERVICE>
    env:
      SAP_JWT_TRUST_ACL: '[{"clientid":"*","identityzone":"*"}]'
      SAP_XBEM_BINDINGS: |
        {
          "inputs": {},
          "outputs": {
            "myOutA" : {
              "service": "<REPLACE WITH YOUR MESSAGING SERVICE>",
              "address": "topic:<REPLACE WITH YOUR TOPIC>",
              "reliable": false
            },
            "myOutB" : {
              "service": "<REPLACE WITH YOUR MESSAGING SERVICE>",
              "address": "topic:<REPLACE WITH YOUR TOPIC>",
              "reliable": false
            }
          }
        }

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create package.json file)]

Create a `package.json` file to list the packages your project depends on and to specify versions of a package that your project can use. This makes your build reproducible, and therefore easier to share with other developers.

```JSON
{
        "name": "producer",
        "description": "Simple NodeJS Application producing messages",
        "version": "0.0.1",
        "engines": {
                "node": ">=6.9.1"
        },
              "dependencies": {
        "@sap/xb-msg": "^0.9.12",
        "@sap/xb-msg-amqp-v100": "^0.9.48",
        "@sap/xb-msg-env": "^0.9.7",
        "@sap/xsenv": "^3.1.0"
        },
        "scripts": {
                "start": "node producer.js"
        }
}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Create producer.js file)]

Use the following code to create a `producer.js` file. This file holds the actual application. Find descriptions of what it does as comments in the coding.

On a higher level you do the following:

1. Perform **settings** in respect to Node.js modules and to messaging.

2. Get the **messaging options** from the environment.

3. The **messaging client** is instantiated using the options.

4. Client and stream **handler methods** are defined.

As a result, the application sends messages to the defined topic and writes a quick info including a counter into the log file.

```JavaScript
"use strict";

//------------------------------------------------------------------------------------------------------------------
//  Basic setup in respect to modules, messaging settings and getting messaging options
//------------------------------------------------------------------------------------------------------------------

const msg = require('@sap/xb-msg');
const env = require('@sap/xb-msg-env');
const xsenv = require('@sap/xsenv');
const service = '<REPLACE WITH YOUR MESSAGING SERVICE>';
const taskList = {
    myOutA : { topic: '<REPLACE WITH YOUR TOPIC>' , timerMin: 1, timerMax: 11 },
    myOutB : { topic: '<REPLACE WITH YOUR TOPIC>' , timerMin: 5, timerMax:  8 }
};
var counter = 1;

xsenv.loadEnv();

//------------------------------------------------------------------------------------------------------------------
// Start messaging client
//------------------------------------------------------------------------------------------------------------------

const client = new msg.Client(env.msgClientOptions(service, [], ['myOutA', 'myOutB']));


function getRandomInt(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return (Math.floor(Math.random() * (max - min + 1)) + min) * 1000;
}

function initTasks(tasks, client) {
    Object.getOwnPropertyNames(tasks).forEach((id) => {
        const task = tasks[id];
        const stream = client.ostream(id);

        const handler = () => {
            console.log('publishing message number ' + counter + ' to topic ' + task.topic);

            const message = {
                payload: Buffer.from("Message Number " + counter)

            };
            if (!stream.write(message)) {
                console.log('wait');
                return;
            }
            setTimeout(handler, getRandomInt(task.timerMin, task.timerMax));
            counter++;
        };

        stream.on('drain', () => {
            setTimeout(handler, getRandomInt(task.timerMin, task.timerMax));
        });

        setTimeout(handler, getRandomInt(task.timerMin, task.timerMax));
    });
}

//------------------------------------------------------------------------------------------------------------------
// Messaging client handler methods
//------------------------------------------------------------------------------------------------------------------

client
    .on('connected', () => {
        console.log('connected');
        initTasks(taskList, client);
    })
    .on('drain', () => {
        console.log('continue');
    })
    .on('error', (error) => {
        console.log(error);
    });

client.connect();
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy application)]

1.	Go to <filepath directory to manifest.yml> and enter `npm install` to download potentially missing Node.js modules.

2.	To deploy the producing application to your Cloud Foundry space enter `cf push`.

Your application gets deployed to the cloud and is started.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test your application)]

Once the application is started up, you can test your message producer. There are several options for this:

- If you already have the consumer app deployed and started you should be able to see the messages sent by the consumer app in the logs. Use  `cf logs producer` or `cf logs producer --recent`  to see the logs.

- If you stop the consuming application or have not deployed it yet the count for the messages in the queue that can be found in the messaging cockpit goes up.

- Alternatively you could use Postman to consume the messages.

[VALIDATE_1]
[ACCORDION-END]
