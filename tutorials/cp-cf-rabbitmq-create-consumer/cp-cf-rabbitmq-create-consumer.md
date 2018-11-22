---
title: Create a Consumer App
description: Create a web-based app that connects to a RabbitMQ message queue and subscribes to event queues.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ tutorial>intermediate, products>sap-cloud-platform, topic>cloud , topic>javascript ]
time: 30
---


## Details
### You will learn  
  - How to subscribe to a message queue
  - How to parse the received messages

---

[ACCORDION-BEGIN [step ](Define and install the dependencies)]

Before you can start the actual development of the application, you need to define the dependencies and  metadata of the consumer app. To do this, create a new `package.json` file in the root folder of your project:
```json
{
  "name": "consumer",
  "scripts": {
    "start": "node app.js"
  },
  "engines": {
    "node": "10.x"
  },
  "description": "This is the sample project to demonstrate how to consume rabbitmq messages on SAP Cloud Platform",
  "dependencies": {
    "amqplib": "^0.5.2",
    "body-parser": "^1.18.3",
    "cfenv": "^1.1.0",
    "express": "4.14.0"
  }
}
```
This file defines four dependencies:

- `Express` and `body-parser`
  A popular web framework that hosts the web server and parses the parameters of the incoming HTTP requests

- `amqplib`
  RabbitMQ (and other message broker) speak the AMQP protocol. This module wraps the required protocol and makes it easy to consume from `Node.js`.

- `cfenv`
  Reads the Cloud Foundry specific environment variables

Now that you defined all dependencies, you can install them with `npm install`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [step ](Create a simple web server)]
First, you need the specify the core file of your application, the `app.js` file. This code snippet will create a new web server, similar to the **Create a Producer App** tutorial.

```javascript
const path = require('path'),
    express = require('express'),
    bodyParser = require('body-parser'),
    cfenv = require('cfenv'),
    appEnv = cfenv.getAppEnv();

var app = express();

app.use('/', express.static(path.join(__dirname, 'webapp')));
app.use(bodyParser.urlencoded({
    extended: true
}));
app.use(bodyParser.json());

//INSERT CODE IN STEP 3

const iPort = appEnv.isLocal ? 3001: appEnv.port;

app.listen(iPort, function () {
    console.log(`Congrats, your consumer app is listening on port ${iPort}!`);
});
```

[VALIDATE_2]
[ACCORDION-END]
[ACCORDION-BEGIN [step ](Add a new endpoint to the web server)]
> Replace the placeholder `INSERT CODE IN STEP 3` with both listed snippets

1. Use an object to map the queue names to an array of messages, which will be used to cache all received messages.
```javascript
var oQueueCaches = {
  'Queue_1': [],
  'Queue_2': [],
  'Queue_3': []
};
//INSERT CODE IN STEP 4
```
2. Add a new endpoint to the web server. This endpoint will return the cached messages of a given queue (specified as the query parameter **`queue`** of the HTTP request).
```javascript
app.get('/inbox', function (oReq, oRes) {
  oRes.send(oQueueCaches[oReq.query.queue]);
});
//INSERT CODE IN STEP 5
```

[VALIDATE_3]
[ACCORDION-END]
[ACCORDION-BEGIN [step ](Consume a queue of the message broker service instance)]
1. Establish a connection to the RabbitMQ service instance and create a channel to consume messages. Use the `callback_api` sub-module to use the callback API. This API won't use promises (as the producer app does), but make use of callback function which may contain an error object.
```javascript
const amqp = require('amqplib/callback_api');
const sMessagingserviceUri = appEnv.isLocal ?
  'amqp://guest:guest@localhost:5672' :
  appEnv.getService('pubsub-messagingservice').credentials.uri;
(function connectToRabbitMQ() {
  amqp.connect(sMessagingserviceUri, function(err, conn) {
    if (err) {
      console.error(err);
      process.exit(0); //fail gracefully
    }
    conn.createChannel(function(err, ch) {
    //INSERT CODE IN STEP 4.2
    });
  });
})();
```
2. Replace placeholder `STEP 4.2` in the code above with the following snippet to assert that the required RabbitMQ queues exists. Use the channel to consume, acknowledge and store all messages, which are transferred to those queue.
```javascript
Object.keys(oQueueCaches).forEach(function(sQueueName) {
  oChannel = ch;
  oChannel.assertQueue(sQueueName, {
    durable: false
  });
  console.log(`Waiting for messages in ${sQueueName}.`);
  oChannel.consume(sQueueName, function(msg) {
  oQueueCaches[sQueueName].push({
      replyTo: msg.properties.replyTo,
      correlationId: msg.properties.correlationId,
      message: msg.content.toString()
  });
  oChannel.ack(msg);
  console.log(`Received ${msg.content.toString()} in queue ${sQueueName}`);
  });
});
```

[VALIDATE_4]
[ACCORDION-END]
[ACCORDION-BEGIN [step ](Reply to received messages)]
Create a second endpoint to accept responses and return them (via the `replyTo` queue) back to the sender of the original message. Use the correlation id to identify this original sender. Also, store the response in the cached message. Use placeholder `STEP 5` to paste the following snippet.

```javascript
app.post('/respond', function(oReq, oRes) {
  const oMessage = oReq.body;
  oChannel.sendToQueue(oMessage.replyTo, Buffer.from(oMessage.response), {
      correlationId: oMessage.correlationId
  });
  console.log(`Sent "${oMessage.response}" with correlation id "${oMessage.correlationId}" back to the original sender`)

  const oCachedItem = oQueueCaches[oMessage.queue].find(( oItem) => oItem.correlationId === oMessage.correlationId);
  if(!oCachedItem){
    oRes.sendStatus(400);
    return;
  }
  oCachedItem.response = oMessage.response;
  oRes.sendStatus(201);
});
```

[VALIDATE_5]
[ACCORDION-END]
[ACCORDION-BEGIN [step ](Test your consumer application locally)]
>Make sure to run the producer application (on port 3000) as well as a local RabbitMQ instance before you start the app locally.
```
docker run -it --rm -p 5672:5672 -p 15672:15672 rabbitmq
```

Run `npm start` from the root directory of your project to test your application.

1. The console output should tell that the application started successfully.

  ![started app](started-app.png)
2. Open the web-based UI of the producer app at <http://localhost:3000> and send a message to the local RabbitMQ service.

  ![producer app](publisher.png)
3. Verify that the consumer received the sent message.

  ![received app](received-app.png)

[VALIDATE_6]
[ACCORDION-END]
[ACCORDION-BEGIN [step ](Deploy your consumer application to SAP Cloud Platform)]
Specify the deployment information in a `manifest.yml` file in the root folder:
```yml
---
applications:
 - name: consumer
   random-route: true
   memory: 128MB
   disk_quota: 128MB
   instances: 1
   buildpack: nodejs_buildpack
   command: node app.js

 services:
  - pubsub-messagingservice
```
>The property `random-route` will generate a route that does not conflict with any other application in the same Cloud Foundry instance.

The command `cf push` reads this file and deploys the application to SAP Cloud Platform. You can find the generated route / URL in the console, once the deployment has finished.
![pushed](./pushed.png)

Now you can consume message queues and print them in the log of the consumer app!

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [step ](Troubleshooting)]

**Problem**: I see the following error message when I execute `npm start`: "Error: Cannot find module 'express'"

**Solution**: This means that there are some missing dependencies. Please run `npm install` to install them.


**Problem**: I see the following error message when I execute `cf push`: "Server error, status code: 400, error code: 210003, message: The host is taken"

**Solution**: This means this application name is already use in the current Cloud Foundry environment (probably by another user). Please change the property 'name' in the `manifest.yml` file.

[DONE]
[ACCORDION-END]

---
