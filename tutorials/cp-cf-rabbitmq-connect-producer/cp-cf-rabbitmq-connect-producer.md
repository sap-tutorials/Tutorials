---
title: Connect an App to an RabbitMQ Service Instance
description: Define the connection to the RabbitMQ service (in Cloud Foundry as well as for local development), read the payload from an incoming request, and forward it to the message broker.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ tutorial>intermediate, products>sap-cloud-platform, topic>cloud , topic>javascript ]
time: 20
---

## Prerequisites  
 - [Install](https://docs.docker.com/install/) Docker

### You will learn  
  - Which node modules are used to establish a connection to the RabbitMQ service instance
  - How to use promises to pass messages to a message queue
  - How to send a message for which you expect a response

---

[ACCORDION-BEGIN [Step : ](Add a new dependency to the project)]

1. RabbitMQ (and other message broker) speak the AMQP protocol. The node module `amqplib` wraps this protocol and makes it easy to consume from the Node.js runtime. Install the module with the following command:
```Bash
npm install amqplib
```
The command will not only install the module, but also add it to the `package.json` file.

2. Import the `amqplib` module right next to the other imports in the `app.js` file in the root folder of the project.
```JavaScript
const amqp = require('amqplib');
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Write an asynchronous function)]
>Insert this snippet in the `app.js` file before you define any routes.

1. This step is the key ingredient to this tutorial! You need to connect your application to the message broker via the environment variables, which are provided by the Cloud Foundry environment. This line checks whether the application runs on a local machine or on Cloud Foundry. When it runs on a local machine it assumes that there is also a local RabbitMQ service running. Otherwise it reads the credentials URI (also containing the user and password information) from the environment variable.
```JavaScript
const sMessagingserviceUri = appEnv.isLocal ?
'amqp://guest:guest@localhost:5672' :
appEnv.getService('pubsub-messagingservice').credentials.uri;
//INSERT CODE IN STEP 2.2 here
```
2. Declare the following asynchronous function, which establishes a new connection to a RabbitMQ service instance.  This function makes use of the `await` keyword (twice), to wait for the promises of the `amqplib` module and eventually return the connection/channel object.
```JavaScript
//INSERT CODE IN STEP 4.1
async function connectToRabbitMQ() {
  try {
    var conn = await amqp.connect(sMessagingserviceUri);
    var oChannel = await conn.createChannel();
    //INSERT CODE IN STEP 4.3
  } catch (err) {
    console.error(err);
    process.exit(0); //fail gracefully
  } finally {
    return oChannel;
  }
}
```

[VALIDATE_2]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Connect to the message broker service instance)]
>Please make sure to replace the existing callback function of the `/send` endpoint with this new callback function.

Checks on each incoming request whether a connection to the message broker service instance has already been established and establish one if necessary. Next, assert that the queue with the name of the variable `oReq.body.queue` already exist and if so, forward the message `oReq.body.msg` to this queue.
```JavaScript
var oChannel;
app.post('/send', async function(oReq, oRes) {
  if (!oChannel) {
    oChannel = await connectToRabbitMQ();
  }

  var oMessage = {
    queue: oReq.body.queue || sDefaultQueue,
    message: oReq.body.msg || sDefaultMessage
  }

  oChannel.assertQueue(oMessage.queue, {
    durable: false
  });

  //REPLACE THIS LINE IN STEP 4.2
  oChannel.sendToQueue(oMessage.queue, Buffer.from(oMessage.message));

  oRes.sendStatus(201);
  console.log(`Received message "${oMessage.message}" via HTTP`);
  aHistory.push(oMessage);
});
```

[VALIDATE_3]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Implement response handling)]
>Correlation bindings are used to match the incoming responses with the sent "questions". It is important that the IDs are unique, otherwise a successful matching cannot be guaranteed.

1. Declare an additional default value, which will be used when there is not queue specified. Also, define the name of the queue the replies should be sent to (the `replyto` queue).
```JavaScript
const sDefaultQueue = 'Queue_1',
  sReplyQueue = 'reply_Queue';
```
2. The following snippet checks if a message requires a response. If so, a new correlation id needs to be generated. For this simple tutorial we assume that we won't generate the same random number twice. Keep in mind that the generated correlation id and the `replyto` queue need to be specified before the message is sent.
```JavaScript
if (oReq.body.reqResponse === 'true') {
  oMessage.correlationId = Math.random().toString(); //assumption, there won't be conflicting correlation ids
  oChannel.sendToQueue(oMessage.queue, Buffer.from(oMessage.message), {
    correlationId: oMessage.correlationId,
    replyTo: sReplyQueue
  });
} else {
  oChannel.sendToQueue(oMessage.queue, Buffer.from(oMessage.message));
}
```
3. Consume all incoming message in the `replyto` queue. Check if the correlation id of the incoming response matches with a saved message ("question"). If so, add the response as a new property of the stored question object.
```JavaScript
oChannel.assertQueue(sReplyQueue, {
  durable: false
});
oChannel.consume(sReplyQueue, function(msg) {
  const oMatch = aHistory.find((oItem) => oItem.correlationId === msg.properties.correlationId);
  if (oMatch) {
    oMatch.response = msg.content.toString()
  }
}, {
  noAck: true
});
```
4. Replace the existing log message with one that provides more information:
```JavaScript
console.log(`Received message "${oMessage.message}" for queue ${oMessage.queue} (Flag: ${oReq.body.reqResponse}) via HTTP`);
```

>It's also possible to generate new "nameless" queues and use them as the `replyto` queue. This makes sense if you're expecting a small number of responses.

[VALIDATE_4]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Test your producer app locally)]

Run `npm start` from the root directory of your project to start the app.

Additionally, you need to run RabbitMQ locally, so that the application can send messages to it. This can be easily done by starting a RabbitMQ instance within a Docker container. Please make sure you have installed [Docker](https://docs.docker.com/install/). The following console command downloads and starts the required Docker image and opens a port so the app can connect to it:
```Bash
docker pull rabbitmq
docker run -it --rm -p 5672:5672 -p 15672:15672 rabbitmq
```

Copy the following values to the Postman form to send a http request to your local server:

|  Property| Value |  
|------------------|---|
| 1 HTTP verb  | **`POST`**  |
| 2 URL  | **`localhost:3000/send`**  |
| 3   | **`body`**  |
| 4   | **`raw`**  |
| 5 Content-Type  | **`JSON(application/json)`**  |
| 6 Payload  | `{"msg": "Hello SAP Cloud Platform",	"queue": "High_Prio_Queue"}` |


 ![sendmessage](./postman-send-message.png)

The response of this message should be a status 201 message. You should also be able to see to see the following output in your console:

![console](./received-message.png)

[VALIDATE_5]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Deploy your producer app to SAP Cloud Platform)]
Re-deploy the application to SAP Cloud Platform to be able to test it in the cloud.
```Bash
cf push
```

Double check that the URL is still unchanged and use Postman to test the service:

![postman_deployed](./postman-deployed.png)

[DONE]
[ACCORDION-END]

---
