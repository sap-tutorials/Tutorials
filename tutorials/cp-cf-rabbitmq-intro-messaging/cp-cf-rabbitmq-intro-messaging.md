---
title: Learn about Messaging Between Microservices
description: Learn about the open source message broker RabbitMQ and its concepts, including why microservices need to communicate in an asynchronous manner.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>intermediate,  products>sap-cloud-platform, topic>cloud ]
time: 20
---

## Details
### You will learn  
  - Why messaging is crucial for microservices architecture
  - What are the fundamental concepts of message brokers
  - Which message brokers are available on the SAP Cloud Platform
  - What are the fundamental concepts of RabbitMQ


---

[ACCORDION-BEGIN [Step ](Messaging in a microservices architecture)]
Microservices architecture became popular around 2014 and is still extremely popular. In such an architecture, applications do not consist of a single monolith, but are built from several "independent" components/services which interact collaboratively ([Martin Fowler](https://martinfowler.com/microservices/) spent some time collecting interesting information about it).

The **advantages** of such an architecture are huge. The **loose coupling** of the services guarantees a high availability. When a single service is not working, this doesn't impact the application as its own. The malicious service needs to **fail gracefully** to be replaced with a new instance of this service in order to restore the full functionality of the application.

The **drawback** of this architecture is its **complexity**. All services need to be able to communicate with each other (to pass sub-tasks), which brings a new level of complexity. HTTP(S) is not necessarily suited for those tasks, since the recipients of the messages need to be updated whenever a service is being replaced. Furthermore, the used protocol **needs to be fail-safe** and provide additional features.

Therefore, it makes sense to use a protocol, like [Advanced Message Queuing Protocol (AMQP)](http://www.amqp.org/), which has been designed for those messaging scenarios. Those protocols are already implemented by so-called message brokers that are tasked with gathering, routing and distributing the messages from senders to the right receivers.

[VALIDATE_1]
[ACCORDION-END]
[ACCORDION-BEGIN [Step ](Basic terminology)]
AMQP uses the following [terms to model its view of the world](https://www.rabbitmq.com/tutorials/amqp-concepts.html). Most terms describe general concepts and are not specific to the protocol:


* **Message**
  A package for information, usually composed of two parts. Some headers, key-value pairs containing metadata, and a body, a binary package containing the actual message itself.
* **Producer**
  Creates and sends a message
* **Consumer**
  Receives and reads a message
* **Queue**
  Communication channel that enqueues/buffers messages. Later those can be retrieved by one or more consumers.
* **Exchange**
  Abstract message queues away and routes messages to the appropriate queue based on some predefined logic

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step ](Communication patterns)]

Besides the simple 1:1 communication pattern, there are several other patterns used. This section will introduce some of them.

* [Working queues](https://www.rabbitmq.com/tutorials/tutorial-two-javascript.html)
  This communication type can also be seen as **1:N communication**. One producer sends the messages to a single queue, which has multiple consumers/workers. Each message will be *handled by a single (available) consumer*.
* [Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-javascript.html)
  Publish/Subscribe means that multiple consumers subscribe to the same **message type** and *all of them will receive all messages of this type*. A simple example is an app which publishes log messages and two subscribers receive them. The first subscriber might write the messages to disk, while the second one outputs them on a screen.
* [Routing](https://www.rabbitmq.com/tutorials/tutorial-four-javascript.html)
  Routing means that a consumer can *subscribe selectively on the messages*. Messages and queue can specify a **routing key**. Queues can only receive messages with the same routing key. A routing key could be, for example, the severity level of the log messages.
* [Topic](https://www.rabbitmq.com/tutorials/tutorial-five-javascript.html)
  Topics are a more generic type of routing. *Messages and queues are associated with a tuple of words* (separated by a dot). Those tuples could have the structure  `<platform>.<environment>.<programming-language>` and be  `cloudplatform.cloudfoundry.java` or `hana.xsa.java`. Additionally, queues can use wildcards to specify a **topic**, the topic `\*.\*.java` would match all Java-related messages and `cloudplatform.\*.\*` would match all messages that specify the `cloudplatform` as platform.
* [Request-Reply](https://www.rabbitmq.com/tutorials/tutorial-six-javascript.html)
  You should use this pattern, when you need to *trigger a remote procedure call (RPC)*. The producer basically sends a **reply queue** along with the original message to the consumer. The consumer executes the function call and sends the response (via the reply queue) back to the producer of the original message / procedure call.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step ](Messaging in SAP Cloud Platform Cloud Foundry environment)]
SAP Cloud Platform supports two messages brokers, both of them implement the AMQP protocol:

- [SAP Enterprise Messaging](https://cloudplatform.sap.com/capabilities/product-info.SAP-Enterprise-Messaging.dc3dcc84-cd9c-477c-ba1c-862340abd874.html)
 SAP Enterprise Messaging provides a cloud-based messaging service for the development of decoupled and resilient applications, services, and systems. It enables asynchronous communication to reliably send and receive messages from and to applications, services, and systems using standard message protocols and exchange patterns and principles.
 &nbsp;
 SAP Enterprise Messaging supports the AMQP and the MQTT protocol and comes with libraries for Node.js and Java.
 &nbsp;
- [RabbitMQ](https://cloudplatform.sap.com/capabilities/product-info.RabbitMQ-on-SAP-Cloud-Platform.b011738d-fa31-4dc4-98b5-cb9acd9aea97.html)
  RabbitMQ is lightweight and easy to deploy on premises and in the cloud. It supports multiple messaging protocols. RabbitMQ can be deployed in distributed and federated configurations to meet high-scale, high-availability requirements.
  &nbsp;
  RabbitMQ is with more than 35,000 production deployments of RabbitMQ world-wide the most popular open source message broker.

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step ](Messaging with RabbitMQ)]

This tutorial mission will focus on messaging with RabbitMQ, since this message broker is available for free in the marketplace of the SAP Cloud Platform trial.

RabbitMQ is an open source message broker for which many libraries are available. RabbitMQ abstracts queue with so called exchanges. An exchange receives on one side messages from producers and on the other side pushes them to queues. The exchange must know exactly what to do with a message it receives. Therefore, RabbitMQ differentiates between the following types of exchanges:

* **Fanout**
  The fanout exchange is very simple. It just broadcasts all the messages it receives to all known queues.
  &rarr; This exchange type would be used for the publish/subscribe communication pattern.
* **Direct**
  The direct exchange is very simple. A message goes to the queues whose `binding key`  matches the `routing key` of the message.
  &rarr; Use this exchange for the routing communication
    - **Nameless**
      The nameless exchange is the default (direct) exchange which will be used if no exchange is specified. Messages are routed to the queue with the specified name, if it exists.
      &rarr; This exchange should be used for simple 1:1 communication.
* **Headers**
  A headers exchange is designed for routing on multiple attributes that are more easily expressed as message headers than a routing key. Headers exchanges ignore the routing key attribute.
  &rarr; You can also use this exchange type for the routing pattern
* **Topic**
  Messages sent to a topic exchange can't have an arbitrary `routing_key`  but rather a list of words, delimited by dot.
  &rarr; This exchange type should be used for the topic communication pattern.

[VALIDATE_5]
[ACCORDION-END]
---
