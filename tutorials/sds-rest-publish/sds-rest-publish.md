---
title: Publish events to SDS via the REST interface
description: See how to publish input events to a streaming project via the http REST interface
primary_tag: products>sap-hana-smart-data-streaming
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:**  One of the following:
 - [Run and Test a streaming project](https://www.sap.com/developer/tutorials/sds-run-test.html)
 - [Generating Alerts Using a Derived Window in SAP HANA Smart Data Streaming](https://www.sap.com/developer/tutorials/sds-event-stream-alerts.html)
 - [Build custom Flex Operators to analyze event streams](http://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
  - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)


## Details
### You will learn  
In this tutorial we will use the **Postman** API test tool to publish input events to our streaming project via the SDS REST interface.

### Time to Complete
**15 Min**

Before you begin:
- Your smart data streaming server must be running
- The Streaming Web Service (SWS) is running on the SDS server

[ACCORDION-BEGIN [Step 1: ](Enable the REST interface for your streaming project)]

Streaming projects can expose REST and Web Socket interfaces, but this is not enabled by default. So our first step is to expose the REST interface for our `freezer_monitor` project.  

> Note that this is technically not an adapter but a service provided by the SDS cluster. Therefore you don't add an http adapter to your project, but rather enable the project for Web Services  


In the Streaming Project Explorer, double click on the project configuration file `freezer_monitor.ccr` to open it in the configuration editor.

![Edit project config file](edit-ccr.png)

Go to the **Advanced** tab and click on **Project Deployment** in the left pane of the configuration editor to display the project deployment details.

Set the **Web Service Enabled** property to `true`

![enable web services](enable-web-svc.png)

Now close the Project Configuration editor and save the changes.

And finally:

1. re-start your project with the new configuration
2. open a viewer on the `MACHINEDATA` input stream - later, after you post an event, you'll be able to see it there.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download or install a REST client)]

For this tutorial you will need a way of creating the calls that you will send to the server. We created this tutorial using Postman - though you can use whatever REST client you like.

You can get Postman from http://www.getpostman.com/


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Confirm that you can connect to SWS)]

The `REST` and `Web Socket` interfaces for streaming projects are provided by the Streaming Web Service (SWS) - a service of the smart data streaming cluster. At this point SWS should already be running on your SDS cluster. Let's make sure we can connect to it.

To do this, send a simple http GET to the server.  You will need substitute the IP address of your HANA system in the URL below:

```
GET  http://10.x.x.x:9093/1/version
```
Here it is in Postman:

![Check SWS connectivity](version.png)

If successful, you should get a response that looks something like this:

`{"version-string":"2.00.000.00/20161121.1/SP00/linux/x86_64/64-bit/OPT/Mon Nov 21 00:13:43 PST 2016"}`

If you get an error indicating no response, then you aren't able to connect to the SWS web server.  Check the following:

- Double check the IP address (or host name) you are using in the http GET request
- Confirm that the smart data streaming SWS web server is running


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Get an authorization token)]

Before you can publish messages, SWS requires that you request for a token for the specific resource you intend to access. The returned token then needs to be included in the headers of subsequent POSTs.  To get an authorization token we need to send a POST request as follows (substituting the IP address - or hostname - of the host where SWS is running):

```
POST     http://10.x.x.x:9093/1/authorization
```
The header needs to include basic authorization credentials, where the username and password are `base64` encoded. So the header will look something like this:

`authorization: Basic U0RTX1VTRVI6UGFzc3dvcmQx`

But Postman will construct this for you, and do the encoding. Just go to the Authorization tab, enter your credentials, and click **"Update Request"**

![set authorization header](auth-header.png)

And now go to the Body tab, select raw, and enter the payload.  For this test, we need to request a token that will allow us to publish to the `MACHINEDATA` stream of the `freezer_monitoring` project running under the default workspace. Copy this and paste it into the body:

```json
[
{ "privilege":"write", "resourceType":"stream", "resource":"default/freezer_monitor/MACHINEDATA"}
]
```
It will look like this in Postman:

![authorization payload](auth-payload.png)

Send the request and if you authenticate successfully, and have permission to write to this stream, the response will contain your authorization token. The response should look something like this:

`[{"sws-token":"biVBFyKyPYgObPtmZc7y3ygn7e2hr4OoIQHjKtgQU1Hx75zunA3xDOBEQr08lCkhQFi4L7Ze9GNDb3rpNF1jEQ=="}]`

Copy the token - you'll need it in the next step.  Don't copy the full response, just the token and the surrounding quotes.

>Note: The token is only valid for a set number of seconds, which is defined in SDS cluster configuration and can be changed from the HANA cockpit or using the command line tools. The default value is only 5 minutes. After this time period, a new token will need to be requested.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Post an Event)]

Now we're ready to post our event to the `MACHINEDATA` input stream of the `freezer_monitor` project running in the `default` workspace. The URL for this call looks like this (substituting your IP address or hostname):

```
POST     http://10.x.x.x:9093/1/workspaces/default/projects/freezer_monitor/streams/MACHINEDATA
```

The header needs to include our authorization token. In Postman, go to the Headers tab and click on **Bulk Edit**.  Paste in your authorization token and then copy and add the header prefix shown below

```
header prefix:  authorization:SWS-Token "sws-token"=
```

So that the full header looks something like this:

```
authorization:SWS-Token "sws-token"="biVBFyKyPYgObPtmZc7y3ygn7e2hr4OoIQHjKtgQU1Hx75zunA3xDOBEQr08lCkhQFi4L7Ze9GNDb3rpNF1jEQ=="
```

Here's how the header will look in Postman:

![set authorization header in event post](publish-header.png)

Since Postman does not offer a "Token" authorization type, we must change the type back to `No Auth` so our new token doesn't get over-written back to a `BasicAuth`. Go to the Authorization tab and select **`No Auth`** from the drop-down.

![set authorization type in event post](no-auth.png)

And now go to the Body tab, select raw, and enter the payload below and then press send.

```json
[
{"ESP_OPS":"i", "MACHINEID":"1A", "EVENT_TIME": "2013-03-10T04:45:36.123", "EVENT_NAME":"DOOR", "EVENT_DESCRIPTION":"door open event", "EVENT_VALUE":"Door open"}
]
```
Here's how the body will look in Postman:

![event publish body](publish-event.png)

> The first part - `"ESP_OPS":"i"` - tells SDS this is an insert event. Our `freezer_monitor` project is only designed to process insert events. SDS also supports `update`, `upsert` and `delete` events based on primary keys, but that's beyond the scope of this tutorial.

In the studio, check the stream viewer to see that your event arrived and was processed.

You can publish additional events if you like, simply editing the body with different values. Note that for all the features of this demo project to work, you'll want to stick to these values:

- `MACHINEID` should be one of:  1A, 2B, 3C, 4D, 5E, 6F, 7G in order to successfully join to the HANA table containing reference data for each machine
- `EVENT_NAME` should be one of:  `TEMP`, `DOOR`, or `POWER`
- For power events, `EVENT_VALUE` should be either `Power on` or `Power off`

Also, some notes on the structure of the body:

The JSON message body can be sent in either of 2 formats:

- Map format
- Array format

Above we used the map format. Note that with the map format, you don't have to include every field. Fields that are not included will be set to `NULL`. You can also include multiple events in a single POST like this:

```json
[
{ "ESP_OPS":"<opcode>", "<field1>":<value1>, "<field2>":<value2>, … },
{ "ESP_OPS":"<opcode>", "<field1>":<value1>, "<field2>":<value2>, … }
]
```


The structure  of the array format is more compact (and a bit more efficient) but is less forgiving:  

```json
[
[ "<opcode>", <value1>, <value2>, <value3>, …],
[ "<opcode>", <value1>, <value2>, <value3>, …]
]
```

With this method, every field must be sent a value, and the order they are typed must correspond to the schema.


[ACCORDION-END]
