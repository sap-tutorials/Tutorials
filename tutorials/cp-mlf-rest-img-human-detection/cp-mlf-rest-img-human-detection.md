---
title: Use the Human Detection API from a REST Client
description: Call the Human Detection API from a REST Client like Postman
auto_validation: true
time: 10
tags: [tutorial>beginner, topic>cloud, topic>machine-learning, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-leonardo-machine-learning-foundation
---

## Prerequisites
 - [Create a Machine Learning Foundation service instance on the Cloud Foundry environment](https://developers.sap.com/tutorials/cp-mlf-create-instance.html)
 - [Install Postman as a REST client](https://developers.sap.com/tutorials/api-tools-postman-install.html)
 - [Get your OAuth Access Token using a REST Client](https://developers.sap.com/tutorials/cp-mlf-rest-generate-oauth-token.html)

## Details
### You will learn
  - Call an API from a REST client like Postman
  - The basics about Machine Learning Foundation Service for Human Detection

> ### **Note:** This service was in alpha version when this tutorial was released.

[ACCORDION-BEGIN [Step](The Human Detection Service)]

The Human Detection service can be used for detecting people in an image or a set of images and returns information declaring boundary box around the people in every image.

This is the list of accepted file extensions:

|Name                  | Description
|----------------------|--------------------
| **Archive file**     | `zip`, `tar`, `gz`, `tgz`
| **Image file**       | `jpg`, `jpe`, `jpeg`, `png`, `gif`, `bmp`

The images should be RGB, or 8-bit gray scale.

If an archive file is provided, no additional files can be provided.

The input file (or the archive file) is provided using form data (as an element named ***file*** in the form data).

The service will return a JSON response that includes the bounding boxes coordinates for the identified humans.

For more details, you can check the [Inference Service for Human Detection on the SAP API Business Hub](https://api.sap.com/api/human_detection_api/resource).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Call the API)]

Open a new tab in ***Postman***.

Make sure that the ***`my-l-foundation`*** environment is selected.

On the **Authorization** tab, select **Bearer Token**, then enter **`{{OAuthToken}}`** as value.

![Postman](01.png)

> ### **Note:**: the **`OAuthToken`** environment variable can be retrieved following the [Get your OAuth Access Token using a REST Client](https://developers.sap.com/tutorials/cp-mlf-rest-generate-oauth-token.html) tutorial.

Fill in the following additional information:

Field Name               | Value
:----------------------- | :--------------
<nobr>HTTP Method</nobr> | POST
<nobr>URL<nobr>          | <nobr>`https://mlftrial-face-detector.cfapps.eu10.hana.ondemand.com/api/v2alpha1/image/human-detection/`</nobr>

> **Note** As a reminder, the URL depends on you Cloud Platform landscape region but for the trial landscape only ***Europe (Frankfurt)*** provide access to the Machine Learning Foundation services.

On the **Body** tab, keep **`form-data`** selected. Add a new key named **file** and switch it to **File** instead of **Text** (default).

Select your image file.

![Postman](02.png)

If you are missing some inspiration, you can use the following pictures from the **Women Tech Influencer Fireside Chat** at SAP TechEd Las Vegas 2018 :

 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="Image 10751.jpg">Image 10751</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134" target="blank" download="Image 10772.jpg">Image 10772</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="Image 10779.jpg">Image 10779</a>

The first image (`SAP_TechEd_LV2018_10751.jpg`) contains only 3 person, the second image (`SAP_TechEd_LV2018_10772.jpg`) contains 6 person and the third image (`SAP_TechEd_LV2018_10779.jpg`) also 6 person.

Click on **Send**.

You should receive a response that includes a series of detected person entries (the following result is for Image 10751):

![Postman](03.png)

```json
{
    "detection_boxes": [
        [
            0.1799386590719223,
            0.02037128061056137,
            0.977997362613678,
            0.47837960720062256
        ],
        [
            0.2418273687362671,
            0.3918556272983551,
            0.9809070825576782,
            0.6808136105537415
        ],
        [
            0.16099587082862854,
            0.6079592704772949,
            0.9867111444473267,
            0.9851326942443848
        ]
    ]
}
```

Each entry in the detection box represents a box that identify one of the person.

Here is the results represented on <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="Image 10751.jpg">Image 10751</a>:

<table border="0">
	<tr><td><img id="img_1"/></div></td></tr><tr><td><canvas id="canvas_1"/></td></tr>
</table>

<script>
function myFunction(imagId) {
	var oImg = document.getElementById("img_" + imagId);;
	var oCanvas = document.getElementById("canvas_" + imagId);
	oCanvas.width = oImg.width;
	oCanvas.height = oImg.height * oImg.width / oImg.naturalWidth;

	var ctx = oCanvas.getContext("2d");
    ctx.drawImage(oImg, 0, 0, oImg.width, oImg.height * oImg.width / oImg.naturalWidth);

    ctx.lineWidth="3";
    ctx.strokeStyle="red";
	ctx.fillStyle = "white";
	ctx.font = "14pt Arial";

	ctx.scale( oCanvas.width  / oImg.naturalWidth, oCanvas.height / oImg.naturalHeight);

    return ctx;
}

window.onload = function() {
	var oImg    = document.getElementById("img_1");
	var oCanvas = document.getElementById("canvas_1");

    var response = {
        "detection_boxes": [
            [0.1799386590719223,  0.02037128061056137, 0.977997362613678,  0.47837960720062256],
			[0.2418273687362671,  0.3918556272983551,  0.9809070825576782, 0.6808136105537415],
			[0.16099587082862854, 0.6079592704772949,  0.9867111444473267, 0.9851326942443848]
        ],
        "detection_classes": [ "human", "human", "human" ],
        "detection_scores": [ 0.99981290102005, 0.9982737302780151, 0.9978749752044678 ],
        "num_detections": 3
    };
	oImg.onload = function(){
		var ctx = myFunction("1")
		ctx.strokeRect(
            response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][0] * oCanvas.height,
            response.detection_boxes[0][3] * oCanvas.width  - response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][2] * oCanvas.height - response.detection_boxes[0][0] * oCanvas.height

        );  
		ctx.strokeRect(
            response.detection_boxes[1][1] * oCanvas.width,
            response.detection_boxes[1][0] * oCanvas.height,
            response.detection_boxes[1][3] * oCanvas.width  - response.detection_boxes[1][1] * oCanvas.width,
            response.detection_boxes[1][2] * oCanvas.height - response.detection_boxes[1][0] * oCanvas.height
        );         
		ctx.strokeRect(
            response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][0] * oCanvas.height,
            response.detection_boxes[2][3] * oCanvas.width  - response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][2] * oCanvas.height - response.detection_boxes[2][0] * oCanvas.height
        );        
        ctx.fillText(response.detection_classes[0] + " " + response.detection_scores[0], response.detection_boxes[0][1] * oCanvas.width, response.detection_boxes[0][0] * oCanvas.height);
        ctx.fillText(response.detection_classes[1] + " " + response.detection_scores[1], response.detection_boxes[1][1] * oCanvas.width, response.detection_boxes[1][0] * oCanvas.height);
        ctx.fillText(response.detection_classes[2] + " " + response.detection_scores[2], response.detection_boxes[2][1] * oCanvas.width, response.detection_boxes[2][0] * oCanvas.height);

    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132";
};
</script>

Here is a simple HTML code you can use to visualize other results:

```HTML
<html>
<body>
<table border="0">
	<tr><td><img id="img_1"/></div></td></tr><tr><td><canvas id="canvas_1"/></td></tr>
</table>

<script>
function myFunction(imagId) {
	var oImg = document.getElementById("img_" + imagId);;
	var oCanvas = document.getElementById("canvas_" + imagId);
	oCanvas.width = oImg.width;
	oCanvas.height = oImg.height * oImg.width / oImg.naturalWidth;

	var ctx = oCanvas.getContext("2d");
    ctx.drawImage(oImg, 0, 0, oImg.width, oImg.height * oImg.width / oImg.naturalWidth);

    ctx.lineWidth="3";
    ctx.strokeStyle="red";
	ctx.fillStyle = "white";
	ctx.font = "14pt Arial";

	ctx.scale( oCanvas.width  / oImg.naturalWidth, oCanvas.height / oImg.naturalHeight);

    return ctx;
}

window.onload = function() {
	var oImg    = document.getElementById("img_1");
	var oCanvas = document.getElementById("canvas_1");

    var response = {
        "detection_boxes": [
            [0.1799386590719223,  0.02037128061056137, 0.977997362613678,  0.47837960720062256],
			[0.2418273687362671,  0.3918556272983551,  0.9809070825576782, 0.6808136105537415],
			[0.16099587082862854, 0.6079592704772949,  0.9867111444473267, 0.9851326942443848]
        ],
        "detection_classes": [ "human", "human", "human" ],
        "detection_scores": [ 0.99981290102005, 0.9982737302780151, 0.9978749752044678 ],
        "num_detections": 3
    };
	oImg.onload = function(){
		var ctx = myFunction("1")
		ctx.strokeRect(
            response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][0] * oCanvas.height,
            response.detection_boxes[0][3] * oCanvas.width  - response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][2] * oCanvas.height - response.detection_boxes[0][0] * oCanvas.height

        );  
		ctx.strokeRect(
            response.detection_boxes[1][1] * oCanvas.width,
            response.detection_boxes[1][0] * oCanvas.height,
            response.detection_boxes[1][3] * oCanvas.width  - response.detection_boxes[1][1] * oCanvas.width,
            response.detection_boxes[1][2] * oCanvas.height - response.detection_boxes[1][0] * oCanvas.height
        );         
		ctx.strokeRect(
            response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][0] * oCanvas.height,
            response.detection_boxes[2][3] * oCanvas.width  - response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][2] * oCanvas.height - response.detection_boxes[2][0] * oCanvas.height
        );        
        ctx.fillText(response.detection_classes[0] + " " + response.detection_scores[0], response.detection_boxes[0][1] * oCanvas.width, response.detection_boxes[0][0] * oCanvas.height);
        ctx.fillText(response.detection_classes[1] + " " + response.detection_scores[1], response.detection_boxes[1][1] * oCanvas.width, response.detection_boxes[1][0] * oCanvas.height);
        ctx.fillText(response.detection_classes[2] + " " + response.detection_scores[2], response.detection_boxes[2][1] * oCanvas.width, response.detection_boxes[2][0] * oCanvas.height);

    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132";
};
</script>
</body>
</html>
```

You will notice that the rectangle uses the following format: (left, top, right - left, bottom - top)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Validate your results)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
