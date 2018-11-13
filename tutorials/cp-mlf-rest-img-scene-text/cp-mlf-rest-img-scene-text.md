---
title: Use the Scene Text Recognition API from a REST Client
description: Discover how to call the Scene Text Recognition API from a REST Client like Postman
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
  - The basics about Machine Learning Foundation Service for Scene Text Recognition

[ACCORDION-BEGIN [Step](The Scene Text Recognition Service)]

The Scene Text Recognition service localizes and reads text from natural images and scenes.

> ***Differences with the OCR service***
In comparison to the Optical Character Recognition service, the Scene Text Recognition service offers
>
- Works with real-life color images
- Ability to work with font-less text
- Extract word-art / `picturized` text
- Works in different orientations of texts
- Text occurring in natural images like low-contrast, emboss/engrave

>When the formats from which the text has to be read are documents or print media scans, the OCR service should be used whereas in case of natural images (e.g. reading the counter of a utility meter or the number-plate of an automobile), the Scene Text Recognition service should be used.

This is the list of accepted file extensions:

|Name                  | Description
|----------------------|--------------------
| **Image file**       | `jpg`, `jpe`, `jpeg`, `png`

The images should be RGB.

The input file is provided using form data (as an element named ***files*** in the form data).

The service will return a JSON response that includes the detected texts within the scene along with the bounding box coordinates.

For more details, you can check the [Inference Service for Scene Text Recognition on the SAP API Business Hub](https://api.sap.com/api/scene_text_recognition_api/resource).

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
<nobr>URL<nobr>          | <nobr>`https://mlftrial-scene-text-recognition.cfapps.eu10.hana.ondemand.com/api/v2/image/scene-text-recognition`</nobr>

> **Note** As a reminder, the URL depends on you Cloud Platform landscape region but for the trial landscape only ***Europe (Frankfurt)*** provide access to the Machine Learning Foundation services.

On the **Body** tab, keep **`form-data`** selected. Add a new key named **files** and switch it to **File** instead of **Text** (default).

Select your image file.

![Postman](02.png)

If you are missing some inspiration, you can use the following pictures from [SAP TechEd Barcelona 2018](https://sapteched2018.event-hosting.com/Gallery-336659.html):

 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551006" target="blank" download="SAP_TechEd_BCN2018_01020.jpg">Image `SAP_TechEd_BCN2018_01020`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551064" target="blank" download="SAP_TechEd_BCN2018_01256.jpg">Image `SAP_TechEd_BCN2018_01256`</a>

The first image (`SAP_TechEd_BCN2018_01020.jpg`) contains the following text :

  - SAP TechEd Registration Check-In

The second image (`SAP_TechEd_BCN2018_01256.jpg`) contains the following text :

  - Pleas have your Photo `I.D.` ready

Click on **Send**.

You should receive a response that includes a series of entries:

![Postman](03.png)

```json
{
    "imageName": "SAP_TechEd_BCN2018_01020.jpg",
    "results": [
        {
            "bboxAccuracy": 0.9788690209388733,
            "boundingBox": {
                "x1": 1139,
                "x2": 1477,
                "x3": 1482,
                "x4": 1144,
                "y1": 291,
                "y2": 276,
                "y3": 402,
                "y4": 417
            },
            "text": "Check-ll"
        },
        {
            "bboxAccuracy": 0.981939971446991,
            "boundingBox": {
                "x1": 799,
                "x2": 1098,
                "x3": 1104,
                "x4": 804,
                "y1": 381,
                "y2": 365,
                "y3": 477,
                "y4": 493
            },
            "text": "Registration"
        },
        {
            "bboxAccuracy": 0.977262020111084,
            "boundingBox": {
                "x1": 530,
                "x2": 632,
                "x3": 640,
                "x4": 538,
                "y1": 472,
                "y2": 458,
                "y3": 516,
                "y4": 530
            },
            "text": "TECHED"
        }
    ]
}
```

Each entry in the response represents a box that identify a bounding box with an associated text.

<table border="0">
	<tr><td><img id="img_1"/></div></td></tr><tr><td><canvas id="canvas_1"/></td></tr>
</table>

<script>
var fontSize = 10;

function drawImage(imagId) {
	var oImg = document.getElementById("img_" + imagId);;
	var oCanvas = document.getElementById("canvas_" + imagId);
	oCanvas.width = oImg.width;
	oCanvas.height = oImg.height * oImg.width / oImg.naturalWidth;

	var ctx = oCanvas.getContext("2d");
    ctx.drawImage(oImg, 0, 0, oImg.width, oImg.height * oImg.width / oImg.naturalWidth);

    ctx.lineWidth="3";
    ctx.strokeStyle="red";
	ctx.font = fontSize + "px Arial";
	// ctx.scale( oCanvas.width  / oImg.naturalWidth, oCanvas.height / oImg.naturalHeight);
    oImg.style.display = "none";

    return ctx;
}

window.onload = function() {
	var oImg    = document.getElementById("img_1");
	var oCanvas = document.getElementById("canvas_1");

    var response = {
        "imageName": "SAP_TechEd_BCN2018_01020.jpg",
        "predictions": [
            {
                "bboxAccuracy": 0.9788690209388733,
                "boundingBox": {
                    "x1": 1139,
                    "x2": 1477,
                    "x3": 1482,
                    "x4": 1144,
                    "y1": 291,
                    "y2": 276,
                    "y3": 402,
                    "y4": 417
                },
                "text": "Check-ll"
            },
            {
                "bboxAccuracy": 0.981939971446991,
                "boundingBox": {
                    "x1": 799,
                    "x2": 1098,
                    "x3": 1104,
                    "x4": 804,
                    "y1": 381,
                    "y2": 365,
                    "y3": 477,
                    "y4": 493
                },
                "text": "Registration"
            },
            {
                "bboxAccuracy": 0.977262020111084,
                "boundingBox": {
                    "x1": 530,
                    "x2": 632,
                    "x3": 640,
                    "x4": 538,
                    "y1": 472,
                    "y2": 458,
                    "y3": 516,
                    "y4": 530
                },
                "text": "TECHED"
            }
        ]
    };
	oImg.onload = function(){
		var ctx = drawImage("1")
		for (var i = 0; i < response.predictions.length; i++) {
            // draw the bouding box
			ctx.moveTo(response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1);
			ctx.lineTo(response.predictions[i].boundingBox.x2, response.predictions[i].boundingBox.y2);
			ctx.lineTo(response.predictions[i].boundingBox.x3, response.predictions[i].boundingBox.y3);
			ctx.lineTo(response.predictions[i].boundingBox.x4, response.predictions[i].boundingBox.y4);
			ctx.lineTo(response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1);
			ctx.stroke();

            // write the text with the box angle
			var text =  "#" + i  + ": " + response.predictions[i].text + " " + ( response.predictions[i].bboxAccuracy * 100 ).toFixed(2) + "%";
			ctx.save();
            // do some trigonometry to calculate the angle
            var ab = Math.abs(response.predictions[i].boundingBox.x2 - response.predictions[i].boundingBox.x1);
            var ac = Math.abs(response.predictions[i].boundingBox.y2 - response.predictions[i].boundingBox.y1);
            var bc = Math.sqrt( Math.pow(ab, 2) + Math.pow(ac, 2));
            var clockwise = (response.predictions[i].boundingBox.y2 - response.predictions[i].boundingBox.y1 > 0 ? 1 : -1);
			var angle = Math.acos(ab /  bc);
			ctx.rotate( clockwise * angle );
            // draw the background box
            ctx.fillStyle = 'red';
            ctx.fillRect(response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1, ctx.measureText(text).width, parseInt(fontSize*1.5));
            // write the text in the box
            ctx.fillStyle = "white";
            ctx.fillText(text, response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1 + fontSize);
            ctx.restore();
		}
    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551006";
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
var fontSize = 10;

function drawImage(imagId) {
	var oImg = document.getElementById("img_" + imagId);;
	var oCanvas = document.getElementById("canvas_" + imagId);
	oCanvas.width = oImg.width;
	oCanvas.height = oImg.height * oImg.width / oImg.naturalWidth;

	var ctx = oCanvas.getContext("2d");
    ctx.drawImage(oImg, 0, 0, oImg.width, oImg.height * oImg.width / oImg.naturalWidth);

    ctx.lineWidth="3";
    ctx.strokeStyle="red";
	ctx.font = fontSize + "px Arial";
	// ctx.scale( oCanvas.width  / oImg.naturalWidth, oCanvas.height / oImg.naturalHeight);
    oImg.style.display = "none";

    return ctx;
}

window.onload = function() {
	var oImg    = document.getElementById("img_1");
	var oCanvas = document.getElementById("canvas_1");

    var response = {
        "imageName": "SAP_TechEd_BCN2018_01020.jpg",
        "predictions": [
            {
                "bboxAccuracy": 0.9788690209388733,
                "boundingBox": {
                    "x1": 1139,
                    "x2": 1477,
                    "x3": 1482,
                    "x4": 1144,
                    "y1": 291,
                    "y2": 276,
                    "y3": 402,
                    "y4": 417
                },
                "text": "Check-ll"
            },
            {
                "bboxAccuracy": 0.981939971446991,
                "boundingBox": {
                    "x1": 799,
                    "x2": 1098,
                    "x3": 1104,
                    "x4": 804,
                    "y1": 381,
                    "y2": 365,
                    "y3": 477,
                    "y4": 493
                },
                "text": "Registration"
            },
            {
                "bboxAccuracy": 0.977262020111084,
                "boundingBox": {
                    "x1": 530,
                    "x2": 632,
                    "x3": 640,
                    "x4": 538,
                    "y1": 472,
                    "y2": 458,
                    "y3": 516,
                    "y4": 530
                },
                "text": "TECHED"
            }
        ]
    };
	oImg.onload = function(){
		var ctx = drawImage("1")
		for (var i = 0; i < response.predictions.length; i++) {
            // draw the bouding box
			ctx.moveTo(response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1);
			ctx.lineTo(response.predictions[i].boundingBox.x2, response.predictions[i].boundingBox.y2);
			ctx.lineTo(response.predictions[i].boundingBox.x3, response.predictions[i].boundingBox.y3);
			ctx.lineTo(response.predictions[i].boundingBox.x4, response.predictions[i].boundingBox.y4);
			ctx.lineTo(response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1);
			ctx.stroke();

            // write the text with the box angle
			var text =  "#" + i  + ": " + response.predictions[i].text + " " + ( response.predictions[i].bboxAccuracy * 100 ).toFixed(2) + "%";
            ctx.save();
            // do some trigonometry to calculate the angle
            var ab = Math.abs(response.predictions[i].boundingBox.x2 - response.predictions[i].boundingBox.x1);
            var ac = Math.abs(response.predictions[i].boundingBox.y2 - response.predictions[i].boundingBox.y1);
            var bc = Math.sqrt( Math.pow(ab, 2) + Math.pow(ac, 2));
            var clockwise = (response.predictions[i].boundingBox.y2 - response.predictions[i].boundingBox.y1 > 0 ? 1 : -1);
			var angle = Math.acos(ab /  bc);
			ctx.rotate( clockwise * angle );

            // draw the background box
            ctx.fillStyle = 'red';
            ctx.fillRect(response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1, ctx.measureText(text).width, parseInt(fontSize*1.5));
            // write the text in the box
            ctx.fillStyle = "white";
            ctx.fillText(text, response.predictions[i].boundingBox.x1, response.predictions[i].boundingBox.y1 + fontSize);
            ctx.restore();
		}
    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551006";
};
</script>
</body>
</html>
```
You will notice that the coordinates are not rectangles, but are 4 points x and y.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Validate your results)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
