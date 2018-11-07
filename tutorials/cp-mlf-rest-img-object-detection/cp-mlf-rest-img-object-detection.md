---
title: Use the Object Detection API from a REST Client
description: Discover how to call the Object Detection API from a REST Client like Postman
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

The Object Detection service can be used for detecting objects in an image or a set of images and returns information declaring boundary box around the objects in every image.

This is the list of accepted file extensions:

|Name                  | Description
|----------------------|--------------------
| **Archive file**     | `zip`, `tar`, `gz`, `tgz`
| **Image file**       | `jpg`, `jpe`, `jpeg`, `png`, `gif`, `bmp`, `tif`, `tiff`

The images should be RGB, or 8-bit gray scale.

If an archive file is provided, no additional files can be provided.

The input file (or the archive file) is provided using form data (as an element named ***file*** in the form data).

The service will return a JSON response that includes the bounding boxes coordinates for the identified humans.

For more details, you can check the following links:

 - [Inference Service for Human Detection on the SAP API Business Hub](https://api.sap.com/api/object_detection_api/resource)
 - [Swagger documentation](https://mlftrial-retrain-od-inference.cfapps.eu10.hana.ondemand.com)

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
<nobr>URL<nobr>          | <nobr>`https://mlftrial-retrain-od-inference.cfapps.eu10.hana.ondemand.com/api/v2alpha1/image/object-detection/`</nobr>

> **Note** As a reminder, the URL depends on you Cloud Platform landscape region but for the trial landscape only ***Europe (Frankfurt)*** provide access to the Machine Learning Foundation services.

On the **Body** tab, keep **`form-data`** selected. Add a new key named **file** and switch it to **File** instead of **Text** (default).

Select your image file.

![Postman](02.png)

If you are missing some inspiration, you can use the following pictures from [SAP TechEd Barcelona 2018](https://sapteched2018.event-hosting.com/Gallery-336659.html):

 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551397" target="blank" download="SAP_TechEd_BCN2018_03504.jpg">Image `SAP_TechEd_BCN2018_03504`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551624" target="blank" download="SAP_TechEd_BCN2018_05109.jpg">Image `SAP_TechEd_BCN2018_05109`</a>

The first image (`SAP_TechEd_BCN2018_03504.jpg`) contains only 3 objects and the second image (`SAP_TechEd_BCN2018_05109.jpg`) contains 33 objects.

Click on **Send**.

You should receive a response that includes the bounding boxes for each detected object, the class for each entry and the associated score (the following result is for Image `SAP_TechEd_BCN2018_03504`):

![Postman](03.png)

```json
{
    "detection_boxes": [
        [
            0.2735770344734192,
            0.10513334721326828,
            0.8366726636886597,
            0.2314799576997757
        ],
        [
            0,
            0.4907439947128296,
            0.8411328196525574,
            0.9973546266555786
        ],
        [
            0.06066185235977173,
            0.1283799558877945,
            0.8287661671638489,
            0.6976333260536194
        ]
    ],
    "detection_classes": [
        "bottle",
        "person",
        "tvmonitor"
    ],
    "detection_scores": [
        0.9997833371162415,
        0.9976527094841003,
        0.9811497926712036
    ]
}
```

Each entry in the detection box represents a box that identify one of the person.

Here is the results represented on <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551397" target="blank" download="SAP_TechEd_BCN2018_03504.jpg">Image `SAP_TechEd_BCN2018_03504`</a>:

<table border="0">
	<tr><td><img id="img_1"/></div></td></tr><tr><td><canvas id="canvas_1"/></td></tr>
</table>

<script>
function drawImage(imagId) {
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
    oImg.style.display = "none";
    return ctx;
}

window.onload = function() {
	var oImg    = document.getElementById("img_1");
	var oCanvas = document.getElementById("canvas_1");

    var response = {
        "detection_boxes": [
	        [ 0.2735770344734192, 0.10513334721326828, 0.8366726636886597, 0.2314799576997757 ],
	        [ 0, 0.4907439947128296, 0.8411328196525574, 0.9973546266555786 ],
			[ 0.06066185235977173, 0.1283799558877945, 0.8287661671638489, 0.6976333260536194 ]
	    ],
		"detection_classes": [ "bottle", "person", "tvmonitor" ],
		"detection_scores": [ 0.9997833371162415, 0.9976527094841003, 0.9811497926712036 ]
    };
	oImg.onload = function(){
		var ctx = drawImage("1")
		ctx.strokeRect(
            response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][0] * oCanvas.height,
            response.detection_boxes[0][3] * oCanvas.width  - response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][2] * oCanvas.height - response.detection_boxes[0][0] * oCanvas.height

        );  
		ctx.strokeRect(
            response.detection_boxes[1][1] * oCanvas.width, //x
            response.detection_boxes[1][0] * oCanvas.height,//y
            response.detection_boxes[1][3] * oCanvas.width  - response.detection_boxes[1][1] * oCanvas.width,
            response.detection_boxes[1][2] * oCanvas.height - response.detection_boxes[1][0] * oCanvas.height
        );         
		ctx.strokeRect(
            response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][0] * oCanvas.height,
            response.detection_boxes[2][3] * oCanvas.width  - response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][2] * oCanvas.height - response.detection_boxes[2][0] * oCanvas.height
        );        
        ctx.fillText(response.detection_classes[0] + " " + response.detection_scores[0], response.detection_boxes[0][1] * oCanvas.width, response.detection_boxes[0][2] * oCanvas.height );
        ctx.fillText(response.detection_classes[1] + " " + response.detection_scores[1], response.detection_boxes[1][1] * oCanvas.width, response.detection_boxes[1][2] * oCanvas.height );
        ctx.fillText(response.detection_classes[2] + " " + response.detection_scores[2], response.detection_boxes[2][1] * oCanvas.width, response.detection_boxes[2][2] * oCanvas.height );

    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551397";
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
function drawImage(imagId) {
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
    oImg.style.display = "none";
    return ctx;
}

window.onload = function() {
	var oImg    = document.getElementById("img_1");
	var oCanvas = document.getElementById("canvas_1");

    var response = {
        "detection_boxes": [
	        [ 0.2735770344734192, 0.10513334721326828, 0.8366726636886597, 0.2314799576997757 ],
	        [ 0, 0.4907439947128296, 0.8411328196525574, 0.9973546266555786 ],
			[ 0.06066185235977173, 0.1283799558877945, 0.8287661671638489, 0.6976333260536194 ]
	    ],
		"detection_classes": [ "bottle", "person", "tvmonitor" ],
		"detection_scores": [ 0.9997833371162415, 0.9976527094841003, 0.9811497926712036 ]
    };
	oImg.onload = function(){
		var ctx = drawImage("1")
		ctx.strokeRect(
            response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][0] * oCanvas.height,
            response.detection_boxes[0][3] * oCanvas.width  - response.detection_boxes[0][1] * oCanvas.width,
            response.detection_boxes[0][2] * oCanvas.height - response.detection_boxes[0][0] * oCanvas.height

        );  
		ctx.strokeRect(
            response.detection_boxes[1][1] * oCanvas.width, //x
            response.detection_boxes[1][0] * oCanvas.height,//y
            response.detection_boxes[1][3] * oCanvas.width  - response.detection_boxes[1][1] * oCanvas.width,
            response.detection_boxes[1][2] * oCanvas.height - response.detection_boxes[1][0] * oCanvas.height
        );         
		ctx.strokeRect(
            response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][0] * oCanvas.height,
            response.detection_boxes[2][3] * oCanvas.width  - response.detection_boxes[2][1] * oCanvas.width,
            response.detection_boxes[2][2] * oCanvas.height - response.detection_boxes[2][0] * oCanvas.height
        );        
        ctx.fillText(response.detection_classes[0] + " " + response.detection_scores[0], response.detection_boxes[0][1] * oCanvas.width, response.detection_boxes[0][2] * oCanvas.height );
        ctx.fillText(response.detection_classes[1] + " " + response.detection_scores[1], response.detection_boxes[1][1] * oCanvas.width, response.detection_boxes[1][2] * oCanvas.height );
        ctx.fillText(response.detection_classes[2] + " " + response.detection_scores[2], response.detection_boxes[2][1] * oCanvas.width, response.detection_boxes[2][2] * oCanvas.height );

    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551397";
};
</script>
</body>
</html>
```
You will notice that the [`strokeRect` function](https://www.w3schools.com/tags/canvas_strokerect.asp) uses the following format:

- x	: The x-coordinate of the upper-left corner of the rectangle
- y	: The y-coordinate of the upper-left corner of the rectangle
- width	: The width of the rectangle, in pixels
- height : The height of the rectangle, in pixels

Which correspond to following attribute of the bounding box (the returned value represent the relative position for x and y using the image width and height):

- x	: index 1 * canvas width
- y	: index 0 * canvas height
- width	: index 3 * canvas width - index 1 * canvas width
- height : index 2 * canvas height - index 0 * canvas height


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Validate your results)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
