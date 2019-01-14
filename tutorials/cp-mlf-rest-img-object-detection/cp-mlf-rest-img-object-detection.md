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
  - The basics about Machine Learning Foundation Service for Object Detection

> ### **Note:** This service was in alpha version when this tutorial was released.

[ACCORDION-BEGIN [Step](The Object Detection Service)]

The Object Detection service can be used for detecting objects in an image or a set of images and returns information declaring boundary box around the objects in every image.

This is the list of accepted file extensions:

|Name                  | Description
|----------------------|--------------------
| **Archive file**     | `zip`, `tar`, `gz`, `tgz`
| **Image file**       | `jpg`, `jpe`, `jpeg`, `png`, `gif`, `bmp`, `tif`, `tiff`

The images should be RGB, or 8-bit gray scale.

If an archive file is provided, no additional files can be provided.

The input file (or the archive file) is provided using form data (as an element named ***file*** in the form data).

The service will return a JSON response that includes the bounding boxes coordinates for the identified objects.

For more details, you can check the following link:

 - [Inference Service for Object Detection on the SAP API Business Hub](https://api.sap.com/api/object_detection_api/resource)

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

<div>
<img id="SAP_TechEd_BCN2018_03504.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551397"/>
<canvas id="canvas_SAP_TechEd_BCN2018_03504.jpg"/>
<script>
var fontSize = 14;

var response = {
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
};

function drawCanvas(imageId) {
	var oImg    = document.getElementById(imageId);
    var oCanvas = document.getElementById("canvas_" + imageId);

	oCanvas.width  = oImg.width;
	oCanvas.height = oImg.height;

	var ctx = oCanvas.getContext("2d");
    ctx.drawImage(oImg, 0, 0, oImg.width, oImg.height);

    ctx.lineWidth="3";
    ctx.strokeStyle="red";
	ctx.fillStyle = "white";
	ctx.font = fontSize + "px Arial";

    oImg.style.display = "none";
    return ctx;
}
function drawBoundingBox(ctx, imageId, item, text) {
    var oCanvas = document.getElementById("canvas_" + imageId);

    // get the box attributes
    var left = item[1] * oCanvas.width  ;
    var top  = item[0] * oCanvas.height ;
    var width  = (item[3] * oCanvas.width  - item[1] * oCanvas.width  );
    var height = (item[2] * oCanvas.height - item[0] * oCanvas.height );

    // draw the box
    ctx.strokeRect(left, top, width, height);

    // write the text with the box angle
    ctx.save();
    ctx.fillStyle = 'red';
    ctx.fillRect(left, top, ctx.measureText(text).width + ctx.lineWidth, ctx.lineWidth + fontSize);
    // write the text in the box
    ctx.fillStyle = "white";
    ctx.fillText(text, left, top + fontSize);
    ctx.restore();
}

window.onload = function() {
    var name = "SAP_TechEd_BCN2018_03504.jpg";
    var oImg = document.getElementById(name);
    if(oImg){
        oImg.onload = function(){
            var ctx = drawCanvas(name);
            for (var i = 0; i < response.detection_boxes.length; i++) {
                var text = "#" + i  + ": " + response.detection_classes[i] + " " + ( response.detection_scores[i] * 100 ).toFixed(2) + "%";
                drawBoundingBox(ctx, name, response.detection_boxes[i], text);
            }                
        }
        oImg.src = oImg.src;
    }
};
</script>
</div>

Here is a simple HTML code you can use to visualize other results:

```HTML
<html>
<body>
<img id="SAP_TechEd_BCN2018_03504.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=551397"/>
<canvas id="canvas_SAP_TechEd_BCN2018_03504.jpg"/>
<script>
var fontSize = 14;

var response = {
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
};

function drawCanvas(imageId) {
	var oImg    = document.getElementById(imageId);
    var oCanvas = document.getElementById("canvas_" + imageId);

	oCanvas.width  = oImg.width;
	oCanvas.height = oImg.height;

	var ctx = oCanvas.getContext("2d");
    ctx.drawImage(oImg, 0, 0, oImg.width, oImg.height);

    ctx.lineWidth="3";
    ctx.strokeStyle="red";
	ctx.fillStyle = "white";
	ctx.font = fontSize + "px Arial";

    oImg.style.display = "none";
    return ctx;
}
function drawBoundingBox(ctx, imageId, item, text) {
    var oCanvas = document.getElementById("canvas_" + imageId);

    // get the box attributes
    var left = item[1] * oCanvas.width  ;
    var top  = item[0] * oCanvas.height ;
    var width  = (item[3] * oCanvas.width  - item[1] * oCanvas.width  );
    var height = (item[2] * oCanvas.height - item[0] * oCanvas.height );

    // draw the box
    ctx.strokeRect(left, top, width, height);

    // write the text with the box angle
    ctx.save();
    ctx.fillStyle = 'red';
    ctx.fillRect(left, top, ctx.measureText(text).width + ctx.lineWidth, ctx.lineWidth + fontSize);
    // write the text in the box
    ctx.fillStyle = "white";
    ctx.fillText(text, left, top + fontSize);
    ctx.restore();
}

window.onload = function() {
    var name = "SAP_TechEd_BCN2018_03504.jpg";
    var oImg = document.getElementById(name);
    if(oImg){
        oImg.onload = function(){
            var ctx = drawCanvas(name);
            for (var i = 0; i < response.detection_boxes.length; i++) {
                var text = "#" + i  + ": " + response.detection_classes[i] + " " + ( response.detection_scores[i] * 100 ).toFixed(2) + "%";
                drawBoundingBox(ctx, name, response.detection_boxes[i], text);
            }                
        }
        oImg.src = oImg.src;
    }
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
