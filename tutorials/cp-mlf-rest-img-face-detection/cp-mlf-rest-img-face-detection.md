---
title: Use the Face Detection API from a REST Client
description: Discover how to call the Face Detection API from a REST Client like Postman
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
  - The basics about Machine Learning Foundation Service for Face Detection

> ### **Note:** This service was in alpha version when this tutorial was released.

[ACCORDION-BEGIN [Step](The Face Detection Service)]

The Face Detection service can be used for detecting faces in an image or a set of images and returns information declaring boundary box for each face in every image.

This is the list of accepted file extensions:

|Name                  | Description
|----------------------|--------------------
| **Archive file**     | `zip`, `tar`, `gz`, `tgz`
| **Image file**       | `jpg`, `jpe`, `jpeg`, `png`, `gif`, `bmp`

The images should be RGB, or 8-bit gray scale.

If an archive file is provided, no additional files can be provided.

The input file (or the archive file) is provided using form data (as an element named ***files*** in the form data).

The service will return a JSON response that includes the bounding boxes coordinates for the identified faces.

For more details, you can check the [Inference Service for Face Detection on the SAP API Business Hub](https://api.sap.com/api/face_detection_api/resource).

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
<nobr>URL<nobr>          | <nobr>`https://mlftrial-face-detector.cfapps.eu10.hana.ondemand.com/api/v2alpha1/image/face-detection`</nobr>

> **Note** As a reminder, the URL depends on you Cloud Platform landscape region but for the trial landscape only ***Europe (Frankfurt)*** provide access to the Machine Learning Foundation services.

On the **Body** tab, keep **`form-data`** selected. Add a new key named **files** and switch it to **File** instead of **Text** (default).

Select your image file.

![Postman](02.png)

If you are missing some inspiration, you can use the following pictures from the **Women Tech Influencer Fireside Chat** at SAP TechEd Las Vegas 2018 :

 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="SAP_TechEd_LV2018_10751.jpg">Image `SAP_TechEd_LV2018_10751`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134" target="blank" download="SAP_TechEd_LV2018_10772.jpg">Image `SAP_TechEd_LV2018_10772`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="SAP_TechEd_LV2018_10779.jpg">Image `SAP_TechEd_LV2018_10779`</a>

The first image (`SAP_TechEd_LV2018_10751.jpg`) contains 3 person with only 1 face visible, the second image (`SAP_TechEd_LV2018_10772.jpg`) contains 5 person with only 3 face visible and the third image (`SAP_TechEd_LV2018_10779.jpg`) also 5 person with only 3 face visible.

Click on **Send**.

You should receive a response that includes a series of faces entries:

![Postman](03.png)

```json
{
    "faces": [
        {
            "bottom": 676,
            "left": 324,
            "right": 510,
            "top": 490
        },
        {
            "bottom": 649,
            "left": 769,
            "right": 924,
            "top": 494
        },
        {
            "bottom": 632,
            "left": 1078,
            "right": 1233,
            "top": 477
        }
    ],
    "name": "550135-pic-2000y2000.jpg",
    "numberOfFaces": 3
}
```

Each entry in the response represents a box that identify one of the face.

Here is the results represented on <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="Image 10779.jpg">Image 10779</a>

<table border="0">
	<tr><td><img id="img_1"/></div></td></tr><tr><td><canvas id="canvas_1"/></td></tr>
</table>

<script>
function drawImage(imagId) {
	var oImg = document.getElementById("img_" + imagId);
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
var imagId = 1;
var response = {
    "faces": [
        {
            "bottom": 676,
            "left": 324,
            "right": 510,
            "top": 490
        },
        {
            "bottom": 649,
            "left": 769,
            "right": 924,
            "top": 494
        },
        {
            "bottom": 632,
            "left": 1078,
            "right": 1233,
            "top": 477
        }
    ],
    "name": "550135-pic-2000y2000.jpg",
    "numberOfFaces": 3
}
window.onload = function() {
	var oImg    = document.getElementById("img_" + imagId);
	oImg.onload = function(){
		var ctx = drawImage("1")
		ctx.strokeRect(response.faces[0].left, response.faces[0].top, response.faces[0].right - response.faces[0].left, response.faces[0].bottom - response.faces[0].top);  
		ctx.strokeRect(response.faces[1].left, response.faces[1].top, response.faces[1].right - response.faces[1].left, response.faces[1].bottom - response.faces[1].top);  
		ctx.strokeRect(response.faces[2].left, response.faces[2].top, response.faces[2].right - response.faces[2].left, response.faces[2].bottom - response.faces[2].top);  

		ctx.fillText("0", response.faces[0].left, response.faces[0].top);
		ctx.fillText("1", response.faces[1].left, response.faces[1].top);
		ctx.fillText("2", response.faces[2].left, response.faces[2].top);
    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135";
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
	var oImg = document.getElementById("img_" + imagId);
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
var imagId = 1;
var response = {
    "faces": [
        {
            "bottom": 676,
            "left": 324,
            "right": 510,
            "top": 490
        },
        {
            "bottom": 649,
            "left": 769,
            "right": 924,
            "top": 494
        },
        {
            "bottom": 632,
            "left": 1078,
            "right": 1233,
            "top": 477
        }
    ],
    "name": "550135-pic-2000y2000.jpg",
    "numberOfFaces": 3
}
window.onload = function() {
	var oImg    = document.getElementById("img_" + imagId);
	oImg.onload = function(){
		var ctx = drawImage("1")
		ctx.strokeRect(response.faces[0].left, response.faces[0].top, response.faces[0].right - response.faces[0].left, response.faces[0].bottom - response.faces[0].top);  
		ctx.strokeRect(response.faces[1].left, response.faces[1].top, response.faces[1].right - response.faces[1].left, response.faces[1].bottom - response.faces[1].top);  
		ctx.strokeRect(response.faces[2].left, response.faces[2].top, response.faces[2].right - response.faces[2].left, response.faces[2].bottom - response.faces[2].top);  

		ctx.fillText("0", response.faces[0].left, response.faces[0].top);
		ctx.fillText("1", response.faces[1].left, response.faces[1].top);
		ctx.fillText("2", response.faces[2].left, response.faces[2].top);
    }
	oImg.src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135";
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
---
