---
title: Use the Similarity Scoring with the Face Feature Extraction API from a REST Client
description: Discover how to call the Similarity Scoring with the Face Feature Extraction API from a REST Client like Postman
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>cloud, topic>machine-learning, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-leonardo-machine-learning-foundation
---

## Prerequisites
 - [Create a Machine Learning Foundation service instance on the Cloud Foundry environment](https://developers.sap.com/tutorials/cp-mlf-create-instance.html)
 - [Install Postman as a REST client](https://developers.sap.com/tutorials/api-tools-postman-install.html)
 - [Get your OAuth Access Token using a REST Client](https://developers.sap.com/tutorial-navigator.html?cp-mlf-rest-get-oauth-token.html)

## Details
### You will learn
  - Call multiple API and reuse the result into the next one from a REST client like Postman
  - The basics about Machine Learning Foundation Service for Similarity Scoring & Face Feature Extraction

> ### **Note:** The Face Feature Extraction service was in alpha version when this tutorial was released.

[ACCORDION-BEGIN [Step](The Face Feature Extraction Service)]

Similarly to the Text Feature Extraction or the Image Feature Extraction service, the Face Feature Extraction service will detect faces in an image or a set of images and extracts a vector of features out of each detected face.

This is the list of accepted file extensions:

|Name                  | Description
|----------------------|--------------------
| **Archive file**     | `zip`, `tar`, `gz`, `tgz`
| **Image file**       | `jpg`, `jpe`, `jpeg`, `png`, `gif`, `bmp`

The images should be RGB, or 8-bit gray scale.

If an archive file is provided, no additional files can be provided.

The input file (or the archive file) is provided using form data (as an element named ***files*** in the form data).

The service will return a JSON response that includes:

 - the number of detected faces
 - the bounding boxes coordinates for the identified faces
 - the associated vector of features for the detected face

For more details, you can check the following link:

 - [Inference Service for Face Feature Extraction on the SAP API Business Hub](https://api.sap.com/api/face_feature_extraction_api/resource).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](The Similarity Scoring Service)]

The Similarity Scoring service compares vectors of extracted features with respect to cosine similarity.

A vector of features can be represented using the following format ***[number, number, number, ..., number]***.

The set of feature vectors which should be compared must be provided either using:

 - an archive file which will contain multiple feature vector file
 - plain text that represents a vector of features

With both options, you can provide either a single set of feature vector entries or two sets of feature vector entries which will drive the way entries are processed:

 - with a single feature vector entries, every feature vector entries will be compared to each other ((n-1)!-1 comparison).
 - with two set of feature vector entries, every feature vector entries in the first set will be compared to every feature vector entries from the second set (n*m comparison).

The input content will provided using form data, either as:

- a single or two element named ***files*** in the form data for the ***archive file***
- a single or two element named ***texts*** in the form data for the ***plain text***

This is the list of accepted file extensions for the archive file:

|Name                  | Description
|----------------------|--------------------
| **Archive file**     | `zip`, `tar`, `gz`, `tgz`

A required setting must also be provided as part of the form data (named ***options*** in the form data) using a JSON string format.

| Name                                    | Description            
|-----------------------------------------|--------------------------------------------
| <nobr><b>`numSimilarVectors`</b></nobr> | Number of most similar vectors to return in the response

The service will return a JSON response that includes a series of scores for each comparison.

For more details, you can check the [Inference Service for Similarity Scoring on the SAP API Business Hub](https://api.sap.com/api/similarity_scoring_api/resource).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Call the Face Feature Extraction API)]

First you will need to select and download a series of pictures with faces.

If you are missing some inspiration, you can use the following pictures from the **Women Tech Influencer Fireside Chat** at SAP TechEd Las Vegas 2018 :

 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="SAP_TechEd_LV2018_10751.jpg">Image `SAP_TechEd_LV2018_10751`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134" target="blank" download="SAP_TechEd_LV2018_10772.jpg">Image `SAP_TechEd_LV2018_10772`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="SAP_TechEd_LV2018_10779.jpg">Image `SAP_TechEd_LV2018_10779`</a>

The first image (`Image SAP_TechEd_LV2018_10751`) contains only one face, the second image (`Image SAP_TechEd_LV2018_10772`) contains three faces and the third image (`Image SAP_TechEd_LV2018_10779`) another three faces.

As you will notice one face is present in all 3 pictures and a second face in only 2.

Open a new tab in ***Postman***.

Make sure that the ***`my-l-foundation`*** environment is selected.

On the **Authorization** tab, select **Bearer Token**, then enter **`{{OAuthToken}}`** as value.

![Postman](01.png)

> ### **Note:**: the **`OAuthToken`** environment variable can be retrieved following the [Get your OAuth Access Token using a REST Client](https://developers.sap.com/tutorial-navigator.html?cp-mlf-rest-get-oauth-token.html) tutorial.

Fill in the following additional information:

Field Name               | Value
:----------------------- | :--------------
<nobr>HTTP Method</nobr> | POST
<nobr>URL<nobr>          | <nobr>`https://mlftrial-face-feature-extractor.cfapps.eu10.hana.ondemand.com/api/v2alpha1/image/face-feature-extraction`</nobr>

> **Note** As a reminder, the URL depends on you Cloud Platform landscape region but for the trial landscape only ***Europe (Frankfurt)*** provide access to the Machine Learning Foundation services.

On the **Body** tab, keep **`form-data`** selected.

Add 3 elements with the **key** named **files** and switch it to **File** instead of **Text** (default).

Then set the file with the downloaded images in the following order:

 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="SAP_TechEd_LV2018_10751.jpg">Image `SAP_TechEd_LV2018_10751`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134" target="blank" download="SAP_TechEd_LV2018_10772.jpg">Image `SAP_TechEd_LV2018_10772`</a>
 - <a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="SAP_TechEd_LV2018_10779.jpg">Image `SAP_TechEd_LV2018_10779`</a>

The sequence of files is important here as you will be using a script to retrieve the corresponding responses.

![Postman](02.png)

Switch to the **Tests** tab and insert the following code:

```
pm.environment.set("Face 0 - SAP_TechEd_LV2018_10751", decodeURIComponent(pm.response.json().predictions[0].faces[0].face_feature))
pm.environment.set("Face 0 - SAP_TechEd_LV2018_10772", decodeURIComponent(pm.response.json().predictions[1].faces[0].face_feature))
pm.environment.set("Face 1 - SAP_TechEd_LV2018_10772", decodeURIComponent(pm.response.json().predictions[1].faces[1].face_feature))
pm.environment.set("Face 2 - SAP_TechEd_LV2018_10772", decodeURIComponent(pm.response.json().predictions[1].faces[2].face_feature))
pm.environment.set("Face 0 - SAP_TechEd_LV2018_10779", decodeURIComponent(pm.response.json().predictions[2].faces[0].face_feature))
pm.environment.set("Face 1 - SAP_TechEd_LV2018_10779", decodeURIComponent(pm.response.json().predictions[2].faces[1].face_feature))
pm.environment.set("Face 2 - SAP_TechEd_LV2018_10779", decodeURIComponent(pm.response.json().predictions[2].faces[2].face_feature))
```

![Postman](03.png)

Click on **Send**.

You should receive a response that includes for each input image an entry with a series of faces entries with the bounding box and the face feature vector:

![Postman](04.png)

```json
{
    "faces": [
        {
            "face_feature": [ "..."],
            "face_location": {
                "bottom": 663,
                "left": 959,
                "right": 1182,
                "top": 440
            }
        }
    ],
    "name": "SAP_TechEd_LV2018_10751.jpg",
    "numberOfFaces": 1
}
```

Each entry in the response represents a box that identify one of the face.

<table border="0">
	<tr><td colspan=2><a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="SAP_TechEd_LV2018_10751.jpg">Image SAP_TechEd_LV2018_10751</a></td></tr>
	<tr><td><img id="SAP_TechEd_LV2018_10751.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132"/></td></tr><tr><td><canvas id="canvas_SAP_TechEd_LV2018_10751.jpg"/></td></tr>
	<tr><td colspan=2><a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134" target="blank" download="SAP_TechEd_LV2018_10772.jpg">Image SAP_TechEd_LV2018_10772</a></td></tr>
	<tr><td><img id="SAP_TechEd_LV2018_10772.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134"/></td></tr><tr><td><canvas id="canvas_SAP_TechEd_LV2018_10772.jpg"/></td></tr>
	<tr><td colspan=2><a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="SAP_TechEd_LV2018_10779.jpg">Image SAP_TechEd_LV2018_10779</a></td></tr>
	<tr><td><img id="SAP_TechEd_LV2018_10779.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135"/></td></tr><tr><td><canvas id="canvas_SAP_TechEd_LV2018_10779.jpg"/></td></tr>				
</table>

<script>
var fontSize = 14;

var response = {
    "id": "e24a976f-b147-4bd9-7ec2-d8882cb3d61f",
    "predictions": [
        {
            "faces": [
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 663,
                        "left": 959,
                        "right": 1182,
                        "top": 440
                    }
                }
            ],
            "name": "SAP_TechEd_LV2018_10751.jpg",
            "numberOfFaces": 1
        },
        {
            "faces": [
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 696,
                        "left": 572,
                        "right": 758,
                        "top": 511
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 696,
                        "left": 77,
                        "right": 262,
                        "top": 511
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 696,
                        "left": 923,
                        "right": 1109,
                        "top": 511
                    }
                }
            ],
            "name": "SAP_TechEd_LV2018_10772.jpg",
            "numberOfFaces": 3
        },
        {
            "faces": [
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 676,
                        "left": 324,
                        "right": 510,
                        "top": 490
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 666,
                        "left": 786,
                        "right": 941,
                        "top": 511
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 655,
                        "left": 1088,
                        "right": 1274,
                        "top": 469
                    }
                }
            ],
            "name": "SAP_TechEd_LV2018_10779.jpg",
            "numberOfFaces": 3
        }
    ],
    "processedTime": "2018-11-14T16:07:16.381410+00:00",
    "status": "DONE"
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

	var oImg    = document.getElementById(imageId);
    var oCanvas = document.getElementById("canvas_" + imageId);

    var widthRatio  = oCanvas.width  / oImg.naturalWidth;
    var heightRatio = oCanvas.height / oImg.naturalHeight;

    // get the box attributes
    var left = item.face_location.left * widthRatio;
    var top  = item.face_location.top  * heightRatio;
    var width  = (item.face_location.right  - item.face_location.left) * widthRatio;
    var height = (item.face_location.bottom - item.face_location.top ) * heightRatio;

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
	for (var idx = 0; idx < response.predictions.length; idx++) {
        var items = response.predictions[idx].faces;
        var name = response.predictions[idx].name;
        var oImg = document.getElementById(name);
        if(oImg){
            oImg.onload = (function (name_, items_) {
                return function () {
                    var ctx = drawCanvas(name_);
                    for (var i = 0; i < items_.length; i++) {
                        drawBoundingBox(ctx, name_, items_[i], "Face #" + i);
                    }                
                }
            })(name, items);
            oImg.src = oImg.src;
        }
    }
};
</script>

Here is a simple HTML code you can use to visualize other results:

```HTML
<html>
<body>
<table border="0">
	<tr><td colspan=2><a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132" target="blank" download="SAP_TechEd_LV2018_10751.jpg">Image SAP_TechEd_LV2018_10751</a></td></tr>
	<tr><td><img id="SAP_TechEd_LV2018_10751.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550132"/></td></tr><tr><td><canvas id="canvas_SAP_TechEd_LV2018_10751.jpg"/></td></tr>
	<tr><td colspan=2><a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134" target="blank" download="SAP_TechEd_LV2018_10772.jpg">Image SAP_TechEd_LV2018_10772</a></td></tr>
	<tr><td><img id="SAP_TechEd_LV2018_10772.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550134"/></td></tr><tr><td><canvas id="canvas_SAP_TechEd_LV2018_10772.jpg"/></td></tr>
	<tr><td colspan=2><a href="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135" target="blank" download="SAP_TechEd_LV2018_10779.jpg">Image SAP_TechEd_LV2018_10779</a></td></tr>
	<tr><td><img id="SAP_TechEd_LV2018_10779.jpg" width="100%" src="https://sapteched2018.event-hosting.com/srv/ds/custom/download?size=2048&images=550135"/></td></tr><tr><td><canvas id="canvas_SAP_TechEd_LV2018_10779.jpg"/></td></tr>				
</table>

<script>
var fontSize = 14;

var response = {
    "id": "e24a976f-b147-4bd9-7ec2-d8882cb3d61f",
    "predictions": [
        {
            "faces": [
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 663,
                        "left": 959,
                        "right": 1182,
                        "top": 440
                    }
                }
            ],
            "name": "SAP_TechEd_LV2018_10751.jpg",
            "numberOfFaces": 1
        },
        {
            "faces": [
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 696,
                        "left": 572,
                        "right": 758,
                        "top": 511
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 696,
                        "left": 77,
                        "right": 262,
                        "top": 511
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 696,
                        "left": 923,
                        "right": 1109,
                        "top": 511
                    }
                }
            ],
            "name": "SAP_TechEd_LV2018_10772.jpg",
            "numberOfFaces": 3
        },
        {
            "faces": [
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 676,
                        "left": 324,
                        "right": 510,
                        "top": 490
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 666,
                        "left": 786,
                        "right": 941,
                        "top": 511
                    }
                },
                {
                    "face_feature": [ "..."],
                    "face_location": {
                        "bottom": 655,
                        "left": 1088,
                        "right": 1274,
                        "top": 469
                    }
                }
            ],
            "name": "SAP_TechEd_LV2018_10779.jpg",
            "numberOfFaces": 3
        }
    ],
    "processedTime": "2018-11-14T16:07:16.381410+00:00",
    "status": "DONE"
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

	var oImg    = document.getElementById(imageId);
    var oCanvas = document.getElementById("canvas_" + imageId);

    var widthRatio  = oCanvas.width  / oImg.naturalWidth;
    var heightRatio = oCanvas.height / oImg.naturalHeight;

    // get the box attributes
    var left = item.face_location.left * widthRatio;
    var top  = item.face_location.top  * heightRatio;
    var width  = (item.face_location.right  - item.face_location.left) * widthRatio;
    var height = (item.face_location.bottom - item.face_location.top ) * heightRatio;

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
	for (var idx = 0; idx < response.predictions.length; idx++) {
        var items = response.predictions[idx].faces;
        var name = response.predictions[idx].name;
        var oImg = document.getElementById(name);
        if(oImg){
            oImg.onload = (function (_name, _items) {
                return function () {
                    var ctx = drawCanvas(_name);
                    for (var i = 0; i < _items.length; i++) {
                        drawBoundingBox(ctx, _name, _items[i], "Face #" + i);
                    }                
                }
            })(name, items);
            oImg.src = oImg.src;
        }
    }
};
</script>
</body>
</html>
```
You will notice that the rectangle uses the following format: (left, top, right - left, bottom - top).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Call the Similarity Scoring API)]

Open a new tab in ***Postman***.

Make sure that the ***`my-l-foundation`*** environment is selected.

On the **Authorization** tab, select **Bearer Token**, then enter **`{{OAuthToken}}`** as value.

Fill in the following additional information:

Field Name               | Value
:----------------------- | :--------------
<nobr>HTTP Method</nobr> | POST
<nobr>URL<nobr>          | <nobr>`https://mlftrial-similarity-scoring.cfapps.eu10.hana.ondemand.com/api/v2/similarity-scoring`</nobr>

On the **Body** tab, keep **`form-data`** selected.

Add a key named **texts** and switch it to **Text** (default).

Paste the following value:

```JSON
{
	"0":
	[
		{"id": "Face 0 - SAP_TechEd_LV2018_10751", "vector": [{{Face 0 - SAP_TechEd_LV2018_10751}}]},
		{"id": "Face 0 - SAP_TechEd_LV2018_10772", "vector": [{{Face 0 - SAP_TechEd_LV2018_10772}}]},
		{"id": "Face 1 - SAP_TechEd_LV2018_10772", "vector": [{{Face 1 - SAP_TechEd_LV2018_10772}}]},
		{"id": "Face 2 - SAP_TechEd_LV2018_10772", "vector": [{{Face 2 - SAP_TechEd_LV2018_10772}}]},
		{"id": "Face 0 - SAP_TechEd_LV2018_10779", "vector": [{{Face 0 - SAP_TechEd_LV2018_10779}}]},
		{"id": "Face 1 - SAP_TechEd_LV2018_10779", "vector": [{{Face 1 - SAP_TechEd_LV2018_10779}}]},
		{"id": "Face 2 - SAP_TechEd_LV2018_10779", "vector": [{{Face 2 - SAP_TechEd_LV2018_10779}}]}
	]
}
```

Add a key named **options** and switch it to **Text** (default).

Paste the following value:

```JSON
{"numSimilarVectors":3}
```

![Postman](05.png)

Click on **Send**.

You should receive a response that includes for each input feature vector (in your case 7 in total) the top 3 most similar feature vectors.

For example here, the face identified as **`Face 0`** from the first image (`SAP_TechEd_LV2018_10751.jpg`) has been matched with faces:

 - **`Face 1`** from the second image (`SAP_TechEd_LV2018_10772.jpg`) with a score of ***0.88***
 - **`Face 2`** from the second image (`SAP_TechEd_LV2018_10772.jpg`) with a score of ***0.93***
 - **`Face 1`** from the second image (`SAP_TechEd_LV2018_10779.jpg`) with a score of ***0.96***

```json
{
    "id": "Face 0 - SAP_TechEd_LV2018_10751",
    "similarVectors": [
        {
            "id": "Face 1 - SAP_TechEd_LV2018_10779",
            "score": 0.9653868993549426
        },
        {
            "id": "Face 2 - SAP_TechEd_LV2018_10772",
            "score": 0.9343656611744725
        },
        {
            "id": "Face 1 - SAP_TechEd_LV2018_10772",
            "score": 0.8803163672575983
        }
    ]
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Validate your results)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
---
