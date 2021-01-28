---
title: Create a SAPUI5 interface
description: Create a SAPUI5 interface enhancing a Fiori Master-detail template
auto_validation: true
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>html5, topic>odata, topic>sapui5, products>sap-hana ]
time: 15
---

## Prerequisites  
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.

## Details
### You will learn  
  - How to create a user interface with a Fiori Master-Detail template


---

[ACCORDION-BEGIN [Step 1: ](Create a UI5 module)]

Create a new module using the `Fiori Master-Detail Module`

![Create Fiori Module](1.png)

Name it `web`

![Name Fiori Module](2.png)

The wizard will present with options to bind the web interface to a data provider. In your case, you have created an OData service. Choose it from the list:

![Name Fiori Module](3.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Map the OData fields with the user interface)]

You will first provide general information about the application:

![Name Fiori Module](4.png)

Scroll down and complete the binding for the header:

![Name Fiori Module](5.png)

Keep scrolling down and complete the binding for the items:

![Name Fiori Module](6_1.png)

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Check dependencies)]

The web module depends on the Node.js module to fill the interface with data. This time, Web IDE has added the dependencies automatically in the `mta.yaml` file:

![Name Fiori Module](7.png)

Locate the dependency in the **Code Editor**.

![Name Fiori Module](validate.png)

Paste the entire web module definition, including its requirements and no resources. Then click on **Validate**

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Modify the dependencies)]

Open the `package.json` file in the web module.

![Open package json](dep1.png)

And change the version of the  `@sap/approuter` module to `2.9.1` to match the version delivered in your virtual machine:

![Open package json](dep2.png)

**Save** and **close** the file.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the module)]

**Build** the entire project.

Make sure the `js` module is running first (remember, it's a dependency).

![js module running](js.png)

Run the web module:

![web module run](web.png)


A new browser tab will open.

![super web app](super_1.png)

Click on any Purchase Order and check the URL to respond to the question below.

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Sync to GitHub and check next steps)]

If you want to take the code with you, remember you can **Stage All** and sync to GitHub.

![Final sync](sync.png)

The application you have created can be enhanced to add more functionalities or features, such as user authentication. Here are some links if you want to get your own system or continue learning for free:

- Get your own SAP HANA, express edition, instance: <https://developers.sap.com/topics/hana.html>
- Tutorials for Development in XS Advanced : <https://developers.sap.com/group.hana-xsa-get-started.html>
- XS Advanced Development for (not so) dummies (blog): <https://goo.gl/vsuy2H>
- Intro to Core Data Services (SAP `CodeTalk`): <https://goo.gl/Fpm4JT>
- Intro to XS OData (SAP `CodeTalk`): <https://goo.gl/hQHmeK>

Visit **`developers.sap.com`** to check the tutorials on XS Advanced and to get your own instance of SAP HANA, express edition.


[DONE]
[ACCORDION-END]
