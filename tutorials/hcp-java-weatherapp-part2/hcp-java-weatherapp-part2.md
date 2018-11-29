---
title: Adding a simple web page to a Java app
description: Adding a simple web page to a Java app
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 1](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part1.html)

## Next Steps
- [End-to-End Weather App Scenario Part 3](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part3.html)

## Details
### You will learn  
In this tutorial you will learn how to add a basic HTML page to a Java app.


### Time to Complete
**5 min**

---


[ACCORDION-BEGIN [Step 1: ](Create a new HTML page)]

Navigate to the **`webapp`** node in the Project Explorer and create a new HTML page by selecting the respective context menu entry: **New > HTML File**.

Choose the name `index.html` and click on **Finish**.

![Creating a new HTML file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add body)]

Add the following line of code in between the opening and the closing <body> tag:

```html
<p>Hello World!</p>
```

![Editing the body of the HTML file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Save and publish locally)]

Save your changes and publish the update project to the local server by selecting the respective context menu on the server in the Server view.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](View app)]

Once the publishing is done you can go back to the internal browser window and remove the `/hello` part of the URL and hit the **Enter** key. Now, you should see the same "Hello World!" message for the URL `http://localhost:8080/weatherapp/` as well. The reason for this is the `<welcome-file-list>` in the `web.xml` (located in the `WEB-INF` directory underneath the `webapp` folder).

![Run the java app, but use the base URL, not the servlet](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Clean up entries)]

For security reasons and for the sake of housekeeping you should remove all <welcome-file> entries except for the `index.html` entry from the `web.xml` file.

![Editing web.xml to remove welcom-file entries](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part2/e2e_02-5.png)


[ACCORDION-END]



