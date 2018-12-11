---
title: Implement a responsive UI using OpenUI5
description: Implement a responsive UI using OpenUI5
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 8](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part8.html)

## Next Steps
- [End-to-End Weather App Scenario Part 10](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part10.html)

## Details
### You will learn  
In this tutorial you will implement a simple responsive, mobile-compatible user interface using OpenUI5.

### Time to Complete
**15 min**

---


[ACCORDION-BEGIN [Step 1: ](Create folders)]

Create a `weather_app` folder underneath the `webapp` folder.

In that `weather_app` folder create another sub-folder called `view`.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create JavaScript and XML files)]

Next create the following JavaScript and XML files:

- `/weather_app/view/App.view.xml`
- `/weather_app/view/Details.controller.js`
- `/weather_app/view/Details.view.xml`
- `/weather_app/view/List.controller.js`
- `/weather_app/view/List.view.xml`
- `/weather_app/Component.js`
- `/weather_app/util/formatter.js`

>Note that `Component.js` is in the `weather_app` directory, not the `weather_app/view` directory, and you must create the `util` directory for the `formatter.js` file.

![Project explorer for UI5 view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part9/e2e_09-3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add file content)]

Copy and paste the content from GitHub into the respective files and familiarize yourself with the content of the individual files.

- `/weather_app/view/`[`App.view.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/App.view.xml)
- `/weather_app/view/`[`Details.controller.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/Details.controller.js)
- `/weather_app/view/`[`Details.view.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/Details.view.xml)
- `/weather_app/view/`[`List.controller.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/List.controller.js)
- `/weather_app/view/`[`List.view.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/List.view.xml)
- `/weather_app/`[`Component.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/Component.js)
- `/weather_app`[`/util/formatter.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/master/src/main/webapp/weather_app/util/formatter.js)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Replace index file contents)]

Replace the content of the `index.html` file to match this [version in GitHub](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/index.html).


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy your app)]

Publish/deploy your updated app and navigate your browser to the root URL: `http://localhost:8080/weatherapp`. After successful authentication you should see a fully operational UI.

![App example showing weather info for a specific city](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part9/e2e_09-6.png)

>Note: Feel free beautify the UI by applying formatters and respective icons etc.


[ACCORDION-END]




## Next Steps
- [End-to-End Weather App Scenario Part 10](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part10.html)
