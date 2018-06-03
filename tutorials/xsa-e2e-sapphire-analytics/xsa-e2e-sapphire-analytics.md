---
title: Create advanced analytic models and implement data protection in SAP HANA
description: Anonymize data to protect privacy and use text analytics to gain insights about SAPPHIRE NOW.
primary_tag: topic>big-data
tags: [  tutorial>beginner, topic>big-data, topic>cloud, products>sap-hana, products>sap-hana\,-express-edition, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
You will learn how to create different analytic models using text analytics based on data inputs from attendees at SAPPHIRE NOW. You will also learn one of the methods to implement data `anonymization`.

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Contribute to the pool of Big Data)]

In Google Chrome, you will find a tab with a Google sheet. If it is not already open, open it using the bookmark in the favorite bar.

![Open Google spreadsheet](1.png)

Locate the last available row and add your answers to the questions below. Your application will use these data.

![Open Google spreadsheet](2.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Log in to SAP Web IDE for SAP HANA)]

Open a new incognito window in Google Chrome.

![Open a new incognito window](incognito.png)

Use the `SAP HANA` button on the bookmarks to access SAP Web IDE for SAP HANA.

![Open a new incognito window](3.png)

If you have not logged in already, use user **`XSA_DEV`** and password **`HXEHana2`**.

![Log in](password.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a Project)]

If you see any existing folders, use the right-click menu to delete them.

![Delete any existing projects](30.png)

Right-click **Workspace** and choose **New > Project from template**.

![Create a new project](4.png)

Click **Next**.

![Create a new project](5.png)

Call your project `SAPPHIRE` and click **Next**.

![Create a new project](6.png)

Click **Finish** to create your project.

![Create a new project](7.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a database module)]

**Right-click** on the project and select **New > SAP HANA Database Module**.

![New DB module](8.png)

Call the module `hana` and click **Next**.

![New DB module](9.png)

Flag **Build module after creation** and click **Finish**.

![New DB module](10.png)

Once the module has been created, expand it, right-click the `src` folder and choose **Import > File or Project**.

![New DB module](11.png)

Use **Browse** to navigate to `Desktop\1-HANA Data Import` and choose the file called `src (analytics).zip`.

![New DB module](12.png)

Delete `src (analytics)` from the path and click **OK**.

![New DB module](13.png)

In the next dialog, confirm overwriting the files.

![New DB module](confirm.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Load data into the tables)]

Open the Google sheet again and copy the data into the clipboard.

![New DB module](15.png)

Go back into SAP Web IDE for SAP HANA and double-click the file `responses.csv` to open it. Then use **Ctrl+V** to paste the contents of the survey.

![New DB module](16.png)

Save the file.

Right-click the `hana` module and choose **Build** from the contextual menu.

![New DB module](17.png)

You can close the console and `responses.csv` file to make more room to work on the models.

![New DB module](close.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create a Text Analysis model)]

Right-click the folder `src` and choose **New > Calculation View**.

![New TA CV](cv.png)

Click **Aggregation** node and then the **+** sign to add a new data source.

![New TA CV](19png.png)

Enter `respon` to find the set of responses you loaded. Choose the result starting with `$TA` and click **Finish**.

![New TA CV](20.png)

Expand the detail panel and double-click the column `TA_TOKEN` to transfer it to the results. Click `Calculated Columns`.

![New TA CV](21.png)

Use the **+** icon to create a new counter.

![New TA CV](22.png)

Click the counter to edit the details.

![New TA CV](counter.png)

Choose column `TA_TOKEN` for the counter.

![New TA CV](23.png)

Save and build the new view.

![New TA CV](24.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Preview the Text Analysis model)]

Once the build is finished, right-click again and choose **Data Preview**.

![Data preview](25.png)

Drag and drop `TA_TOKEN` to both the `Label` and the `Value` axis. Choose the donut to visualize your results.

![Data preview](26.png)

> ### Congratulations!
> You have a preview of the most used words so far.
> These models can be consumed by different visualization tools, such as SAP `Lumira` or SAP Analytics Cloud, to produce advanced visualizations.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Anonymize the data)]

Create a new calculation view by right-clicking on the `src` folder and choosing **New > Calculation View**.

![New view](27.png)

Call it `ANONYMIZE` and click **Create**.

![New view](28.png)

Click the `anonymize` data node and then click the white modelling area to drop it.

![New view](29.png)

Use the **+** sign to incorporate data into the model.

![New view](31.png)

Type `respon` and choose the result starting with `SAPPHIRE` and ending with `RESPONSE_DATA`.

![New view](32.png)

Click the **+** sign to

Use the button to expand details and double-click the data source to transfer the field to the results.

![New view](33.png)

Then click **Details**. Configure the anonymity algorithm as follows:

- `Sequence Column`: **ID**
- `k`: **2**
- `Quasi Columns`: Use the **+** sign to choose column `ORIGIN_COUNTRY`

![New view](34.png)

Use the arrow on the anonymity column to configure the values.

![New view](35.png)

Type in a higher level of `anonymization` for the countries. You do not need to type in all of the values.

Click **Back** when you are finished.

![New view](36.png)

Drag and drop the arrow on the `anonymization` node into the aggregation node.

![New view](drag.gif)

Click the aggregation node and double-click the root data source to transfer the columns to the output.

![New view](37.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Preview the anonymization)]

Save and build the calculation view.

![Data preview](38.png)

Once the build has finished, right-click and choose **Data Preview**.

![Data preview](39.png)

Click **Raw data** and you will see the values for the countries have been replaced


> ### Why is this useful?
> `Anonymization` of data helps companies protect privacy and comply with data protection regulations, such as GDPR, together with many other measures to mask data that could potentially identify an individual.
>&nbsp;
> In this example, there are single combinations of job roles in specific countries. In other words, if the lucky number was sensitive information such as the salary and somebody knew who filled in the spreadsheet, they could deduce an individual's salary because of the unique combination of job role and country.


[ACCORDION-END]

---
