---
title: Internet of Things (IoT) Posting data with a REST Client
description: Part 3 of 10, Use a standard HTTP Client to post data to your SAP HANA tables
tags: [products>sap-hana, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Internet of Things (IoT) Setup SAP HANA XS (On-premise or stand-alone server)](http://www.sap.com/developer/tutorials/iot-part2-hanaxs-setup.html)


## Next Steps
 - [Internet of Things (IoT) Check your data](http://www.sap.com/developer/tutorials/iot-part4-checking-data.html)

## Details
### You will learn  
Now that your foundation in place it's time to do a test of posting our data and then to try it with our device. To test you will be using the [Chrome Postman extension](https://chrome.google.com/webstore/detail/postman-rest-client/fdmmgilgnpjigdojojpjoooidkmcomcm?hl=en).

### Time to Complete
**10 Min**.

---
1. If you haven't done so, please install Postman now.

2. You will now use Postman to do a quick test. Open Postman and enter the parameters below. Be sure to modify the URL with the correct IP address and name you gave your schema.

     Field                    | Content
     ------------------------ | -------------
     Request Type             | `POST`
     URL to `xsodata` service | `http://52.90.177.151/CODEJAMMER/johndoe/myiot/mydata.xsodata/DATA`

3. Click on the **Headers** tab and enter `Content-Type` for the header, and `application/json` for its value.

     ![Header definition](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part3-posting-data-hana/p3_3.png)

4. Click on the **Authorization** tab, select **Basic Auth**, enter the values below and click **Update Request**.

     ![User Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part3-posting-data-hana/p3_4.png)

5. Click on the **Body** tab, select the **raw** radio button and `JSON (application/json)` from the pulldown menu. Enter the text below as the body content.

     `{"ID":"0", "TEMPERATURE":"22.09", "HUMIDITY":"45.47", "BRIGHTNESS":"27"}`

     ![JSON value](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part3-posting-data-hana/p3_5.png)
6. Click **Send**. If it is successful, you will get a **201 Created** response back indicating a successful entry, and you should skip to step 8. If you get an error, see step 7.

     ![JSON Result](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part3-posting-data-hana/p3_6.png)
 ￼
7. If you get an error: **Request execution failed due to missing or invalid XSRF token**,  this is a cross site scripting errors. To correct that, you need to modify the `.xsaccess` and change `prevent_xsrf: true` to `prevent_xsrf: false` and save. You might also notice just a **403** error and if you are sure everything else is correct then check this as well.

     ![.xsaccess definition](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part3-posting-data-hana/p3_7.png)


     Send the POST request again, and you should get a 201 response.
￼

8. You can modify the values in the POST body and send a few more requests to add a bit more data if you'd like (but it is not required).

## Next Steps
 - [Internet of Things (IoT) Check your data](http://www.sap.com/developer/tutorials/iot-part4-checking-data.html)
