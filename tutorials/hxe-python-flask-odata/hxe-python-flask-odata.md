---
title: Create a Tiny World Python Application using the Flask framework, to consume OData services to connect to SAP HANA, Express Edition.
description: Create a sample Python application using the Flask Web framework, which connects to SAP HANA, Express Edition using OData Services
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, topic>big-data, topic>cloud, products>sap-hana, products>sap-hana\,-express-edition, tutorial>how-to  ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - You have an instance of SAP HANA, Express Edition deployed and running.
 - You have an OData service deployed on the instance of SAP HANA, Express Edition as per the prerequisite OData tutorial.
 - **Tutorials:** [SAP HANA, express edition, in Google Cloud Platform Launcher](https://developers.sap.com/tutorials/hxe-gcp-getting-started-launcher.html)


## Next Steps
 - Go to [SAP HANA, express edition](https://developers.sap.com/topics/sap-hana-express.tutorials.html) tutorials page

## Details
### You will learn  
This tutorial will guide you through the process of creating a sample Python/Flask application to connect to SAP HANA, Express Edition, using OData services. This tutorial assumes that you have already created an OData service as mentioned in the prerequisite tutorial.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create a sample python application on your local machine)]
>**Note:**
> The tutorial assumes you have a local Unix machine. Please use equivalent commands if running a Windows Environment.


Open terminal and create a directory.
```
    ~$ mkdir TinyWorldFlaskApp
    ~$ cd TinyWorldFlaskApp/

```
Run the following commands.

```
    ~TinyWorldFlaskApp$ touch main.py
    ~TinyWorldFlaskApp$ touch requirements.txt
    ~TinyWorldFlaskApp$ mkdir templates
    ~TinyWorldFlaskApp$ mkdir static
    ~TinyWorldFlaskApp$ cd templates
    ~TinyWorldFlaskApp/templates$ touch odata.html
    ~$ cd ..

```
Copy the below code to the file `main.py` in this directory and edit the code to change IP Address, OData Service Name and Credentials:

```

        from flask import Flask, render_template, request
        from pyslet.odata2.client import Client
        from pyslet.http.auth import BasicCredentials
        from pyslet.http.client import Client as http
        from pyslet.http.client import ClientRequest

        app = Flask(__name__)

        @app.route('/odata')
        def hello():
            products = testClient()
            return render_template('odata.html', products=products)

        # Edit the below code to use the IP address, URL, table name, column names as per the table exposed in the Odata service
        # This example assumes you have the table as mentioned in the prerequisite OData tutorial.
        def testClient():
            c = MyAuthenticatedClient('http://<YOUR_HOST_NAME>:8090/ODataTutorial/product.xsodata')
            products = c.feeds['Product'].OpenCollection()
            productNames = []
            for p in products.iteritems():
                c1 = p['PRODUCTID'].value
                c2 = p['CATEGORYID'].value
                c3 = p['LINEID'].value
                c4 = p['PRODUCTNAME'].value
                c5 = p['PRODUCTDESCRIPTION'].value
                c6 = p['CATEGORYNAME'].value
                c7 = p['CATEGORYDESCRIPTION'].value
                c8 = p['LINENAME'].value
                c9 = p['LINEDESCRIPTION'].value
                c10 = p['DISCONTINUED'].value
                c11 = p['CAPACITY'].value
                c12 = p['SCREENSIZE'].value
                productNames.append((c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12))

            return productNames

        #Overridden Class to handle HTTP authentication
        class MyAuthenticatedClient(Client):

            def process_request(self, request):
                # Edit the code below to add required credentials to access the OData service.
                if (self.credentials is not None and not request.has_header("Authorization")):
                    credentials = BasicCredentials()
                    credentials.userid = "<YOUR_USERNAME>"
                    credentials.password = "<YOUR_PASSWORD>"
                    request.set_header('Authorization', str(credentials))
                    super(MyAuthenticatedClient, self).process_request(request)

        if __name__ == '__main__':
          app.run(host='0.0.0.0', port=80)
```

Copy the below code to the  `requirements.txt` file in the directory.

```
        Flask==0.12.1
        pyslet
```        

Copy the following code to the file `odata.html` under the `templates` directory.

```
  Hello World From HANA Express through OData!
  </br>
  </br>

  <!-- Edit the below code to use the column names as per the table exposed in the Odata service
   This example assumes you have the table as mentioned in the prerequisite OData tutorial. -->

  <table style="width:100%">
    <tr>
      <th>PRODUCT ID</th>
      <th>CATEGORY ID</th>
      <th>LINE ID</th>
      <th>PRODUCT NAME</th>
      <th>PRODUCT DESCRIPTION</th>
      <th>CATEGORY NAME</th>
      <th>CATEGORY DESCRIPTION</th>
      <th>LINE NAME</th>
      <th>LINE DESCRIPTION</th>
      <th>DISCONTINUED</th>
      <th>CAPACITY</th>
      <th>SCREEN SIZE</th>
    </tr>
    {% for p in products:%}
        <tr>
          <td> {{ p[0] }}</td>
          <td> {{ p[1] }}</td>
          <td> {{ p[2] }}</td>
          <td> {{ p[3] }}</td>
          <td> {{ p[4] }}</td>
          <td> {{ p[5] }}</td>
          <td> {{ p[6] }}</td>
          <td> {{ p[7] }}</td>
          <td> {{ p[8] }}</td>
          <td> {{ p[9] }}</td>
          <td> {{ p[10] }}</td>
          <td> {{ p[11] }}</td>
        </tr>
    {% endfor %}
  </table>

```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run the application locally to test.)]

Run the following commands at the root of the project directory.

```
    ~TinyWorldFlaskApp$ pip install virtualenv
    ~TinyWorldFlaskApp$ virtualenv env
    ~TinyWorldFlaskApp$ source env/bin/activate
    ~TinyWorldFlaskApp$ pip install -r requirements.txt
    ~TinyWorldFlaskApp$ python main.py
```

Browse the application at `http://localhost:8080/odata`.
The results would look like below.

![Output of OData service](1.png)

You now have a working Python/Flask application connecting to SAP HANA, Express Edition using OData services.


[ACCORDION-END]

---
