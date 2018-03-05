---
title: Deploy an application built to connect to SAP HANA Express Edition, on App engine of the Google Cloud Platform (GCP)
description: Deploy an application built to connect to SAP HANA Express Edition, on App engine of the Google Cloud Platform (GCP)
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, topic>big-data, topic>cloud, products>sap-hana, products>sap-hana\,-express-edition  ]
---

## Prerequisites and Assumptions
 - **Proficiency:** Beginner
 - You have a working application to connect to SAP HANA, Express Edition.
 - You have a Google Account and/or have registered for the free trial on `cloud.google.com`.
 - You have a project created on the Google Cloud Platform and billing is enabled.


## Next Steps
 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page

## Details
### You will learn  
This tutorial will guide you through the process to deploy a sample application to Google Cloud Platform (GCP).

### Time to Complete
**30 Min**

---

[ACCORDION-BEGIN [Step 1: ](Prepare your application locally)]

1. Run the following commands.

  ```
      mkdir ruby-odata
      cd ruby-odata/
      touch config.ru
      touch app.rb
      touch Gemfile
      mkdir views
      touch views/products.slim
  ```
2. Copy following code to file `app.rb`

  ```
      #!/usr/bin/env ruby
      require 'ruby_odata'
      require 'sinatra'
      require 'slim'

      set :bind, '0.0.0.0'
      $headers = ['PRODUCTID', 'CATEGORYID', 'LINEID', 'PRODUCTNAME', 'PRODUCTDESCRIPTION', 'CATEGORYNAME', 'CATEGORYDESCRIPTION', 'LINENAME', 'LINEDESCRIPTION', 'DISCONTINUED', 'CAPACITY', 'SCREENSIZE']

      def retrieveProductData
        src = OData::Service.new "http://<YOUR_HOSTNAME>:8090/ODataTutorial/product.xsodata", { :username => "<YOUR_USERNAME>", :password=> "<YOUR_PASSWORD>" }

        # Querying, pull all products  from service 'src'
        src.products
        products = src.execute
        # Generate an 2d array that contains products data
        productData = products.map do |p|
          # 'p.send h' calls method p."h", e.g. p.PRODUCTID when h == "PRODUCTID"
          $headers.collect {|h| p.send h}
        end
        return productData
      end

      get '/' do
        @data = retrieveProductData
        @headers = $headers
        slim :products
      end
  ```

3. Copy following code to file `views/products.slim`

    ```
      doctype html
      html
        head
          title HANA Express OData Service Example
        body
          h1 Show data from Table 'Products'

          table border="1px"
            thead
              tr
              - @headers.each do |header|
                th = header
            tbody
            - @data.each do |row|
              tr
              - row.each do |cell|
                td = cell
    ```
4. Copy the below code to the  `Gemfile` file in the directory.
  ```
        source "https://rubygems.org"

        gem "sinatra"
        gem "slim"
        gem "ruby_odata"
  ```
5. Run the following commands at the root of the project directory.

      ```
      ~ruby-odata$bundle install
      # This command should generate 'Gemfile.lock' file, which is needed in deploying
      ~ruby-odata$ruby app.rb
      (dev_appserver.py does not support Ruby currently)

      ```
6. Browse the application at `http://localhost:4567`.
     The results should look like below.

    ![Output of OData service](2.png)


[ACCORDION-END]
---

## Next Steps
 - Go to [SAP HANA, express edition](https://www.sap.com/developer/topics/sap-hana-express.tutorials.html) tutorials page
