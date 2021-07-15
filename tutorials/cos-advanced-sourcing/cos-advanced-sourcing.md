---
title: Create Personalized Sourcing Strategies Depending on Location and Priorities
description: Use SAP Customer Order Sourcing to build personalized sourcing strategies for different locations with different and changing needs.
auto_validation: true
time: 30
tags: [ tutorial>beginner, topic>sap-api-business-hub, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: topic>cloud
---

## Prerequisites
 - You have completed the tutorial [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial).
 - You have registered for a trial account on [SAP Business Technology Platform](https://cloudplatform.sap.com/index.html).
 - You are familiar with the SAP Business Technology Platform trial landscape [Get Started with SAP Business Technology Platform Trial](cp-trial-quick-onboarding).
 - You have a subaccount in the **Cloud Foundry** environment, with **Amazon Web Services (AWS)** as provider and the **Europe (Frankfurt)** region.


## Details
### You will learn
  - How to check availability raw data already included in your service
  - How to create source lists
  - How to create custom goals from custom attributes
  - How to create and change sourcing strategies
  - How different goals affect the sourcing algorithm

---

[ACCORDION-BEGIN [Step 1: ](Get to know the business scenario)]
You are the owner of an e-commerce business or an online shop in the fashion industry with the following characteristics:

**Sources**

You own several stores and distribution centers in Germany (Distribution centers in Berlin, Frankfurt and Munich and a store in Hamburg), the US (Distribution centers in Atlanta, Chicago, Houston, Philadelphia, Detroit, Orlando, San Jose and stores New York and Los Angeles) and Canada (Distribution center in Toronto).

**Products**

You have several products available:

|                | Grey Sports Shoe                    | White Sports Shoe                        | Black Sports Trouser     |
| :------------- | :-------------                      | :-------------                           | :-------------           |
| Product Id     | `grey_sports_shoe`                  | `white_sports_shoe`                      | `black_sports_trouser`   |
| Product Image  | ![Grey Shoe](grey_sports_shoe.png)  |  ![White Shoe](white_sports_shoe.png)    |  ![Black Trouser](black_sports_trouser.png)|


**Business Goal**:

  As a business owner, you want to create different sourcing strategies for different customers:

  - Customers in the US want their products as fast as possible, which means in a country as big as the United States that products should be sourced from the source with the **shortest distance to the destination**.

  - For Canada, there is only one distribution center available, no store or third party organization. Customers should get their products not only from Canada, but also from the US or Germany. Products should be sourced from the source with the **highest product availability**.

  - In Germany the sourcing strategy changes:
      - At first the goal was to serve the Customers from the source with the **shortest distance to the destination**.
      - As the distribution center in Berlin got renovated and now is the biggest one in Germany it is the number one priority source. To achieve this, the goal **sources according to priority** will be added on top of the already existing goal for Germany.
  - Customer who are not located in Germany, Canada or the US should be served by the DC or store with the **highest capacity**.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Check which sources are already included in your trial)]
You check the master data from the SAP API Business Hub.

1. Select the environment you have created in the previous tutorial: [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial)

2. On the left side choose **Sources**.

3. Open `GET/sources` and choose **Try Out**.

4. You don't have to enter anything, just choose **Execute**.

[VALIDATE_8]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Check which availability raw data is already included in your trial)]
You check the master data from the SAP API Business Hub.

1. Select the environment you have created in the previous tutorial: [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial)

2. On the left side choose **Availability Raw Data**.

3. Open `GET/availabilityRawData` and choose **Try Out**.

4. For the product id, enter `black_sports_shoe`.

5. Choose **Execute**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Upload sources)]

You upload any master data from the SAP API Business Hub. Since there are only sources in Germany included in the trial, you need to add sources in the US and in Canada:

1. Open [SAP Customer Order Sourcing on SAP API Business Hub](https://api.sap.com/api/Sourcing_API/resource), and log in with your email address.

2. Select the environment you created in the [previous tutorial](cos-getting-started-trial), named `trial_test`.

3. On the left side, choose **Sources**.

4. Open `POST/sources` and choose **Try Out**.

5. Copy the following source master data and paste it into the body:

    [Sources&#46;json](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cos-advanced-sourcing/Sources.json)


6. Click **Execute**.

[VALIDATE_2]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Upload availability raw data)]

You upload any master data from the SAP API Business Hub. Since you have now all the needed sources included in your SAP Customer Order Sourcing trial, you need to upload availability raw data.

1. Open [SAP Customer Order Sourcing on SAP API Business Hub](https://api.sap.com/api/Sourcing_API/resource).

2. Select the environment you have created in the [previous tutorial](cos-getting-started-trial), named `trial_test`.

3. On the left side, choose **Availability Raw Data**.

4. Open `POST/availabilityRawData` and choose **Try Out**.

5. Copy the following availability raw data and paste it into the body:

    [Availability&#46;json](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cos-advanced-sourcing/Availability_raw_data.json)


6. Click **Execute**.

 As you might have noticed with the sources you just uploaded, there was a attribute called **Capacity** included.

This is a custom numeric attribute, you can extend your service with. To make it easier for you, there is an custom attribute, named capacity, already included in the trial and capacities are provided for the sources you got when signing up for the trial.

If you want to create your own custom attribute you can do that with the `POST/attributes` endpoint on the [SAP API Business Hub](https://api.sap.com/api/Sourcing_API/resource).

For more information on how to create Custom Attributes, see [Help Portal documentation on Custom Attributes](https://help.sap.com/viewer/0e899d665ec840908ddb3eba9a6c25dc/Cloud/en-US).

[DONE]
[ACCORDION-END]   

[ACCORDION-BEGIN [Step 6: ](Create sourcing strategy for the US)]

If you want to learn how to create a sourcing strategy in a short and effective way, here's a short [video](https://sapvideoa35699dc5.hana.ondemand.com/?entry_id=1_b510ofkl) for you to watch:

<iframe id="kaltura_player" src="https://www.kaltura.com/p/1921661/sp/192166100/embedIframeJs/uiconf_id/35919811/partner_id/1921661?iframeembed=true&playerId=kaltura_player&entry_id=1_b510ofkl&flashvars[leadWithHTML5]=true&flashvars[sideBarContainer.plugin]=true&flashvars[sideBarContainer.position]=left&flashvars[sideBarContainer.clickToClose]=true&flashvars[chapters.plugin]=true&flashvars[chapters.layout]=vertical&flashvars[chapters.thumbnailRotator]=false&flashvars[streamSelector.plugin]=true&flashvars[EmbedPlayer.SpinnerTarget]=videoHolder&flashvars[dualScreen.plugin]=true&flashvars[hotspots.plugin]=true" width="608" height="402" allowfullscreen webkitallowfullscreen mozAllowFullScreen frameborder="0"></iframe>

1. In the Strategy Builder app, create a new strategy. For the strategy ID, enter `UnitedStates`. This is important for the sourcing to work properly.

2. Open your newly created strategy.

3. To make things easier create a source list for the US, which includes the following sources: Atlanta, Chicago, Houston, Philadelphia, Detroit, Orlando, San Jose, Store New York, Store Los Angeles.

4. On the Building Block, **Source Lists**, click **Manage**.

5. In the **Manage Source Lists** view, create a new source list by clicking **+**.

6. For the ID, enter `US_Sources` and as the name enter `Sources in the US`.

7. In the sources section, click **+** to add sources to your source list.

8. Search for and add the sources listed above to your list.

9. Save your source list.

    ![Sources USA](SourceList_USA.png)

10. Go back to your strategy creation screen.

11. Under **Templates**, drag and drop the template **Products from Several Sources** onto the canvas for modeling your strategy, under the shopping cart.

12. Drag and drop the source list **Sources in the US** into the first box.

13. Drag and drop the goal **Shortest Distance to Destination** into the second box.

14. Save your strategy.

    ![Strategy USA](SourcingStrategy_USA.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create sourcing strategy for Canada)]

1. In the Strategy Builder app, create a new strategy. For the strategy ID, enter `Canada`. This is important for the sourcing to work properly.

2. Open your newly created strategy.

3. To make things easier create a source list for Canada, which includes the following sources: Toronto, Atlanta, Chicago, Houston, Philadelphia, Detroit, Orlando, San Jose, Store New York, Store Los Angeles, Berlin, Frankfurt, Munich, Hamburg Store.  

4. On the Building Block, Source Lists, click **Manage**.

5. In the **Manage Source Lists** view, create a new source list by clicking **+**.

6. For the ID, enter `All_Sources` and as name enter `Sources for Canada or worldwide`.

7. In the sources section, click **+** to add sources to your source list.

8. Search for and add the sources listed above to your list.

9. Save your source list.

    ![Sources USA](SourceList_Canada_Worldwide.png)

10. Go back to your strategy creation screen.

11. Under Templates, drag and drop the template **Products from Several Sources** onto the canvas for modeling your strategy, under the shopping cart.

12. Drag and drop the source list **Sources for Canada or worldwide** into the first box.

13. Drag and drop the goal **Highest Availability** into the second box.

14. Save your strategy.

    ![Strategy Canada](SourcingStrategy_Canada.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create first sourcing strategy for Germany)]

In Germany, the sourcing strategy will change. This change will take place after you have ordered for the first time, which means you looked at the sourcing result for this strategy.


1. In the Strategy Builder app create a new strategy. For the strategy ID, enter ``Germany``. This is important for the Sourcing to work properly.

2. Open your newly created strategy.

3. Under Templates, drag and drop the template **Products from Several Sources** onto the canvas for modeling your strategy, under the shopping cart.

4. Drag and drop the source list **DCs in Germany** into the first box. Additionally to that add the source **Berlin** as well. Make sure, that Berlin is at the first position in the box.

5. Drag and drop the goal **Shortest Distance to Destination** into the second box.

6. Save your strategy.

    ![First Strategy Germany](SourcingStrategy_Germany1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create sourcing strategy for customers outside Germany, Canada and the US)]

1. In the Strategy Builder app, create a new strategy. For the strategy ID, enter `Worldwide`. This is important for the sourcing to work properly.

2. Open your newly created strategy.

3. Create a custom goal, which will be used in this strategy:

4. Go to **Goals > Manage** to open the **Manage Goals** screen.

5. Create a new goal by clicking **+** in the top-right corner.

6. For **Name**, enter **`Highest Capacity`**.

7. For **Numeric Attribute**, choose **`Capacity`**.

8. For **Comparator**, choose **`Highest Value`**.

9. Save your goal.

    ![Capacity](Rule_Capacity.png)

10. Go back to your strategy creation screen.

11. Under Templates, drag and drop the template **Products from Several Sources** onto the canvas for modeling your strategy, under the shopping cart.

12. Drag and drop source list **Sources for Canada or worldwide** into the first box.

13. Drag and drop the goal **Highest Capacity** into the second box.

14. Save your strategy.

    ![Strategy Worldwide](SourcingStrategy_World.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Test your US strategy)]

**Scenario:** A customer located in the city of New York orders a `grey_sports_shoe`.

Let's check where the products are sourced from:

1. Open SAP Customer Order Sourcing on the SAP API Business Hub in a new window.

2. Select the environment you have created in the previous tutorial, probably named `trial_test`.

3. On the left side, choose **Sourcing**.

4. In `POST/sourcing` choose **Try out**.

5. Post the following sourcing request:

```JSON
{
  "strategyId": "UnitedStates",
  "items": [
      {
          "productId": "grey_sports_shoe",
          "quantity": 1
      }
  ],
  "destinationCoordinates": {
      "latitude": 40.8240,
      "longitude": -73.9448
  }
}
```

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Test your Strategy for Canada)]

**Scenario:** A small retailer in Vancouver wants to sell your products in his shop and therefore orders 50 `grey_sports_shoe`, 50 `white_sports_shoe` and 60 `black_sports_trouser`.

Let's check where the products are sourced from:

1. Open SAP Customer Order Sourcing on the SAP API Business Hub in a new window.

2. Select the environment you have created in the previous tutorial, probably named `trial_test`.

3. On the left-hand side, choose **Sourcing**.

4. In `POST/sourcing` choose **Try out**.

5. Post the following sourcing request:

```JSON
{
    "strategyId": "Canada",
    "items": [
        {
            "productId": "grey_sports_shoe",
            "quantity": 50
        },
        {
            "productId": "white_sports_shoe",
            "quantity": 50
        },
        {
            "productId": "black_sports_trouser",
            "quantity": 60
        }
    ],
    "destinationCoordinates": {
        "latitude": 49.2827,
        "longitude": -123.1207
    }
}
```


[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Test your Strategy for Germany )]

**Scenario:** A customer located in Berlin orders a `grey_sports_shoe` and a `black_sports_trouser`. One month later he orders the same again. The sourcing strategy for Germany changed before his second order.

Let's check where the products are sourced from:

1. Open SAP Customer Order Sourcing on the SAP API Business Hub in a new window.

2. Select the environment you have created in the previous tutorial, probably named `trial_test`.

3. On the left-hand side, choose **Sourcing**.

4. In `POST/sourcing` choose **Try out**.

5. Post the following sourcing request:

```JSON
{
    "strategyId": "Germany",
    "items": [
        {
            "productId": "grey_sports_shoe",
            "quantity": 1
        },
        {
            "productId": "black_sports_trouser",
            "quantity": 1
        }
    ],
    "destinationCoordinates": {
        "latitude": 52.5200,
        "longitude": 13.4050
    }
}

```

[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Change your Strategy for Germany and test again)]

1. In the Strategy Builder open the strategy with the Strategy ID **Germany**.

2. Drag and drop the goal **Sources according to Priority** above the goal **Shortest Distance to Destination** into the second box.

3. Save your strategy.

    ![Second Strategy Germany](SourcingStrategy_Germany2.png)

Let's check where the products are sourced from for the second order:

1. Open SAP Customer Order Sourcing on the SAP API Business Hub in a new window.

2. Select the environment you have created in the previous tutorial, probably named `trial_test`.

3. On the left-hand side, choose **Sourcing**.

4. In `POST/sourcing` choose **Try out**.

5. Post the following sourcing request:

```JSON
{
    "strategyId": "Germany",
    "items": [
        {
            "productId": "grey_sports_shoe",
            "quantity": 1
        },
        {
            "productId": "black_sports_trouser",
            "quantity": 1
        }
    ],
    "destinationCoordinates": {
        "latitude": 52.5200,
        "longitude": 13.4050
    }
}

```

[VALIDATE_6]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Test your Strategy for Customers not located in Germany, Canada or the US)]

**Scenario:** A customer located in Rome orders a `grey_sports_shoe` and two `black_sports_trousers`.

Let's check where the products are sourced from:

1. Open SAP Customer Order Sourcing on SAP API Business Hub in a new window.

2. Select the environment you have created in the previous tutorial, probably named `trial_test`.

3. On the left-hand side, choose Sourcing.

4. In `POST/sourcing` choose Try out.

5. Post the following sourcing request:

```JSON
{
    "strategyId": "Worldwide",
    "items": [
        {
            "productId": "grey_sports_shoe",
            "quantity": 1
        },
        {
            "productId": "black_sports_trouser",
            "quantity": 2
        }
    ],
    "destinationCoordinates": {
        "latitude": 41.9028,
        "longitude": 12.4964
    }
}
```


[VALIDATE_7]
[ACCORDION-END]

Congratulations! You have successfully completed this tutorial.

### Additional Information

- [Official Product Documentation](https://help.sap.com/viewer/product/SAP_CUSTOMER_ORDER_SOURCING/Cloud/en-US)
- [Trial Documentation](https://help.sap.com/viewer/DRAFT/cd03af1a94a440f1b5dbc0dc50a0989b/Cloud/en-US)
- [How do the Goals work?](https://help.sap.com/viewer/a8094e21e0ed43b39ad79ade28eefabb/Cloud/en-US/a96a37284b5142ee968e9c9392304920.html#loiob368d40ba4a7438b857d394ab6e6c7a6)
- [API Reference Documentation](https://help.sap.com/viewer/59d653d22328437c9e0817340181b896/Cloud/en-US)
- [SAP Customer Order Sourcing on the SAP API Business Hub](https://api.sap.com/package/CustomerOrderSourcing?section=Artifacts)
- [Additional information on how to configure Environments on the SAP API Business Hub](https://help.sap.com/viewer/84b35b9c39b247e3ba2a31f02beee46d/Cloud/en-US/f7796baaef6a48e9867298827f5028ff.html)
- [**Video:** SAP Customer Order Sourcing in a nutshell](https://youtu.be/novFLk35X2I)
- [**Video:** How to create a Sourcing Strategy](https://sapvideoa35699dc5.hana.ondemand.com/?entry_id=1_b510ofkl)

---
