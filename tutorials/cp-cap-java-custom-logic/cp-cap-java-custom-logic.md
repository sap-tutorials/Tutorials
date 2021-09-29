---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Extend the Bookstore with Custom Code
description: Extend the previously built bookstore with custom coding, for example, to validate requests.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-business-technology-platform, topic>java, products>sap-business-application-studio]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Details
### You will learn
  - How to use the CAP Java SDK
  - How to use the debugger in SAP Business Application Studio

In the previous tutorial, you have built the data model and exposed the services of your bookstore application. In this tutorial, you'll extend the bookstore with custom code to calculate the `total` and `netAmount` elements of the `Orders` and `OrderItems` entity. In addition, when creating an order the available stock of a book will be checked and decreased.

---

[ACCORDION-BEGIN [Step 1: ](Define custom handler for OrdersService)]

In one of the previous tutorials, you have already seen how to register a [custom event handler](https://cap.cloud.sap/docs/java/provisioning-api#phases) to handle `READ` or `CREATE` events of an entity. We used the `@On` annotation, which replaces the default handling of an event that is provided by the CAP Java runtime.

As we want to augment the default handling now, we'll use the `@Before` and `@After` annotations. Event handlers registered using the [`@Before`](https://cap.cloud.sap/docs/java/srv-impl#before) annotation are meant to perform validation of the input entity data. This makes it possible to validate the available stock of a particular book before creating an order. In contrast event handlers registered using the [`@After`](https://cap.cloud.sap/docs/java/srv-impl#after) annotation can post-process returned entities. This is useful for calculating the `total` and `netAmount` elements after reading orders or their items from the database.

First of all, a new Java class for your event handler methods needs to be defined:

1. From the terminal, stop your application if it's still running using **`CTRL+C`**.

2. Go back to the project explorer by choosing the following icon:

    !![project explorer](project-explorer.png)

3. Go to `srv/src/main/java/com/sap/cap/bookstore` and create a new folder called `handlers`.

    !![handlers package](handlers-package.png)

4. In the created package, create the `OrdersService.java` file with the following content and make sure you **Save** the file:

```Java
package com.sap.cap.bookstore.handlers;

import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.ServiceName;

import org.springframework.stereotype.Component;

@Component
@ServiceName("OrdersService")
public class OrdersService implements EventHandler {
//Replace this comment with the code of Step 2 of this tutorial
}
```

!![OrdersService class overview](ordersservice-class.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Decrease stock when posting order)]

You'll now add a method to the `OrdersService` Java class to decrease the stock whenever a new order item is posted.

1. Add the following code to your `OrdersService` Java class and make sure you **Save** the file:

    ```Java
    @Autowired
    PersistenceService db;

    @Before(event = CdsService.EVENT_CREATE, entity = "OrdersService.OrderItems")
    public void validateBookAndDecreaseStock(List<OrderItems> items) {
        for (OrderItems item : items) {
            String bookId = item.getBookId();
            Integer amount = item.getAmount();

            // check if the book that should be ordered is existing
            CqnSelect sel = Select.from(Books_.class).columns(b -> b.stock()).where(b -> b.ID().eq(bookId));
            Books book = db.run(sel).first(Books.class)
                    .orElseThrow(() -> new ServiceException(ErrorStatuses.NOT_FOUND, "Book does not exist"));

            // check if order could be fulfilled
            int stock = book.getStock();
            if (stock < amount) {
                throw new ServiceException(ErrorStatuses.BAD_REQUEST, "Not enough books on stock");
            }

            // update the book with the new stock, means minus the order amount
            book.setStock(stock - amount);
            CqnUpdate update = Update.entity(Books_.class).data(book).where(b -> b.ID().eq(bookId));
            db.run(update);
        }
    }

    @Before(event = CdsService.EVENT_CREATE, entity = "OrdersService.Orders")
    public void validateBookAndDecreaseStockViaOrders(List<Orders> orders) {
        for (Orders order : orders) {
            if (order.getItems() != null) {
                validateBookAndDecreaseStock(order.getItems());
            }
        }
    }
    ```  

2. Add the following import statements to the top of the `OrdersService` Java class and make sure you **Save** the file:

    ```Java
    import java.util.List;
    import org.springframework.beans.factory.annotation.Autowired;

    import com.sap.cds.ql.Select;
    import com.sap.cds.ql.Update;
    import com.sap.cds.ql.cqn.CqnSelect;
    import com.sap.cds.ql.cqn.CqnUpdate;
    import com.sap.cds.services.ErrorStatuses;
    import com.sap.cds.services.ServiceException;
    import com.sap.cds.services.cds.CdsService;
    import com.sap.cds.services.handler.annotations.Before;
    import com.sap.cds.services.persistence.PersistenceService;

    import cds.gen.ordersservice.OrderItems;
    import cds.gen.ordersservice.Orders;
    import cds.gen.sap.capire.bookstore.Books;
    import cds.gen.sap.capire.bookstore.Books_;
    import java.math.BigDecimal;
    import cds.gen.sap.capire.bookstore.OrderItems_;

    import com.sap.cds.services.handler.annotations.After;
    ```

Let's break down what is happening:

- The method `validateBookAndDecreaseStock` is registered using the `@Before` annotation. This means that the method is called before the `OrderItems` entities are persisted. The annotation also specifies, that the method should be called whenever an entity `OrderItems` is created.

- The method has a parameter `items`, which gives access to the list of `OrderItems`. The interface used here is generated by CAP Java. It generates a [POJO interface](https://cap.cloud.sap/docs/java/data#typed-access) for each entity defined in the CDS model.

- The `CqnSelect sel` variable defines a database query to retrieve the book that is referenced by the order item. The query is performed and the returned entity data is accessed using the POJO interface for `Books`.

- After that the available stock of the book is compared against the ordered amount. If enough stock is available the stock is decreased on the book and the book is updated within the database.

- As order items can also be created via a [deep insert](https://cap.cloud.sap/docs/java/query-execution#deep-insert-upsert) on the `Orders` entity, the same validation is triggered by the `validateBookAndDecreaseStockViaOrders` method.

It's important to note, that the CAP Java SDK automatically takes care of combining all database queries and updates in a single transaction. This means, that if the creation of the order item fails for some reason, the stock of the book won't be decreased.

The complete **OrdersService.java** file should now have the following format:

```Java
package com.sap.cap.bookstore.handlers;

import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.ServiceName;

import org.springframework.stereotype.Component;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.ErrorStatuses;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.persistence.PersistenceService;

import cds.gen.ordersservice.OrderItems;
import cds.gen.ordersservice.Orders;
import cds.gen.sap.capire.bookstore.Books;
import cds.gen.sap.capire.bookstore.Books_;
import java.math.BigDecimal;
import cds.gen.sap.capire.bookstore.OrderItems_;

import com.sap.cds.services.handler.annotations.After;

@Component
@ServiceName("OrdersService")
public class OrdersService implements EventHandler {

  @Autowired
  PersistenceService db;

  @Before(event = CdsService.EVENT_CREATE, entity = "OrdersService.OrderItems")
  public void validateBookAndDecreaseStock(List<OrderItems> items) {
      for (OrderItems item : items) {
          String bookId = item.getBookId();
          Integer amount = item.getAmount();

          // check if the book that should be ordered is existing
          CqnSelect sel = Select.from(Books_.class).columns(b -> b.stock()).where(b -> b.ID().eq(bookId));
          Books book = db.run(sel).first(Books.class)
                  .orElseThrow(() -> new ServiceException(ErrorStatuses.NOT_FOUND, "Book does not exist"));

          // check if order could be fulfilled
          int stock = book.getStock();
          if (stock < amount) {
              throw new ServiceException(ErrorStatuses.BAD_REQUEST, "Not enough books on stock");
          }

          // update the book with the new stock, means minus the order amount
          book.setStock(stock - amount);
          CqnUpdate update = Update.entity(Books_.class).data(book).where(b -> b.ID().eq(bookId));
          db.run(update);
      }
  }

  @Before(event = CdsService.EVENT_CREATE, entity = "OrdersService.Orders")
  public void validateBookAndDecreaseStockViaOrders(List<Orders> orders) {
      for (Orders order : orders) {
          if (order.getItems() != null) {
              validateBookAndDecreaseStock(order.getItems());
          }
      }
  }
}
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Test handler)]

1. Go to the terminal in SAP Business Application Studio and stop your application if it's still running by using **`CTRL+C`**.

2. Choose the **Run Configuration** icon on the side panel of SAP Business Application Studio.

    !![select the run configurations item](run-configurations.png)

3. Choose the **Create Configuration** icon (plus sign) and select **`Bookstore`** as your project to run.

4. Click the green arrow to start the application, which appears when you hover over the run configuration.

    !![start run configuration](start-runconfiguration.png)

    You should see the application starting in the **Debug Console**.

5. Test your application, by using Curl from the terminal. To open a new terminal, choose **Terminal** > **New Terminal** from the main menu.

6. Create a new order:

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/OrdersService/Orders \
    -H "Content-Type: application/json" \
    -d '{"ID": "50425a69-48b9-45f1-b6d2-687d55355e03", "currency_code": "USD"}'
    ```

7. Create a new order item:

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/OrdersService/OrderItems \
    -H "Content-Type: application/json" \
    -d '{"parent_ID": "50425a69-48b9-45f1-b6d2-687d55355e03", "book_ID": "abed2f7a-c50e-4bc5-89fd-9a00a54b4b16", "amount": 2}'
    ```

8. From the welcome page, choose **Books** and you'll see that the stock of the book `Wuthering Heights` was decreased to 10.

    You can also add `/odata/v4/BooksService/Books` to the end of your app URL. Remember the app URL is the URL created when you run your application.

9. Repeat the Curl request from step 7 (create new order items), until you get an error that the book ran out of stock.

      Basically, by repeating the Curl request, you're ordering 2 books each time and therefore decreasing the stock by 2.

10. To reset the database to the initial state, run the following command from the root of your project:

    ```Shell/Bash
    cd ~/projects/bookstore && cds deploy --to sqlite
    ```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Calculate netAmount of order item)]

Next, let's add a method to the `OrdersService` Java class to calculate the `netAmount` element of the `OrderItems` entity.

1. Add the following code to the class and make sure you **Save** the file:

    ```Java
        @After(event = { CdsService.EVENT_READ, CdsService.EVENT_CREATE }, entity = "OrdersService.OrderItems")
        public void calculateNetAmount(List<OrderItems> items) {
            for (OrderItems item : items) {
                String bookId = item.getBookId();

                // get the book that was ordered
                CqnSelect sel = Select.from(Books_.class).where(b -> b.ID().eq(bookId));
                Books book = db.run(sel).single(Books.class);

                // calculate and set net amount
                item.setNetAmount(book.getPrice().multiply(new BigDecimal(item.getAmount())));
            }
        }
    ```

Let's break it down again:

- The method `calculateNetAmount` is registered using the `@After` annotation. This means that the method is called after the `OrderItems` entities have been read from the database. The annotation also specified, that the method should be called whenever an entity `OrderItems` is read or created.

- The method has a parameter `items` that gives access to all `OrderItems` entities that were either read or created.

- The `CqnSelect sel` variable defines a database [query](https://cap.cloud.sap/docs/java/query-api), to retrieve the book that is referenced by the order item. The query is [executed](https://cap.cloud.sap/docs/java/query-execution#queries) and the [query result](https://cap.cloud.sap/docs/java/query-execution#result) is accessed using the POJO interface for `Books`.

- In the last line the net amount of the order item is calculated, based on the price of the book and the amount of books ordered.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test handler)]

1. In SAP Business Application Studio, stop your application if it's still running by clicking on the red stop icon in the **Debug** side panel.

    !![stop debugging button](stop-debug.png)

2. Choose the **Run Configuration** icon on the side panel of SAP Business Application Studio.

    !![select the run configurations item](run-configurations.png)

3. Click the green arrow to start the application, which appears when you hover over the run configuration.

    !![start run configuration](start-runconfiguration.png)  

    You should see the application starting in the **Debug Console**.

4.  Test your application, by using Curl from the terminal. To open a new terminal, choose **Terminal** > **New Terminal** from the main menu.

5. Create a new order:

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/OrdersService/Orders \
    -H "Content-Type: application/json" \
    -d '{"ID": "017f4ac4-7f07-4c07-a009-3168317a10cf", "currency_code": "USD"}'
    ```

6. Create a new order item. You can already observe the calculated element in the returned response

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/OrdersService/OrderItems \
    -H "Content-Type: application/json" \
    -d '{"parent_ID": "017f4ac4-7f07-4c07-a009-3168317a10cf", "book_ID": "fd0c5fda-8811-4e20-bcff-3a776abc290a", "amount": 4}'
    ```

7. From the welcome page, choose **`OrderItems`** and you'll see that the `netAmount` element is filled with the calculated value.

    !![order items welcome page](order-item-welcome.png)

    !![calculated net amount](calculated-net-amount.png)

    >You can also add `/odata/v4/OrdersService/OrderItems` to the end of your app URL.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Calculate total amount of order)]

Finally, add a method to the `OrdersService` Java class to calculate the `total` element of the `Orders` entity.

1. Add the following code to the class and make sure you **Save** the file:

    ```Java
        @After(event = { CdsService.EVENT_READ, CdsService.EVENT_CREATE }, entity = "OrdersService.Orders")
        public void calculateTotal(List<Orders> orders) {
            for (Orders order : orders) {
                // calculate net amount for expanded items
                if(order.getItems() != null) {
                    calculateNetAmount(order.getItems());
                }

                // get all items of the order
                CqnSelect selItems = Select.from(OrderItems_.class).where(i -> i.parent().ID().eq(order.getId()));
                List<OrderItems> allItems = db.run(selItems).listOf(OrderItems.class);

                // calculate net amount of all items
                calculateNetAmount(allItems);

                // calculate and set the orders total
                BigDecimal total = new BigDecimal(0);
                for(OrderItems item : allItems) {
                    total = total.add(item.getNetAmount());
                }
                order.setTotal(total);
            }
        }
    ```

2. Also add the following import statements to the top of the `OrdersService` Java class and make sure you **Save** the file:

    ```Java
    import cds.gen.sap.capire.bookstore.OrderItems_;
    ```

Let's break down the code:

- Again, the `@After` annotation is used to register the method and to indicate that the method should be called, whenever the `Orders` entity was read or created.

- For the order items, that may be returned as part of the operation, the net amount is calculated, reusing the `calculateNetAmount` method. Please note that this may only be a subset of all of the order's items.

- For each order, all of the order items are read from the database using the query defined in `CqnSelect selItems`.

- For each order item, the net amount is calculated first by reusing the method `calculateNetAmount`. After that all net amounts are added to the order's total amount.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test the handler)]

1. In SAP Business Application Studio stop your application if it's still running by clicking on the stop icon in the **Debug** side panel.

    !![stop debugging button](stop-debug.png)

2. Choose the **Run Configuration** icon on the side panel of SAP Business Application Studio.

    !![select the run configurations item](run-configurations.png)

3. Click on the green arrow to start the application, which appears when you hover over the run configuration.

    !![start run configuration](start-runconfiguration.png)

    You should see the application starting in the **Debug Console**.

4. Create a new order:

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/OrdersService/Orders \
    -H "Content-Type: application/json" \
    -d '{"ID": "997f4ac4-7f07-4c07-a009-3168317a10aa", "currency_code": "USD"}'
    ```

5. Create a new order item. You can already observe the calculated element in the returned response

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/OrdersService/OrderItems \
    -H "Content-Type: application/json" \
    -d '{"parent_ID": "997f4ac4-7f07-4c07-a009-3168317a10aa", "book_ID": "fd0c5fda-8811-4e20-bcff-3a776abc290a", "amount": 10}'
    ```

6. From the welcome page, choose **Orders**. You'll see that the `total` element is filled with the calculated value.

    !![total calculated](total-calculated.png)

    >You can also add `/odata/v4/OrdersService/Orders` to the end of your app URL.

7. Add `/odata/v4/OrdersService/Orders?$expand=items` to the end of your app URL.

    This expands the `Orders` with its `OrderItems`. You'll see that the `netAmount` element of the `OrderItems` is calculated.

    !![expand items of Order](expand-item.png)

5. Stop your application by clicking on the stop icon in the **Debug** side panel.

    !![stop debugging button](stop-debug.png)

[DONE]
[ACCORDION-END]

Great job!

You've extended the application with quite some custom business logic. In the next tutorial you'll start to make the application ready for SAP Business Technology  Platform, by running it with SAP HANA as the database.
