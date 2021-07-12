---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Create a Reusable Service
description: Create a service that will later on be reused in another CAP Java project.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-business-technology-platform, topic>java]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Details
### You will learn
  - How to write an entity definition
  - How to use some use generic CAP artifacts like aspects and types
  - What associations and compositions are
  - How to deploy to a SQLite database
  - How to make the project reusable

  The previous tutorial was about quickly setting up a working CAP application and read/write some mock data. This tutorial is about learning how to extend the application to a complete products service.

  You'll take advantage of all the out-of-the-box features provided by the CAP Java SDK such as using SQLite as a database for local development. As a result, you'll remove your custom handlers written in the first tutorial. In subsequent tutorials you will swap out SQLite with the SAP HANA service, when preparing your application for the cloud.

---

[ACCORDION-BEGIN [Step 1: ](Define the domain model)]

In the previous tutorial, you defined a service, which defined its own entity. When modeling with CDS the best practice is to separate services from the domain model.

Therefore, you will now define the complete domain model that is used by the products service. The domain model is stored in the `db` folder of your CAP application.

1. Go to your `db` folder and create a file called `schema.cds`.

    !![schema cds file creation](schema-cds.png)

2. Add the following code to your newly created `schema.cds` file and make sure you **Save** the file:

    ```CDS
        namespace sap.capire.products;

        using { Currency, cuid, managed, sap.common.CodeList } from '@sap/cds/common';

        entity Products : cuid, managed {
            title    : localized String(111);
            descr    : localized String(1111);
            stock    : Integer;
            price    : Decimal(9,2);
            currency : Currency;
            category : Association to Categories;
        }

        entity Categories : CodeList {
            key ID   : Integer;
            parent   : Association to Categories;
            children : Composition of many Categories on children.parent = $self;
        }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Understand keywords)]

As you can see, the domain model defines two entities:

- `Products`
- `Categories`

It also imports various common definitions from the `@sap/cds/common` package (a globally available reuse package):

- `Currency`
- `cuid`
- `managed`
- `CodeList`

In addition, the domain model uses the CDS keywords `localized`, `Association` , and `Composition`.
Let's explain these imports and keywords in more detail:

### The `localized` Keyword

The `localized` keyword can be used to mark elements, which require translation. The ability to store translations for different languages and to store a default fallback translation is automatically handled by CDS for you. You will see this in action in more detail in the next tutorial.

### Associations and Compositions

Associations and compositions can be used to define relationships between entities. They often allow you to define these relationships without explicitly working with foreign keys.

While associations define a rather loose coupling between the entities, compositions define a containment relationship. Compositions can also be thought of as defining deep structures. You can perform `deep inserts` and `upserts` along these structures.

In your domain model, the `Categories` entities define a `parent` and `children` element. This enables a hierarchy of categories. The children of a category are modelled as a composition. A category with all of its children defines a deep nested structure. Deleting a category would automatically delete all of its children. However, the parent of a category is modelled as an association. Deleting a category obviously shouldn't delete its parent.

### The `cuid` and `managed` Aspects

Both `cuid` and `managed` are [aspects](https://cap.cloud.sap/docs/cds/cdl#aspects). Aspects extend an entity with additional elements. The [`cuid`](https://cap.cloud.sap/docs/cds/common#aspect-cuid) aspect adds a `key` element `ID` of type `UUID` to the entity.

The [`managed`](https://cap.cloud.sap/docs/cds/common#aspect-managed) aspect adds four additional elements to the entity. These capture the time of the creation and last update of the entity, and the user, which performed the creation and last update.

### The `CodeList` Aspect and the `Currency` Type

[`CodeLists`](https://cap.cloud.sap/docs/cds/common#aspect-sapcommoncodelist) can be used to store global, translatable definitions based on codes, such as currencies, countries, or languages. Especially for UIs, a `CodeList` can be useful to provide a value help for certain input fields.

The [`Currency`](https://cap.cloud.sap/docs/cds/common#type-currency) definition is a type. It defines an association to a `Currencies` entity. The [`Currencies`](https://cap.cloud.sap/docs/cds/common#entity-sapcommoncurrencies) entity is based on ISO 4217 and uses three-letter alpha codes as keys such as `EUR` or `USD` and provides the possibility to store the corresponding currency symbol such as `€` or `$`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get more information about @sap/cds/common)]

Look at these explained keywords yourself and learn more about it.

1. To jump to the imported definitions directly in your editor, press **CTRL**, hover over the keyword, and click. This will open the source file in a separate editor tab.

2. Hold **CTRL** and hover over a keyword, then move the hand cursor over the keyword. This opens up you a tiny overlay with the definition of this particular item.

    !![overlay of definition](overlay-commons.png)

3. A right-click (or F12) on the definition opens up the context menu. There you can find, for example,  **Peek definition** to get a much bigger overlay.  Not only the definition of this particular item is displayed, you also have the ability to navigate through the whole source file of this definition without opening the file itself.

    !![right-click to peek definition information](rightclick-peek.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Rewrite the AdminService)]

In the first tutorial, you defined a simple service, called `AdminService`, which directly defined the entity `Products`. As you now have defined the `Products` entity in your domain model, the `AdminService` just needs to expose it. In addition, you defined the `Categories` entity, which should also be part of your service.

This can be achieved by using projections. Services expose projections of the entities defined in the domain model. These projections can be used to include (and also exclude) only certain elements of an entity or to rename the entity's elements.

In this example you will use the most simple projection, which exposes the domain model entity without any changes.

1. Go to your `srv` folder and open the `admin-service.cds` file.

2. Replace the content with the following code and make sure you **Save** the file:

    ```CDS
    using { sap.capire.products as db } from '../db/schema';

    service AdminService {
        entity Products   as projection on db.Products;
        entity Categories as projection on db.Categories;
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy the domain model)]

Let's deploy the domain model to a database. Now, you will use SQLite, a light-weight file-based database, which fits the needs for local development perfectly.

1. First, install SQLite to the project. Go to the terminal where your application is running and use **CTRL+C** to stop the application.

    Execute the following command in the terminal:

    ```Shell/Bash
    npm install --save-dev sqlite3
    ```

2. To initialize the database with the defined domain model, execute the following command in the terminal:

    ```Shell/Bash
    cds deploy --to sqlite
    ```

    !![cds deploy](cds-deploy.png)

    This will create a file called `sqlite.db` in your project root. The name of this database is defined by an entry in your `package.json`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure CAP application to use SQLite database)]

Right now, the Java application itself isn't aware of the SQLite database.

Configure your Java application to use the `sqlite.db` database file.

1. Go to `srv/src/main/resources`, locate, and open the `application.yaml` file. This file was created when you initialized the application.

2. For the field `url`, replace the string `"jdbc:sqlite::memory:?cache=shared"` with a reference to your local database file `"jdbc:sqlite:/home/user/projects/products-service/sqlite.db"`

3. Set the value of `initialization-mode` from `always` to `never`, because you've already initialized the database when running `cds deploy --to sqlite`.

Your `application.yaml` file should look like this:

```YAML
---
spring:
  profiles: default
  datasource:
    url: "jdbc:sqlite:/home/user/projects/products-service/sqlite.db"
    driver-class-name: org.sqlite.JDBC
    initialization-mode: never
    hikari:
      maximum-pool-size: 1
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Use CAP's generic persistence handling)]

The CAP Java SDK has a Persistence Service that provides out-of-the-box capabilities to store and retrieve entities from a database. Therefore, no custom coding is required for this. The entities defined in your `AdminService` will be automatically served via OData and you can just delete the `AdminService.java` file that was created earlier.

Delete the `AdminService.java` file in the `handlers` folder.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run and test your application)]

1. Start your application by running `mvn spring-boot:run` in the terminal and open it in a new tab.

2. Test your application by using Curl from the terminal.

    You can open additional terminals by choosing **Terminal** > **New Terminal** from the main menu.

    This request will create multiple, nested categories, at once through a deep insert:

    ```Shell/Bash
    curl -X POST http://localhost:8080/odata/v4/AdminService/Categories \
    -H "Content-Type: application/json" \
    -d '{"ID": 1, "name": "TechEd", "descr": "TechEd related topics", "children": [{"ID": 10, "name": "CAP Java", "descr": "Run on Java"}, {"ID": 11, "name": "CAP Node.js", "descr": "Run on Node.js"}]}'
    ```

3. Try to query individual categories, for example by adding the following to the end of your app URL:

    **`/odata/v4/AdminService/Categories(10)`**

4. Restart your application by switching to the terminal, using **CTRL+C**, and running `mvn spring-boot:run` again.

5. Query the categories again using the URL from step 3. You'll see that the categories are stored in a persistent database.

6. You can also expand the nested structures. Add the following to the end of your app URL:
    - `/odata/v4/AdminService/Categories?$expand=children`
    - `/odata/v4/AdminService/Categories(10)?$expand=parent`
    - `/odata/v4/AdminService/Categories(1)?$expand=children`

7. Make sure to stop your application after testing it by using **CTRL+C**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Set up for reuse)]

In the following tutorial, the application will be reused by a bookstore application. The reuse of models can be achieved by publishing NPM modules with the models and defining dependencies to these NPM modules in other applications. There are a two steps we need to perform to prepare the `products-service` application for reuse.

1. The name of the `products-service` reuse module, should be `@sap/capire-products`. Therefore, open the `package.json` file within the `~/projects/products-service` folder and change the value of `name` field from `products-service-cds` to `@sap/capire-products`. If you want to you can also provide a meaningful description in the `description` field.

    !![adjust the package.json in the root folder of your project](package-json.png)

2. To make it easier to reuse the module, an `index.cds` file can be added to the `products-service`. This ensures a better decoupling from other applications.

    Create a new file `index.cds` in the `~/projects/products-service` folder and place the following content inside this file, making sure you **Save** the file:

    ```CDS
    using from './db/schema';
    using from './srv/admin-service';
    ```

[DONE]
[ACCORDION-END]

Congratulations! You have successfully developed the products service application, which is based on a CDS domain model and service definition.

In the next tutorial, you will build a bookstore application, reusing the products service application. You will later extend the bookstore application with custom business logic and deploy it to the cloud, using SAP HANA as the database.
