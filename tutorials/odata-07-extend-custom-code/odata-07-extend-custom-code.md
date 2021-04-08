---
author_name: DJ Adams
author_profile: https://github.com/qmacro
auto_validation: true
title: Extend the Built-In OData Features with Custom Code
description: Learn how to customize your OData service with event handlers.
primary_tag: software-product-function>sap-cloud-application-programming-model
tags: [ software-product-function>sap-business-application-studio, topic>odata, tutorial>beginner ]
time: 20
---

## Details
### You will learn
- What custom event handlers are
- Where and how to define a simple event handler
- How to use a custom event handler to define an OData function import

This tutorial assumes you've completed the tutorial [Extend your Simple Data Model with a Second Entity](odata-06-extend-odata-service). If you have done, you'll have an OData service `Northbreeze` with two related entities. All OData operations - create, read, update, delete and query - are supported out of the box.

In this tutorial, you'll learn how to add custom behaviour, in the form of handlers, to make your OData service do what you want it to do, beyond the standard operation handling.

Before you start, open up the workspace in the SAP Business Application Studio (App Studio) dev space you were using in that previous tutorial, ready to add code.

---

[ACCORDION-BEGIN [Step N: ](Review the product data)]

Let's take the `Products` entity as the target for our explorations of custom functions. Remind yourself of what the data looks like by starting up the service with `cds watch` in a terminal, just like you've done in the previous tutorial.

Open up the service in a new browser tab or window, and navigate to the `Products` entity set. You should see the familiar list of products, with values for the properties in each case, and it should look like this (only the first two products are shown here):

```JSON
{
  "@odata.context": "$metadata#Products",
  "value": [
    {
      "ProductID": 1,
      "ProductName": "Chai",
      "UnitsInStock": 39,
      "Category_CategoryID": 1
    },
    {
      "ProductID": 2,
      "ProductName": "Chang",
      "UnitsInStock": 17,
      "Category_CategoryID": 1
    }
  ]
}
```

Remember that at this stage your fully functioning OData service is a result of purely declarative definitions. Now it's time to add some simple business logic.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step N: ](Create a service implementation)]

Business logic in OData services belongs in a [service implementation](https://cap.cloud.sap/docs/node.js/services#srv-impls). The simplest way to do this is to create a `service.js` file in the same directory as your `service.cds` file, i.e. in the `srv/` directory. The framework will automatically recognize and use this "sibling" file.

In a new `srv/service.js` file, add the following JavaScript:

```JavaScript
module.exports = srv => {
    srv.after('READ', 'Products', items => {
        return items.map(item => {
            if (item.UnitsInStock > 100) {
                item.ProductName += ' SALE NOW ON!'
            }
            return item
        })
    })
}
```

Let's stare at this for a few moments. You won't be far wrong if you guess that it's something to do with adding an indication of a product sale for items where there's a high number of units in stock. But how does it work, and in what context?

First, in order to be used by CAP's runtime framework, a service implementation file such as this needs to offer a function definition for the framework to call on startup. This "offer" is via Node.js's module export mechanism, and what's exported here is the anonymous function which (apart from the `module.exports =` part itself) is the entire file contents.

When the framework finds and invokes this anonymous function, it passes a server object, which we can use to define event handlers via the [Handler Registration API](https://cap.cloud.sap/docs/node.js/services#event-handlers). That's why we have a single `srv` parameter defined, and that's what we use to access the [srv.after](https://cap.cloud.sap/docs/node.js/services#srv-after) API to declare a function to be run under specific circumstances (more on that shortly).

Examining that API call, we see this pattern:

```JavaScript
srv.after('READ', 'Products', items => { ... })
```

This is how we can add custom business logic to extend the standard handling that is provided for us out of the box. Specifically, this call defines a function (`items => { ... }`) that should be executed whenever there's an OData READ (or query) operation on the `Products` entity data.

The use of the specific `after` API call is quite common, and allows us to jump onto the request processing flow towards the end, when the heavy lifting of data retrieval from the persistence layer has been done for us. As well as `after`, the Handler Registration API supports `before` and `on` events, but right now, `after` is what we want here.

What does the function specified in this API call do? As you'd correctly guessed, it just adds a string on to the end of the value for each of the product names, specifically for the cases where the number of units in stock is high.

In its simplest form, the function provided is given the data retrieved, and whatever the function returns is what ends up in the response to the original request. Note, however, that in the context of the `after` API call, the handler function cannot change the "shape" of the data, such as omit specific items. We'll look at how to do that later on in this tutorial.

So with the simple `map` invocation, we are modifying the values for the `ProductName` properties of those items where the `UnitsInStock` value is more than 100.

Once you've added this code and saved the file, check that the `cds watch` process has restarted the service successfully, and have another look at the `Products` entity set.

Here's an example of what you should see; this data was retrieved using the system query options `$skip=4` and `$top=2` to narrow in on just two of the products, with "Grandma's Boysenberry Spread" having 120 units in stock and the extra "SALE NOW ON!" text:

```JSON
{
  "@odata.context": "$metadata#Products",
  "value": [
    {
      "ProductID": 5,
      "ProductName": "Chef Anton's Gumbo Mix",
      "UnitsInStock": 0,
      "Category_CategoryID": 2
    },
    {
      "ProductID": 6,
      "ProductName": "Grandma's Boysenberry Spread SALE NOW ON!",
      "UnitsInStock": 120,
      "Category_CategoryID": 2
    }
  ]
}
```

[DONE]
[ACCORDION-END]





[ACCORDION-BEGIN [Step N: ](Modify the custom code)]

That's great, but let's look now at a simple example of where we might want to change the shape of the data, or, as the documentation describes it, to make "asynchronous modifications".

If we wanted to reduce the list of products returned - to omit those products that had a low stock count - we would not use the `after` API call, but the `on` API call, and provide a function that effectively replaces the standard processing.

The prospect of doing this isn't as daunting as it first seems, as we're given everything that we need to be able to do this.

Remove the entire call to `srv.after` and replace it with a call to `srv.on`, so that the resulting `service.js` content looks like this:

```JavaScript
module.exports = srv => {
    srv.on('READ', 'Products', async (req, next) => {
        const items = await next()
        return items.filter(item => item.UnitsInStock > 100)
    })
}
```

This differs from the previous step thus in a number of ways.

First, we're using the `on` API call to provide a function that should be run _instead of_ standard processing when product data is requested.

Next, the function we provide doesn't expect the data (like we did in the previous function, with the `items` parameter), as the data will not be provided to it. Instead, it's expecting to be given the original request object (`req`), and a reference to the subsequent standard handler (`next`). We can use this `next` handler to actually do the work of retrieving the data for us, and are then free to do what we want with it.

Finally, because we're wanting to call that `next` function synchronously (with `await`), we must declare our function with the `async` keyword.

Once we have the data, in `items`, we return a filtered subset that only includes those products where the value of the `UnitsInStock` property is greater than 100.

Once you have this new implementation saved, and your service has restarted, check the `Products` entity set once more, and you should see only a small number of entries; if you're still using the data provided in the tutorials prior to this, there should be 10.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step N: ](Define a function import)]

That's great, but there's more that can be done in such a service implementation file.

The two JavaScript functions you've provided so far have been to affect the processing of standard OData operations on the `Products` entity. But OData V4 defines [actions and functions](http://docs.oasis-open.org/odata/odata/v4.0/os/part1-protocol/odata-v4.0-os-part1-protocol.html#_Toc372793604), in addition to entities. Actions and functions can be bound, or unbound. Think of such things as the next generation of function imports that you might know from OData V2.

So to round off this tutorial, let's define a simple unbound function on our OData service.

> Bear in mind the distinction between "function" in the JavaScript sense, and "function" in the OData sense.

While the custom logic that we've written so far has been implicit in our OData service's definition, as they work as handlers for existing operations, an OData function needs to be explicitly declared and described in the service's metadata.

To do this, extend the CDS definition in `srv/service.cds`, where you should add a line to define a function `TotalStockCount` in the `Main` service. The resulting content of `srv/service.cds` should look like this:

```CDS
using northbreeze from '../db/schema';

service Main {
    entity Products as projection on northbreeze.Products;
    entity Categories as projection on northbreeze.Categories;
    function TotalStockCount() returns Integer;
}
```

At this point, it's worth checking to see if this has any effect on your OData service. Once the CDS file is saved, and your service has restarted, navigate to the metadata document (that's the relative path `/main/$metadata`, but you knew that already, right?). It should look something like this:

```XML
<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EntityContainer Name="EntityContainer">
        <EntitySet Name="Categories" EntityType="Main.Categories">
          <NavigationPropertyBinding Path="Products" Target="Products"/>
        </EntitySet>
        <EntitySet Name="Products" EntityType="Main.Products">
          <NavigationPropertyBinding Path="Category" Target="Categories"/>
        </EntitySet>
        <FunctionImport Name="TotalStockCount" Function="Main.TotalStockCount"/>
      </EntityContainer>
      <EntityType Name="Categories">
        <Key>
          <PropertyRef Name="CategoryID"/>
        </Key>
        <Property Name="CategoryID" Type="Edm.Int32" Nullable="false"/>
        <Property Name="CategoryName" Type="Edm.String"/>
        <Property Name="Description" Type="Edm.String"/>
        <NavigationProperty Name="Products" Type="Collection(Main.Products)" Partner="Category"/>
      </EntityType>
      <EntityType Name="Products">
        <Key>
          <PropertyRef Name="ProductID"/>
        </Key>
        <Property Name="ProductID" Type="Edm.Int32" Nullable="false"/>
        <Property Name="ProductName" Type="Edm.String"/>
        <Property Name="UnitsInStock" Type="Edm.Int32"/>
        <NavigationProperty Name="Category" Type="Main.Categories" Partner="Products">
          <ReferentialConstraint Property="Category_CategoryID" ReferencedProperty="CategoryID"/>
        </NavigationProperty>
        <Property Name="Category_CategoryID" Type="Edm.Int32"/>
      </EntityType>
      <Function Name="TotalStockCount" IsBound="false" IsComposable="false">
        <ReturnType Type="Edm.Int32"/>
      </Function>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

We can see that this simple declarative definition has already had an effect.

There are two places where we see this new function import:

* in the `EntityContainer` element we can see the function import listed alongside the two entity sets
* near the bottom, after the definitions of the `Categories` and `Products` entity types, we see the definition for this function import

The function import definition here in the metadata document reflects what we intended; in particular, the function is called `TotalStockCount`, is unbound, and has an integer return type:

```XML
<Function Name="TotalStockCount" IsBound="false" IsComposable="false">
  <ReturnType Type="Edm.Int32"/>
</Function>
```

Great. Now we can get to writing the implementation of this function import.

[VALIDATE_1]
[ACCORDION-END]




[ACCORDION-BEGIN [Step N: ](Implement the function import)]

The implementation of this function import might as well go in the same `srv/service.js` file as before, to keep things simple. Here's what the entire contents of the file should look like with all the additions:

```JavaScript
const cds = require('@sap/cds')
const { Products } = cds.entities('northbreeze')

module.exports = srv => {
    srv.on('READ', 'Products', async (req, next) => {
        const items = await next()
        return items.filter(item => item.UnitsInStock > 100)
    })

    srv.on('TotalStockCount', async (req) => {
        const items = await cds.tx(req).run(SELECT.from(Products))
        return items.reduce((a, item) => a + item.UnitsInStock, 0)
    })
}
```

Let's look at what's new.

First, at the top of the file, there are these two new lines:

```JavaScript
const cds = require('@sap/cds')
const { Products } = cds.entities('northbreeze')
```

Bringing in the `cds` module here allows us to introspect on our `northbreeze` service definition, and pull out the `Products` entity that we'll need shortly.

Next, directly below the existing `srv.on('READ', 'Products', async (req, next) => { ... })` call that you already had, there is now a second call to the Handler Registration API to define a handler for the `TotalStockCount` function import.

This handler is an anonymous function just like the other, except that it only expects and needs the request (in `req`). It uses this as a context for the transaction that it creates, within which it then retrieves the product data.

> Note that `Products` is a constant, not a literal string, and refers to the entity set object that we retrieved via `cds.entities` earlier.

The product data retrieved is stored in the `items` constant, and looks like this:

```JavaScript
[ { ProductID: 1,
    ProductName: 'Chai',
    UnitsInStock: 39,
    Category_CategoryID: 1 },
  { ProductID: 2,
    ProductName: 'Chang',
    UnitsInStock: 17,
    Category_CategoryID: 1 },
  ...
]
```

It's then just a simple case of summing the values of the `UnitsInStock` property for each of the items, which we do cleanly with a simple `reduce` function, and return the result. Being a numeric value, the result type corresponds to what we defined as what the function import returns, back in the CDS file:

```CDS
function TotalStockCount() returns Integer;
```

Once you've saved the service implementation file and the `cds watch` process has restarted the service, you should try this function import out. Switch to the other tab and navigate to the relative path:

```
/main/TotalStockCount()
```

The response should look something like this:

```JSON
{
  "@odata.context": "$metadata#Edm.Int32",
  "value": 3119
}
```

That is, there are a total of 3119 stock units across all products.

Well done! You've now successfully implemented an OData V4 unbound function, and hopefully feel comfortable enough to implement your own custom business logic for your CAP-powered OData services.

[DONE]
[ACCORDION-END]

---
