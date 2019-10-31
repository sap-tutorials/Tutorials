---
title: Create a Basic CAP-Based Service
description: Initialize a CAP-based service, which you will later extend via SAP Cloud SDK.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-s-4hana-cloud-sdk]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
- You should start off in the same place as you were at the end of the previous tutorial -- in VS Code, with your `teched2019-mission-mock-service-bupa` project still open.

## Details
### You will learn
- How to install `@sap/cds` globally
- How to initialize a new CAP project
- What the basic structure of a CAP-based service looks like
- How to supply seed data in CSV form
- How to start up a CAP service locally

So you have a mock service running, and supporting V2 as well as V4 flavored responses to OData operation requests. Now it's time to put together a second service that will eventually consume data from (make requests to) this mock service. We'll use the SAP Cloud Application Programming Model for this second consumer service so we can take advantage of the powerful Core Data Services language to bridge local and remote data sources in service definitions.

To keep things simple, the consumer service will be based on the simple bookshop model that you may have seen before, so that you can focus on the consumption parts you'll eventually add and use.

---

[ACCORDION-BEGIN [Step 1: ](Install @sap/cds globally)]

The first step is to create a new app using the `cds` command line tool which is part of the `@sap/cds` package. While you've already used the `@sap/cds` package in the preceding tutorials in this mission, it's been within the context of an individual project directory where `@sap/cds` was installed locally. Node.js packages can be installed globally, too, and that's what you'll do now with `@sap/cds` so that the `cds` command line client is available everywhere.

Execute the following commands in a command prompt (even one in an integrated terminal within VS Code will do).

The `@sap` part of `@sap/cds` is a Node Package Manager (NPM) scope, which can be associated with a particular NPM registry. In this case the `@sap` scope is associated with the SAP NPM registry, so set your global NPM configuration to reflect this, by running the following command:

```Bash
npm set @sap:registry=https://npm.sap.com
```

Now you can run the following command to install `@sap/cds` globally, knowing that `npm` will request the package from the correct registry:

```Bash
npm install -g @sap/cds
```

To satisfy yourself that the install proceeded successfully, invoke the `cds` executable with the `-v` option and check that you get sensible output. Here's an example of what that might look like (versions may be different):

```Bash
$ cds -v
@sap/cds: 3.13.0
@sap/cds-compiler: 1.15.0
@sap/cds-ql: 1.14.0
@sap/cds-hana: 1.13.0
@sap/cds-sql: 1.13.0
@sap/cds-sqlite: 1.13.0
@sap/cds-reflect: 2.5.0
@sap/cds-services: 1.14.0
@sap/odata-server: 1.3.4
@sap/odata-commons: 2.1.1
@sap/generator-cds: 2.4.11
CDS home: /Users/i347491/.nvm/versions/node/v10.16.0/lib/node_modules/@sap/cds
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Initialize a new CAP project)]

With your freshly installed `cds` command line tool, you can now create a new CAP-based project, in the form of a new directory with various files preconfigured. Do this now, in your home directory or another directory where you have write access.

> To keep things together, we recommend you create this new project directory next to the mock service project directory you created in a previous tutorial in this mission.

```Bash
cds init --modules db,srv consumer-app
```

This will emit some output as it goes, and when finished, you will have a new `consumer-app/` directory which you can now open up in VS Code. Do this either by creating a new top-level window in your running VS Code instance (with **File | New Window**) and then opening this new directory (with **File | Open...**), or simply by running the following (if your operating system allows this):

```
code consumer-app
```

You should end up with two VS Code top-level windows, one showing your `teched2019-mission-mock-service-bupa` project, and the other one showing this new `consumer-app` project, like this:

![two VS Code top-level windows](two-windows.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enhance the project with another couple of entities)]

Using the `--modules db,srv` option when initializing this project caused a sample data model and service definition to be created, so you have something to start with. With the recent versions of `cds` this includes three entities related together - `Authors`, `Books` and `Orders` (previously it was just `Authors` and `Books`). This is enough for our needs, but there are a couple of minor modifications required.

First, edit the `db/data-model.cds`, changing the `using` statement and the `Orders` entity so the file ends up looking like this:

```CDS
namespace my.bookshop;

using cuid from '@sap/cds/common';

entity Books {
  key ID : Integer;
  title  : String;
  stock  : Integer;
  author : Association to Authors;
}

entity Authors {
  key ID : Integer;
  name   : String;
  books  : Association to many Books on books.author = $self;
}

entity Orders : cuid {
  book     : Association to Books;
  quantity : Integer;
}
```

Now edit the `srv/cat-service.cds` file to have each of these three entities exposed, removing the `@readonly` annotation while you're at it:

```CDS
using my.bookshop as my from '../db/data-model';

service CatalogService {
    entity Books as projection on my.Books;
    entity Authors as projection on my.Authors;
    entity Orders as projection on my.Orders;
}
```

That's better.

In data model definitions, entities are related through associations, which can be either unmanaged (where you have to specify the foreign key and join conditions yourself) or managed.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add sample data)]

Seed data can be supplied in the form of CSV files, one for each entity type. This data will be loaded into the appropriate tables at the persistence layer when the `cds deploy` command is used.

Create a new directory `csv/` inside the `db/` directory, and add three files, named as follows:

- `my.bookshop-Authors.csv`
- `my.bookshop-Books.csv`
- `my.bookshop-Orders.csv`

Add the following CSV data sets into each of these corresponding new files:

**`my.bookshop-Authors.csv`**

```CSV
ID,NAME
42,Douglas Adams
101,Emily Brontë
107,Charlote Brontë
150,Edgar Allen Poe
170,Richard Carpenter
```

**`my.bookshop-Books.csv`**

```CSV
ID,TITLE,AUTHOR_ID,STOCK
421,The Hitch Hiker's Guide To The Galaxy,42,1000
427,"Life, The Universe And Everything",42,95
201,Wuthering Heights,101,12
207,Jane Eyre,107,11
251,The Raven,150,333
252,Eleonora,150,555
271,Catweazle,170,22
```

**`my.bookshop-Orders.csv`**

```CSV
ID,BOOK_ID,QUANTITY
7e2f2640-6866-4dcf-8f4d-3027aa831cad,421,15
64e718c9-ff99-47f1-8ca3-950c850777d4,271,9
```

To effect the loading of this seed data, run the following command in an integrated terminal within your project in VS Code (ensure you're in the project directory before you do):

```Bash
cds deploy --to sqlite
```

You should see output similar to this:

```
 > filling my.bookshop.Authors from db/csv/my.bookshop-Authors.csv
 > filling my.bookshop.Books from db/csv/my.bookshop-Books.csv
 > filling my.bookshop.Orders from db/csv/my.bookshop-Orders.csv
/> successfully deployed database to ./sqlite.db
```

Now you can start the service up ...

```Bash
cds run
```

... and explore the data that's just been loaded, and the relationships between the items. Here are a few examples:

- The book orders: <http://localhost:4004/catalog/Orders?$expand=book>
- The authors and their books: <http://localhost:4004/catalog/Authors?$expand=books>
- Books that are low on stock: <http://localhost:4004/catalog/Books?$filter=stock%20lt%2050>

[DONE]
[ACCORDION-END]

At this point in the mission, you have a mocked SAP S/4HANA Business Partner service supplying address data, and a bookshop style service (to which you'll eventually add a simple user interface), which will be extended to consume that address data and combine it with the bookshop order information.
