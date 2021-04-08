---
author_name: DJ Adams
author_profile: https://github.com/qmacro
title: Extend the built-in OData features with custom code
description: Learn how to add custom handlers to your OData service.
primary_tag: software-product-function>sap-cloud-application-programming-model
tags: [ software-product-function>sap-business-application-studio, topic>odata, tutorial>beginner ]
time: 20
---

## Details
### You will learn
- What custom handlers are
- How custom handlers fit into your OData service definition
- How to write a simple custom handler and debug it

This tutorial assumes you've completed the tutorial [Extend your Simple Data Model with a Second Entity](odata-06-extend-odata-service). If you have done, you'll have an OData service `Northbreeze` with two related entities. All OData operations - create, read, update, delete and query - are supported out of the box.

In this tutorial, you'll learn how to add custom behaviour, in the form of handlers, to make your OData service do what you want it to do, beyond the standard operation handling.

Before you start, open up the workspace in the SAP Business Application Studio (App Studio) dev space you were using in that previous tutorial, ready to add code.

---
