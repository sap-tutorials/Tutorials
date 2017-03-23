---
title: Creating an OData Service with an Entity Relationship
description: Creating an OData Service with an Entity Relationship
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>odata, products>sap-hana, products>sap-hana\,-express-edition ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Create a Simple OData Service](http://www.sap.com/developer/tutorials/xsa-xsodata.html)

## Next Steps
- [Creating an OData Service with Create Operation and XSJS Exit](http://www.sap.com/developer/tutorials/xsa-xsodata-create.html)

## Details
### You will learn  
The first example of this exercise was very simplistic because it only exposed one database table as a single entity.  Often you need to also represent relationships between multiple entities. For example you might want to build an OData service which has both the Purchase Order Header and Items. For this you would build a 1:many relationship between the two entities.

**Please note - This tutorial is based on SPS11**

### Time to Complete
**10 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Create new OData service)]

Returning to the editor, you should now create a new OData service named `purchaseOrders.xsodata` and extend it to include the `dev602.data::PO.Header` and `dev602.data::PO.Item` tables. Next create a navigation 1:many association.The new content of the definition file should look like this:Note: if you don't want to type this code, we recommend that you cut and paste it from this web address  	`http://<hostname>:51013/workshop/admin/ui/exerciseMaster/?workshop=dev602&sub=ex3_11`
	```
service namespace "dev602.services" {	"dev602.data::PO.Header"	as "POHeader" navigates ("Items" as "POItem");	"dev602.data::PO.Item"	as "POItem";	association "Items"	principal "POHeader"("PURCHASEORDERID")	multiplicity "1"	dependent "POItem"("PURCHASEORDERID")	multiplicity "*";}	```

[DONE]
[ACCORDION-END]  

[ACCORDION-BEGIN [Step 2: ](Save, run, and test)]

Save, run and test using the same steps as in the previous section of this exercise. Notice that the base service definition now has two entities.

![entities](3.png)

The Header data now has a hyperlink relationship to the item entity.

![metadata](4.png)

[DONE]
[ACCORDION-END]  

[ACCORDION-BEGIN [Step 4: ](test again)]

Associations can be an excellent way to load child elements on demand; however there is also an option to expand the children details in place so that all levels can be retrieved with one request.  Test the service again using the same steps as in the previous section of this exercise. This time add `$expand=POItem` to the end of the URL. You will then see that all the items are embedded within each header record.

![associations](5.png)
[DONE]
[ACCORDION-END]  



## Next Steps
- [Creating an OData Service with Create Operation and XSJS Exit](http://www.sap.com/developer/tutorials/xsa-xsodata-create.html)
