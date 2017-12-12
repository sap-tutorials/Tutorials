---
title: Enhancing the persistence layer for multi-tenency
description: Enhancing the persistence layer for multi-tenency
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 6](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part6.html)

## Next Steps
- [End-to-End Weather App Scenario Part 8](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part8.html)

## Details
### You will learn  
In this tutorial you will enhance the persistence layer with a multi-tenancy feature so that each user will be able to store their own favorite list.

### Time to Complete
**10 min**

---


[ACCORDION-BEGIN [Step 1: ](Add annotations to base object class)]

First, we'll add the necessary annotations to the persistence `BaseObject` class. Open it and add the following two annotations to the class definition:

```java
@MappedSuperclass
@Multitenant
@TenantDiscriminatorColumn(name = "TENANT_ID", contextProperty="tenant.id")
public abstract class BaseObject
```

![Adding Java annotations to a Java Class](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part7/e2e_07-1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Obtain reference)]

Next, we need to slightly adjust the way we obtain a reference to the `EntityManager` within the `FavoriteCityService` class as we now need to pass the current tenant ID (in our case the user ID). The following code snippet illustrates the concept:

```java
@SuppressWarnings("unchecked")
@GET
@Path("/")
public List<FavoriteCity> getFavoriteCities(@Context SecurityContext ctx)
{
List<FavoriteCity> retVal = null;

String userName = (ctx.getUserPrincipal() != null) ? ctx.getUserPrincipal().getName() : "anonymous";
Map<String,String> props = new HashMap<String,String>();
props.put("tenant.id", userName);

EntityManager em = this.getEntityManagerFactory().createEntityManager(props);

retVal = em.createNamedQuery("FavoriteCities").getResultList();

return retVal;
}
```
![Modifying Java methods to handle the tenant ID](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part7/e2e_07-2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Edit methods)]

Make your changes to the methods (`getFavoriteCity()`, `addFavoriteCity()` and `removeFavoriteCity()` ) in the `FavoriteCityService` respectively and save all edits.

![Modifying Java methods to handle the tenant ID](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part7/e2e_07-3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy your changes)]

Deploy/publish your changes. Please explicitly stop and start the server, as you have updated the persistence model!


[ACCORDION-END]



## Next Steps
- [End-to-End Weather App Scenario Part 8](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part8.html)
