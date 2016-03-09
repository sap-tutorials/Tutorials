---
title: End-to-End Weather App Scenario Part 7
description: Enhancing the persistence layer for multi-tenency
tags: [tutorial:interest/gettingstarted, tutorial:interest/cloud, tutorial:product/hcp, tutorial:technology/java]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 6](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part6.html)

## Next Steps
 - [End-to-End Weather App Scenario Part 8](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part8.html)
 
## Details
### You will learn  
In this tutorial you will enhance the persistence layer with a multi-tenancy feature so that each user will be able to store their own favorite list.

### Time to Complete
**10 min**

1. First, we’ll add the necessary annotations to the persistence **BaseObject** class. Open it and add the following two annotations to the class definition:

 ```java
 @MappedSuperclass
 @Multitenant
 @TenantDiscriminatorColumn(name = "TENANT_ID", contextProperty="tenant.id")
 public abstract class BaseObject
 ```
 
 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part7/e2e_07-1.png)

2. Next, we need to slightly adjust the way we obtain a reference to the **EntityManager** within the **FavoriteCityService** class as we now need to pass the current tenant ID (in our case the user ID). The following code snippet illustrates the concept:

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
 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part7/e2e_07-2.png)

3. Make your changes to the methods (getFavoriteCity, addFavoriteCity and removeFavoriteCity) in the **FavoriteCityService** respectively and save all edits. 

 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part7/e2e_07-3.png)
 
4. Deploy/publish your changes. Please explicitly stop and start the server, as we have updated the persistence model! 


 
## Next Steps
 - [End-to-End Weather App Scenario Part 8](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part8.html)
