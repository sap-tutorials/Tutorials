package org.example;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;

@Path("hello")
public class HelloRest {

	@GET
	@Path("/repeat/{param}")
	public Response getMsg(@PathParam("param") String msg) {

		String output = "Jersey say : " + msg;

		return Response.status(200).entity(output).build();
	}

	@GET
	@Path("/square/{value}")
	public Response getSquare(@PathParam("value") Integer value) {
		if (value == null)
			value = 48;

		double returnValue = Math.pow(value, 2);

		return Response.status(200).entity("Square of " + value + " is : " + returnValue).build();
	}
}
