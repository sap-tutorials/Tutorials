package org.example;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import javax.sql.DataSource;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;

import org.springframework.cloud.Cloud;
import org.springframework.cloud.CloudFactory;

import liquibase.Liquibase;
import liquibase.database.jvm.JdbcConnection;
import liquibase.resource.ClassLoaderResourceAccessor;

@Path("hello")
public class HelloData {

	private static final Cloud cloud = new CloudFactory().getCloud();

	/**
	 * Default initialization. This allows us to set up the database - once -
	 * using Liquibase
	 */
	static {
		try {
			DataSource dataSource = cloud.getServiceConnector("mydb", DataSource.class, null);
			Connection connection = dataSource.getConnection();
			JdbcConnection liquibaseConnection = new JdbcConnection(connection);
			Liquibase liquibase = new Liquibase("/db.changelog.xml", new ClassLoaderResourceAccessor(),
					liquibaseConnection);
			liquibase.update("");
		} catch ( Exception ex ) {
			throw new RuntimeException( ex );
		}
	}

	@GET
	@Path("/repeat/{param}")
	public Response getMsg(@PathParam("param") String msg) {

		String output = "Jersey say : " + msg;

		return Response.status(200).entity(output).build();
	}

	@GET
	@Path("/square/{value}")
	public Response getSquare(@PathParam("value") Integer value ) {
		if ( value == null ) value = 48;

		double returnValue = Math.pow(value, 2);

		return Response.status(200).entity("Square of " + value + " is : " + returnValue).build();
	}

	@GET
	@Path("/dbread")
	public Response databaseRead() throws SQLException {
		return databaseRead(1);
	}

	@GET
	@Path("/dbread/{key}")
	public Response databaseRead(@PathParam("key") Integer key) throws SQLException {

		if (key == null)
			key = 1;

		DataSource ds = cloud.getServiceConnector("mydb", DataSource.class, null);

		Connection conn = ds.getConnection();
		try {
			PreparedStatement ps = conn.prepareStatement("select USECOUNT from EXAMPLE_TABLE where ID = ?");
			try {
				ps.setInt(1, key);
				ResultSet rs = ps.executeQuery();
				try {
					if (rs.next())
						return Response.status(200)
								.entity("Current example table data is (key " + key + "): " + rs.getInt(1)).build();
				} finally {
					rs.close();
				}
			} finally {
				ps.close();
			}
		} finally {
			conn.close();
		}

		return Response.status(200).entity("No row found").build();
	}

	@GET
	@Path("/dbwrite/{key}")
	public Response databaseWrite(@PathParam("key") Integer key) throws SQLException {

		if (key == null)
			key = 1;

		DataSource ds = cloud.getServiceConnector("mydb", DataSource.class, null);

		Connection conn = ds.getConnection();
		try {
			Statement stmt = conn.createStatement();
			try {
				int rows = conn.createStatement()
						.executeUpdate("update EXAMPLE_TABLE set USECOUNT = USECOUNT + 1 where ID = " + key);
				if (rows != 1) {
					stmt.executeUpdate("insert into EXAMPLE_TABLE(ID, USECOUNT) values ( " + key + " , 1 )");
				}

				return databaseRead(key);
			} finally {
				stmt.close();
			}
		} finally {
			conn.close();
		}

	}

	@GET
	@Path("/dbreset/{key}")
	public Response databaseReset(@PathParam("key") Integer key) throws SQLException {
		if (key == null)
			key = 1;

		DataSource ds = cloud.getServiceConnector("mydb", DataSource.class, null);

		Connection conn = ds.getConnection();
		try {
			Statement stmt = conn.createStatement();
			try {
				int rows = conn.createStatement()
						.executeUpdate("update EXAMPLE_TABLE set USECOUNT = 0 where ID = " + key);
				if (rows != 1) {
					stmt.executeUpdate("insert into EXAMPLE_TABLE(ID, USECOUNT) values ( " + key + " , 1 )");
				}

				return databaseRead(key);
			} finally {
				stmt.close();
			}
		} finally {
			conn.close();
		}
	}
}
