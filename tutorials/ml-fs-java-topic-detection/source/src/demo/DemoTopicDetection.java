package demo;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.security.SecureRandom;
import java.util.Scanner;

public class DemoTopicDetection {

  public static void main(String[] args) {
    DataOutputStream dataOut = null;
    BufferedReader in = null;
    FileInputStream fileInputStream = null;

    try {

      String url = "https://sandbox.api.sap.com/ml/topicdetection/topic-detection";

      URL urlObj = new URL(url);
      HttpURLConnection connection = (HttpURLConnection) urlObj.openConnection();
      // setting request method
      connection.setRequestMethod("POST");

      // adding headers
      connection.setRequestProperty("content-type", "multipart/form-data; boundary=---011000010111000001101001");
      connection.setRequestProperty("Accept", "application/json");
      connection.setRequestProperty("APIKey", "<API_KEY>");

      connection.setDoInput(true);

      // sending POST request
      connection.setDoOutput(true);

      // read the input file name from user input
      Scanner scanner = new Scanner(System.in);

      // Reading from System.in
      String filePath = "";
      File file = null;
      boolean formatOk = false;
      do {
        System.out.println("Enter the text archive full path: (only zip or tar format are supported)");
        filePath = scanner.nextLine().replaceAll("\\/", "/");
        file = new File(filePath);
        String format = Files.probeContentType(file.toPath());
        if (format != null && //
            (format.startsWith("application/x-zip") //
                || format.startsWith("application/x-tar") //
            )) {
          formatOk = true;
        } else {
          System.out.println("format " + format);
        }
      } while (!file.exists() || file.isDirectory() || !formatOk);

      String line = "";
      int numTopicsDefault = 2;
      int numTopics = numTopicsDefault;
      System.out.println(
          "Enter the total number of topic to be detected (default: \"" + numTopicsDefault + "\") : ");
      line = scanner.nextLine();
      while (!!line.matches("\\d+") && line.length() > 0)
        line = scanner.nextLine();
      if (line.length() > 0)
        numTopics = Integer.valueOf(line);

      int numTopicsPerDocDefault = 2;
      int numTopicsPerDoc = numTopicsPerDocDefault;
      System.out.println("Enter the number of most relevant topics to be listed per document (default: \"" + numTopicsPerDocDefault + "\") : ");
      line = scanner.nextLine();
      while (!!line.matches("\\d+") && line.length() > 0)
        line = scanner.nextLine();
      if (line.length() > 0)
        numTopicsPerDoc = Integer.valueOf(line);

      int numKeywordsPerTopicDefault = 10;
      int numKeywordsPerTopic = numKeywordsPerTopicDefault;
      System.out.println("What is the number of keywords to be listed per topic (default: \"" + numKeywordsPerTopicDefault + "\") : ");
      line = scanner.nextLine();
      while (!!line.matches("\\d+") && line.length() > 0)
        line = scanner.nextLine();
      if (line.length() > 0)
        numKeywordsPerTopic = Integer.valueOf(line);

      int numFeaturesDefault = 10;
      int numFeatures = numFeaturesDefault;
      System.out.println("Enter is the maximum number of keywords to be extracted per documents (default: \"" + numFeaturesDefault + "\") : ");
      line = scanner.nextLine();
      while (!!line.matches("\\d+") && line.length() > 0)
        line = scanner.nextLine();
      if (line.length() > 0)
        numFeatures = Integer.valueOf(line);

      scanner.close();

      // prepare the constant for the form data
      String LINE_FEED = "\r\n";
      String SEPARATOR = "--";
      String BOUNDARY = "------Boundary" + new BigInteger(128, new SecureRandom()).toString(32);

      // set the form content as multipart
      connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + BOUNDARY);

      // open the input file
      fileInputStream = new FileInputStream(file);

      // write the form data content
      dataOut = new DataOutputStream(connection.getOutputStream());
      dataOut.writeBytes(SEPARATOR + BOUNDARY + LINE_FEED);
      dataOut.writeBytes("Content-Disposition: form-data; name=\"options\"" + LINE_FEED);
      dataOut.writeBytes(LINE_FEED);
      dataOut.writeBytes("{ " + //
          " \"numTopics\"           : \"" + numTopics + "\"" + //
          ",\"numTopicsPerDoc\"     : \"" + numTopicsPerDoc + "\"" + //
          ",\"numKeywordsPerTopic\" : \"" + numKeywordsPerTopic + "\"" + //
          ",\"numFeatures\"         : \"" + numFeatures + "\"" + //
          "}");
      dataOut.writeBytes(LINE_FEED);

      dataOut.writeBytes(SEPARATOR + BOUNDARY + LINE_FEED);
      dataOut.writeBytes("Content-Disposition: form-data; name=\"files\"; filename=\"" + filePath + "\"" + LINE_FEED);
      dataOut.writeBytes(LINE_FEED);

      // read the file as byte array
      int maxBufferSize = 1 * 1024 * 1024;
      int bytesAvailable = fileInputStream.available();
      int bufferSize = Math.min(bytesAvailable, maxBufferSize);
      byte[] buffer = new byte[bufferSize];
      int bytesRead = fileInputStream.read(buffer, 0, bufferSize);
      while (bytesRead > 0) {
        dataOut.write(buffer, 0, bufferSize);
        bytesAvailable = fileInputStream.available();
        bufferSize = Math.min(bytesAvailable, maxBufferSize);
        bytesRead = fileInputStream.read(buffer, 0, bufferSize);
      }

      // finish the form content
      dataOut.writeBytes(LINE_FEED);
      dataOut.writeBytes(SEPARATOR + BOUNDARY + SEPARATOR + LINE_FEED);
      dataOut.flush();
      fileInputStream.close();

      int responseCode = connection.getResponseCode();
      if (responseCode != 200) {
        in = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
      } else {
        in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      }
      String inputLine;
      StringBuffer response = new StringBuffer();
      while ((inputLine = in.readLine()) != null) {
        response.append(inputLine);
      }
      // printing response
      String TAB = "\t";
      String QUOTE = "\"";
      String CR = "\r\n";
      System.out.println(response.toString()//
          .replace("  " + QUOTE + "", "" + TAB + "" + QUOTE + "")//
          .replace("  ", "" + TAB + "")//
          .replace("" + TAB + " ", "" + TAB + "")//
          .replace(", ", ",")//
          .replace(": {", ":{" + CR + "")//
          .replace(": [", ":[" + CR + "")//
          .replace(":" + CR + "" + TAB + "{", ": {")//
          .replace(":" + CR + "" + TAB + "[", ": [")///
          .replace("{" + TAB + "", "{" + CR + "" + TAB + "")//
          .replace("[" + TAB + "", "[" + CR + "" + TAB + "")//
          .replace("" + QUOTE + ",", "" + QUOTE + "," + CR + "")//
          .replace("," + TAB + "", "," + CR + "" + TAB + "")//
          .replace("" + QUOTE + "" + TAB + "", "" + QUOTE + "" + CR + "" + TAB + "")//
          .replace("" + TAB + " " + TAB + "", "" + TAB + "" + TAB + "")//
          .replace("" + TAB + " {", "" + TAB + "{")//
          .replace("" + TAB + " [", "" + TAB + "[")//
          .replace("]", "]" + CR + "")//
          .replace("}", "}" + CR + "")//
          .replace("]" + CR + "," + CR + "", "]," + CR + "")//
          .replace("}" + CR + "," + CR + "", "}," + CR + "")//
          .replace("[" + CR + "]", "[]")//
          .replace("{" + CR + "}", "{}")//
          .replaceAll("([0-9])(\t)", "$1" + CR + "$2"));
    } catch (Exception e) {
      // do something with exception
      e.printStackTrace();
    } finally {
      try {
        if (dataOut != null) {
          dataOut.close();
        }
        if (in != null) {
          in.close();
        }
        if (fileInputStream != null) {
          fileInputStream.close();
        }
      } catch (IOException e) {
        // do something with exception
        e.printStackTrace();
      }
    }
  }
}