import java.lang.*;
import java.util.*;

import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;

import java.io.*;

class Source {
    int rows = 0;
    int columns = 0;

    ArrayList<List<String>> data;
    Source(String path) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(path));
            Iterator<String> lines = reader.lines().iterator();

            data = new ArrayList<List<String>>();

            while (lines.hasNext()) {
                String line = lines.next();
                data.add(Arrays.asList(line.split(", ")));
            }
            reader.close();

            rows = data.size();
            int columns = 0;

            for (List<String> row : data) {
                if (columns < row.size()) {
                    columns = row.size();
                }
            }
            } catch (Exception e) {
                System.err.println(e.getMessage());
            }
    }

    Document process() {
        javax.xml.stream.
        DocumentBuilder toxml;
    }
}

public class Csvxml {

    public static void main(String[] args) {
        for (String path : args) {
            String newpath = new StringBuilder(path.substring(0, path.lastIndexOf("."))).append(".xml").toString();
            Source csv = new Source(path);
        }
    }
}