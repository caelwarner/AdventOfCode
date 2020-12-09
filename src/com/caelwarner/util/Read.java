package com.caelwarner.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

public class Read {

    public static List<String> asStringArray(String path) {
        List<String> output = new ArrayList<>();

        try {
            File file = new File("src/com/caelwarner/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            while (bufferedReader.ready()) {
                output.add(bufferedReader.readLine());
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return output;
    }

    public static String asString(String path) {
        StringBuilder output = new StringBuilder();

        try {
            File file = new File("src/com/caelwarner/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            while (bufferedReader.ready()) {
                output.append(bufferedReader.readLine()).append("\n");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return output.toString();
    }

    public static List<Integer> asIntArray(String path) {
        List<Integer> output = new ArrayList<>();

        try {
            File file = new File("src/com/caelwarner/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            while (bufferedReader.ready()) {
                output.add(Integer.valueOf(bufferedReader.readLine()));
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return output;
    }

    public static Stream<String> asStringStream(String path) {
        return asStringArray(path).stream();
    }

}
