package com.caelwarner.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Read {

    public static ArrayList<String> asStringArray(String path) {
        ArrayList<String> output = new ArrayList<>();

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
        String output = "";

        try {
            File file = new File("src/com/caelwarner/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            output = bufferedReader.lines().collect(Collectors.joining());
        } catch (Exception e) {
            e.printStackTrace();
        }

        return output;
    }

    public static ArrayList<Integer> asIntArray(String path) {
        ArrayList<Integer> output = new ArrayList<>();

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
