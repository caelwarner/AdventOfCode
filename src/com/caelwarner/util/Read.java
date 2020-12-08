package com.caelwarner.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Scanner;

public class Read {

    public static ArrayList<String> readToStringArrayList(String path) {
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

    public static ArrayList<Integer> readToIntArrayList(String path) {
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

}
