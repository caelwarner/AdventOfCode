package com.caelwarner.adventofcode.twentytwenty.five;

import com.caelwarner.util.Read;

import java.util.ArrayList;

public class BinaryBoarding {

    public static void main(String[] args) {
        ArrayList<String> input = Read.readToStringArrayList("twentytwenty/five/input.txt");

        System.out.println(highestSeatID(input));
    }

    private static int highestSeatID(ArrayList<String> input) {


        return 0;
    }

    private static int calculateSeatID(String input) {
        int rowMin = 1;
        int rowMax = 128;
        int colMin = 1;
        int colMax = 8;

        for (char character : input.toCharArray()) {
            if (character == "F".charAt(0)) {
                rowMax /= 2;
            } else if (character == "B".charAt(0)) {
                rowMin = rowMax / 2;
            }
        }

        return 0;
    }

}
