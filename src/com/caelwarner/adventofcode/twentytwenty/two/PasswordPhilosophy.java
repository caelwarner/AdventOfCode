package com.caelwarner.adventofcode.twentytwenty.two;

import com.caelwarner.util.Read;

import java.util.ArrayList;

public class PasswordPhilosophy {

    public static void main(String[] args) {
        ArrayList<String> input = Read.asStringArray("twentytwenty/two/input.txt");

        System.out.println(checkIndexPasswords(input));
    }

    private static int checkCountPasswords(ArrayList<String> input) {
        int count = 0;

        for (String line : input) {
            String[] lineArray = line.split("[- ]");

            String character = String.valueOf(lineArray[2].charAt(0));
            int min = Integer.parseInt(lineArray[0]);
            int max = Integer.parseInt(lineArray[1]);

            int charCount = lineArray[3].length() - lineArray[3].replaceAll(character, "").length();

            if (charCount >= min && charCount <= max) {
                count++;
            }
        }

        return count;
    }

    private static int checkIndexPasswords(ArrayList<String> input) {
        int count = 0;

        for (String line : input) {
            String[] lineArray = line.split("[- ]");

            char[] password = lineArray[3].toCharArray();

            char character = lineArray[2].charAt(0);
            int index1 = Integer.parseInt(lineArray[0]) - 1;
            int index2 = Integer.parseInt(lineArray[1]) - 1;

            if (password[index1] == character ^ password[index2] == character) {
                count++;
            }
        }

        return count;
    }

}
