package com.caelwarner.twentytwenty.three;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;

public class TobogganTrajectory {

    public static void main(String[] args) {
        ArrayList<String> input = Read.readToStringArrayList("twentytwenty/three/input.txt");

        System.out.println(checkMultipleTrajectorys(input));
    }

    private static int checkTrajectory(ArrayList<String> input, int yAmount, int xAmount) {
        char[][] map = new char[input.size()][input.get(0).length()];
        char tree = "#".charAt(0);
        int count = 0;

        for (int i = 0; i < map.length; i++) {
            map[i] = input.get(i).toCharArray();
        }

        int xPos = 0;

        for (int i = 0; i < map.length; i += yAmount) {
            if (map[i][xPos] == tree) count++;

//            System.out.println(xPos);

            xPos += xAmount;

            if (xPos >= map[i].length) xPos -= map[i].length;
        }

        return count;
    }

    private static long checkMultipleTrajectorys(ArrayList<String> input) {
        int[] yAmounts = {1, 1, 1, 1, 2};
        int[] xAmounts = {1, 3, 5, 7, 1};
        long count = 1;

        for (int i = 0; i < yAmounts.length; i++) {
            count *= checkTrajectory(input, yAmounts[i], xAmounts[i]);
        }

        return count;
    }

}
