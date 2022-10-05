package com.caelwarner.adventofcode.twentytwenty.one;

import com.caelwarner.util.Read;

import java.util.List;

public class ReportRepair {

    public static void main(String[] args) {
        List<Integer> input = Read.asIntList("twentytwenty/one/input.txt");

        System.out.println(findThreeSum(input));
    }

    private static int findTwoSum(List<Integer> input) {
        for (int number1 : input) {
            for (int number2 : input) {
                if ((number1 + number2) == 2020) {
                    return number1 * number2;
                }
            }
        }

        return 0;
    }

    private static int findThreeSum(List<Integer> input) {
        for (int number1 : input) {
            for (int number2: input) {
                for (int number3 : input) {
                    if ((number1 + number2 + number3) == 2020) {
                        return number1 * number2 * number3;
                    }
                }
            }
        }

        return 0;
    }

}
