package com.caelwarner.adventofcode.twentytwenty.ten;

import com.caelwarner.util.Read;

import java.util.Collections;
import java.util.List;

public class AdapterArray {

    public static void main(String[] args) {
        List<Integer> input = Read.asIntArray("adventofcode/twentytwenty/ten/input.txt");

        System.out.println(findPossibleSolutions(input));
    }

    private static int findJoltage(List<Integer> input) {
        Collections.sort(input);

        int oneDifference = 1;
        int threeDifference = 1;

        for (int i = 0; i < input.size() - 1; i++) {
            int difference = input.get(i + 1) - input.get(i);

            if (difference == 1) {
                oneDifference++;
            } else if (difference == 3) {
                threeDifference++;
            }
        }

        return oneDifference * threeDifference;
    }

    private static long findPossibleSolutions(List<Integer> input) {
        Collections.sort(input);

        input.add(0, 0);
        input.add(input.get(input.size() - 1) + 3);

        int[] multiplyList = {1, 1, 1, 2, 4, 7};
        int currentLength = 0;
        long combinations = 1;

        for (int i = 0; i < input.size() - 1; i++) {
            int difference = input.get(i + 1) - input.get(i);

            currentLength++;

            if (difference == 1) {

            } else if (difference == 3) {
                combinations *= multiplyList[currentLength];

                currentLength = 0;
            }
        }

        System.out.println(input);

        return combinations;
    }

}
