package com.caelwarner.adventofcode.twentytwenty.ten;

import com.caelwarner.util.Read;

import java.util.Collections;
import java.util.List;

public class AdapterArray {

    public static void main(String[] args) {
        List<Integer> input = Read.asIntArray("adventofcode/twentytwenty/ten/input.txt");

        System.out.println(findJoltage(input));
    }

    private static int findJoltage(List<Integer> input) {
        Collections.sort(input);

        int oneDifference = 0;
        int threeDifference = 0;

        for (int i = 0; i < input.size() - 1; i++) {
            int difference = input.get(i + 1) - input.get(i);

            if (difference == 1) {
                oneDifference++;
            } else if (difference == 3) {
                threeDifference++;
            }
        }

        System.out.println(oneDifference);
        System.out.println(threeDifference);

        return oneDifference * threeDifference;
    }

}
