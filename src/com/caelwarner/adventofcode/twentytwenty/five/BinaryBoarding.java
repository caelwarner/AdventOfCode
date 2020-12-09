package com.caelwarner.adventofcode.twentytwenty.five;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.stream.Stream;

public class BinaryBoarding {

    public static void main(String[] args) {
        ArrayList<String> input = Read.asStringArray("adventofcode/twentytwenty/five/input.txt");

        System.out.println(findMissingSeatID(input));
    }

    private static int calculateHighestSeatID(ArrayList<String> input) {
        int highestSeat = 0;

        for (String seat : input) {
            int seatId = calculateSeatID(seat);

            if (seatId > highestSeat) highestSeat = seatId;
        }

        return highestSeat;
    }

    private static long findMissingSeatID(ArrayList<String> input) {
        int missing = 0;

        int[] seatIDs = new int[input.size()];
        int[] register = new int[input.size() * 2];

        for (int i = 0; i < input.size(); i++) {
            seatIDs[i] = calculateSeatID(input.get(i));
        }

        for (int i : seatIDs) {
            register[i] = 1;
        }

        for (int i = 1; i < register.length; i++) {
            if (register[i] == 0) {
                if (register[i - 1] == 1 && register[i + 1] == 1) missing = i;
            }
        }

        return missing;
    }

    private static int calculateSeatID(String input) {
        input = input.replaceAll("[LF]", "0").replaceAll("[RB]", "1");

        return Integer.parseInt(input, 2);
    }

}
