package com.caelwarner.adventofcode.twentytwenty.eight;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.List;

public class HandheldHalting {

    private static int accumulator = 0;
    private static int currentLine = 0;
    private static boolean changedLine = false;
    private static final List<Integer> changedLines = new ArrayList<>();
    private static final List<Integer> readLines = new ArrayList<>();

    public static void main(String[] args) {
        List<String> input = Read.asStringArray("adventofcode/twentytwenty/eight/input.txt");

        System.out.println(runCode(input));
    }

    private static int runCode(List<String> input) {
        while (!readLines.contains(currentLine)) {
            if (currentLine == input.size()) {
                System.out.println("found");
                return accumulator;
            }

            readLines.add(currentLine);
            parseLine(input.get(currentLine).split(" "));
            currentLine++;
        }

        return accumulator;
    }

    private static int findBrokenInstruction(List<String> input) {
        for (int i = 0; i < 86; i++) {
            changedLine = false;
            accumulator = 0;
            currentLine = 0;
            readLines.clear();

            System.out.println(runCode(input));
        }

        return 0;
    }

    private static void parseLine(String[] line) {
        switch (line[0]) {
            case "acc":
                accumulator += Integer.parseInt(line[1]);
                break;
            case "jmp":
//                if (!changedLine && !changedLines.contains(currentLine)) {
//                    changedLines.add(currentLine);
//                    System.out.println(changedLines);
//                    changedLine = true;
//
//                    break;
//                }

                currentLine += Integer.parseInt(line[1]) - 1;
                break;
            case "nop":
//                if (!changedLine && !changedLines.contains(currentLine)) {
//                    changedLines.add(currentLine);
//                    System.out.println(changedLines);
//                    changedLine = true;
//
//                    currentLine += Integer.parseInt(line[1]) - 1;
//                }
                break;
            default:
                break;
        }
    }
}
