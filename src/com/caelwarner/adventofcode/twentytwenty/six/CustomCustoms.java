package com.caelwarner.adventofcode.twentytwenty.six;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CustomCustoms {

    public static void main(String[] args) {
        String input = Read.asString("adventofcode/twentytwenty/six/input.txt");
        Stream<String> inputStream = Arrays.stream(input.split("\n"));

        inputStream.forEach(System.out::println);

//        System.out.println(findSumQuestions(inputStream));
    }

    private static long findSumQuestions(Stream<String> input) {
        input = input.map(s -> s.replaceAll("\n", ""));

        input.forEach(System.out::println);

        return 0;
    }
}
