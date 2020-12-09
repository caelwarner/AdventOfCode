package com.caelwarner.adventofcode.twentytwenty.six;

import com.caelwarner.util.Read;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CustomCustoms {

    public static void main(String[] args) {
        String input = Read.asString("adventofcode/twentytwenty/six/input.txt");
        Stream<String> inputStream = Arrays.stream(input.split("\n\n"));

//        inputStream.forEach(System.out::println);

        System.out.println(findSumQuestionsAll(inputStream));
    }

    private static long findSumQuestionsAny(Stream<String> input) {
        return input.map(s -> s.replaceAll("\n", ""))
                .mapToLong(s -> s.chars().distinct().count())
                .sum();
    }

    private static int findSumQuestionsAll(Stream<String> input) {
        int sum = 0;
        List<String[]> inputList = input.map(s -> s.split("\n")).collect(Collectors.toList());

        for (String[] strings : inputList) {
            Set<Integer> chars = strings[0].chars().boxed().collect(Collectors.toSet());

            for (String string : strings) {
                chars.retainAll(string.chars().boxed().collect(Collectors.toSet()));
            }

            sum += chars.size();
        }

        return sum;
    }
}
