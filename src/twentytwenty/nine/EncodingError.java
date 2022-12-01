package twentytwenty.nine;

import util.java.Read;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class EncodingError {

    private static final List<Long> numbers = new ArrayList<>();

    public static void main(String[] args) {
        List<Long> input = Read.asLongList("adventofcode/twentytwenty/nine/input.txt");

        System.out.println(findEncryptionWeakness(input));
    }

    private static long findMissingSum(List<Long> input) {
        for (int i = 0; i < 25; i++) {
            numbers.add(input.get(i));
        }

        for (int i = 25; i < input.size(); i++) {
            long checkSum = input.get(i);
            boolean foundSum = false;

            loop:
            for (long number1 : numbers) {
                for (long number2 : numbers) {
                    if (number1 + number2 == checkSum && number1 != number2) {
                        foundSum = true;
                        break loop;
                    }
                }
            }

            if (!foundSum) {
                System.out.println("no sum");
                return checkSum;
            }

            numbers.remove(0);
            numbers.add(checkSum);
        }

        return 0;
    }

    private static long findEncryptionWeakness(List<Long> input) {
        long missingSum = findMissingSum(input);

        for (int i = 2; i < input.size(); i++) {
            for (int j = 0; j < input.size() - i; j++) {
                List<Long> contiguousSet = input.subList(j, j + i);

                long sum = contiguousSet.stream().mapToLong(Long::longValue).sum();

                if (sum == missingSum) {
                    return Collections.min(contiguousSet) + Collections.max(contiguousSet);
                }
            }
        }

        return 0;
    }

}
