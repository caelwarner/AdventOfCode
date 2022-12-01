package util.java;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Read {

    public static List<String> asStringList(String path) {
        List<String> output = new ArrayList<>();

        try {
            File file = new File("src/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            while (bufferedReader.ready()) {
                output.add(bufferedReader.readLine());
            }

        } catch (IOException exception) {
            exception.printStackTrace();
        }

        return output;
    }

    public static List<List<Character>> as2DCharacterList(String path) {
        List<List<Character>> output = new ArrayList<>();

        try {
            File file = new File("src/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            output = bufferedReader.lines()
                    .map(line -> line.chars()
                            .mapToObj(c -> (char) c)
                            .collect(Collectors.toList()))
                    .collect(Collectors.toList());

        } catch (IOException exception) {
            exception.printStackTrace();
        }

        return output;
    }

    public static String asString(String path) {
        String output = "";

        try {
            File file = new File("src/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            output = bufferedReader.lines().collect(Collectors.joining("\n"));
        } catch (IOException exception) {
            exception.printStackTrace();
        }

        return output;
    }

    public static List<Integer> asIntList(String path) {
        List<Integer> output = new ArrayList<>();

        try {
            File file = new File("src/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            while (bufferedReader.ready()) {
                output.add(Integer.valueOf(bufferedReader.readLine()));
            }

        } catch (IOException exception) {
            exception.printStackTrace();
        }

        return output;
    }

    public static List<List<Integer>> as2DIntList(String path) {
        List<List<Integer>> output = new ArrayList<>();

        try {
            File file = new File("src/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            output = bufferedReader.lines()
                    .map(line -> line.chars()
                            .mapToObj(c -> c - '0')
                            .collect(Collectors.toList()))
                    .collect(Collectors.toList());

        } catch (IOException exception) {
            exception.printStackTrace();
        }

        return output;
    }

    public static List<Long> asLongList(String path) {
        List<Long> output = new ArrayList<>();

        try {
            File file = new File("src/" + path);
            BufferedReader bufferedReader = new BufferedReader(new FileReader(file));

            while (bufferedReader.ready()) {
                output.add(Long.parseLong(bufferedReader.readLine()));
            }

        } catch (IOException exception) {
            exception.printStackTrace();
        }

        return output;
    }

    public static Stream<String> asStringStream(String path) {
        return asStringList(path).stream();
    }

}
