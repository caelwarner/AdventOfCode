package com.caelwarner.adventofcode.twentytwenty.four;

import com.caelwarner.util.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class PassportProcessing {

    public static void main(String[] args) {
        ArrayList<String> input = Read.readToStringArrayList("adventofcode/twentytwenty/four/input.txt");

        System.out.println(processPassportsAdvanced(input));
    }

    private static int processPassports(ArrayList<String> input) {
        int count = 0;

        ArrayList<String> passports = generatePassports(input);

        for (String currentPassport : passports) {
            String[] passportProperties = currentPassport.split(" ");
            String[] passportKeys = new String[passportProperties.length];

            for (int i = 0; i < passportProperties.length; i++) {
                passportKeys[i] = passportProperties[i].split(":")[0];
            }

            List<String> list = Arrays.asList(passportKeys);

            if (list.containsAll(Arrays.asList("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"))) count++;
        }

        return count;
    }

    private static int processPassportsAdvanced(ArrayList<String> input) {
        int count = 0;

        ArrayList<String> passports = generatePassports(input);

        for (String currentPassport : passports) {
            String[] passportProperties = currentPassport.split(" ");
            HashMap<String, String> map = new HashMap<>();

            for (String passportProperty : passportProperties) {
                map.put(passportProperty.split(":")[0], passportProperty.split(":")[1]);
            }

            if (!map.keySet().containsAll(Arrays.asList("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"))) continue;

            if (map.get("byr").matches("^(19[2-9][0-9]|200[0-2])$") &&
                    map.get("iyr").matches("^(201[0-9]|2020)$") &&
                    map.get("eyr").matches("^(202[0-9]|2030)$") &&
                    map.get("hgt").matches("^(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)$") &&
                    map.get("hcl").matches("^#[0-9a-f]{6}$") &&
                    map.get("ecl").matches("^(amb|blu|brn|gry|grn|hzl|oth)$") &&
                    map.get("pid").matches("^[0-9]{9}$")) count++;
        }

        return count;
    }

    private static ArrayList<String> generatePassports(ArrayList<String> input) {
        ArrayList<String> passports = new ArrayList<>();
        String passport = "";

        for (String line : input) {
            passport += line + " ";

            if (line.equals("")) {
                passports.add(passport);
                passport = "";
            }
        }

        return passports;
    }


}
