package com.caelwarner.adventofcode.twentytwenty.eleven;

import com.caelwarner.util.Read;

import java.util.Arrays;
import java.util.List;

public class SeatingSystem {

    public static void main(String[] args) {
        List<String> input = Read.asStringArray("adventofcode/twentytwenty/eleven/input.txt");

        System.out.println(countOccupiedSeats(input));
    }

    private static int countOccupiedSeats(List<String> input) {
        char[][] map = new char[input.size() + 2][input.get(0).length() + 2];

        char floor = ".".charAt(0);
        char seat = "L".charAt(0);
        char occupied = "#".charAt(0);

        for (int i = 1; i < map.length - 1; i++) {
            
            map[i] = input.get(i).toCharArray();
        }

        char[][] mapMutable = Arrays.copyOf(map, map.length);

        do {
            map = Arrays.copyOf(mapMutable, mapMutable.length);

            for (int y = 1; y < map.length - 1; y++) {
                for (int x = 1; x < map[y].length - 1; x++) {
                    if (map[y][x] == seat) {

                        checkForEmptySeat:
                        for (int yOffset = -1; yOffset < 2; yOffset++) {
                            for (int xOffset = -1; xOffset < 2; xOffset++) {
                                if (map[y + yOffset][x + xOffset] != seat || map[y + yOffset][x + xOffset] != floor) {
                                    break checkForEmptySeat;
                                }

                                mapMutable[y][x] = occupied;
                            }
                        }

                    } else if (map[y][x] == occupied) {

                        int occupiedCount = 0;

                        for (int yOffset = -1; yOffset < 2; yOffset++) {
                            for (int xOffset = -1; xOffset < 2; xOffset++) {
                                if (map[y + yOffset][x + xOffset] == occupied) {
                                    occupiedCount++;
                                }

                                if (occupiedCount == 4) {
                                    mapMutable[y][x] = seat;
                                }
                            }
                        }
                    }
                }
            }
        } while (!Arrays.equals(map, mapMutable));

        System.out.println("thats cool");

        return 0;
    }

}
