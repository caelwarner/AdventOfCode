package twentytwenty.eleven;

import util.java.Read;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class SeatingSystem {

    private static final char occupiedSeat = "#".charAt(0);
    private static final char emptySeat = "L".charAt(0);
    private static final char floor = ".".charAt(0);

    public static void main(String[] args) {
        List<List<Character>> input = Read.as2DCharacterList("adventofcode/twentytwenty/eleven/input.txt");

        char[][] charInput = input.stream()
                .map(a -> a.stream()
                        .map(Objects::toString)
                        .collect(Collectors.joining())
                        .toCharArray())
                .toArray(char[][]::new);

        System.out.println(findOccupiedSeats(charInput));
    }

    private static int findOccupiedSeats(char[][] input) {
        char[][] nextRound = Arrays.stream(input).map(char[]::clone).toArray(char[][]::new);
        char[][] prevRound;

        do {
            prevRound = Arrays.stream(nextRound).map(char[]::clone).toArray(char[][]::new);

            for (int y = 0; y < prevRound.length; y++) {
                for (int x = 0; x < prevRound[y].length; x++) {
                    if (nextRound[y][x] == floor) {
                        continue;
                    }

                    int nearbyOccupiedSeats = getLineOfSightOccupiedSeats(prevRound, x, y);

                    if (nearbyOccupiedSeats == 0) {
                        nextRound[y][x] = occupiedSeat;

                    } else if (nearbyOccupiedSeats >= 5) {
                        nextRound[y][x] = emptySeat;
                    }
                }
            }

        } while (!equals(prevRound, nextRound));

        int occupiedSeats = 0;

        for (char[] row : prevRound) {
            for (char c : row) {
                if (c == occupiedSeat)
                    occupiedSeats++;
            }
        }

        return occupiedSeats;
    }

    private static int getLineOfSightOccupiedSeats(char[][] input, int x, int y) {
        int occupiedSeats = 0;

        for (int yOffset = -1; yOffset <= 1; yOffset++) {
            for (int xOffset = -1; xOffset <= 1; xOffset++) {
                if (yOffset == 0 && xOffset == 0) {
                    continue;
                }

                int multiplier = 1;

                try {
                    while (input[y + (yOffset * multiplier)][x + (xOffset * multiplier)] == floor) {
                        multiplier++;
                    }

                    if (input[y + (yOffset * multiplier)][x + (xOffset * multiplier)] == occupiedSeat) {
                        occupiedSeats++;
                    }

                } catch (IndexOutOfBoundsException ignored) {}
            }
        }

        return occupiedSeats;
    }

    private static int getNearbyOccupiedSeats(char[][] input, int x, int y) {
        int occupiedSeats = 0;

        for (int yOffset = -1; yOffset <= 1; yOffset++) {
            for (int xOffset = -1; xOffset <= 1; xOffset++) {

                if (yOffset == 0 && xOffset == 0)
                    continue;

                try {
                    if (input[y + yOffset][x + xOffset] == occupiedSeat)
                        occupiedSeats++;

                } catch (IndexOutOfBoundsException ignored) {}
            }
        }

        return occupiedSeats;
    }

    private static boolean equals(char[][] a1, char[][] a2) {
        if (a1 == null || a2 == null) {
            return false;
        }

        if (a1.length != a2.length) {
            return false;
        }

        for (int row = 0; row < a1.length; row++) {
            for (int column = 0; column < a1[row].length; column++) {
                if (a1[row][column] != a2[row][column]) {
                    return false;
                }
            }
        }

        return true;
    }

}
