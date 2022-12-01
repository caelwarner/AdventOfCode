package twentytwentyone.four;

import util.java.Read;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class GiantSquid {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwentyone/four/input.txt");

		System.out.println(getLosingScore(input));
	}

	private static int getWinningScore(List<String> input) {
		int[] numbers = Arrays.stream(input.get(0).split(",")).mapToInt(Integer::parseInt).toArray();
		List<BingoBoard> boards = new ArrayList<>();

		// Create all bingo boards
		for (int i = 2; i < input.size(); i += 6) {
			boards.add(new BingoBoard(input.subList(i, i + 5)));
		}

		for (int number : numbers) {
			for (BingoBoard board : boards) {
				board.checkForNumber(number);

				if (board.checkIfWon()) {
					return number * Arrays.stream(board.board).mapToInt(row -> Arrays.stream(row).filter(tile -> !tile.isMarked()).mapToInt(tile -> tile.value).sum()).sum();
				}
			}
		}

		return 0;
	}

	private static int getLosingScore(List<String> input) {
		int[] numbers = Arrays.stream(input.get(0).split(",")).mapToInt(Integer::parseInt).toArray();
		List<BingoBoard> boards = new ArrayList<>();
		int score = 0;

		// Create all bingo boards
		for (int i = 2; i < input.size(); i += 6) {
			boards.add(new BingoBoard(input.subList(i, i + 5)));
		}

		for (int number : numbers) {
			for (BingoBoard board : boards) {
				if (board.hasWon) {
					continue;
				}

				board.checkForNumber(number);

				if (board.checkIfWon()) {
					score = number * Arrays.stream(board.board).mapToInt(row -> Arrays.stream(row).filter(tile -> !tile.isMarked()).mapToInt(tile -> tile.value).sum()).sum();
				}
			}
		}

		return score;
	}

	private static class BingoBoard {
		public final BingoTile[][] board = new BingoTile[5][5];
		public boolean hasWon = false;

		public BingoBoard(List<String> input) {
			for (int y = 0; y < board.length; y++) {
				for (int x = 0; x < board[y].length; x++) {
					board[y][x] = new BingoTile(Integer.parseInt(Arrays.stream(input.get(y).split(" ")).filter(value -> !value.equals("")).collect(Collectors.toList()).get(x)));
				}
			}
		}

		public void checkForNumber(int value) {
			for (BingoTile[] row : board) {
				for (BingoTile tile : row) {
					if (tile.value == value) {
						tile.mark();
					}
				}
			}
		}

		public boolean checkIfWon() {
			// Check rows for win
			rows:
			for (BingoTile[] row : board) {
				for (BingoTile tile : row) {
					if (!tile.isMarked()) {
						continue rows;
					}
				}

				this.hasWon = true;
				return true;
			}

			// Check columns for wins
			columns:
			for (int x = 0; x < board.length; x++) {
				for (int y = 0; y < board[x].length; y++) {
					if (!board[y][x].isMarked()) {
						continue columns;
					}
				}

				this.hasWon = true;
				return true;
			}

			return false;
		}

	}

	private static class BingoTile {

		private boolean marked = false;
		public final int value;

		public BingoTile(int value) {
			this.value = value;
		}

		public void mark() {
			this.marked = true;
		}

		public boolean isMarked() {
			return this.marked;
		}
	}

}
