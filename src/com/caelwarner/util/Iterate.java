package com.caelwarner.util;

public class Iterate {

	public static void over2DArray(int[][] grid, TriConsumer<Integer, Integer, Integer> action) {
		for (int row = 0; row < grid.length; row++) {
			for (int col = 0; col < grid[row].length; col++) {
				action.accept(grid[row][col], row, col);
			}
		}
	}

	public static <T> void over2DArray(T[][] grid, TriConsumer<T, Integer, Integer> action) {
		for (int row = 0; row < grid.length; row++) {
			for (int col = 0; col < grid[row].length; col++) {
				action.accept(grid[row][col], row, col);
			}
		}
	}

	public static <T> void around2DArray(T[][] grid, int row, int col, TriConsumer<T, Integer, Integer> action) {
		// Above
		if (row > 0)
			action.accept(grid[row - 1][col], row - 1, col);

		// Below
		if (row < grid.length - 1)
			action.accept(grid[row + 1][col], row + 1, col);

		// Left
		if (col > 0)
			action.accept(grid[row][col - 1], row, col - 1);

		// Right
		if (col < grid[0].length - 1)
			action.accept(grid[row][col + 1], row, col + 1);
	}

	public static <T> void around2DArrayDiagonally(T[][] grid, int row, int col, TriConsumer<T, Integer, Integer> action) {
		around2DArray(grid, row, col, action);

		// Above Left
		if (row > 0 && col > 0)
			action.accept(grid[row - 1][col - 1], row - 1, col - 1);

		// Above Right
		if (row > 0 && col < grid[0].length - 1)
			action.accept(grid[row - 1][col + 1], row - 1, col + 1);

		// Below Left
		if (row < grid.length - 1 && col > 0)
			action.accept(grid[row + 1][col - 1], row + 1, col - 1);

		// Below Right
		if (row < grid.length - 1 && col < grid[0].length - 1)
			action.accept(grid[row + 1][col + 1], row + 1, col + 1);
	}

}
