package util.java;

import java.lang.reflect.Array;
import java.util.List;

public class Convert {

	public static int[][] to2DArray(List<List<Integer>> input) {
		return input.stream().map(list -> list.stream().mapToInt(i -> i).toArray()).toArray(int[][]::new);
	}

	@SuppressWarnings("unchecked")
	public static <T, U> T[][] to2DArray(Class<T> clazz, List<List<U>> input, TriFunction<U, Integer, Integer, T> function) {
		T[][] array2d = (T[][]) Array.newInstance(clazz, input.size(), input.get(0).size());

		for (int row = 0; row < array2d.length; row++) {
			T[] array = (T[]) Array.newInstance(clazz, input.get(row).size());

			for (int col = 0; col < array.length; col++) {
				array[col] = function.apply(input.get(row).get(col), row, col);
			}

			array2d[row] = array;
		}

		return array2d;
	}

}
