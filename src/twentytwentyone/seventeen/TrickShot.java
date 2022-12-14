package twentytwentyone.seventeen;

import util.java.Read;

public class TrickShot {

	public static void main(String[] args) {
		String input = Read.asString("twentytwentyone/seventeen/input.txt");

		System.out.println(countInitialVelocities(input));
	}

	private static int solveHighestY(String input) {
		double y = Double.parseDouble(input.split("y=-")[1].split("\\.\\.")[0]);

		return (int) ((y) * ((y - 1) / 2));
	}

	private static int countInitialVelocities(String input) {
		int initialVelocities = 0;

		String[] targetArea = input.split("(,?\\s.=|\\.\\.)");

		int minX = Integer.parseInt(targetArea[1]);
		int maxX = Integer.parseInt(targetArea[2]);
		int minY = Integer.parseInt(targetArea[3]);
		int maxY = Integer.parseInt(targetArea[4]);

		for (int x = 0; x <= maxX; x++) {
			for (int y = Math.abs(minY); y >= minY; y--) {
				if (simulate(x, y, minX, maxX, minY, maxY)) {
					initialVelocities++;
				}
			}
		}

		return initialVelocities;
	}

	private static boolean simulate(int xVelocity, int yVelocity, int minX, int maxX, int minY, int maxY) {
		int x = 0;
		int y = 0;

		while (x <= maxX && y >= minY) {
			if (x >= minX && y <= maxY)
				return true;

			x += xVelocity;
			y += yVelocity;

			if (xVelocity > 0)
				xVelocity--;

			yVelocity--;
		}

		return false;
	}

}
