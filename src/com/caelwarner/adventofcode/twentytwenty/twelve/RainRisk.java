package com.caelwarner.adventofcode.twentytwenty.twelve;

import com.caelwarner.util.Read;

import java.util.List;

public class RainRisk {

	public static void main(String[] args) {
		List<String> input = Read.asStringList("adventofcode/twentytwenty/twelve/input.txt");

		System.out.println(findWaypointManhattanDistance(input));
	}

	private static int findWaypointManhattanDistance(List<String> input) {
		int shipX = 0;
		int shipY = 0;

		int waypointX = 10;
		int waypointY = 1;

		for (String command : input) {
			String prefix = command.substring(0, 1);
			int amount = Integer.parseInt(command.substring(1));

			if (prefix.equals("N")) {
				waypointY += amount;

			} else if (prefix.equals("E")) {
				waypointX += amount;

			} else if (prefix.equals("S")) {
				waypointY -= amount;

			} else if (prefix.equals("W")) {
				waypointX -= amount;

			} else if (prefix.equals("F")) {
				shipX += waypointX * amount;
				shipY += waypointY * amount;

			} else if (prefix.equals("L") || prefix.equals("R")) {
				int rotation = prefix.equals("L") ? Math.floorMod(-amount, 360) : amount;

				int oldWaypointX = waypointX;
				int oldWaypointY = waypointY;

				if (rotation == 90) {
					waypointX = oldWaypointY;
					waypointY = -oldWaypointX;

				} else if (rotation == 180) {
					waypointX = -oldWaypointX;
					waypointY = -oldWaypointY;

				} else if (rotation == 270) {
					waypointX = -oldWaypointY;
					waypointY = oldWaypointX;

				}
			}
		}

		return Math.abs(shipX) + Math.abs(shipY);
	}

	private static int findManhattanDistance(List<String> input) {
		int facing = 90;

		int xPos = 0;
		int yPos = 0;

		for (String command : input) {
			String prefix = command.substring(0, 1);
			int amount = Integer.parseInt(command.substring(1));

			System.out.println(facing);

			if (prefix.equals("N") || (prefix.equals("F") && facing == 0)) {
				yPos += amount;

			} else if (prefix.equals("E") || (prefix.equals("F") && facing == 90)) {
				xPos += amount;

			} else if (prefix.equals("S") || (prefix.equals("F") && facing == 180)) {
				yPos -= amount;

			} else if (prefix.equals("W") || (prefix.equals("F") && facing == 270)) {
				xPos -= amount;

			} else if (prefix.equals("L")) {
				facing -= amount;
				facing = Math.floorMod(facing, 360);

			} else if (prefix.equals("R")) {
				facing += amount;
				facing = Math.floorMod(facing, 360);

			}
		}

		return Math.abs(xPos) + Math.abs(yPos);
	}

}
