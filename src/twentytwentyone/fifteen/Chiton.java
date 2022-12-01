package twentytwentyone.fifteen;

import util.java.Convert;
import util.java.Iterate;
import util.java.Read;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Chiton {

	public static void main(String[] args) {
		List<List<Integer>> input = Read.as2DIntList("adventofcode/twentytwentyone/fifteen/input.txt");

		System.out.println(findLowestRiskPathLarge(input));
	}

	/**
	 * A* Pathfinding algorithm with no diagonal movement and risk level applied as a movement penalty
	 */
	private static int findLowestRiskPath(List<List<Integer>> input) {
		Node[][] map = Convert.to2DArray(Node.class, input, Node::new);

		Node start = map[0][0];
		Node end = map[map.length - 1][map[0].length - 1];

		Set<Node> open = new HashSet<>();
		Set<Node> closed = new HashSet<>();
		open.add(start);

		while (true) {
			Node current = open.stream().min(Comparator.comparing(Node::fCost)).orElseThrow();

			open.remove(current);
			closed.add(current);

			if (current.equals(end))
				break;

			Iterate.around2DArray(map, current.y, current.x, (neighbour, row, col) -> {
				if (!closed.contains(neighbour)) {
					int movementCost = current.gCost + neighbour.riskLevel + 1;

					if (movementCost < neighbour.gCost || !open.contains(neighbour)) {
						neighbour.gCost = movementCost;
						neighbour.hCost = neighbour.distance(end);

						neighbour.parent = current;
						open.add(neighbour);
					}
				}
			});
		}

		return end.pathRiskLevel();
	}

	private static int findLowestRiskPathLarge(List<List<Integer>> input) {
		expandMap(input);
		return findLowestRiskPath(input);
	}

	private static void expandMap(List<List<Integer>> input) {
		int height = input.size();
		int width = input.get(0).size();

		// Expand right to five times original width
		for (int i = 0; i < 4; i++) {
			for (List<Integer> row : input) {
				for (int col = 0; col < width; col++) {
					int newRiskLevel = row.get(row.size() - width) == 9 ? 1 : row.get(row.size() - width) + 1;
					row.add(newRiskLevel);
				}
			}
		}

		// Expand down to five times original height
		for (int i = 0; i < 4; i++) {
			for (int row = 0; row < height; row++) {
				List<Integer> newRow = new ArrayList<>();

				for (int col : input.get(input.size() - height)) {
					int newRiskLevel = col == 9 ? 1 : col + 1;
					newRow.add(newRiskLevel);
				}

				input.add(newRow);
			}
		}
	}

	private static class Node {

		public int riskLevel;
		public int y;
		public int x;

		public Node parent;
		public int gCost = 0;
		public int hCost = 0;

		public Node(int riskLevel, int y, int x) {
			this.riskLevel = riskLevel;
			this.y = y;
			this.x = x;
		}

		public int fCost() {
			return gCost + hCost;
		}

		public int distance(Node from) {
			int y = Math.abs(from.y - this.y);
			int x = Math.abs(from.x - this.x);

			return y + x;
		}

		public int pathRiskLevel() {
			return parent == null ? 0 : parent.pathRiskLevel() + riskLevel;
		}

	}

}
