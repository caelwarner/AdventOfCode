package twentytwenty.twentythree;

import util.java.Read;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class CrabCups {

	public static void main(String[] args) {
		List<Integer> input = Read.asIntList("adventofcode/twentytwenty/twentythree/input.txt");

		System.out.println(findStars(input));
	}

	private static String getCupLabels(List<Integer> input) {
		StringBuilder label = new StringBuilder();
		Map<Integer, Node> lookup = new HashMap<>();
		Node[] pickUp = new Node[3];

		Node current = parseInput(input, lookup);

		for (int i = 0; i < 100; i++) {
			current = completeMove(current, lookup, pickUp, 9);
		}

		Node one = lookup.get(1);
		for (int i = 0; i < 8; i++) {
			one = one.next;
			label.append(one.value);
		}

		return label.toString();
	}

	private static long findStars(List<Integer> input) {
		Map<Integer, Node> lookup = new HashMap<>();
		Node[] pickUp = new Node[3];

		Node current = parseInput(input, lookup);
		Node end = current.previous;

		for (int i = 1000000; i >= 10; i--) {
			Node node = new Node(i);

			end.insertNext(node);
			lookup.put(node.value, node);
		}

		for (int i = 0; i < 10000000; i++) {
			current = completeMove(current, lookup, pickUp, 1000000);
		}

		Node one = lookup.get(1);

		return (long) one.next.value * (long) one.next.next.value;
	}

	private static Node parseInput(List<Integer> input, Map<Integer, Node> lookup) {
		Node current = new Node(input.get(0));
		Node end = current;

		lookup.put(current.value, current);

		for (int i = 1; i < input.size(); i++) {
			end.insertNext(new Node(input.get(i)));
			end = end.next;
			lookup.put(end.value, end);
		}

		current.previous = end;
		end.next = current;

		return current;
	}

	private static Node completeMove(Node current, Map<Integer, Node> lookup, Node[] pickUp, int maxValue) {
		int destinationValue = current.value;

		for (int i = 0; i < 3; i++)
			pickUp[i] = current.removeNext();

		Node destination;

		loop:
		while (true) {
			if (--destinationValue == 0)
				destinationValue = maxValue;

			for (Node node : pickUp) {
				if (node.value == destinationValue)
					continue loop;
			}

			destination = lookup.get(destinationValue);
			break;
		}

		for (int i = 2; i >= 0; i--)
			destination.insertNext(pickUp[i]);

		return current.next;
	}

	private static class Node {

		public Node next;
		public Node previous;
		public int value;

		public Node(int value) {
			this.value = value;
		}

		public void insertNext(Node node) {
			node.previous = this;

			if (next != null) {
				node.next = next;
				next.previous = node;
			}

			next = node;
		}

		public Node removeNext() {
			Node remove = next;

			remove.next.previous = this;
			next = remove.next;

			return remove;
		}

	}
}
