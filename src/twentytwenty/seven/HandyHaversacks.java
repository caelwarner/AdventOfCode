package twentytwenty.seven;

import util.java.Read;

import java.util.*;
import java.util.stream.Collectors;

public class HandyHaversacks {

    private static final List<String> checkedBags = new ArrayList<>();
    private static final List<Node> checkedNodes = new ArrayList<>();

    public static void main(String[] args) {
        List<String> input = Read.asStringList("adventofcode/twentytwenty/seven/input.txt");

        System.out.println(findBagsIn(input, Collections.singletonList(new Node("shiny gold", 1, null))));

        int count = 0;

        for (Node node : checkedNodes) {
            count += node.calculateNumBags();
        }

        System.out.println(checkedNodes);
        System.out.println(count);
    }

    private static int findBags(List<String> input, List<String> bags) {
        List<List<String>> inputList = input.stream().map(s -> Arrays.asList(s.split(" "))).collect(Collectors.toList());
        List<String> checkNextBags = new ArrayList<>();

        for (String bag : bags) {
            String[] bagSplit = bag.split(" ");

            for (List<String> strings : inputList) {
                for (int i = 4; i < strings.size(); i++) {
                    if (strings.get(i).equals(bagSplit[0]) && strings.get(i + 1).equals(bagSplit[1])) {
                        String bagString = strings.get(0) + " " + strings.get(1);

                        if (!checkedBags.contains(bagString)) {
                            checkedBags.add(bagString);
                            checkNextBags.add(bagString);
                        }
                    }
                }
            }
        }

        if (checkNextBags.size() > 0) {
            findBags(input, checkNextBags);
        }

        return checkedBags.size();
    }

    private static int findBagsIn(List<String> input, List<Node> bags) {
        List<List<String>> inputList = input.stream().map(s -> Arrays.asList(s.split(" "))).collect(Collectors.toList());
        List<Node> checkNextBags = new ArrayList<>();

        bags.forEach(node -> {
            String[] bagSplit = node.bag.split(" ");

            for (List<String> strings : inputList) {
                if (strings.get(0).equals(bagSplit[0]) && strings.get(1).equals(bagSplit[1])) {
                    if (!strings.get(4).equals("no")) {
                        for (int i = 1; i < strings.size() / 4; i++) {
                            String bagString = strings.get(i * 4 + 1) + " " + strings.get(i * 4 + 2);

                            checkNextBags.add(new Node(bagString, Integer.parseInt(strings.get(i * 4)), node));
                            checkedNodes.add(new Node(bagString, Integer.parseInt(strings.get(i * 4)), node));
                        }
                    }
                }
            }
        });

        System.out.println(checkNextBags);

        if (checkNextBags.size() > 0) {
            findBagsIn(input, checkNextBags);
        }

        return 0;
    }

    private static class Node {
        public String bag;
        public int bagNum;
        public Node parent;

        public Node(String bag, int bagNum, Node parent) {
            this.bag = bag;
            this.bagNum = bagNum;
            this.parent = parent;
        }

        public int calculateNumBags() {
            return parent != null ? bagNum * parent.calculateNumBags() : bagNum;
        }
    }

}
