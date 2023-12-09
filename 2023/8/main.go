package main

import (
	"fmt"
	"utils"
	"strings"
	"regexp"
)

type node struct {
	left string
	right string
}

func getNode(line string, pattern string) (label, left, right string) {
	nodes := regexp.MustCompile(pattern).FindAllStringIndex(line, -1)
		label = line[nodes[0][0]: nodes[0][1]]
		left = line[nodes[1][0]: nodes[1][1]]
		right = line[nodes[2][0]: nodes[2][1]]

		return
}

func getNextNodeLabel(nodeMap map[string]node, instruction byte, currNodeLabel string) string {
	if instruction == 'L' {
		return nodeMap[currNodeLabel].left
	} else if instruction == 'R' {
		return nodeMap[currNodeLabel].right
	}
	panic("Invalid instruction provided")
}

func parseInput(startingNodePredicate func(string) bool) (instructions string, nodeMap map[string]node, startingNodes []string) {
	nodeMap = map[string]node{}
	lineChan := make(chan string)

	go utils.ReadInputLines("input.txt", lineChan)

	for line := range lineChan {
		if len(line) == 0 {
			continue
		}
		if strings.Contains(line, "=") {
			curr, l, r := getNode(line, `[A-Z]+`)

			currNode := node{left: l, right: r}
			nodeMap[curr] = currNode
			if startingNodePredicate(curr) {
				startingNodes = append(startingNodes, curr)
			}
		} else {
			instructions = line
		}
	}
	return 
}

func sol1() int {

	instructions, nodeMap, _ := parseInput(func (s string)bool { return false })

	i := 0
	currNode := "AAA"
	count := 0
	for currNode != "ZZZ" && count <= 1e6 {
		instruction := string(instructions[i])
		if instruction == "L" {
			currNode = nodeMap[currNode].left
		} else {
			currNode = nodeMap[currNode].right
		}
		count++
		i = (i+1) % len(instructions)
	}

	return count
}

type NodeIdxPair struct {
	node string
	idx int
}

func GCD(a, b int) int {
	for b != 0 {
			t := b
			b = a % b
			a = t
	}
	return a
}

// find Least Common Multiple (LCM) via GCD
func LCM(a, b int, integers ...int) int {
	result := a * b / GCD(a, b)

	return result
}

// This solution works only because the input maze has the very special property that the trajectory of every "starting node"
// (which will eventually become periodic) contains only exactly one "target node", so the first time each of them is hit simultaneously
// is the least common multiple of their distance to the start of the period in their respective trajectories.
func sol2() int {

	instructions, nodeMap, startingNodes := parseInput(func(s string)bool { return s[len(s)-1] == 'A'})

	out := 1

	for i := 0; i < len(startingNodes); i++ {
		currLabel := startingNodes[i]
		instructionIdx := 0
		curr := NodeIdxPair{currLabel, instructionIdx}
		visited := map[NodeIdxPair]int{}
		totalLength := 0
		for {
			visited[curr] = totalLength
			instruction := instructions[instructionIdx]
			currLabel = getNextNodeLabel(nodeMap, instruction, currLabel)
			instructionIdx = (instructionIdx+1) % len(instructions)
			totalLength++
			curr = NodeIdxPair{currLabel, instructionIdx}
			loopStart, ok := visited[curr]
			if ok {
				out = LCM(out, totalLength - loopStart)
				break
			}
		}
	}

	return out
}

func main() {
	fmt.Println(sol1())
}