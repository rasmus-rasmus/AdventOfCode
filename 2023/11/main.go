package main

import (
	"fmt"
	"utils"
	"strings"
	"regexp"
)

func getExpansions(universe []string) (rowExpansions, colExpansions []bool) {
	rowExpansions = make([]bool, len(universe[0]))
	colExpansions = make([]bool, len(universe))
	for i, row := range universe {
		if !strings.Contains(row, "#") {
			rowExpansions[i] = true
		}
	}
	for j := 0; j < len(universe[0]); j++ {
		isColExpansion := true
		for _, row := range universe {
			if string(row[j]) == "#" {
				isColExpansion = false
				break
			}
		}
		colExpansions[j] = isColExpansion
	}
	return
}

func computeL1Dist(gal1, gal2 [2]int, rowExpansions, colExpansions []bool, expansionFactor int) int {
	verticalDist, horizontalDist := 0, 0
	for i := min(gal1[0], gal2[0])+1; i <= max(gal1[0], gal2[0]); i++ {
		verticalDist++
		if rowExpansions[i] {
			verticalDist += expansionFactor-1
		}
	}
	for j:= min(gal1[1], gal2[1])+1; j <= max(gal1[1], gal2[1]); j++ {
		horizontalDist++
		if colExpansions[j] {
			horizontalDist += expansionFactor-1
		}
	}
	return verticalDist + horizontalDist
}

func sol1(inputFile string) int {
	lineChan := make(chan string)

	go utils.ReadInputLines(inputFile, lineChan)

	universe := []string{}
	galaxies := [][2]int{}

	rowIdx := 0
	for line := range lineChan {
		universe = append(universe, line)
		rowGalaxies := regexp.MustCompile("#").FindAllStringIndex(line, -1)
		for _, g := range rowGalaxies {
			galaxies = append(galaxies, [2]int{rowIdx, g[0]})
		}
		rowIdx++
	}
	rowExpansions, colExpansions := getExpansions(universe)

	sum := 0

	for i := 0; i < len(galaxies); i++ {
		for j := i+1; j < len(galaxies); j++ {
			sum += computeL1Dist(galaxies[i], galaxies[j], rowExpansions, colExpansions, 2)

		}
	}
	return sum
}

func sol2(inputFile string) int {
	lineChan := make(chan string)

	go utils.ReadInputLines(inputFile, lineChan)

	universe := []string{}
	galaxies := [][2]int{}

	rowIdx := 0
	for line := range lineChan {
		universe = append(universe, line)
		rowGalaxies := regexp.MustCompile("#").FindAllStringIndex(line, -1)
		for _, g := range rowGalaxies {
			galaxies = append(galaxies, [2]int{rowIdx, g[0]})
		}
		rowIdx++
	}
	rowExpansions, colExpansions := getExpansions(universe)

	sum := 0

	for i := 0; i < len(galaxies); i++ {
		for j := i+1; j < len(galaxies); j++ {
			sum += computeL1Dist(galaxies[i], galaxies[j], rowExpansions, colExpansions, 1000000)

		}
	}
	return sum
}

func main() {
	fmt.Println(sol2("input.txt"))
}