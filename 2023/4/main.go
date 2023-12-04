package main

import (
	"fmt"
	"os"
	"bufio"
	"regexp"
	"strings"
	"strconv"
	"slices"
)

func getWinningNumbers(line string) []int {
	colonIdx := regexp.MustCompile(":").FindAllStringIndex(line, 1)
	pipeIdx := regexp.MustCompile(`\|`).FindAllStringIndex(line, 1)
	numberStrings := strings.Fields(line[colonIdx[0][0]+1: pipeIdx[0][0]-1])

	numerals := make([]int, len(numberStrings))
	for idx, n := range numberStrings {
		num, _ := strconv.Atoi(n)
		numerals[idx] = num
	}
	return numerals
}

func getGotNumbers(line string) []int {
	pipeIdx := regexp.MustCompile(`\|`).FindAllStringIndex(line, 1)
	numberStrings := strings.Fields(line[pipeIdx[0][0]+1:])

	numerals := make([]int, len(numberStrings))
	for idx, n := range numberStrings {
		num, _ := strconv.Atoi(n)
		numerals[idx] = num
	}
	return numerals
}

func sol1() int {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	sum := 0
	
	for scanner.Scan() {
		line := scanner.Text()
		winningNumbers := getWinningNumbers(line)
		gotNumbers := getGotNumbers(line)
		currCardPoints := 0
		for _, gotNum := range gotNumbers {
			if slices.Contains(winningNumbers, gotNum) {
				if currCardPoints == 0 {
					currCardPoints = 1
				} else {
					currCardPoints *= 2
				}
			}
		}
		sum += currCardPoints
	}
	return sum
}

func sol2() int {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	sum := 0
	copies := make([]int, 216)
	for i := 0; i < 216; i++ {
		copies[i] = 1
	}
	
	cardNumber := 0
	for scanner.Scan() {
		if cardNumber >= 216{
			break
		}
		line := scanner.Text()
		sum += copies[cardNumber]
		winningNumbers := getWinningNumbers(line)
		gotNumbers := getGotNumbers(line)
		matchingNumbers := 0
		for _, gotNum := range gotNumbers {
			if slices.Contains(winningNumbers, gotNum) {
				matchingNumbers++
			}
		}
		for i := cardNumber+1; i <= cardNumber+matchingNumbers; i++{
		}

		for i := cardNumber+1; i<=cardNumber+matchingNumbers; i++ {
			if i >= len(copies){
				break
			}
			copies[i] += copies[cardNumber]
		}
		for i := cardNumber+1; i <= cardNumber+matchingNumbers; i++{
		}
		cardNumber++
	}
	return sum
}

func main() {
	fmt.Println(sol2())
}