package main

import (
	"fmt"
	"os"
	"bufio"
	"regexp"
	"strconv"
)

func getCharacter(idx int, line string) string {
	if idx < 0 || idx >= len(line) {
		return "."
	}
	return string(line[idx])
}

func getSubstring(firstIdx int, secondIdx int, line string) string {
	if line == "" {
		return ""
	}
	if firstIdx < 0 {
		firstIdx = 0
	}
	if secondIdx >= len(line) {
		secondIdx = len(line) - 1
	}
	return line[firstIdx: secondIdx]
}

func sol1() int {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	lines := make([]string, 0)
	
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	sum := 0

	for i := 0; i < len(lines); i++ {
		var prevLine, currLine, nextLine string
		currLine = lines[i]
		if i == 0 {
			prevLine = ""
		} else
		{
			prevLine = lines[i-1]
		}
		if i == len(lines) - 1 {
			nextLine = ""
		} else {
			nextLine = lines[i+1]
		}

		numerals := regexp.MustCompile(`\d+`).FindAllStringIndex(currLine, -1)
		

		for _, idc := range numerals {
			if getCharacter(idc[0]-1, currLine) != "." || getCharacter(idc[1], currLine) != "." {
				val, err := strconv.Atoi(currLine[idc[0]: idc[1]])
				if err == nil {
					sum += val
				}
				continue
			}
			if regexp.MustCompile(`[^0-9.]`).FindString(getSubstring(idc[0]-1, idc[1]+1, prevLine)) != "" {
				val, err := strconv.Atoi(currLine[idc[0]: idc[1]])
				if err == nil {
					sum += val
				}
				continue
			}
			if regexp.MustCompile(`[^0-9.]`).FindString(getSubstring(idc[0]-1, idc[1]+1, nextLine)) != "" {
				val, err := strconv.Atoi(currLine[idc[0]: idc[1]])
				if err == nil {
					sum += val
				}
				continue
			}
		}
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

	lines := make([]string, 0)
	numeralsPerLine := make([][][]int, 0)
	
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
		numerals := regexp.MustCompile(`\d+`).FindAllStringIndex(lines[len(lines) - 1], -1)
		numeralsPerLine = append(numeralsPerLine, numerals)
	}

	sum := 0

	for i := 0; i < len(lines); i++ {
		var prevLine, currLine, nextLine string
		var prevNumerals, currNumerals, nextNumerals [][]int
		currLine = lines[i]
		currNumerals = numeralsPerLine[i]
		if i == 0 {
			prevLine = ""
			prevNumerals = [][]int{}
			} else
		{
			prevLine = lines[i-1]
			prevNumerals = numeralsPerLine[i-1]
		}
		if i == len(lines) - 1 {
			nextLine = ""
			nextNumerals = [][]int{}
			} else {
				nextLine = lines[i+1]
				nextNumerals = numeralsPerLine[i+1]
			}
			
		gears:= regexp.MustCompile(`\*`).FindAllStringIndex(currLine, -1)
		

		for _, idc := range gears {
			neighbourNumerals := []int{}
			for _, numeralIdc := range prevNumerals {
				if numeralIdc[1] >= idc[0] && numeralIdc[0] <= idc[1] {
					val, err := strconv.Atoi(prevLine[numeralIdc[0]: numeralIdc[1]])
					if err == nil {
						neighbourNumerals = append(neighbourNumerals, val)
					}
				}
			}
			for _, numeralIdc := range currNumerals {
				if numeralIdc[1] == idc[0] || numeralIdc[0] == idc[1] {
					val, err := strconv.Atoi(currLine[numeralIdc[0]: numeralIdc[1]])
					if err == nil {
						neighbourNumerals = append(neighbourNumerals, val)
					}
				}
			} 
			for _, numeralIdc := range nextNumerals {
				if numeralIdc[1] >= idc[0] && numeralIdc[0] <= idc[1] {
					val, err := strconv.Atoi(nextLine[numeralIdc[0]: numeralIdc[1]])
					if err == nil {
						neighbourNumerals = append(neighbourNumerals, val)
					}
				}
			}

			if len(neighbourNumerals) == 2 {
				gearRatio := neighbourNumerals[0] * neighbourNumerals[1]
				sum += gearRatio
			}
		}
	}

	return sum
}

func main() {
	fmt.Println(sol2())
}