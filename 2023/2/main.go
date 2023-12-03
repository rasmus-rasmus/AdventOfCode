package main

import (
	"fmt"
	"os"
	"bufio"
	"regexp"
	"strconv"
)

type rgbConfig struct {
	game  int // -1 means bag config
	red   int
	green int
	blue  int
}

func getBagConfig() rgbConfig {
	return rgbConfig{-1, 12, 13, 14}
}

func getSampleConfigs(line string) []rgbConfig {
	semicolons := regexp.MustCompile(`;`).FindAllStringIndex(line, -1)
	out := make([]rgbConfig, 0)
	
	game := regexp.MustCompile(`Game \d+:`).FindString(line)
	game_number, err := strconv.Atoi(regexp.MustCompile(`\d+`).FindString(game))
	if err != nil {
		panic(err)
	}
	for i:= 0; i < len(semicolons)+1; i++ {
		var curr, next int
		if i == 0 {
			curr = 0
			next = semicolons[i][0]
		} else if i == len(semicolons) {
			curr = semicolons[i-1][0]
			next = len(line)
		} else {
			curr = semicolons[i-1][0]
			next = semicolons[i][0]
		}
		sample := line[curr: next]

		red := regexp.MustCompile(`\d+ red`).FindString(sample)
		red_number, err := strconv.Atoi(regexp.MustCompile(`\d+`).FindString(red))
		if err != nil {
			red_number = 0
		}

		green := regexp.MustCompile(`\d+ green`).FindString(sample)
		green_number, err := strconv.Atoi(regexp.MustCompile(`\d+`).FindString(green))
		if err != nil {
			green_number = 0
		}

		blue := regexp.MustCompile(`\d+ blue`).FindString(sample)
		blue_number, err := strconv.Atoi(regexp.MustCompile(`\d+`).FindString(blue))
		if err != nil {
			blue_number = 0
		}
		out = append(out, rgbConfig{game_number, red_number, green_number, blue_number})
	}
	return out
}

func sol1() int {
	f, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	sum := 0

	bagConfig := getBagConfig()

	for scanner.Scan() {
		line := scanner.Text()
		configs := getSampleConfigs(line)
		game_number := configs[0].game
		possible := true
		for _, c := range configs {
			if c.red > bagConfig.red || c.green > bagConfig.green || c.blue > bagConfig.blue {
				possible = false
				break
			}
		}
		if possible {
			sum += game_number
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

	sum := 0


	for scanner.Scan() {
		line := scanner.Text()
		configs := getSampleConfigs(line)
		min_red, min_green, min_blue := 0, 0, 0
		for _, c := range configs {
			if c.red > min_red {
				min_red = c.red
			}
			if c.green > min_green {
				min_green = c.green
			}
			if c.blue > min_blue {
				min_blue = c.blue
			}
		}
		sum += min_red * min_green * min_blue
	}

	return sum
}

func main() {
	fmt.Println(sol2())
}