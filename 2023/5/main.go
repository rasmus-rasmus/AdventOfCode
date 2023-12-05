package main

import (
	"fmt"
	"strings"
	"utils"
)

func extractAlmanac() (seeds []int, map_names []string, maps [][][]int) {
	lineChan := make(chan string)

	go utils.ReadInputLines("input.txt", lineChan)

	for line := range lineChan {
		if len(line) == 0 {
			continue
		}
		if line[:6] == "seeds:" {
			seeds = utils.GetNumeralsFromString(line[6:])
			continue
		}
		if !strings.Contains("0123456789", string(line[0])) {
			map_names = append(map_names, line)
			maps = append(maps, [][]int{})
			continue
		}
		maps[len(maps)-1] = append(maps[len(maps)-1], utils.GetNumeralsFromString(line))
	}
	return
}

func sol1() int {
	seeds, _, maps := extractAlmanac()

	closest_location := int(^uint(0) >> 1)

	for _, seed := range seeds {
		curr_seed := seed
		for _, m := range maps {
			for _, range_vals := range m {
				if curr_seed >= range_vals[1] && curr_seed < range_vals[1] + range_vals[2] {
					curr_seed = range_vals[0] + (curr_seed - range_vals[1])
					break
				}
			}
		}
		if curr_seed < closest_location {
			closest_location = curr_seed
		}
	}

	return closest_location
}

// NB: *Very* slow!!!
func sol2() int {
	seeds, _, maps := extractAlmanac()

	closest_location := int(^uint(0) >> 1)

	for i := 0; i < len(seeds)-1; i += 2 {
		for seed := seeds[i]; seed < seeds[i]+seeds[i+1]; seed++ {
			curr_seed := seed
			for _, m := range maps {
				for _, range_vals := range m {
					if curr_seed >= range_vals[1] && curr_seed < range_vals[1] + range_vals[2] {
						curr_seed = range_vals[0] + (curr_seed - range_vals[1])
						break
					}
				}
			}
			if curr_seed < closest_location {
				closest_location = curr_seed
			}
		}
	}

	return closest_location
}

func main() {
	fmt.Println(sol2())
}