# Advent Of FPGA

This repository contains solutions to the Advent of Code (https://adventofcode.com/) written in Hardcaml by me, as part of the Advent of FPGA (https://blog.janestreet.com/advent-of-fpga-challenge-2025/) challenge by Jane Street.

## How to run
Clone this repository locally by running the following command 
```zsh
git clone https://github.com/dronedangwal/advent-of-fpga-2025.git
```

`cd` into the day of choice from Advent of Code, edit `input.txt` to contain a valid input of your choice, and run 
```zsh
dune runtest
```

## Solution Walkthroughs

### Day 01

Every line of the input is passed into the circuit as two 16 bit input ports, corresponding to the direction (L or R) and the number of clicks. The circuit computes the number of crossings at zero as `clicks // 100` + 1 if `clicks % 100` number of clicks in the said direction would make it cross zero and `clicks // 100` otherwise. Division is done using a combinational circuit described in `day01/src/day01.ml`.
