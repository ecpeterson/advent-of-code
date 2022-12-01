# Advent of Code

[Advent of Code](https://adventofcode.com) is a yearly set of puzzle problems
released through early December.  Here are some solutions, organized by year.

## Usage

To compile and run, do something like:

```shell
$ erlc +debug_info day_1.erl
$ dialyzer --add_to_plt day_1.beam
$ erl -noshell -run day_1 start "day_1.dat" 3
```
