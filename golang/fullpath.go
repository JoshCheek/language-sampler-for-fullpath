package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type Invocation struct {
	fullpaths []string
	doHelp    bool
	doCopy    bool
}

func main() {
	invocation := parseArgs(os.Args, os.Stdin)
	doMain(invocation, os.Stdout)
}

func doMain(invocation Invocation, outstream io.Writer) {
	if invocation.doHelp {
		printHelp(outstream)
	} else {
		pathsString := join(invocation.fullpaths, "\n")
		if 1 < len(invocation.fullpaths) {
			pathsString += "\n"
		}
		fmt.Fprint(outstream, pathsString)
		if invocation.doCopy {
			copyToClipboard(pathsString)
		}
	}
}

func parseArgs(args []string, instream io.Reader) Invocation {
	argvFullPaths := mapToFullPaths(selectPaths(args[1:]))
	fullpaths := argvFullPaths
	if 0 == len(argvFullPaths) {
		fullpaths = mapToFullPaths(readlines(instream))
	}
	return Invocation{
		fullpaths: fullpaths,
		doHelp:    doHelp(args),
		doCopy:    doCopy(args),
	}
}

func printHelp(outstream io.Writer) {
	fmt.Fprintln(outstream, "usage: fullpath *[relative-paths] [-c]")
	fmt.Fprintln(outstream, "")
	fmt.Fprintln(outstream, "  Prints the fullpath of the paths")
	fmt.Fprintln(outstream, "  If no paths are given as args, it will read them from stdin")
	fmt.Fprintln(outstream, "")
	fmt.Fprintln(outstream, "  If there is only one path, the trailing newline is omitted")
	fmt.Fprintln(outstream, "")
	fmt.Fprintln(outstream, "  The -c flag will copy the results into your pasteboard")
}

func includes(haystack []string, needles ...string) bool {
	for _, hay := range haystack {
		for _, needle := range needles {
			if hay == needle {
				return true
			}
		}
	}
	return false
}

func doHelp(args []string) bool {
	return includes(args, "-h", "--help")
}

func doCopy(args []string) bool {
	return includes(args, "-c", "--copy")
}

func mapToFullPaths(relativePaths []string) []string {
	paths := []string{}
	for _, relPath := range relativePaths {
		if relPath != "" {
			absPath, _ := filepath.Abs(relPath)
			paths = append(paths, absPath)
		}
	}
	return paths
}

func readlines(stream io.Reader) []string {
	reader := bufio.NewReader(stream)
	lines := []string{}
	for true {
		line, metadata := reader.ReadString('\n')
		lines = append(lines, strings.Trim(line, "\r\n")) // trims both \r and \n
		if metadata == io.EOF {
			break
		}
	}
	return lines
}

func selectPaths(args []string) []string {
	paths := []string{}
	for _, maybePath := range args {
		if !isFlag(maybePath) {
			paths = append(paths, maybePath)
		}
	}
	return paths
}

func isFlag(maybeFlag string) bool {
	return maybeFlag == "-c" || maybeFlag == "--copy" ||
		maybeFlag == "-h" || maybeFlag == "--help"
}

func join(strs []string, delimiter string) string {
	joined := ""
	for index, str := range strs {
		if index != 0 {
			joined += delimiter
		}
		joined += str
	}
	return joined
}

func copyToClipboard(str string) {
	cmd := exec.Command("pbcopy")
	stdin, _ := cmd.StdinPipe()
	cmd.Start()
	fmt.Fprint(stdin, str)
	stdin.Close()
	cmd.Wait()
}
