#!/usr/bin/env jruby -S mirah

import java.util.ArrayList
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.io.InputStream
import java.lang.Runtime

class Fullpath
  def initialize(dir:String, argv:ArrayList, instream:InputStream, outstream:PrintStream)
    @dir         = dir
    @instream    = instream
    @outstream   = outstream
    @print_help  = false
    @copy_output = false
    @paths       = ArrayList.new
    argv.each do |arg|
      if arg == "-h" || arg == "--help"
        @print_help = true
      elsif arg == "-c" || arg == "--copy"
        @copy_output = true
      elsif arg != ""
        @paths.add arg
      end
    end
  end

  def call
    if help?
      print_help @outstream
      return
    end

    if @paths.size == 0
      @paths = read_lines @instream
    end

    if copy?
      process = Runtime.getRuntime.exec("pbcopy");
      writer  = PrintStream.new process.getOutputStream
      print_paths writer
      writer.close
    end

    print_paths @outstream
  end

  def print_paths(outstream:PrintStream)
    if @paths.size == 1
      outstream.print "#{@dir}/#{@paths[0]}"
    else
      @paths.each do |path|
        outstream.puts "#{@dir}/#{path}"
      end
    end
  end

  def print_help(oustream:PrintStream)
    outstream.puts "usage: fullpath *[relative-paths] [-c]"
    outstream.puts ""
    outstream.puts "  Prints the fullpath of the paths"
    outstream.puts "  If no paths are given as args, it will read them from stdin"
    outstream.puts ""
    outstream.puts "  If there is only one path, the trailing newline is omitted"
    outstream.puts ""
    outstream.puts "  The -c flag will copy the results into your pasteboard"
  end

  def help?
    @print_help
  end

  def copy?
    @copy_output
  end

  def read_lines(instream:InputStream)
    reader = BufferedReader.new InputStreamReader.new instream
    lines  = []
    loop do
      line = reader.readLine
      break if !line
      lines << line
    end
    lines
  end
end

dir = System.getProperty("user.dir")

# The ARGV constant is only available when run through jruby
# but it takes way longer when I do it that way
# so just defining the constant for now
argv = ArrayList.new
argv.add("a")
argv.add("b")
argv.add("")
argv.add("c")
Fullpath.new(dir, argv, System.in, System.out).call

argv = ArrayList.new
argv.add("-h")
Fullpath.new(dir, argv, System.in, System.out).call

argv = ArrayList.new
Fullpath.new(dir, argv, System.in, System.out).call

argv = ArrayList.new
argv.add("a")
argv.add("-c")
Fullpath.new(dir, argv, System.in, System.out).call
