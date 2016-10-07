#!/usr/bin/env jruby -S mirah

import java.util.ArrayList
import java.util.List
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.io.InputStream
import java.lang.Runtime
import java.util.Arrays

def self.main(argv:String[]):void
  dir = System.getProperty("user.dir")
  Fullpath.new(dir, Arrays.asList(argv), System.in, System.out).call
end

class Fullpath
  def initialize(dir:String, argv:List, instream:InputStream, outstream:PrintStream)
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
      @paths = read_nonempty_lines  @instream
    end

    if copy?
      process = Runtime.getRuntime.exec("pbcopy");
      writer  = PrintStream.new process.getOutputStream
      print_paths writer
      writer.close
    end

    print_paths @outstream
  end

  def print_paths(stream:PrintStream)
    if @paths.size == 1
      stream.print "#{@dir}/#{@paths[0]}"
    else
      @paths.each do |path|
        stream.println "#{@dir}/#{path}"
      end
    end
  end

  def print_help(oustream:PrintStream)
    outstream.println "usage: fullpath *[relative-paths] [-c]"
    outstream.println ""
    outstream.println "  Prints the fullpath of the paths"
    outstream.println "  If no paths are given as args, it will read them from stdin"
    outstream.println ""
    outstream.println "  If there is only one path, the trailing newline is omitted"
    outstream.println ""
    outstream.println "  The -c flag will copy the results into your pasteboard"
  end

  def help?
    @print_help
  end

  def copy?
    @copy_output
  end

  def read_nonempty_lines (instream:InputStream)
    reader = BufferedReader.new InputStreamReader.new instream
    lines  = []
    loop do
      line = reader.readLine
      break         if !line
      lines << line if line != ""
    end
    lines
  end
end
