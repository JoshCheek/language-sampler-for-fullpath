def self.cucumber(lang)
  original_path = ENV['PATH']
  puts "\e[35m=====  #{lang}  =====\e[39m"
  ENV['FULLPATH_DIR'] = File.expand_path(lang, __dir__)
  sh 'bundle exec cucumber -t ~@not-implemented'
end

desc 'Build / test all languages'
task :default


# =====  Go  =====
task default: :golang
task go: :golang # shorthand

desc 'Build / test fullpath in Go'
task(golang: 'golang/fullpath') { cucumber 'golang' }
file 'golang/fullpath' => 'golang/fullpath.go' do
  sh 'go', 'build', '-o', 'golang/fullpath', 'golang/fullpath.go'
end


# =====  Ruby  =====
task default: :ruby

desc 'Test fullpath in Ruby'
task(:ruby) { cucumber 'ruby' }


# =====  Elixir  =====
task default: :elixir

desc 'Build / test fullpath in Elixir'
task(elixir: 'elixir/fullpath') { cucumber 'elixir' }
file 'elixir/fullpath' do # TODO: if we declare deps, can avoid rebuilding every time we run tests
  cd('elixir') { sh 'mix', 'escript.build' }
end


# =====  Common Lisp  =====
task default: :cl

desc 'Build / test fullpath in Common Lisp'
task(cl: 'common_lisp/fullpath') { cucumber 'common_lisp' }

file 'common_lisp/fullpath' => 'common_lisp/fullpath.lisp' do
  sh 'sbcl',
       '--load', 'common_lisp/fullpath.lisp',
       '--eval', %[(save-lisp-and-die
                     "common_lisp/fullpath"
                     :executable           t
                     :toplevel             'main
                     :save-runtime-options t)],
       '--end-toplevel-options'
end


# =====  Kotlin  =====
task default: :kotlin

desc 'Build / test fullpath in Kotlin'
task(kotlin: 'kotlin/fullpath') { cucumber 'kotlin' }

file 'kotlin/fullpath' => 'kotlin/fullpath.jar' do
  sh 'echo "#!/usr/bin/env java -jar" >  kotlin/fullpath'
  sh 'cat kotlin/fullpath.jar         >> kotlin/fullpath'
  chmod '+x', 'kotlin/fullpath'
end

file 'kotlin/fullpath.jar' => 'kotlin/fullpath.kt' do
  sh 'kotlinc',
     '-include-runtime',
     '-d', 'kotlin/fullpath.jar',
     'kotlin/fullpath.kt'
end


# =====  Python  =====
task default: :python

desc 'Test fullpath in Ruby'
task(:python) { cucumber 'python' }


# =====  Java  =====
task default: :java

desc 'Build / test fullpath in Java'
task(java: 'java/fullpath') { cucumber 'java' }
file 'java/fullpath' => 'java/Fullpath.java' do
  cd 'java' do
    rm_r 'build'
    mkdir_p 'build/META-INF'
    File.write 'build/META-INF/MANIFEST.MF', "Manifest-Version: 1.0\n"+
                                             "Created-By: Josh Cheek\n"+
                                             "Main-Class: Fullpath\n"
    sh 'javac', '-d', "build", "-Xlint:unchecked", "Fullpath.java"
    cd('build') { sh 'jar', 'cvmf', 'META-INF/MANIFEST.MF', 'Fullpath.jar', *FileList['*.class'] }
    sh 'echo "#!/usr/bin/env java -jar" >  fullpath'
    sh 'cat build/Fullpath.jar          >> fullpath'
    chmod '+x', 'fullpath'
  end
end


# =====  Bash  =====
task default: :bash

desc 'Test fullpath in Bash'
task(:bash) { cucumber 'bash' }


# =====  C  =====
task default: :c

desc 'Test fullpath in Bash'
task(c: 'c/fullpath') { cucumber 'c' }
file 'c/fullpath' => 'c/fullpath.c' do
  sh 'gcc', 'c/fullpath.c', '-o', 'c/fullpath'
end

# =====  C  =====
task default: :clojure
clojure_jarfile = 'clojure/target/fullpath-0.1.0-SNAPSHOT-standalone.jar'

desc 'Build / test fullpath in Clojure'
task(clojure: 'clojure/fullpath') { cucumber 'clojure' }

file 'clojure/fullpath' => clojure_jarfile do
  sh 'echo "#!/usr/bin/env java -jar" >  clojure/fullpath'
  sh "cat #{clojure_jarfile}          >> clojure/fullpath"
  chmod '+x', 'clojure/fullpath'
end

file clojure_jarfile => 'clojure/src/fullpath/core.clj'
file clojure_jarfile => 'clojure/project.clj'
file clojure_jarfile do
  cd('clojure') { sh 'lein', 'uberjar' }
end


# =====  JavaScript  =====
task default: :javascript

desc 'Test fullpath in JavaScript'
task(:javascript) { cucumber 'javascript' }
task js: :javascript


# =====  PHP  =====
task default: :php

desc 'Test fullpath in PHP'
task(:php) { cucumber 'php' }


# =====  Haskell  =====
task default: :haskell
task hs: :haskell

desc 'Build / test fullpath in Haskell'
task(haskell: 'haskell/fullpath') { cucumber 'haskell' }
file 'haskell/fullpath' => ['haskell/src/Main.hs', 'haskell/Fullpath.cabal'] do
  chdir 'haskell' do
    sh 'cabal', 'install', '-j'
    cp '.cabal-sandbox/bin/Fullpath', '.'
  end
end


# =====  Haxe  =====
task default: :haxe

desc 'Build / test fullpath in Haxe'
task(haxe: 'haxe/fullpath') { cucumber 'haxe' }
file 'haxe/fullpath' => 'haxe/Fullpath.hx' do
  chdir 'haxe' do
    sh 'haxe', '-main', 'Fullpath', '-cpp', 'cpp'
    sh 'cp', 'cpp/Fullpath', 'fullpath'
  end
end


# =====  Fish  =====
task default: :fish

desc 'Test fullpath in Fish'
task(:fish) { cucumber 'fish' }


# =====  Rust  =====
task default: :rust

desc 'Build / Test fullpath in Rust'
task(rust: 'rust/fullpath')  { cucumber 'rust' }
file 'rust/fullpath' => 'rust/src/main.rs' do
  chdir 'rust' do
    sh 'cargo', 'build'
    cp 'target/debug/fullpath', 'fullpath'
    # # this is how to build the release version (takes longer to compile, but more optimized code)
    # sh 'cargo', 'build', '--release'
    # cp 'target/release/fullpath', 'fullpath'
  end
end


# =====  C Sharp  =====
task default: :c_sharp

desc 'Build / Test fullpath in C Sharp'
task(c_sharp: 'c_sharp/fullpath')  { cucumber 'c_sharp' }
file 'c_sharp/fullpath' => 'c_sharp/fullpath.exe' do
  touch 'c_sharp/fullpath'
end
file 'c_sharp/fullpath.exe' => 'c_sharp/fullpath.cs' do
  sh 'mcs', '-out:c_sharp/fullpath.exe', 'c_sharp/fullpath.cs'
end


# =====  Crystal  =====
desc 'Build / Test fullpath for Crystal'
task(crystal: 'crystal/fullpath') { cucumber 'crystal' }
task 'crystal/fullpath' => 'crystal/fullpath.cr' do
  sh 'crystal', 'build', 'crystal/fullpath.cr',
                         '-o', 'crystal/fullpath',
                         '--release'
end


# =====  Julia  =====
desc 'Test fullpath for Julia'
task(:julia) { cucumber 'julia' }

# =====  Scheme  =====
desc 'Build / Test fullpath for Scheme'
task(scheme: 'scheme/fullpath') { cucumber 'scheme' }
task 'scheme/fullpath' => 'scheme/fullpath.scm' do
  sh 'csc', 'scheme/fullpath.scm'
end

# =====  Swift  =====
def self.macosx_sdk_path
  @macosx_sdk_path ||= `xcrun --show-sdk-path --sdk macosx`.chomp
end

desc 'Build / Test fullpath for Swift'
task(swift: 'swift/fullpath') { cucumber 'swift' }
task 'swift/fullpath' => 'swift/fullpath.swift' do
  sh 'swiftc', '-o', 'swift/fullpath',
               '-sdk', macosx_sdk_path,
               'swift/Fullpath.swift'
end
