def self.cucumber(lang)
  original_path = ENV['PATH']
  puts "\e[35m=====  #{lang}  =====\e[39m"
  ENV['FULLPATH_DIR'] = File.expand_path(lang, __dir__)
  sh 'bundle exec cucumber'
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
