def self.cucumber(lang)
  original_path = ENV['PATH']
  puts "\e[35m=====  #{lang}  =====\e[39m"
  ENV['FULLPATH_DIR'] = File.expand_path(lang, __dir__)
  sh 'bundle exec cucumber'
end

desc 'Build / test all languages'
task :default


desc 'Build / test fullpath in Go'
task golang: 'golang/fullpath' do
  cucumber 'golang'
end
file 'golang/fullpath' do
  sh 'go', 'build', '-o', 'golang/fullpath', 'golang/fullpath.go'
end
task go: :golang # shorthand
task default: :golang


desc 'Test fullpath in Ruby'
task :ruby do
  cucumber 'ruby'
end
task default: :ruby


desc 'Build / test fullpath in Elixir'
task elixir: 'elixir/fullpath' do
  cucumber 'elixir'
end
file 'elixir/fullpath' do
  cd('elixir') { sh 'mix', 'escript.build' }
end
task default: :elixir


desc 'Test fullpath in Common Lisp'
task(:cl) { cucumber 'common_lisp' }
task default: :cl
