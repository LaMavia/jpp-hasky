#!/usr/bin/env fish

function test_file
  set -l f $argv[1]

  set_color --bold cyan
  printf '>>> '
  set_color --bold white
  printf '%s... ' "$f"
  set_color normal

  if not cat $f | cabal run test 1> /dev/null;
    set_color --bold red
    printf '<<< '
    set_color --bold magenta
    printf 'failed example %s\n' "$f"
    exit 1
  end

  set_color --bold green
  printf 'OK\n'
  set_color normal
end

set dir (dirname (status --current-file))

if not cabal build;
  exit 1
end

if test (count $argv) -gt 1
  for f in $argv;
    test_file $f
  end
else
  for f in $dir/examples/$argv[1]/*;
    test_file $f
  end
end


